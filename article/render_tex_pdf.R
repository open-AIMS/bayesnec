library(tidyverse)
rm(list = ls())
source("R/pred_equations.R")

modify_tex <- function(file_in, file_out) {
  tex <- readLines(file_in)
  author_ands <- grep("\\\\AND", tex, ignore.case = TRUE)
  if (Sys.info()["sysname"] == "Windows") {
    for (i in seq_along(author_ands)) {
      if (i %in% c(1, 3)) {
        tex[author_ands[i]] <- gsub("\\\\AND", "\\\\And", tex[author_ands[i]])
      } else {
        tex[author_ands[i]] <- gsub("\\\\And", "\\\\AND", tex[author_ands[i]])
      }
    }
  } else {
    tex[author_ands] <- gsub("AND Diego", "And Diego", tex[author_ands]) |>
      gsub("And Gerard", "AND Gerard", x = _) |>
      gsub("AND David", "And David", x = _)
  }
  tab_begs <- grep("^\\\\begin\\{table", tex)
  tab_caps <- grep("^\\\\caption\\{", tex)
  tar_caps <- sapply(tab_begs, function(x)which((tab_caps - x) == 2))
  tab_caps <- tab_caps[tar_caps]
  cap_txt  <- tex[tab_caps]
  tex <- tex[-tab_caps]
  end_tabs <- grep("^\\\\end\\{tabular", tex)
  for (i in seq_along(tab_begs)) {
    tmp_tab <- gsub("Ametryn", "Ame.", tex[tab_begs[i]:end_tabs[i]]) |>
      gsub("Atrazine", "Atr.", x = _) |>
      gsub("Diuron", "Diu.", x = _) |>
      gsub("Hexazinone", "Hex.", x = _) |>
      gsub("Irgarol", "Irg.", x = _) |>
      gsub("Simazine", "Sim.", x = _) |>
      gsub("Tebuthiuron", "Teb.", x = _)
    tex[tab_begs[i]:end_tabs[i]] <- tmp_tab
  }
  beg_cen_cap <- "\\captionsetup{justification=centering}"
  tex <- c(tex[1:end_tabs[1]], beg_cen_cap, cap_txt[1],
           tex[(end_tabs[1] + 1):end_tabs[2]], beg_cen_cap,
           cap_txt[2], tex[(end_tabs[2] + 1):length(tex)])
  tex <- gsub("e.g.,", "\\eg", tex, fixed = TRUE) |>
    gsub("e.g.", "\\eg", x = _, fixed = TRUE) |>
    gsub("i.e.,", "\\ie", x = _, fixed = TRUE) |>
    gsub("i.e.", "\\ie", x = _, fixed = TRUE) |>
    gsub("\\textasciitilde{}\\ref{", "~\\ref{", x = _, fixed = TRUE) |>
    gsub("\\textless\\textasciitilde", "\\textless~", x = _, fixed = TRUE) |>
    gsub("\\textasciitilde package", "~package", x = _, fixed = TRUE) |>
    gsub("\\textasciitilde Mb", "~Mb", x = _, fixed = TRUE) |>
    gsub("bot + (top - bot + exp(slope) * x)/(1 + exp(exp(beta)",
         "bot + (top - bot + exp(slope) * x) / (1 + exp(exp(}\n\\linebreak \\code{beta)", x = _, fixed = TRUE)
  writeLines(tex, file_out)
  tex
}

make_clean_pdf <- function(file_in, file_out) {
  tinytex::xelatex(file_in, pdf_file = file_out)
}

make_plot_data <- function(model_name, args_, x) {
  fct <- get(model_name)
  my_args <- c(
    sapply(args_[[model_name]], get, envir = sys.frame(sys.parent(0)),
           simplify = FALSE),
    list(x = x)
  )
  if (model_name == "pred_ecxhormebc5") {
    my_args$b_slope <- 1
  }
  data.frame(x = x, y = do.call(fct, my_args))
}

all_args <- sapply(ls(), function(x) {
  (deparse(args(get(x)))[1]) %>%
    gsub("function \\(|x\\) ", "", .) %>%
    gsub("function \\(|\\)", "", .) %>%
    strsplit(., ", ", fixed = TRUE) %>%
    `[[`(1)
})

x_vec <- seq(0.01, 0.99, 0.01)
b_top <- 1
b_bot <- -1
b_ec50 <- mean(x_vec)
b_beta <- 2
b_slope <- 0
b_f <- 0
b_d <- 0.4
b_nec <- quantile(x_vec, 0.3)

model_names <- names(all_args)
names(model_names) <- model_names

ecx_levels <- c(
  "ecxlin", "excexp", "ecxexp", "ecxsigm", "ecx4param", "ecxwb1", "ecxwb1p3",
  "ecxwb2", "ecxwb2p3", "ecxll5", "ecxll4", "ecxll3", "ecxhormebc5",
  "ecxhormebc4"
)
ecx_data <- purrr::map_dfr(
  grep("^pred_ecx", model_names, value = TRUE), make_plot_data,
  args_ = all_args, x = x_vec, .id = "fct"
) %>%
  dplyr::mutate(
    fct = gsub("pred_", "", fct, fixed = TRUE) |>
      factor(x = _, levels = .env$ecx_levels)
  )

ecx_plots <- ggplot(data = ecx_data, mapping = aes(x, y)) +
  geom_line(colour = "darkgrey", linetype = 1) +
  facet_wrap(~fct, ncol = 4, scales = "free") +
  labs(x = "Concentration (predictor)", y = "Response", title = "ecx models") +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())
ggsave("article/vignette-fig-exmp2b-theoretical_ecx_curves.pdf",
       ecx_plots, width = 7.8, height = 6.4)

nec_levels <- c(
  "neclin", "nec3param", "nec4param", "nechorme", "nechormepwr", "neclinhorme",
  "nechorme4", "nechorme4pwr", "nechormepwr01", "necsigm"
)
nec_data <- purrr::map_dfr(
  grep("^pred_nec", model_names, value = TRUE), make_plot_data,
  args_ = all_args, x = x_vec, .id = "fct"
) %>%
  dplyr::mutate(
    fct = gsub("pred_", "", fct, fixed = TRUE) |>
      factor(x = _, levels = .env$nec_levels)
  )

nec_plots <- ggplot(data = nec_data, mapping = aes(x, y)) +
  geom_line(colour = "darkgrey", linetype = 1) +
  facet_wrap(~fct, ncol = 4, scales = "free") +
  labs(x = "Concentration (predictor)", y = "Response", title = "nec models") +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())
ggsave("article/vignette-fig-exmp2b-theoretical_nec_curves.pdf",
       nec_plots, width = 7.8, height = 4.88)

rmarkdown::render("article/article.Rmd")
out <- modify_tex("article/article.tex", "article/article_mod.tex")
system("cp article/article_mod.tex article/paper.tex")
cwd <- getwd()
setwd("article")
make_clean_pdf("article_mod.tex", "article.pdf")
setwd(cwd)
knitr::purl(input = "article/article.Rmd",
  output = "article/code_new.R",documentation = 0
)
