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
  writeLines(tex, file_out)
  tex
}

make_clean_pdf <- function(file_in, file_out) {
  tinytex::xelatex(file_in, pdf_file = file_out)
}

rmarkdown::render("article/article.Rmd")
modify_tex("article/article.tex", "article/article_mod.tex")
cwd <- getwd()
setwd("article")
make_clean_pdf("article_mod.tex", "article.pdf")
setwd(cwd)
