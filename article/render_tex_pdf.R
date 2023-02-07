modify_tex <- function(file_in, file_out) {
  tex <- readLines(file_in)
  author_ands <- grep("\\\\AND", tex, ignore.case = TRUE)
  tex[author_ands] <- gsub("AND Diego", "And Diego", tex[author_ands]) %>%
    gsub("And Gerard", "AND Gerard", .) %>%
    gsub("AND David", "And David", .)
  tab_begs <- grep("^\\\\begin\\{table", tex)
  tab_caps <- grep("^\\\\caption\\{", tex)
  tar_caps <- sapply(tab_begs, function(x)which((tab_caps - x) == 2))
  tab_caps <- tab_caps[tar_caps]
  cap_txt  <- tex[tab_caps]
  tex <- tex[-tab_caps]
  end_tabs <- grep("^\\\\end\\{tabular", tex)
  tex <- c(tex[1:end_tabs[1]], cap_txt[1], tex[(end_tabs[1] + 1):end_tabs[2]],
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
