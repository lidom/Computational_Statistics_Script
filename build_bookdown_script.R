bookdown::render_book(input = "index.Rmd")
bookdown::render_book(input = "index.Rmd", output_format = "bookdown::pdf_book")
## 
knitr::purl("02-EM-Algorithmus.Rmd",     output = "RCodes/1_EM-Algorithmus.R")
knitr::purl("03-Linear-Models-Regr.Rmd", output = "RCodes/2_LinRegrModels.R")
