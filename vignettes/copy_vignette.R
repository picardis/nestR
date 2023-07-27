# This function knits the vignette to the directory "inst/doc" as suggested
# by Mathieu Basille. This makes the built PDF available on GitHub prior
# to package installation.

rmarkdown::render("vignettes/nestR.Rmd", output_format = "pdf_document",
                  output_dir = "inst/doc/", output_options = list(toc = "true"))
