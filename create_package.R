## https://adv-r.hadley.nz/

library(usethis)
library(devtools)

pkg_name <- "mypkg"
pkg_dir <- path.expand("~/Documents")
path_to_pkg <- file.path(pkg_dir, pkg_name)
pkg_fields <- list()
pkg_fields[["Package"]] <- pkg_name
pkg_fields[["Version"]] <- "0.1.0"
pkg_fields[["Title"]] <- "My First package"
pkg_fields[["Description"]] <- "This package is my first one."
pkg_fields[["Authors@R"]] <- "person('Timothee','Flutre','','timothee.flutre@inrae.fr',c('aut','cre'),comment=c(ORCID='0000-0003-4489-4782'))"
pkg_fields[["License"]] <- "AGPL-3"
usethis::create_package(path=path_to_pkg, fields=pkg_fields)

devtools::document(pkg=path_to_pkg)
built_pkg <- devtools::build(pkg=path_to_pkg)
devtools::check(pkg=path_to_pkg)
devtools::install_local(path=built_pkg)

dir.create(path=file.path(path.to.pkg, "misc"))
ignore.file <- file.path(path.to.pkg, ".Rbuildignore")
cat("^misc$", file=ignore.file, sep="\n")

setwd(path.to.pkg)
usethis::use_package("ggplot2")
usethis::use_package_doc()
usethis::use_testthat()
devtools::test(pkg=path.to.pkg)
usethis::use_vignette("Example")
