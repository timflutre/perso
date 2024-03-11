## Tim's .Rprofile
## Versioning: https://github.com/timflutre/perso

Sys.setenv(LANGUAGE="en")

options(repos=c(CRAN="https://cloud.r-project.org/"))
local({
  r <- getOption("repos")
  if (is.null(r)) r["CRAN"] <- "https://cloud.r-project.org/"
  r["breedR"] <- "https://famuvie.github.io/breedR"
  options(repos = r)
})
