## Tim's configuration file for R
## Versioning: https://github.com/timflutre/perso

Sys.setenv(LANGUAGE="en")

options(repos=c(CRAN="http://ftp.igh.cnrs.fr/pub/CRAN/"))

## Machine specific environment
if(Sys.getenv("COMPUTERNAME") == ""){ # should be defined in .bashrc
  warning("can't find environment variable COMPUTERNAME in .Rprofile")
} else{
  libs.user <- c("~/lib/R")
  if(Sys.getenv("COMPUTERNAME") == "laptop-pro"){
    R.v.min.1 <- as.numeric(strsplit(R.version$minor, "\\.")[[1]][1])
    libs.user <- c(libs.user, paste0("~/lib/R/", R.version$platform,
                                     "-library", "/", R.version$major, ".",
                                     R.v.min.1))
  } else if(Sys.getenv("COMPUTERNAME") == "urgi"){
    libs.user <- c(libs.user, "/home/fruitselgen/lib/R")
  }
  .libPaths(libs.user)
}
