.onAttach <- function(libname, pkgname) {
  install_knitr_hooks()
  learnr:::remove_knitr_hooks()
  options(STAGE = Sys.getenv("STAGE", "dev"))

  assets_url <- Sys.getenv("ASSETS_URL")
  if (assets_url == "") {
    options(ASSETS_URL = "/assets/courses")
  } else {
    options(ASSETS_URL = assets_url)
  }
}

# remove knitr hooks when package is detached from search path
.onDetach <- function(libpath) {
  remove_knitr_hooks()
}
