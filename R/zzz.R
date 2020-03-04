.onAttach <- function(libname, pkgname) {
  install_knitr_hooks()
  learnr:::remove_knitr_hooks()
  options(STAGE = Sys.getenv("STAGE", "dev"))
  if (options("STAGE") == "dev") {
    options(ASSETS_URL = "https://next.quantargo.com/assets/courses")
  } else if (options("STAGE") == "prod") {
    options(ASSETS_URL = "https://www.quantargo.com/assets/courses")
  }
}

# remove knitr hooks when package is detached from search path
.onDetach <- function(libpath) {
  remove_knitr_hooks()
}
