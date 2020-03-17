.onAttach <- function(libname, pkgname) {
  install_knitr_hooks()
  learnr:::remove_knitr_hooks()
  options(STAGE = Sys.getenv("STAGE", "dev"))

  assets_url <- Sys.getenv("ASSETS_URL")
  if (assets_url == "") {
    if (options("STAGE") == "dev") {
      options(ASSETS_URL = "https://next.quantargo.com/assets/courses")
    } else if (options("STAGE") == "prod") {
      options(ASSETS_URL = "https://www.quantargo.com/assets/courses")
    }
  } else {
    options(ASSETS_URL = assets_url)
  }
}

# remove knitr hooks when package is detached from search path
.onDetach <- function(libpath) {
  remove_knitr_hooks()
}
