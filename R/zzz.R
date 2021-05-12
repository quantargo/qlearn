.onAttach <- function(libname, pkgname) {
  install_knitr_hooks()
  learnr:::remove_knitr_hooks()
  stage = Sys.getenv("STAGE", "dev")
  options(STAGE = stage)

  url_prefix <- if(stage == "prod") "https://cdn.quantargo.com" else "https://cdn-next.quantargo.com"
  assets_url <- file.path(url_prefix, Sys.getenv("ASSETS_URL", "assets/courses"))
  options(ASSETS_URL = assets_url)
  options(width=69)

  assign("question", qlearn::question, envir = .GlobalEnv)
  assign("answer", qlearn::answer, envir = .GlobalEnv)
}

# remove knitr hooks when package is detached from search path
.onDetach <- function(libpath) {
  remove_knitr_hooks()
}
