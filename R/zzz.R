.onAttach <- function(libname, pkgname) {
  install_knitr_hooks()
  #initialize_tutorial()
}

# remove knitr hooks when package is detached from search path
.onDetach <- function(libpath) {
  remove_knitr_hooks()
}
