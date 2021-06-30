#' Get Image path of QBit
#'
#' @param img character; File name of image.
#' @param stage character; Stage, either 'dev', or 'prod'
#' @export
getImagePath <- function(img, stage = getOption("STAGE")) {
  prefix <- if (stage == "dev") "next" else "www"
  sprintf("https://%s.quantargo.com/qbit/img/%s", prefix, img)
}
