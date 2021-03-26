#' Create Progress image for chapter
#'
#' @importFrom magick image_read image_annotate image_composite image_write image_read_svg image_scale image_info
#' @importFrom  magrittr "%>%"
#' @importFrom stringr str_locate_all str_trim str_sub
#' @examples
#' \dontrun{
#' progress_image("R is everywhere", 1, 6, "out.png")
#' progress_image("Use existing functions and data through packages", 5, 6, "out.png")
#' progress_image("Extract or replace columns in a data frame using `$`", 5, 6, "out.png")
#' }
#' @export
progress_image <- function(title,
                           idx_doing,
                           idx_total,
                           file_out,
                           line_max_char = 24,
                           file_badge = "badge.svg") {

  img_badge <- image_read_svg(file_badge)

  img_bg = image_read(system.file("og-image-background-logo.png", package = "qlearn", mustWork = TRUE))
  img_bg_height <- image_info(img_bg)[1, c("height"), drop=TRUE]
  img_progress_doing <- image_read(system.file("Recipe-progress-indicator-doing.png", package = "qlearn", mustWork = TRUE))
  img_progress_doing_height <- image_info(img_progress_doing)[1, c("height"), drop=TRUE]
  img_progress_done <- image_read(system.file("Recipe-progress-indicator-done.png", package = "qlearn", mustWork = TRUE))
  img_progress_not_done <- image_read(system.file("Recipe-progress-indicator-not-done.png", package = "qlearn", mustWork = TRUE))

  img <- img_bg %>%
    image_composite(image_scale(img_badge, "200x"), offset = "+84+198")

  if (nchar(title) > line_max_char) {
    loc <- str_locate_all(title, "\\s+")[[1]][, "end"]
    split_candidates <- seq(line_max_char, nchar(title), by = line_max_char)
    split <- loc[findInterval(split_candidates, loc)]

    title <- str_trim(str_sub(title, c(1, split), c(split, nchar(title))))
  }

  font_size <- 65
  line_gap <- 5
  progress_gap <- 20

  block_middle_height <- length(title) * (font_size + line_gap) +
    progress_gap + img_progress_doing_height

  offset_ti_y <- (img_bg_height - block_middle_height) / 2

  for (ti in title) {
    offset_ti <- sprintf("+345+%d", offset_ti_y)
    img <- image_annotate(img, ti,
                  color = "white",
                  font = "Inter Bold",
                  size = font_size,
                  weight = 700,
                  location = offset_ti)
    offset_ti_y <- offset_ti_y + font_size + line_gap
  }

  progress <- rep(0, idx_total)
  progress[idx_doing] <- 2
  progress[1:idx_doing] <- pmax(progress[1:idx_doing], 1)

  offset_x <- 345
  offset_y <- offset_ti_y + progress_gap
  gap_x <- 101

  for (p in progress) {
    offset <- sprintf("+%d+%d", offset_x, offset_y)
    if (p == 2) {
     img <- image_composite(img, img_progress_doing, offset = offset)
    } else if (p == 1) {
     img <- image_composite(img, img_progress_done, offset = offset)
    } else {
     img <- image_composite(img, img_progress_not_done, offset = offset)
    }
    offset_x <- offset_x + gap_x
  }
  #img # interactive print to plot window
  image_write(img, path = file_out, format = "png")
  file_out
}

#' Export Chapter file as image
#' @export
chapter_progress_image <- function(fname) {
  chapterId <- tail(strsplit(fname, "/")[[1]], 1)
  idx_doing <- as.integer(strsplit(chapterId, '-')[[1]][1])
  file_out <- sprintf("%s.png", chapterId)
  img_path <- sprintf("%s.png", tools::file_path_sans_ext(fname))
  files_rmd_all = list.files(dirname(fname), pattern = "*.Rmd")
  chapter_title <- rmarkdown::yaml_front_matter(fname)$title
  file_badge <- file.path(dirname(fname), "badge.svg")

  progress_image(chapter_title,
                  idx_doing = idx_doing,
                  idx_total = length(files_rmd_all),
                  file_out = img_path,
                  file_badge = file_badge)

  img_path
}
