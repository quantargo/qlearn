#' Create Progress image for chapter
#'
#' @importFrom magick image_read image_annotate image_composite image_write image_read_svg image_scale image_info
#' @importFrom  magrittr "%>%"
#' @importFrom stringr str_locate_all str_trim str_sub
#' @export
course_image <- function(title,
                           file_out,
                           line_max_char = 24,
                           file_badge = "badge.svg",
                           file_course = "main/course.svg") {

  img_bg = image_read(system.file("og-image-background-empty.png", package = "qlearn", mustWork = TRUE))
  img_bg_height <- image_info(img_bg)[1, c("height"), drop=TRUE]
  img_bg_width <- image_info(img_bg)[1, c("width"), drop=TRUE]
  img_course = image_read_svg(file_course)

  font_size <- 85
  badge_width <- 170
  course_image_width <- 200
  badge_gap_bottom <- 80
  offset_y <- img_bg_height - (badge_width + badge_gap_bottom)
  gap_x <- 100
  course_image_gapx <- 50

  badge_block_width <- (gap_x * (length(file_badge)-1)) + badge_width
  badge_block_width <- badge_block_width + course_image_width + course_image_gapx
  offset_x <- round((img_bg_width + badge_block_width) / 2) - badge_width

  img <- img_bg
  for (i in length(file_badge):1) {
    img_badge <- image_read_svg(file_badge[i])
    offset <- sprintf("+%d+%d", offset_x, offset_y)
    img <- img %>%
      image_composite(image_scale(img_badge, sprintf("%sx", badge_width)), offset = offset)
    offset_x <- offset_x - gap_x
  }
  offset_course_image_x <- offset_x + gap_x - course_image_width - course_image_gapx

  img <- img %>%
    image_composite(image_scale(img_course, sprintf("%sx", course_image_width)),
                    offset = sprintf("+%d+%d", offset_course_image_x, offset_y))

  if (nchar(title) > line_max_char) {
    loc <- str_locate_all(title, "\\s+")[[1]][, "end"]
    split_candidates <- seq(line_max_char, nchar(title), by = line_max_char)
    split <- loc[findInterval(split_candidates, loc)]

    title <- str_trim(str_sub(title, c(1, split), c(split, nchar(title))))
  }

  char_width <- 40
  line_gap <- 5
  progress_gap <- 150

  block_middle_height <- length(title) * (font_size + line_gap) + progress_gap

  offset_ti_y <- (img_bg_height - block_middle_height) / 2

  for (ti in title) {
    offset_title_x <- (img_bg_width - nchar(ti) * char_width) / 2
    offset_ti <- sprintf("+%d+%d", offset_title_x, offset_ti_y)
    img <- image_annotate(img, ti,
                  color = "white",
                  font = "Inter Bold",
                  size = font_size,
                  weight = 700,
                  location = offset_ti)
    offset_ti_y <- offset_ti_y + font_size + line_gap
  }
  img


  #img # interactive print to plot window
  image_write(img, path = file_out, format = "png")
  file_out
}

#' Export Chapter file as image
#' @export
course_main_image <- function(dname = getwd()) {
  idx_files <- grep("^[0-9]{2}", list.files(path = dname, pattern = "index.yml", recursive = TRUE), value=TRUE)
  file_badge <- sapply(idx_files, function(x) file.path(dirname(x), yaml::read_yaml(x)$image), USE.NAMES = FALSE)
  idx_course <- yaml::read_yaml("index.yml")
  title <- idx_course$title
  file_out <- idx_course$ogImage
  file_course <- idx_course$image

  dir.create(dirname(file_out), recursive = TRUE, showWarnings = FALSE)
  course_image(title,
               file_out,
               line_max_char = 24,
               file_badge = file_badge,
               file_course = file_course)
}

