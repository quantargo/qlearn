#' @import rmarkdown
#' @export
qlearn <- function(toc = FALSE, toc_depth = 3, toc_float = FALSE, number_sections = FALSE,
                             section_divs = TRUE, fig_width = 7, fig_height = 5, fig_retina = 2,
                             fig_caption = TRUE, dev = "png", df_print = "default", code_folding = c("none",
                                                                                                     "show", "hide"),
                             code_download = FALSE, smart = TRUE,
                             self_contained = FALSE, theme = NULL, highlight = NULL,
                             mathjax = NULL, template = rmarkdown:::rmarkdown_system_file("rmd/fragment/default.html"),
                             extra_dependencies = NULL,
                             css = NULL, includes = NULL, keep_md = TRUE, lib_dir = NULL,
                             md_extensions = NULL, pandoc_args = "--mathjax",
                             ...)  {

  args <- c("--standalone")

  if (section_divs)
    args <- c(args, "--section-divs")
  args <- c(args, pandoc_toc_args(toc, toc_depth))
  md_extensions <- rmarkdown:::smart_extension(smart, md_extensions)
  if (toc && !identical(toc_float, FALSE)) {
    if (is.null(theme))
      stop("You must use a theme when specifying the 'toc_float' option")
    toc_float_options <- list(collapsed = TRUE, smooth_scroll = TRUE,
                              print = TRUE)
    if (is.list(toc_float)) {
      toc_float_options <- merge_lists(toc_float_options,
                                       toc_float)
      toc_float <- TRUE
    }
    else if (!isTRUE(toc_float)) {
      stop("toc_float must be a logical or a list with options")
    }
    extra_dependencies <- append(extra_dependencies, list(html_dependency_jquery(),
                                                          html_dependency_jqueryui(), html_dependency_tocify()))
    args <- c(args, pandoc_variable_arg("toc_float", "1"))
    selectors <- paste0("h", seq(1, toc_depth), collapse = ",")
    args <- c(args, pandoc_variable_arg("toc_selectors",
                                        selectors))
    if (toc_float_options$collapsed)
      args <- c(args, pandoc_variable_arg("toc_collapsed",
                                          "1"))
    if (toc_float_options$smooth_scroll)
      args <- c(args, pandoc_variable_arg("toc_smooth_scroll",
                                          "1"))
    if (toc_float_options$print)
      args <- c(args, pandoc_variable_arg("toc_print",
                                          "1"))
  }
  if (identical(template, "default"))
    args <- c(args, "--template", pandoc_path_arg(rmarkdown_system_file("rmd/h/default.html")))
  else if (!is.null(template))
    args <- c(args, "--template", pandoc_path_arg(template))
  code_folding <- match.arg(code_folding)
  if (!is.null(theme)) {
    code_menu <- !identical(code_folding, "none") || code_download
    source_embed <- code_download
    extra_dependencies <- append(extra_dependencies, list(html_dependency_jquery(),
                                                          html_dependency_navigation(code_menu = code_menu,
                                                                                     source_embed = source_embed)))
  }
  args <- c(args, rmarkdown:::pandoc_html_highlight_args(template, highlight))
  if (identical(template, "default") && is_highlightjs(highlight)) {
    extra_dependencies <- append(extra_dependencies, list(html_dependency_highlightjs(highlight)))
  }
  if (number_sections)
    args <- c(args, "--number-sections")
  for (css_file in css) args <- c(args, "--css", pandoc_path_arg(css_file))
  exit_actions <- list()
  on_exit <- function() {
    for (action in exit_actions) try(action())
  }
  source_code <- NULL
  source_file <- NULL
  pre_knit <- function(input, ...) {
    if (code_download) {
      source_file <<- basename(input)
      source_code <<- paste0("<div id=\"rmd-source-code\">",
                             base64enc::base64encode(input), "</div>")
    }
  }
  if (identical(df_print, "paged")) {
    extra_dependencies <- append(extra_dependencies, list(html_dependency_pagedtable()))
  }
  post_knit <- function(metadata, input_file, runtime, encoding,
                        ...) {
    args <- c()
    if (!is.null(theme)) {
      navbar <- file.path(normalize_path(dirname(input_file)),
                          "_navbar.html")
      if (!file.exists(navbar)) {
        navbar_yaml <- file.path(dirname(navbar), "_navbar.yml")
        if (file.exists(navbar_yaml))
          navbar <- navbar_html_from_yaml(navbar_yaml)
        config <- site_config(input_file, encoding)
        if (!is.null(config) && !is.null(config$navbar))
          navbar <- navbar_html(config$navbar)
      }
      if (file.exists(navbar)) {
        includes <- list(before_body = navbar)
        args <- c(args, includes_to_pandoc_args(includes,
                                                filter = if (is_shiny_classic(runtime)) function(x) normalize_path(x,
                                                                                                                   mustWork = FALSE) else identity))
        args <- c(args, pandoc_variable_arg("navbar",
                                            "1"))
        args <- c(args, pandoc_body_padding_variable_args(theme))
        iconDeps <- navbar_icon_dependencies(navbar)
        if (length(iconDeps) > 0)
          knitr::knit_meta_add(list(iconDeps))
      }
    }
    args
  }
  pre_processor <- function(metadata, input_file, runtime,
                            knit_meta, files_dir, output_dir) {
    if (is.null(lib_dir))
      lib_dir <- files_dir
    args <- c()
    code_menu <- FALSE
    if (code_folding %in% c("show", "hide")) {
      if (is.null(theme))
        stop("You must use a theme when specifying the 'code_folding' option")
      args <- c(args, pandoc_variable_arg("code_folding",
                                          code_folding))
      code_menu <- TRUE
    }
    if (code_download) {
      if (is.null(theme))
        stop("You must use a theme when specifying the 'code_download' option")
      args <- c(args, pandoc_variable_arg("source_embed",
                                          source_file))
      sourceCodeFile <- tempfile(fileext = ".html")
      write_utf8(source_code, sourceCodeFile)
      args <- c(args, pandoc_include_args(after_body = sourceCodeFile))
      code_menu <- TRUE
    }
    if (code_menu)
      args <- c(args, pandoc_variable_arg("code_menu",
                                          "1"))
    args <- c(args, includes_to_pandoc_args(includes, filter = if (is_shiny_classic(runtime)) function(x) normalize_path(x,
                                                                                                                         mustWork = FALSE) else identity))
    args
  }

  output_format(knitr = knitr_options_html(fig_width, fig_height,
                                           fig_retina, keep_md, dev),
                pandoc = pandoc_options(to = "html",
                                        from = from_rmarkdown(fig_caption, md_extensions),
                                        args = args),
                keep_md = keep_md,
                clean_supporting = self_contained,
                df_print = df_print,
                pre_knit = pre_knit,
                post_knit = post_knit,
                pre_processor = pre_processor,
                on_exit = on_exit,
                base_format = html_document_base( smart = smart,
                                                  theme = theme, self_contained = self_contained, lib_dir = lib_dir,
                                                  mathjax = mathjax, template = template, pandoc_args = pandoc_args,
                                                  extra_dependencies = extra_dependencies,
                                                  ...))
}
