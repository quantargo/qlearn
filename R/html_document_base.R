#' @import xml2
#' @import jsonlite
#' @importFrom magrittr "%>%"
html_document_base <-
  function (smart = TRUE, theme = NULL, self_contained = FALSE,
            lib_dir = NULL, mathjax = "default", pandoc_args = NULL,
            template = "default", dependency_resolver = NULL, copy_resources = FALSE,
            extra_dependencies = NULL, bootstrap_compatible = FALSE,
            ...) {
    if (is.null(dependency_resolver))
      dependency_resolver <- rmarkdown:::html_dependency_resolver
    args <- c()
    if (smart && !rmarkdown:::pandoc2.0())
      args <- c(args, "--smart")
    args <- c(args, "--email-obfuscation", "none")
    if (self_contained) {
      if (copy_resources)
        stop("Local resource copying is incompatible with self-contained documents.")
      rmarkdown:::validate_self_contained(mathjax)
      args <- c(args, "--self-contained")
    }
    args <- c(args, pandoc_args)
    preserved_chunks <- character()
    output_dir <- ""
    pre_knit <- function(input, ...) {
    }
    post_knit <- function(metadata, input_file, runtime, ...) {
    }
    pre_processor <- function(metadata, input_file, runtime,
                              knit_meta, files_dir, output_dir) {
      args <- c()
      if (is.null(lib_dir))
        lib_dir <<- files_dir
      output_dir <<- output_dir
      if (!is.null(theme)) {
        theme <- match.arg(theme, rmarkdown:::themes())
        if (identical(theme, "default"))
          theme <- "bootstrap"
        args <- c(args, "--variable", paste0("theme:", theme))
      }
      format_deps <- list()
      if (!is.null(theme)) {
        format_deps <- append(format_deps, list(html_dependency_jquery(),
                                                html_dependency_bootstrap(theme)))
      }
      else if (isTRUE(bootstrap_compatible) && rmarkdown:::is_shiny(runtime)) {
        format_deps <- append(format_deps, list(html_dependency_bootstrap("bootstrap")))
      }
      format_deps <- append(format_deps, extra_dependencies)
      extras <- rmarkdown:::html_extras_for_document(knit_meta, runtime,
                                                     dependency_resolver, format_deps)
      args <- c(args, rmarkdown:::pandoc_html_extras_args(extras, self_contained,
                                                          lib_dir, output_dir))
      args <- c(args, rmarkdown:::pandoc_mathjax_args(mathjax, template,
                                                      self_contained, lib_dir, output_dir))
      preserved_chunks <<- rmarkdown:::extract_preserve_chunks(input_file)
      if (rmarkdown:::pandoc2.0() && is.null(metadata$title) && is.null(metadata$pagetitle))
        args <- c(args, "--metadata", paste0("pagetitle=",
                                             input_file))
      args <- c(args, rmarkdown:::pandoc_lua_filters(c("pagebreak.lua",
                                                       "latex-div.lua")))
      args
    }
    intermediates_generator <- function(original_input, encoding,
                                        intermediates_dir) {
      return(rmarkdown:::copy_render_intermediates(original_input, encoding,
                                                   intermediates_dir, !self_contained))
    }
    # call the base html_document function
    post_processor <- function (metadata, input_file, output_file, clean, verbose) {
      if (length(preserved_chunks) == 0 && !isTRUE(copy_resources) &&
          self_contained)
        return(output_file)
      output_str <- rmarkdown:::read_utf8(output_file)
      if (length(preserved_chunks) > 0) {
        for (i in names(preserved_chunks)) {
          output_str <- gsub(paste0("<p>", i, "</p>"), i, output_str,
                             fixed = TRUE, useBytes = TRUE)
          output_str <- gsub(paste0(" id=\"[^\"]*?", i, "[^\"]*?\" "),
                             " ", output_str, useBytes = TRUE)
        }
        output_str <- htmltools::restorePreserveChunks(output_str, preserved_chunks)
      }
      if (copy_resources) {
        output_str <- rmarkdown:::copy_html_resources(rmarkdown:::one_string(output_str),
                                                      lib_dir, output_dir)
      }
      #else if (!self_contained) {
      image_relative <- function(img_src, src) {
        url_prefix <- options("ASSETS_URL")
        img_path <- gsub("#", "/", metadata$tutorial$id, fixed = TRUE)
        if (!startsWith(img_path, "blog")) {
          img_path <- strsplit(img_path, "/")[[1]]
          img_path <- paste(img_path[-length(img_path)], collapse = "/")
        }

        image_prefix <- file.path(url_prefix, img_path)

        in_file <- utils::URLdecode(src)
        if (grepl("^[.][.]", in_file))
          return(img_src)
        if (length(in_file) && file.exists(in_file)) {
          image_path_full <- file.path(image_prefix, src)
          img_src <- sub(src, image_path_full, img_src)

          ext <- tolower(tools::file_ext(in_file))
          IMG <- tryCatch({
            if (ext == "png") png::readPNG(in_file)
            else if (ext == "jpeg" || ext == "jpg") jpeg::readJPEG(in_file)
            else if (ext == "gif") magick::image_read(in_file)
            else NULL
          }, error = function(e) NULL)

          if (!is.null(IMG)) {
            if (ext != "gif") {
              imageDim <- dim(IMG)
            }
            else {
              imageDim <- magick::image_info(IMG)[1, c("width", "height"), drop=TRUE]
            }

            img_src <- paste(img_src, sprintf('width="%s" height="%s"',
                                              imageDim[2], imageDim[1]))
          }
        }
        img_src
      }

      output_str <- rmarkdown:::process_images(output_str, image_relative)
      #}

      #rmarkdown:::write_utf8(output_str, "output_str.html")
      html_doc <- xml2::read_html(paste(output_str, collapse = "\n"))

      # Remove unncecessary nodes
      nodes_shiny_prerendered <- xml2::xml_find_all(html_doc, "//script[@type='application/shiny-prerendered']")
      xml_remove(nodes_shiny_prerendered)
      nodes_html_widgets <- xml2::xml_find_all(html_doc, "//div[starts-with(@id, 'htmlwidget-')]")
      xml_remove(nodes_html_widgets)
      nodes_comment <- xml2::xml_find_all(html_doc, "//comment()")
      xml_remove(nodes_comment)
      nodes_exercise <- xml_find_all(html_doc, ".//div[starts-with(@class, 'tutorial-exercise')]")
      xml_remove(nodes_exercise)

      sections <- xml2::xml_find_all(html_doc, "//div[@class='section level2']")
      contentId <- metadata$tutorial$id
      moduleId <- strsplit(contentId, "#")[[1]][1]

      qid <- 1
      json_out <- list()
      qbit_out <- list()

      # Extract exercises from content and replace with placeholders
      for (s in sections) {
        sectionContents <- list()
        node_title <- xml_find_all(s, ".//h2[1]")
        section_title <- node_title %>% xml_text()
        xml_remove(node_title)

        objExercise <- parse_exercise(s, moduleId, contentId, section_title)
        objQuizOut <- parse_quiz(s, moduleId, contentId, section_title, qid)
        objRecipeOut <- parse_recipe(s, moduleId, contentId, section_title)
        objOut <- NULL
        sectionId <- s %>% xml_attr("id")

        if (!is.null(objExercise)) {
          objOut  <- objExercise
          # Generate Qbit
          qbitName <- sprintf("qbit-%s", paste(contentId, sectionId, sep = "#"))
          tstamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%OS3Z", tz = "UTC")
          code <- sprintf("%s\n", paste(
                    paste("library(%s)", objOut$packagesLoaded, collapse = "\n"),
                    objOut$setup,
                    objOut$solution, collapse = "\n\n"))

          qbitOut <- list(
            contentId = unbox(qbitName),
            contentType = unbox("main"),
            createdBy = unbox("SYSTEM"),
            description = unbox(sectionId),
            lastModified = unbox(tstamp),
            moduleId = unbox(qbitName),
            moduleType = unbox("qbit"),
            title = unbox(objOut$title),
            visibility = unbox("public"),
            code = unbox(code),
            qbitName = unbox(objExercise$qbitName)
          )
          qbit_out[[length(qbit_out) + 1]]  <- qbitOut

          dependencies <- c('askpass-1.1', 'assertthat-0.2.1', 'aws.s3-0.3.20', 'aws.signature-0.5.2', 'backports-1.1.5', 'base64enc-0.1-3', 'BiocManager-1.30.10', 'callr-3.4.2', 'carData-3.0-3', 'checkmate-2.0.0', 'cli-2.0.1', 'coda-0.19-3', 'colorspace-1.4-1', 'crayon-1.3.4', 'curl-4.3', 'desc-1.2.0', 'digest-0.6.25', 'dplyr-0.8.4', 'ellipsis-0.3.0', 'evaluate-0.14', 'fansi-0.4.1', 'farver-2.0.3', 'gapminder-0.3.0', 'GGally-1.4.0', 'ggimage-0.2.7', 'ggplot2-3.2.1', 'ggplotify-0.0.4', 'glue-1.3.1', 'gradethis-0.1.0.9002', 'gridExtra-2.3', 'gridGraphics-0.4-1', 'gtable-0.3.0', 'hexbin-1.28.1', 'hexSticker-0.4.6', 'hms-0.5.3', 'httr-1.4.1', 'jsonlite-1.6.1', 'labeling-0.3', 'lattice-0.20-40', 'lazyeval-0.2.2', 'lifecycle-0.1.0', 'lubridate-1.7.4', 'magick-2.3', 'magrittr-1.5', 'markdown-1.1', 'MASS-7.3-51.5', 'Matrix-1.2-18', 'mgcv-1.8-31', 'mime-0.9', 'munsell-0.5.0', 'network-1.16.0', 'nlme-3.1-144', 'openssl-1.4.1', 'pillar-1.4.3', 'pkgbuild-1.0.6', 'pkgconfig-2.0.3', 'pkgload-1.0.2', 'plyr-1.8.5', 'politicaldata-0.1.3', 'praise-1.0.0', 'prettyunits-1.1.1', 'primes-0.1.0', 'processx-3.4.2', 'progress-1.2.2', 'ps-1.3.2', 'purrr-0.3.3', 'R6-2.4.1', 'RColorBrewer-1.1-2', 'Rcpp-1.0.3', 'reshape-0.8.8', 'reshape2-1.4.3', 'rlang-0.4.4', 'rprojroot-1.3-2', 'rstudioapi-0.11', 'rvcheck-0.1.7', 'rvest-0.3.5', 'scales-1.1.0', 'selectr-0.4-2', 'showtext-0.7-1', 'showtextdb-2.0', 'sna-2.5', 'Stat2Data-2.0.0', 'statnet.common-4.3.0', 'stringdist-0.9.5.5', 'stringi-1.4.6', 'stringr-1.4.0', 'sys-3.3', 'sysfonts-0.8', 'testthat-2.3.1', 'testwhat-4.11.1', 'tibble-2.1.3', 'tidyselect-1.0.0', 'utf8-1.1.4', 'uuid-0.1-2', 'vctrs-0.2.3', 'viridisLite-0.3.0', 'withr-2.1.2', 'xfun-0.12', 'XML-3.99-0.3', 'xml2-1.2.2', 'zoo-1.8-7')
          for (dep in dependencies) {
            moduleId <- "package-amazonlinux-2018.03-r-4.0.0"
            depTo <- sprintf("%s#cran#%s", moduleId, dep)
            depOut <- list(
              contentId = unbox(sprintf("%s_dependency_%s", qbitName, depTo)),
              contentType = unbox("dependency"),
              dependencyFrom = unbox(qbitName),
              dependencyTo = unbox(depTo),
              moduleId = unbox(qbitName)
            )
            qbit_out[[length(qbit_out) + 1]]  <- depOut
          }
        } else if (!is.null(objQuizOut)) {
          qid <- qid + 1
          objOut  <- objQuizOut
        } else if (!is.null(objRecipeOut)) {
          objOut  <- objRecipeOut
        } else {
          objOut <- list(
            moduleId = unbox(moduleId),
            contentId = unbox(paste(contentId, sectionId, sep = "#")),
            title = unbox(section_title),
            contentType = unbox("section")
          )
        }

        if (is.null(objQuizOut) &&
            (is.null(objRecipeOut) || !is.null(objRecipeOut$contents[[1]]$highlightLines))) {
          objOut$contents <- c(objOut$contents, parse_content_children(s, contentId, sectionId))
        } else {
          objOut$contents <- c(parse_content_children(s, contentId, sectionId), objOut$contents)
        }

        if (!is.null(objRecipeOut)) {
          filterContent <- sapply(objOut$contents, function(x) x$content == "")
          if (any(filterContent)) {
            objOut$contents <- objOut$contents[!filterContent]
          }
        }

        json_out[[length(json_out) + 1]]  <- objOut
        sectionContents[[length(sectionContents) + 1]] <- list(
          type = unbox("contentId"),
          content = unbox(objOut$contentId)
        )
      }

      ctypes <- sapply(json_out, function(x) x$contentType)
      recipeIdx <- which(ctypes == "recipe")

      # TODO: should be changed to `stopifnot(length(recipeIdx) != 1)`
      stopifnot(length(recipeIdx) <= 1)

      ## Add contentIds if recipe is present
      if (length(recipeIdx) == 1) {
        exercise_indices <- (ctypes == "exercise")
        json_out[[recipeIdx]]$dependencies <- sapply(json_out[exercise_indices],
                                                 function(x) x$contentId)


        includeRecipe <- sapply(json_out, function(x) !is.null(x$includeRecipe) && x$includeRecipe)
        example_indices <- (ctypes == "exercise") & includeRecipe
        json_out[[recipeIdx]]$examples <- sapply(json_out[example_indices],
                                                 function(x) x$contentId)
      }

      objIndex <- list(
        moduleId = unbox(moduleId),
        contentId = unbox(contentId),
        title = unbox(metadata$title),
        contentType = unbox("index"),
        contents = lapply(json_out, function(x) {
          list(type = unbox("contentId"),
               content = unbox(x$contentId),
               contentId = unbox(x$contentId),
               title = unbox(x$title))
        })
      )

      for (n in names(metadata)) {
        if ((!n %in% names(objIndex)) &&
            (!n %in% c("output", "tutorial", "runtime"))) {
          objIndex[[n]] <- metadata[[n]]
          if (n %in% c("author", "date", "slug", "image", "ogImage", "runtime")) {
            objIndex[[n]] <- unbox(objIndex[[n]])
          }
          if (n %in% "tutorial" && !is.null(objIndex[[n]]$tutorial$id)) {
            objIndex[[n]]$tutorial$id <- unbox(objIndex[[n]]$tutorial$id)
            objIndex[[n]]$tutorial$version <- NULL
          }
        }
      }

      files_rmd_all <- list.files(pattern = "*.Rmd$")

      if (startsWith(objIndex$moduleId, "course")) {
        chapterId <- tail(strsplit(objIndex$contentId, "#")[[1]], 1)
        idx_doing <- as.integer(strsplit(chapterId, '-')[[1]][1])
        file_out <- sprintf("%s.png", chapterId)

        sub("#", "/", metadata$tutorial$id, fixed = TRUE)

        img_path <- gsub('#', '/', objIndex$contentId, fixed = TRUE)
        progress_image_url <- file.path(options("ASSETS_URL"), paste0(img_path, ".png"))

        progress_image(objIndex$title,
                       idx_doing = idx_doing,
                       idx_total = length(files_rmd_all),
                       file_out = sprintf("%s.png", chapterId))
        objIndex$ogImage <- progress_image_url
      }

      json_out[[length(json_out) + 1]]  <- objIndex

      #rmarkdown:::write_utf8(output_str, output_file)
      output_file_json <- sub("\\.html$", "\\.json", output_file)
      jsonlite::write_json(c(json_out, qbit_out), output_file_json, auto_unbox = FALSE)

      output_file_json
    }
    output_format(knitr = NULL, pandoc = pandoc_options(to = "html",
                                                        from = NULL, args = args), keep_md = FALSE, clean_supporting = FALSE,
                  pre_knit = pre_knit, post_knit = post_knit, pre_processor = pre_processor,
                  intermediates_generator = intermediates_generator, post_processor = post_processor)
  }
