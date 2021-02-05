#' @import xml2
#' @import jsonlite
#' @importFrom magrittr "%>%"
#' @importFrom qbit create_qbit_metadata
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
      # args <- c(args, rmarkdown:::pandoc_lua_filters(c("pagebreak.lua",
      #                                                  "latex-div.lua")))
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
      qbitModuleId <- sprintf("qbit-%s", moduleId)

      qid <- 1
      json_out <- list()
      qbit_out <- list()
      pkgLock <- jsonlite::read_json("../renv.lock")
      pkgLock$Packages <- lapply(pkgLock$Packages, function(x) lapply(x, function(y) unbox(y)))

      mainItem <- yaml::read_yaml("../index.yml")
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

          code <- objOut$solution
          qbitTitle <- sub("^Exercise:?\\s+", "", objOut$title)
          qbitContentId <- paste("qbit", objOut$contentId, sep = "-")
          qbit_env <- NULL
          setup_env <- NULL
          if (!is.null(objExercise$setup)) {
            setup_env <- new.env()
            eval(parse(text = objExercise$setup), envir = setup_env)
          }

          usagePlan = if(!is.null(metadata$usagePlan)) {
            if(metadata$usagePlan == "free") "public" else "pro"
          } else if (!is.null(mainItem$usagePlan)) {
            if(mainItem$usagePlan == "free") "public" else "pro"
          } else {
            "pro"
          }

          qbit_main_item <- list(
            contentId = unbox(qbitContentId),
            contentType = unbox("main"),
            createdBy = unbox("SYSTEM"),
            description = unbox(sectionId),
            moduleId = unbox(qbitModuleId),
            moduleType = unbox("qbit"),
            title = unbox(qbitTitle),
            visibility = unbox("public"),
            qbitName = unbox(qbitModuleId),
            qbitRuntime = unbox(objExercise$qbitRuntime),
            usagePlan = unbox(usagePlan)
          )

          file_main <- file.path("..", gsub("#", "/", qbitContentId, fixed = TRUE), "main.R")
          dir.create(dirname(file_main), recursive = TRUE, showWarnings = FALSE)
          writeLines(code, file_main)
          files <- c(file_main)

          qbit_out <- c(qbit_out, create_qbit_metadata(
            qbit_main_item,
            files,
            pkgLock,
            setup_env,
            objExercise$packagesLoaded,
            objExercise$advertiseQBit
          ))

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
          if (n %in% c("author", "date", "slug", "image", "ogImage", "runtime", "usagePlan")) {
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
