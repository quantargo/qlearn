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
        split_path <- strsplit(img_path, "/")[[1]]
        image_prefix <- file.path(url_prefix, paste(split_path[-length(split_path)], collapse = "/"))

        in_file <- utils::URLdecode(src)
        if (grepl("^[.][.]", in_file))
          return(img_src)
        if (length(in_file) && file.exists(in_file)) {
          image_path_full <- file.path(image_prefix, src)
          img_src <- sub(src, image_path_full, img_src)
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

      # Extract exercises from content and replace with placeholders
      for (s in sections) {
        sectionId <- s %>% xml_attr("id")
        nodes_exercise <- xml_find_all(s, ".//div[starts-with(@class, 'placeholder-exercise')]")
        nodes_quizzes <- xml_find_all(s, ".//script[starts-with(@data-for, 'htmlwidget-')]")

        sectionContents <- list()
        node_title <- xml_find_all(s, ".//h2[1]")
        section_title <- node_title %>% xml_text()
        xml_remove(node_title)

        if (length(nodes_exercise) > 0) {
          for (e in nodes_exercise) {
            objExercise <- e %>%
              xml_text() %>%
              jsonlite::unserializeJSON()

            objExercise$moduleId <- unbox(moduleId)
            objExercise$contentId <- paste(contentId, objExercise$contentId, sep = "#")

            qbitName <- sprintf("qbit-%s", moduleId)
            if (options("STAGE") == "dev") {
              qbitName <- sprintf("%s-dev", qbitName)
            }

            objExercise$qbitName <- qbitName
            objExercise$title <- section_title
            attributes_unbox <- c("contentId", "qbitName", "contentType", "exerciseType", "solution", "title", "template", "hintsAll", "includeRecipe", "setup")
            attributes_unbox <- attributes_unbox[attributes_unbox %in% names(objExercise)]
            for (a in attributes_unbox) {
              objExercise[[a]] <- unbox(objExercise[[a]])
            }
            # Strange issue with hints
            if (!is.null(objExercise$hints)) {
              objExercise$hints <- objExercise$hints
              rownames(objExercise$hints) <- NULL
            }
            if (!is.null(objExercise$check)) {
              objExercise$check <- unbox(objExercise$check)
            }
            if (!is.null(objExercise$engine)) {
              objExercise$engine <- unbox(objExercise$engine)
            }
            xml_remove(e)
            objExercise$contents <- lapply(xml_find_all(s, "."), function(x) list(
              type = unbox("html"),
              content = unbox(as.character(x))
            ))
            json_out[[length(json_out) + 1]]  <- objExercise
            sectionContents[[length(sectionContents) + 1]] <- list(
              type = unbox("contentId"),
              content = unbox(objExercise$contentId)
            )
          }
        }

        if (length(nodes_quizzes) > 0) {
          for (q in nodes_quizzes) {
            objQuiz <- q %>%
              xml_text() %>%
              jsonlite::fromJSON()

            # Check if node is a valid quesiton
            if (is.null(objQuiz$x$answers)) {
              # node does not seem to be a quiz, skipping
              next
            }

            num_answers_correct <- which(objQuiz$x$answers$correct)

            objQuizOut <- list(
              moduleId = unbox(moduleId),
              contentId = unbox(paste(contentId, paste("quiz", qid, sep = "-"), sep = "#")),
              title = unbox(section_title),
              contentType = unbox("exercise"),
              exerciseType = unbox(if (length(num_answers_correct) == 1) "quiz-single-choice" else "quiz-multiple-choice"),
              answers = objQuiz$x$answers
            )

            if (nchar(objQuiz$x$question) > 0) {
              objQuizOut$contents <- list(list(
                type = unbox("html"),
                content = unbox(objQuiz$x$question)
              ))
            } else {
              xml_remove(q)
              objQuizOut$contents <- lapply(xml_find_all(s, "."), function(x) list(
                type = unbox("html"),
                content = unbox(as.character(x))
              ))
            }
            json_out[[length(json_out) + 1]]  <- objQuizOut
            sectionContents[[length(sectionContents) + 1]] <- list(
              type = unbox("contentId"),
              content = unbox(objQuizOut$contentId)
            )
            qid <- qid + 1
          }
        }

        contentType <- "content"
        contentIdSection <- paste(contentId, sectionId, sep = "#")

        # If no exercise or quiz in section -> Add entire content chunk
        if (length(nodes_exercise) < 1 && length(nodes_quizzes) < 1) {

          contents <- list()
          children <- xml_children(s)

          ignore_next_elem <- FALSE
          for (child in children) {
            if (ignore_next_elem) {
              ignore_next_elem <- FALSE
              next
            }
            subnode <- xml2::read_html(as.character(child))
            nodes_editor_input <- xml_find_all(subnode, ".//pre[@class]/code")
            nodes_editor_output <- xml_find_all(subnode, ".//pre[not(@class)]/code")
            nodes_editor_img <- xml_find_all(subnode, ".//img")
            nodes_recipe <- xml_find_all(subnode, ".//div[starts-with(@class, 'placeholder-recipe')]")

            if (length(nodes_recipe) > 0) {
              for (e in nodes_recipe) {
                objRecipe <- e %>%
                  xml_text() %>%
                  jsonlite::unserializeJSON()

                elem <- list(
                  type = unbox("code-highlight"),
                  content = unbox(paste(objRecipe$code, collapse = "\n")),
                  engine = unbox(objRecipe$engine),
                  label = unbox(objRecipe$label)
                )
                if (objRecipe$contentType == "recipe") {
                  contentIdSection <- paste(contentId, sub("^section-", "recipe-", sectionId), sep = "#")
                  contentType <- "recipe"
                }

                if (!is.null(objRecipe$highlightLines)) {
                  elem$highlightLines <- objRecipe$highlightLines
                }
                contents[[length(contents) + 1]]  <- elem

                #ignore_next_elem <- TRUE
              }
            } else if (length(nodes_editor_img) > 0) {
              for (e in nodes_editor_img) {
                elem = list(
                  type = unbox("image"),
                  content = unbox(xml_attr(e, "src"))
                )
                title <- xml_text(e, "src")
                if (title != "") {
                  elem$title <-title
                }
                contents[[length(contents) + 1]]  <- elem
              }
            } else if (length(nodes_editor_input) > 0) {
              for (e in nodes_editor_input) {
                elem = list(
                  type = unbox("code-input"),
                  content = unbox(xml_text(e))
                )
                contents[[length(contents) + 1]]  <- elem
              }
            } else if (length(nodes_editor_output) > 0) {
              for (e in nodes_editor_output) {
                elem = list(
                  type = unbox("code-output"),
                  content = unbox(xml_text(e))
                )
                contents[[length(contents) + 1]]  <- elem
              }
            } else {
              elem = list(
                type = unbox("html"),
                content = unbox(as.character(child))
              )
              contents[[length(contents) + 1]]  <- elem
            }
          }


          objSectionOut <- list(
            moduleId = unbox(moduleId),
            contentId = unbox(contentIdSection),
            title = unbox(section_title),
            contentType = unbox(contentType),
            contents = contents
          )
          json_out[[length(json_out) + 1]]  <- objSectionOut
          sectionContents[[length(sectionContents) + 1]] <- list(
            type = unbox("contentId"),
            content = unbox(contentIdSection)
          )
        }
      }

      ctypes <- sapply(json_out, function(x) x$contentType)
      recipeIdx <- which(ctypes == "recipe")

      # TODO: should be changed to `stopifnot(length(recipeIdx) != 1)`
      stopifnot(length(recipeIdx) <= 1)

      ## Add contentIds if recipe is present
      if (length(recipeIdx) == 1) {
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
               title = unbox(x$title))
        })
      )
      json_out[[length(json_out) + 1]]  <- objIndex


      #rmarkdown:::write_utf8(output_str, output_file)
      output_file_json <- sub("\\.html$", "\\.json", output_file)
      jsonlite::write_json(json_out, output_file_json, auto_unbox = FALSE)

      output_file_json
    }
    output_format(knitr = NULL, pandoc = pandoc_options(to = "html",
                                                        from = NULL, args = args), keep_md = FALSE, clean_supporting = FALSE,
                  pre_knit = pre_knit, post_knit = post_knit, pre_processor = pre_processor,
                  intermediates_generator = intermediates_generator, post_processor = post_processor)
  }
