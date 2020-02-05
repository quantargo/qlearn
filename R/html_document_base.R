#' @import jsonlite
#' @import xml2
html_document_base <-
  function (smart = TRUE, theme = NULL, self_contained = TRUE,
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
        theme <- match.arg(theme, themes())
        if (identical(theme, "default"))
          theme <- "bootstrap"
        args <- c(args, "--variable", paste0("theme:", theme))
      }
      format_deps <- list()
      if (!is.null(theme)) {
        format_deps <- append(format_deps, list(html_dependency_jquery(),
                                                html_dependency_bootstrap(theme)))
      }
      else if (isTRUE(bootstrap_compatible) && is_shiny(runtime)) {
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
      return(copy_render_intermediates(original_input, encoding,
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
        output_str <- copy_html_resources(one_string(output_str),
                                          lib_dir, output_dir)
      }
      else if (!self_contained) {
        image_relative <- function(img_src, src) {
          in_file <- utils::URLdecode(src)
          if (grepl("^[.][.]", in_file))
            return(img_src)
          if (length(in_file) && file.exists(in_file)) {
            img_src <- sub(src, utils::URLencode(normalized_relative_to(output_dir,
                                                                        in_file)), img_src, fixed = TRUE)
          }
          img_src
        }
        output_str <- rmarkdown:::process_images(output_str, image_relative)
      }

      writeLines(output_str, "output_str.html")
      html_doc <- xml2::read_html(paste(output_str, collapse = "\n"))
      sections <- xml2::xml_find_all(html_doc, "//div[@class='section level2']")
      contentId <- metadata$tutorial$id

      json_out <- list(
        list(
          moduleId = unbox("course-platform-new"),
          contentId = unbox(contentId),
          contentType = unbox("index"),
          contents = lapply(sapply(sections, xml2::xml_attr, "id"),
                            function(x) list(type = unbox("contentId"),
                                             content = unbox(sprintf("%s#%s", contentId, x))))
        )
      )

      i <- 2
      # Extract exercises from content and replace with placeholders
      for (s in sections) {
        nodes_exercise <- xml_find_all(s, ".//div[starts-with(@class, 'placeholder-')]")
        if (length(nodes_exercise) > 0) {
          for (e in nodes_exercise) {
            browser()
            objExercise <- e %>%
              xml_text() %>%
              jsonlite::fromJSON()
          }
        }

        # get exercise chunks

        # get quiz chunks


        # json_out[[i+1]] <- list(
        #   moduleId = unbox("course-platform-new"),
        #   contentId = unbox(sprintf("%s#%s", contentId, section_names[i])),
        #   contentType = unbox("html"),
        #   contents = list(list(
        #     type = unbox("html"),
        #     content = unbox(section_chunks[[i]])
        #   )))

      }

      output_file_json <- sub("\\.html$", "\\.json", output_file)
      jsonlite::write_json(json_out, output_file_json, auto_unbox = FALSE)
      output_file_json

      #rmarkdown:::write_utf8(output_str, output_file)
      #output_file
    }
    output_format(knitr = NULL, pandoc = pandoc_options(to = "html",
                                                        from = NULL, args = args), keep_md = FALSE, clean_supporting = FALSE,
                  pre_knit = pre_knit, post_knit = post_knit, pre_processor = pre_processor,
                  intermediates_generator = intermediates_generator, post_processor = post_processor)
  }
