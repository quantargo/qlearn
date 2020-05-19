#' @importFrom rlang "%||%"
#' @importFrom reshape2 melt
install_knitr_hooks <- function() {

  # set global tutorial option which we can use as a basis for hooks
  # (this is so we don't collide with hooks set by the user or
  # by other packages or Rmd output formats)
  knitr::opts_chunk$set(qlearn = TRUE, comment=NA)

  # helper to check for runtime: shiny_prerendered being active
  is_shiny_prerendered_active <- function() {
    identical(knitr::opts_knit$get("rmarkdown.runtime"),"shiny_prerendered")
  }

  # helper to check for an exercise chunk
  is_exercise_chunk <- function(options) {
    isTRUE(options[["exercise"]])
  }
  is_recipe_chunk <- function(options) {
    isTRUE(options[["recipe"]])
  }

  # helper to find chunks that name a chunk as their setup chunk
  exercise_chunks_for_setup_chunk <- function(label) {
    label_query <- paste0("knitr::all_labels(exercise.setup == '", label, "')")
    eval(parse(text = label_query))
  }

  # helper to check for an exercise support chunk
  is_exercise_support_chunk <- function(options, type = c("setup",
                                                          "hint",
                                                          "hint-\\d+",
                                                          "solution",
                                                          "code-check",
                                                          "check")) {
    support_regex <- paste0("-(", paste(type, collapse = "|"), ")$")
    if (grepl(support_regex, options$label)) {
      exercise_label <- sub(support_regex, "", options$label)
      label_query <- "knitr::all_labels(exercise == TRUE)"
      all_exercise_labels <- eval(parse(text = label_query))
      exercise_label %in% all_exercise_labels
    }
    else if ("setup" %in% type) {
      # look for another chunk which names this as it's setup chunk
      length(exercise_chunks_for_setup_chunk(options$label)) > 0
    }
    else {
      FALSE
    }
  }

  # hook to amend output for exercise related chunks
  knitr::knit_hooks$set(qlearn = function(before, options, envir) {

    # # helper to produce an exercise wrapper div w/ the specified class
    placeholder_div <- function(suffix = NULL, extra_html = NULL, type = "exercise") {
      if (!is.null(suffix))
          suffix <- paste0("-", suffix)
        class <- paste0(type, suffix)
        lines <- ifelse(is.numeric(options$exercise.lines),
                        options$exercise.lines, 0)
        completion  <- as.numeric(options$exercise.completion %||% 1 > 0)
        diagnostics <- as.numeric(options$exercise.diagnostics %||% 1 > 0)
        startover <- as.numeric(options$exercise.startover %||% 1 > 0)
        caption <- ifelse(is.null(options$exercise.cap), "Code", options$exercise.cap)
        paste0('<div class="placeholder-', class,
               '" data-label="', options$label,
               '" data-caption="', caption,
               '" data-completion="', completion,
               '" data-diagnostics="', diagnostics,
               '" data-startover="', startover,
               '" data-lines="', lines, '">', extra_html, '</div>')

    }

    # handle exercise chunks
    if (before) {

      if (is_exercise_chunk(options) ) {
        # TODO: Build exercise object
        label_query <- "knitr::knit_code$get()"
        all_exercise_chunks <- eval(parse(text = label_query))
        related_chunks <- all_exercise_chunks[grep(options$label, names(all_exercise_chunks))]

        setup_chunk <- which(names(all_exercise_chunks) == sprintf("%s-setup", options$label))
        if (length(setup_chunk) > 0) {
          setup_chunk <- all_exercise_chunks[[setup_chunk[1]]]
        } else {
          setup_chunk <- NULL
        }

        related_setup_chunks <- unlist(sapply(related_chunks, function(x) attr(x, "chunk_opts")$exercise.setup))[[1]]

        # Build exercise object
        solution_id <- grep("solution$", names(related_chunks))[1]
        solution_id <- solution_id[!is.na(solution_id)]
        solution <- if (length(solution_id) > 0) {
          paste(related_chunks[[solution_id]], collapse = "\n")
        } else {
          NULL
        }
        hints <- sapply(related_chunks[grep("hint", names(related_chunks))], paste, collapse = "\n")
        if (length(hints) > 0) {
          engines <- sapply(related_chunks[grep("hint", names(related_chunks))],
                            function(x) attr(x, "chunk_opts")$engine)
          type <- ifelse(engines == "html", "html", "code-input")
          hints <- data.frame(label = names(hints), type = type, content = hints)
        } else {
          hints <- NULL
        }

        check <- paste(do.call("c", related_chunks[grep("check", names(related_chunks))]), collapse = "\n")

        template_code <- paste(options$code, collapse = "\n")

        packages_loaded <- NULL
        if ("packagesLoaded" %in% names(options)){
          if (length(options$packagesLoaded) > 0) {
            packages_loaded <- options$packagesLoaded
          }
        } else {
          packages_loaded <- names(sessionInfo()$otherPkgs)
          packages_loaded <- packages_loaded[!packages_loaded %in% c("shiny", "qlearn", "learnr", "testwhat")]
        }
        advertiseQBit <- NULL
        if ("advertiseQBit" %in% names(options)){
          advertiseQBit <- options$advertiseQBit
        }
        useImage <- NULL
        if ("useImage" %in% names(options)){
          useImage <- options$useImage
        }

        exObj <- list(
          contentId = options$label,
          contentType = "exercise",
          exerciseType = "code",
          engine = options$engine
        )
        if (!is.null(packages_loaded)) {
          exObj$packagesLoaded = packages_loaded
        }
        if(nchar(template_code) > 0) {
          exObj$template <- template_code
        }
        if (!is.null(hints)) {
          exObj$hints <- hints
          exObj$hintsAll <- nrow(hints)
        } else {
          exObj$hintsAll <- 0
        }
        if (!is.null(solution)) {
          exObj$solution <- solution
        }
        if (!is.null(check) && (nchar(check) > 0) ) {
          exObj$check <- check
        }
        if (!is.null(setup_chunk)) {
          exObj$setup <- paste(setup_chunk, collapse = "\n")
        }
        if(!is.null(options$includeRecipe)) {
          exObj$includeRecipe <- TRUE
        }
        if(!is.null(options$advertiseQBit)) {
          exObj$advertiseQBit <- unbox(advertiseQBit)
        }
        if(!is.null(options$useImage)) {
          exObj$useImage <- useImage
        }

        #attributes(exObj$hints) <- NULL
        #attributes(exObj$solution) <- NULL
        exObj$solution <- exObj$solution

        extra_html <- paste0(c('<script type="application/json" data-opts-chunk="1">',
                        jsonlite::serializeJSON(exObj),
                        '</script>'), collapse = "")
        suffix <- sub("exercise-", "", options$label)
        placeholder_div(suffix, extra_html = extra_html, type = "exercise")

      } else if (is_recipe_chunk(options) || !is.null(options$highlightLines)) {
        recObj <- list(
          label = options$label,
          code = options$code,
          engine = options$engine,
          contentType = "content"
        )

        if (is_recipe_chunk(options)) {
          recObj$contentType = "recipe"
        }

        if (!is.null(options$highlightLines)) {
          vec <- sort(unique(options$highlightLines))
          vecdiff <- c(1, diff(vec))
          vecjmp <- which(vecdiff != 1)
          starts <- c(1, vecjmp)
          ends <- c(vecjmp - 1, length(vecdiff))
          recObj$highlightLines <- data.frame(startRow = vec[starts] - 1, startCol = 0, endRow = vec[ends] - 1, endCol = 0, fullLine = TRUE)
        }

        # markers <- suppressWarnings(highlight_markers_extract(options$code))
        # if (nrow(markers) > 0) {
        #   recObj$code <- highlight_markers_remove(options$code)
        #   recObj$options$highlightMarkers <- markers
        # }
        extra_html <- paste0(c('<script type="application/json" data-opts-chunk="1">',
                               jsonlite::serializeJSON(recObj),
                               '</script>'), collapse = "")
        suffix <- sub("exercise-", "", options$label)
        suffix <- sub("editor-", "", options$label)

        placeholder_div(suffix, extra_html = extra_html, type = "recipe")
      } else {
        ""
      }
    }
  })
}

remove_knitr_hooks <- function() {
  knitr::opts_hooks$set(qlearn = NULL)
  knitr::knit_hooks$set(qlearn = NULL)
}


verify_tutorial_chunk_label <- function() {
  label <- knitr::opts_current$get('label')
  unnamed_label <- knitr::opts_knit$get('unnamed.chunk.label')
  if (isTRUE(grepl(paste0('^', unnamed_label), label))) {
    stop("Code chunks with exercises or quiz questions must be labeled.",
         call. = FALSE)
  }
}


