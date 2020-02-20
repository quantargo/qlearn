#' @importFrom rlang "%||%"
install_knitr_hooks <- function() {

  # set global tutorial option which we can use as a basis for hooks
  # (this is so we don't collide with hooks set by the user or
  # by other packages or Rmd output formats)
  knitr::opts_chunk$set(qlearn = TRUE)

  # helper to check for runtime: shiny_prerendered being active
  is_shiny_prerendered_active <- function() {
    identical(knitr::opts_knit$get("rmarkdown.runtime"),"shiny_prerendered")
  }

  # helper to check for an exercise chunk
  is_exercise_chunk <- function(options) {
    isTRUE(options[["exercise"]])
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
    exercise_placeholder_div <- function(suffix = NULL, extra_html = NULL) {
      if (!is.null(suffix))
          suffix <- paste0("-", suffix)
        class <- paste0("exercise", suffix)
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
        related_setup_chunks <- unlist(sapply(related_chunks, function(x) attr(x, "chunk_opts")$exercise.setup))[[1]]

        # Build exercise object
        solution_id <- grep("solution$", names(related_chunks))[1]
        solution_id <- solution_id[!is.na(solution_id)]
        solution <- if (length(solution_id) > 0) {
          paste(related_chunks[[solution_id]], collapse = "\n")
        } else {
          NULL
        }
        hints <- unlist(related_chunks[grep("hint", names(related_chunks))], use.names = FALSE)

        exObj <- list(
          contentId = unbox(options$label),
          contentType = unbox("exercise"),
          exerciseType = unbox("code"),
          template = unbox(paste(related_chunks[[1]], collapse = "\n"))
        )
        if (!is.null(hints)) {
          exObj$hints <- hints
        }
        if (!is.null(solution)) {
          exObj$solution <- solution
        }
        if (length(related_setup_chunks) > 0) {
          exObj$setup <- as.character(all_exercise_chunks[[related_setup_chunks]], use.names = FALSE)
        }

        attributes(exObj$hints) <- NULL
        attributes(exObj$solution) <- NULL
        exObj$solution <- unbox(exObj$solution)

        extra_html <- paste0(c('<script type="application/json" data-opts-chunk="1">',
                        jsonlite::toJSON(exObj),
                        '</script>'), collapse = "")
        suffix <- sub("exercise-", "", options$label)
        exercise_placeholder_div(suffix, extra_html = extra_html)

      } else {
        ""
      }

      # else if (is_exercise_support_chunk(options)) {
      #   write_json(unclass(options), path = paste0(options$label, ".json"), auto_unbox = TRUE)
      # }

    } else {
      ""
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


