


parse_exercise <- function(s, moduleId, contentId, section_title) {
  objExercise <- NULL
  nodes_exercise <- xml_find_all(s, ".//div[starts-with(@class, 'placeholder-exercise')]")
  if (length(nodes_exercise) > 0) {
    #stopifnot(length(nodes_exercise) == 1)
    e <- nodes_exercise[[1]]
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
  }
  xml_remove(nodes_exercise)
  objExercise
}


parse_quiz <- function(s, moduleId, contentId, section_title, qid) {
  objQuizOut <- NULL

  nodes_quizzes <- xml_find_all(s, ".//script[starts-with(@data-for, 'htmlwidget-')]")
  if (length(nodes_quizzes) > 0) {
    #stopifnot(length(nodes_quizzes) == 1)
    q <- nodes_quizzes[[1]]
    objQuiz <- q %>%
      xml_text() %>%
      jsonlite::fromJSON()

    # Check if node is a valid quesiton
    if (is.null(objQuiz$x$answers)) {
      # node does not seem to be a quiz, skipping
      return(NULL)
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

    objQuizOut$answers$answerId <- paste0(objQuizOut$contentId, "#answer-", rownames(objQuizOut$answers))

    if (nchar(objQuiz$x$question) > 0) {
      objQuizOut$contents <- list(list(
        type = unbox("html"),
        content = unbox(objQuiz$x$question)
      ))
    }
    xml_remove(nodes_quizzes)
  }
  objQuizOut
}


parse_recipe <- function(s, moduleId, contentId, section_title) {

  sectionId <- s %>% xml_attr("id")
  objRecipe <- NULL
  nodes_recipe <- xml_find_all(s, ".//div[starts-with(@class, 'placeholder-recipe')]")

  if (length(nodes_recipe) > 0) {
    #stopifnot(length(nodes_recipe) == 1)
    e <- nodes_recipe[[1]]

    objRecipe <- e %>%
      xml_text() %>%
      jsonlite::unserializeJSON()


    objRecipe$contentType <- if (objRecipe$contentType != "recipe") "section" else "recipe"
    elem <- list(
      moduleId = unbox(moduleId),
      contentId = unbox(paste(contentId, sub("^section-", sprintf("%s-", objRecipe$contentType), sectionId), sep = "#")),
      title = unbox(section_title),
      contentType = unbox(objRecipe$contentType),

      contents = list(
        list(
          type = unbox("code-highlight"),
          content = unbox(paste(objRecipe$code, collapse = "\n")),
          engine = unbox(objRecipe$engine),
          label = unbox(objRecipe$label)
        )
      )
    )

    if (!is.null(objRecipe$highlightLines)) {
      elem$contents[[1]]$highlightLines <- objRecipe$highlightLines
    }
    objRecipe <- elem
    xml_remove(nodes_recipe)
  }
  objRecipe
}


parse_content_children <- function(s, contentId, sectionId) {
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

    if (length(nodes_editor_img) > 0) {
      for (e in nodes_editor_img) {
        elem = list(
          type = unbox("image"),
          content = unbox(xml_attr(e, "src"))
        )
        title <- xml_text(e, "src")
        if (title != "") {
          elem$title <-title
        }

        width = xml_attr(e, "width")
        if (!is.na(width)) {
          if (!is.na(as.integer(width))) {
            width <- as.integer(width)
          }
          elem$width <- unbox(width)
        }

        height = xml_attr(e, "height")
        if (!is.na(height)) {
          if (!is.na(as.integer(height))) {
            height <- as.integer(height)
          }
          elem$height <- unbox(height)
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

  return(contents)
}
