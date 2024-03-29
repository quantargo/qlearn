#' Tutorial quiz questions
#'
#' Add interative multiple choice quiz questions to a tutorial.
#'
#' @param text Question or option text
#' @param caption Optional quiz caption (defaults to "Quiz")
#' @param type Type of quiz question. Typically this can be automatically determined
#'   based on the provided answers Pass \code{"single"} to indicate that even though
#'   multiple correct answers are specified that inputs which include only one correct
#'   answer are still correct. Pass \code{"multiple"} to force the use of checkboxes
#'   (as opposed to radio buttons) even though only once correct answer was provided.
#' @param correct For \code{question}, text to print for a correct answer (defaults
#'   to "Correct!"). For \code{answer}, a boolean indicating whether this answer is
#'   correct.
#' @param incorrect Text to print for an incorrect answer (defaults to "Incorrect.")
#' @param message Additional message to display along with correct/incorrect feedback.
#' @param ... One or more questions or answers
#' @param allow_retry Allow retry for incorrect answers.
#' @param random_answer_order Display answers in a random order.
#'
#' @examples
#' \dontrun{
#' question("What number is the letter A in the alphabet?",
#'   answer("8"),
#'   answer("14"),
#'   answer("1", correct = TRUE),
#'   answer("23"),
#'   incorrect = "See [here](https://en.wikipedia.org/wiki/English_alphabet) and try again.",
#'   allow_retry = TRUE
#' )
#'
#' question("Where are you right now? (select ALL that apply)",
#'   answer("Planet Earth", correct = TRUE),
#'   answer("Pluto"),
#'   answer("At a computing device", correct = TRUE),
#'   answer("In the Milky Way", correct = TRUE),
#'   incorrect = paste0("Incorrect. You're on Earth, ",
#'                      "in the Milky Way, at a computer.")
#' )
#' }
#'
#' @name quiz
#' @export
quiz <- function(..., caption = "Quiz") {

  # create table rows from questions
  index <- 1
  questions <- lapply(list(...), function(question) {
    if (!is.null(question$x$label)) {
      question$x$label <- paste(question$x$label, index, sep="-")
      index <<- index + 1
    }
    tags$tr(tags$td(question))
  })


  htmltools::browsable(div(class = "panel panel-default",
    div(class = "panel-heading tutorial-panel-heading", caption),
    tags$table(class = "table quiz-table", questions)
  ))
}


#' @rdname quiz
#' @export
question <- function(text,
                     ...,
                     type = c("auto", "single", "multiple"),
                     correct = "Correct!",
                     incorrect = "Incorrect.",
                     allow_retry = FALSE,
                     random_answer_order = FALSE) {

  # one time tutor initialization
  learnr::initialize_tutorial()

  # capture/validate answers
  answers <- list(...)
  lapply(answers, function(answer) {
    if (!inherits(answer, "tutorial_quiz_answer"))
      stop("Object which is not an answer passed to question function")
  })

  # verify chunk label if necessary
  verify_tutorial_chunk_label()

  # create question
  question <- list(
    q = quiz_text(text),
    a = answers,
    correct = quiz_text(correct),
    incorrect = quiz_text(incorrect)
  )
  type <- match.arg(type)
  if (type == "single")
    question$select_any <- TRUE
  if (type == "multiple")
    question$force_checkbox <- TRUE

  # save all state/options into "x"
  x <- list()
  x$question <- quiz_text(text)
  x$answers <- answers
  x$label <- knitr::opts_current$get('label')
  x$skipStartButton <- TRUE
  x$perQuestionResponseAnswers <- TRUE
  x$perQuestionResponseMessaging <- TRUE
  x$preventUnanswered <- TRUE
  x$displayQuestionCount <- FALSE
  x$displayQuestionNumber <- FALSE
  x$disableRanking <- TRUE
  x$nextQuestionText <- ""
  x$checkAnswerText <- "Submit Answer"
  x$allowRetry <- allow_retry
  x$randomSortAnswers = random_answer_order
  x$json <- list(
    info = list(
      name = "",
      main = ""
    ),
    questions = list(question)
  )

  # define dependencies
  # dependencies <- list(
  #   rmarkdown::html_dependency_jquery(),
  #   rmarkdown::html_dependency_bootstrap(theme = "default"),
  #   bootbox_html_dependency(),
  #   localforage_html_dependency(),
  #   tutorial_html_dependency(),
  #   tutorial_autocompletion_html_dependency(),
  #   tutorial_diagnostics_html_dependency(),
  #   htmltools::htmlDependency(
  #     name = "slickquiz",
  #     version = "1.5.20",
  #     src = html_dependency_src("htmlwidgets", "lib", "slickquiz"),
  #     script = "js/slickQuiz.js",
  #     stylesheet = c("css/slickQuiz.css", "css/slickQuizTutorial.css")
  #   )
  # )

  dependencies <- list()

  # create widget
  htmlwidgets::createWidget(
    name = 'quiz',
    x = x,
    width = "100%",
    height = "auto",
    dependencies = dependencies,
    sizingPolicy = htmlwidgets::sizingPolicy(knitr.figure = FALSE,
                                             knitr.defaultWidth = "100%",
                                             knitr.defaultHeight = "auto",
                                             viewer.defaultWidth = "100%",
                                             viewer.defaultHeight = "auto"),
    package = 'learnr'
  )
}

#' @rdname quiz
#' @export
answer <- function(text, correct = FALSE, message = NULL) {
  structure(class = "tutorial_quiz_answer", list(
    option = quiz_text(text),
    correct = correct,
    message = quiz_text(message)
  ))
}

# render markdown (including equations) for quiz_text
quiz_text <- function(text) {
  if (!is.null(text)) {
    # convert markdown
    md <- markdown::markdownToHTML(
      text = text,
      options = c("use_xhtml", "fragment_only", "mathjax"),
      extensions = markdown::markdownExtensions(),
      fragment.only = TRUE
    )
    # remove leading and trailing paragraph
    md <- sub("^<p>", "", md)
    md <- sub("</p>\n?$", "", md)
    md
  }
  else {
    NULL
  }
}


quiz_html <- function(id, style, class, ...) {
  htmltools::HTML(sprintf('
<div id="%s" style="%s", class = "%s">
<div class="panel panel-default">
<div class="panel-body quizArea">
</div>
</div>
</div>
', id, style, class))
}


