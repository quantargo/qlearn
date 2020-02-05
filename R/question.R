# @export
# question <-
# function (text, ..., type = c("auto", "single", "multiple"),
#           correct = "Correct!", incorrect = "Incorrect.", allow_retry = FALSE,
#           random_answer_order = FALSE) {
#   initialize_tutorial()
#   answers <- list(...)
#   lapply(answers, function(answer) {
#     if (!inherits(answer, "tutorial_quiz_answer"))
#       stop("Object which is not an answer passed to question function")
#   })
#   verify_tutorial_chunk_label()
#   question <- list(q = quiz_text(text), a = answers, correct = quiz_text(correct),
#                    incorrect = quiz_text(incorrect))
#   type <- match.arg(type)
#   if (type == "single")
#     question$select_any <- TRUE
#   if (type == "multiple")
#     question$force_checkbox <- TRUE
#   x <- list()
#   x$question <- quiz_text(text)
#   x$answers <- answers
#   x$label <- knitr::opts_current$get("label")
#   x$skipStartButton <- TRUE
#   x$perQuestionResponseAnswers <- TRUE
#   x$perQuestionResponseMessaging <- TRUE
#   x$preventUnanswered <- TRUE
#   x$displayQuestionCount <- FALSE
#   x$displayQuestionNumber <- FALSE
#   x$disableRanking <- TRUE
#   x$nextQuestionText <- ""
#   x$checkAnswerText <- "Submit Answer"
#   x$allowRetry <- allow_retry
#   x$randomSortAnswers = random_answer_order
#   x$json <- list(info = list(name = "", main = ""), questions = list(question))
#   dependencies <- list(rmarkdown::html_dependency_jquery(),
#                        rmarkdown::html_dependency_bootstrap(theme = "default"),
#                        bootbox_html_dependency(), localforage_html_dependency(),
#                        tutorial_html_dependency(), tutorial_autocompletion_html_dependency(),
#                        tutorial_diagnostics_html_dependency(), htmltools::htmlDependency(name = "slickquiz",
#                                                                                          version = "1.5.20", src = html_dependency_src("htmlwidgets",
#                                                                                                                                        "lib", "slickquiz"), script = "js/slickQuiz.js",
#                                                                                          stylesheet = c("css/slickQuiz.css", "css/slickQuizTutorial.css")))
#   htmlwidgets::createWidget(name = "quiz", x = x, width = "100%",
#                             height = "auto", dependencies = dependencies, sizingPolicy = htmlwidgets::sizingPolicy(knitr.figure = FALSE,
#                                                                                                                    knitr.defaultWidth = "100%", knitr.defaultHeight = "auto",
#                                                                                                                    viewer.defaultWidth = "100%", viewer.defaultHeight = "auto"),
#                             package = "learnr")
# }
