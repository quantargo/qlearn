#' @importFrom highr hi_html
#' @importFrom webshot webshot
highlight_code_image <- function(code, file = "webshot.png") {
  tf_html <- tempfile(fileext = ".html")
  on.exit(file.remove(tf_html))
  html_code <- paste(hi_html(code), collapse = "\n")
  head <- "<head>
  <style>
    body {
      background-color: black;
      color: rgba(255, 255, 255, .9);
      padding: 2em;
      font-family: 'DM Mono', 'Source Code Pro', 'Fira Mono', 'Ubuntu Mono', 'Monaco', 'Menlo', 'Consolas', monospace;
      line-height: 1.6;
      white-space: pre;
      margin: 0;
    }
    .kwd, .kwa {
      color: #569BD7;
    }
    .num {
      color: #B4CEA6;
    }
    .str {
      color: #CD9176;
    }
  </style>
</head>"
  html <- sprintf("<html>%s<body style=\"background-color:black;color: white;\">%s</body></html>", head, html_code)
  writeLines(html, tf_html)
  webshot::webshot(tf_html, file = file, vwidth = 400, vheight = 400)
}
