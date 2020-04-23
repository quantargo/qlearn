highlight_markers_remove <- function(code,
                                     highlight_start = "@_",
                                     highlight_end = "_@",
                                     highlight_line = "@@") {

  patterns_all <- c(highlight_start, highlight_end, highlight_line)
  for (p in patterns_all) {
    code <-  gsub(p, "", code, fixed = TRUE)
  }
  code
}

highlight_markers_extract <- function(code,
                                      highlight_start = "@_",
                                      highlight_end = "_@",
                                      highlight_line = "@@") {

  range_start <- str_locate_all(code, highlight_start)
  range_end <- str_locate_all(code, highlight_end)
  range_all <- str_locate(code, highlight_line)

  highlight_markers <- bind_rows(mutate(melt(range_start), type = "highlight_start"),
                                 mutate(melt(range_end), type = "highlight_end")) %>%
    filter(Var2 == "start") %>%
    arrange(L1, value) %>%
    separate(type, c("highlight", "type")) %>%
    mutate(match = rep(1:(nrow(.)/2), each = 2)) %>%
    nest(-match) %>%
    mutate(data = map(match, function(x) {
      dat <- filter(., match==x) %>% unnest(data)
      filter(dat, type == "start") %>% select(L1)
      data.frame(
        rowStart = dat[dat$type == "start", "L1", drop=TRUE],
        colStart = dat[dat$type == "start", "value", drop=TRUE],
        rowEnd = dat[dat$type == "end", "L1", drop=TRUE],
        colEnd = dat[dat$type == "end", "value", drop=TRUE],
        entireLine = FALSE
      )
    })) %>%
    unnest(data) %>%
    select(-match)

  range_all_df <- range_all %>%
    as_tibble() %>%
    filter(!is.na(start) & !is.na(end)) %>%
    mutate(line = as.integer(rownames(.))) %>%
    mutate(rowStart = line, colStart = 1, rowEnd = line, colEnd = 1, entireLine = TRUE) %>%
    select(rowStart, colStart, rowEnd, colEnd, entireLine)

  highlight_df <- bind_rows(highlight_markers, range_all_df)

}
