# print.bare-mpoly


print.bare_mpoly <- function (x, silent = FALSE, ...) {
  
  # print each term
  terms <- vapply(x, print.mpoly_term, character(1), silent = TRUE)
  
  # merge printed terms
  printed_string <- str_c(terms, collapse = " + ")
  
  # change " + -1 "'s to " - "'s
  printed_string <- str_replace_all(printed_string, " \\+ -1[ *]", " - ")
  
  # print/return
  if(!silent) cat(printed_string)
  invisible(printed_string)
  
}