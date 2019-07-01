# 

print.mpoly <- function (x, silent = FALSE, ...) {
  
  # make printouts of bare_mpolys
  printed_strings <- vapply(x, print.bare_mpoly, character(1), silent = TRUE)
  
  # add back dimensions (if dimensionless, assumed to be a column/row vector)
  if (!is.null(dim(x))) dim(printed_strings) <- dim(x)
  
  # replace "'s with spaces
  printed_strings <- capture.output(print(printed_strings))
  printed_strings <- str_replace_all(printed_strings, "(?<= )\"", "")
  printed_strings <- str_replace_all(printed_strings, "\"(?= |$)", "  ")
  
  # print/return
  if(!silent) cat(printed_strings, sep = "\n")
  invisible(printed_strings)
  
}