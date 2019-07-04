# print mpoly

print.mpoly <- function (x, silent = FALSE, ...) {
  
  if (length(x) == 1) {
    # if mpoly is bare_mpoly, use print.bare_mpoly
    print.bare_mpoly(x[[1]])
    invisible(x[[1]])
  } 
  else {
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
  
}

