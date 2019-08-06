# print.mpoly-term

# term <- list[[2]]

print.mpoly_term <- function (term, silent = FALSE, stars = FALSE) {
  
  # treat constant terms (no indeterminates) and those with vars differently
  if (length(term$core) == 0L) { 
    
    printed_string <- str_sub(capture.output(print(term$coef)), 5)
    
  } else {
    
    # set times sign
    times <- if(stars) "*" else " "
    
    # format coefficient
    coef_to_disp <- if (class(term$coef) == "complex") { # checking to see if coef is complex
      if (Im(term$coef) == 0) { # if purely real
        str_sub(capture.output(print(Re(term$coef))), 5)
      } else if (Re(term$coef) == 0) { # if purely imaginary
        str_c(str_sub(capture.output(print(Im(term$coef))), 5), "i")
      } else {
        str_c("(", str_sub(capture.output(print(term$coef)), 5), ")") # the 5 is for where in the printed output the actual part starts
      }
    } else {
      str_sub(capture.output(print(term$coef)), 5)
    }  
    
    # format indeterminates, e.g. "x^1 y^2" or "x^1*y^2"
    core_to_disp <- str_c(
      names(term$core), term$core, 
      sep = "^", collapse = times
    )
    
    # remove ^1's
    if (any(term$core == 1L)) {
      core_to_disp <- str_remove_all(core_to_disp, "\\^1(?![0-9])")
    }
    
    # put coef and core together, unless coef = 1
    if ( term$coef == 1 && length(term$core) != 0 ) {
      printed_string <- core_to_disp
    } else {
      printed_string <- str_c(coef_to_disp, core_to_disp, sep = times)
    }  
    
  }  
  
  # print/return
  if(!silent) cat(printed_string)
  invisible(printed_string)
}
