# mpoly-term

# mpoly(list)
# mp("")
# 
# list <- list(
#   c(x = 1, coef = 1, y = 0),
#   c(x = 0, y = 1, coef = 2),
#   c(y = 1, coef = -6),
#   c(z = 1, coef = -3, x = 2),
#   c(x = 1, coef = 0, x = 3),
#   c(t = 1, coef = 4, t = 2, y = 4),
#   c(x = 1),
#   c(x = 1),
#   c(coef = 5),
#   c(coef = 5),
#   c(coef = -5)
# )
# 
# term <- list[[4]]
# 
# term <- unclass(structure(
#   list(
#     coef = complex(real = 1, imaginary = -2),
#     core = c("x" = 3L, "y" = 4L)
#   ),
#   class = "mpoly_term"
# ))
# 
# term <- list(
#     coef = complex(real = 1, imaginary = -2),
#     core = c("x" = 3L, "y" = 4L)
#   )
# 
# str(mpoly_term(term))
# 
# 
# term <- structure(
#   list(
#     coef = complex(real = 1, imaginary = -2),
#     core = c("x" = 3L, "y" = 4L)
#   ),
#   class = "mpoly_term"
# )


mpoly_term <- function(term, varorder){

  flatList <- unlist(term$core)



  v <- term$core
  ## set intrinsic varorder - done again after 0 degrees are removed
  vars <- unique(names(flatList))


  # deal with varorder argument
  if(!missing(varorder)){
    if( !setequal(vars, varorder) ){
      stop(paste(
        "if specified varorder must be a permutation of",
        paste(vars, collapse = ", ")
      ), call. = FALSE)
    }
    vars <- varorder
  }

  v <- term



  ## return classed list
  class(term) <- "mpoly_term"
  term
}








