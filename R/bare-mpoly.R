#' Define an mpoly object.
#' 
#' mpoly is the most basic function used to create objects of class
#' mpoly. However, it is not a general purpose function; for that
#' see mp.
#' 
#' @param list a list from which to construct an mpoly object
#' @param varorder (optional) a character vector setting the
#'   intrinsic variable order of the polynomial
#' @return Object of class mpoly.
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \code{\link{mp}}
#' @export
#' @examples
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
#' 
#' mpoly(list) # 3 x  -  4 y  -  3 x^2 z  +  4 y^4 t^3  +  5
#' mpoly(list, varorder = c("y", "z", "t", "x"))
#' 
#' list <- list(  c(x = 5, x = 2, coef = 5, coef = 6, y = 0) )
#' mpoly(list)
#' 
#' 


# mpol <- structure(
#   list(p, p, p, p),
#   class = "mpoly",
#   coefring = "numeric",
#   vars = c("x", "y"),
#   .Dim = c(2L,2L)
# )
# 
# mpol2 <- structure(
#   list(p),
#   class = "mpoly",
#   coefring = "numeric",
#   #  vars = c("x", "y"),
#   .Dim = c(1L,1L)
# )
# 
# mpoly <- function(list,varorder) {
#   
# }
# 
# 

# list <- unclass(structure(
#   list(
#     structure(
#       list(
#         coef = complex(real = -5, imaginary = 1),
#         core = c("z" = -1L)
#       ),
#       class = "mpoly_term"
#     ),
#     structure(
#       list(
#         coef = complex(real = -1),
#         core = c("x" = 2L, "y" = 1L)
#       ),
#       class = "mpoly_term"
#     ),
#     structure(
#       list(
#         coef = complex(real = 3),
#         core = c("x" = 2L, "y" = 1L)
#       ),
#       class = "mpoly_term"
#     )
#   ),
#   class = "bare_mpoly"
# ))
# 
# 
# list <- structure(
#   list(
#     structure(
#       list(
#         coef = complex(real = -5, imaginary = 1),
#         core = integer(0)
#       ),
#       class = "mpoly_term"
#     ),
#     structure(
#       list(
#         coef = complex(real = -1),
#         core = c("x" = 2L, "y" = 1L)
#       ),
#       class = "mpoly_term"
#     ),
#     structure(
#       list(
#         coef = complex(real = 2),
#         core = c("x" = 2L, "y" = 1L)
#       ),
#       class = "mpoly_term"
#     )
#   ),
#   class = "bare_mpoly"
# )
# 
# 
# 
# bare_mpoly(list)
# 
# bare <- structure(
#   list(
#     structure(
#       list(
#         coef = complex(real = -5, imaginary = 1),
#         core = integer(0)
#       ),
#       class = "mpoly_term"
#     ),
#     structure(
#       list(
#         coef = complex(real = -1),
#         core = c("x" = 2L, "y" = 1L)
#       ),
#       class = "mpoly_term"
#     ),
#     structure(
#       list(
#         coef = complex(real = 2),
#         core = c("x" = 2L, "y" = 1L)
#       ),
#       class = "mpoly_term"
#     )
#   ),
#   class = "bare_mpoly"
# )

bare_mpoly <- function(list, varorder){

  ## argument checking
  core_list <- lapply(list, function (x) x$`core`)
    
  core_equal <- function(index) {
    equal_cores <- which(core_list[(index + 1) : (length(core_list))] %in% core_list[index])
    if (length(equal_cores) == 0) {
      return (c(0,0))
    } else {
      return (c(index,index + equal_cores))
    }
  }
  
  # make vector of pairs where cores match
  core_pairs_matching <- as.vector(sapply(1:length(core_list) - 1, core_equal))
  
  # if there is a matching pair, make one coef the addition of the two, and the other one becomes zero
  combine_matching_cores <- function(index) {
    pair <- core_pairs_matching[(index * 2 - 1):(index * 2)]
    if (!identical(pair, c(0,0))) {
      coef1 <- list[[pair[1]]]$coef
      coef2 <- list[[pair[2]]]$coef
      new_coef <- coef1 + coef2
      list[[pair[1]]]$coef <<- 0 #get rid of one coef
      list[[pair[2]]]$coef <<- new_coef #add coefs and put into second coef
    }
    return (invisible(c()))
  }
  
  invisible(lapply(1:length(list), combine_matching_cores))
  
  # remove terms with coef 0
  list <- filterOutZeroTerms(list)
  
  
  
  
  ## set intrinsic varorder - done again after 0 degrees are removed
  vars <- unique(names(unlist(core_list)))
  
  
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

  

  
  
  ## return classed list
  class(list) <- "bare_mpoly"
  list  
}










filterOutZeroTerms <- function(list){
  v <- list[[1]]
  # a term is zero if any of its coefficients are 0
  hasNonZeroCoef <- function(v) (v$coef != 0)
  
  # remove terms with coef 0
  list <- fastFilter(hasNonZeroCoef, list)
  
  # if all terms have been eliminated, recreate it with a 0 coef
  if(length(list) == 0) list <- list(coef = 0, core = integer(0))  
  
  # return
  list  
}







isLengthOne    <- function(x) length(x) == 1L
isNotLengthOne <- function(x) length(x) > 1L
fastFilter     <- function(f, x) x[vapply(x, f, logical(1))]







fastNamedVecTapply <- function(x, f, type = double(1)){
  uniqueNames  <- unique(names(x))
  matchedNames <- match(names(x), uniqueNames) # indices
  matchedNames <- factor(matchedNames, levels = 1:max(matchedNames))
  groupIndices <- split.default(1:length(x), matchedNames)
  out <- vapply(groupIndices, function(ndcs) f(x[ndcs]), type)
  names(out) <- uniqueNames
  out
}
# x <- 1:10
# names(x) <- sample(letters[1:3], 10, replace = TRUE)
# fastNamedVecTapply(x, sum)
# tapply(x, names(x), sum)



