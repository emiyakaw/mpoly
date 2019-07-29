# mpoly

#bare_mpoly(list)

p <- structure(
  list(
    structure(
      list(
        coef = complex(real = -5, imaginary = 1),
        core = integer(0)
      ),
      class = "mpoly_term"
    ),
    structure(
      list(
        coef = complex(real = -1),
        core = c("x" = 2L, "y" = 1L)
      ),
      class = "mpoly_term"
    )
  ),
  class = "bare_mpoly"
)

list <- unclass(structure(
  list(p, p, p, p),
  class = "mpoly",
  coefring = "numeric",
  vars = c("x", "y"),
  .Dim = c(2L,2L)
))

mpol <- structure(
  list(p),
  class = "mpoly",
  coefring = "numeric",
  #  vars = c("x", "y"),
  .Dim = c(1L,1L)
)

mpol2 <- structure(
  list(p, p, p, p, p, p, p, p),
  class = "mpoly",
  coefring = "numeric",
  vars = c("x", "y"),
  .Dim = c(2L,2L,2L)
)

mpoly(list)

list <- unclass(structure(
  list(p, p, p, p),
  class = "mpoly",
  coefring = "numeric",
  vars = c("x", "y"),
  .Dim = c(2L,2L)
))

mpoly <- function(list, varorder){
  
  ## argument checking
  if(!is.list(list)) stop("input to mpoly must be a list.", call. = FALSE)
  
  class(list) <- "mpoly"
  list  
}
