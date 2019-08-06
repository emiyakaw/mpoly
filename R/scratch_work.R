library(mpoly)




list <- unclass(structure(
  list(
    structure(
      list(
        coef = complex(real = -5, imaginary = 1),
        core = c("z" = -1L)
      ),
      class = "mpoly_term"
    ),
    structure(
      list(
        coef = complex(real = -1),
        core = c("x" = 2L, "y" = 1L)
      ),
      class = "mpoly_term"
    ),
    structure(
      list(
        coef = complex(real = 3),
        core = c("x" = 2L, "y" = 1L)
      ),
      class = "mpoly_term"
    )
  ),
  class = "bare_mpoly"
))


list <- structure(
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
    ),
    structure(
      list(
        coef = complex(real = 2),
        core = c("x" = 2L, "y" = 1L)
      ),
      class = "mpoly_term"
    )
  ),
  class = "bare_mpoly"
)



bare_mpoly(list)

bare <- structure(
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
    ),
    structure(
      list(
        coef = complex(real = 2),
        core = c("x" = 2L, "y" = 1L)
      ),
      class = "mpoly_term"
    )
  ),
  class = "bare_mpoly"
)


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

list <- structure(
  list(p, p, p, p),
  class = "mpoly",
  coefring = "numeric",
  vars = c("x", "y"),
  .Dim = c(2L,2L)
)

