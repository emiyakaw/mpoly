# following check uses Euler's four-square identity:

library("mvp")

LHS <- as.mvp(
  "
  (a1^2 + a2^2 + a3^2 + a4^2)*
  (b1^2 + b2^2 + b3^2 + b4^2)
  "
)

RHS <- as.mvp("
   (a1*b1-a2*b2-a3*b3-a4*b4)^2
  +(a1*b2+a2*b1+a3*b4-a4*b3)^2
  +(a1*b3-a2*b4+a3*b1+a4*b2)^2
  +(a1*b4+a2*b3-a3*b2+a4*b1)^2
")

stopifnot(LHS==RHS)
