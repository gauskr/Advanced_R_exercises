
# 6 Functions -------------------------------------------------------------

# 6.1 Introduction --------------------------------------------------------

# 6.2 Function fundamentals -----------------------------------------------

# 6.2.1 Function components -----------------------------------------------

f02 <- function(x, y) {
  # A comment
  x + y
}

formals(f02)
body(f02)
environment(f02)
attr(f02, "srcref")

# 6.2.2 Primitive functions -----------------------------------------------

sum # Primitive functions calls C code directly.
`[`
typeof(sum)
typeof(`[`)
formals(sum)
body(sum)
environment(sum)

# 6.2.3 First-class functions ---------------------------------------------

f01 <- function(x) {
  sin(1 / x ^ 2)
}

lapply(mtcars, function(x) length(unique(x)))
Filter(function(x) !is.numeric(x), mtcars)
integrate(function(x) sin(x) ^ 2, 0, 1)
