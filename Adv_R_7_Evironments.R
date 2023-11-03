
# 7.1 Introduction --------------------------------------------------------


library(rlang)

# 7.2 Environment basics --------------------------------------------------

# 7.2.1 Basics

e1 <- rlang::env(
  a = FALSE,
  b = "a",
  c = 2.3,
  d = 1:3
)

e1$d <- e1 # e1 now contains itself.

e1 # not very useful

rlang::env_print(e1) # more useful

rlang::env_names(e1)
names(e1) # R 3.2 or higher.


# 7.2.2 Important environments --------------------------------------------

identical(current_env(), global_env())

current_env() == global_env() # ERROR. Because == is a vectorized function.


# 7.2.3 Parents -----------------------------------------------------------

e2a <- env(d = 4, e = 5)
e2b <- env(e2a, a = 1, b = 2, c = 3)

rlang::env_parent(e2a)
rlang::env_parent(e2b)

e2c <- env(empty_env(), d = 4, e = 5)
e2d <- env(e2c, a = 1, b = 2, c = 3)

env_parents(e2d)
env_parents(e2b)

env_parents(e2b, last = empty_env())

# 7.2.4 Super assignment, <<- ---------------------------------------------

x <- 0

f <- function() {
  x <<- 1
}

f()
x

# 7.2.5 Getting and setting -----------------------------------------------

e3 <- env(x = 1, y = 2)
e3$x
e3$z <- 3
e3[["z"]]

e3[[1]]
e3[c("x", "y")]

e3$xyz # returns NULL
env_get(e3, "xyz") # throws an error
env_get(e3, "xyz", default = NA) # returns a specific value

