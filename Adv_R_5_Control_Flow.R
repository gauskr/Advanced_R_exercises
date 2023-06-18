
# 5 Control flow ----------------------------------------------------------


# 5.1 Introduction --------------------------------------------------------


# 5.2 Choices -------------------------------------------------------------


if (condition) true_action
if (condition) true_action else false_action

grade <- function(x) {
  if (x > 90) {
    "A"
  } else if (x > 80) {
  "B"
  } else if (x > 50) {
  "C"
  } else {
  "F"
}
}

x1 <- if (TRUE) 1 else 2
x2 <- if (FALSE) 1 else 2

c(x1, x2)

greet <- function(name, birthday = FALSE) {
  paste0(
    "Hi ", name,
    if (birthday) " and Happy Birthday!"
  )
}

greet("Maria")
greet("Jaime", TRUE)

# 5.2.1 Invalid inputs ----------------------------------------------------

if ("x") 1
if (logical()) 1
if (NA) 1

if (c(TRUE, FALSE)) 1

# Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "true") # probably old...
# if (c(TRUE, FALSE)) 1

# 5.2.2 Vectorised if -----------------------------------------------------

x <- 1:10
ifelse(x %% 5 == 0, "XXX", as.character(x))
ifelse(x %% 2 == 0, "even", "odd")

dplyr::case_when(
  x == 35 ~ "fizz buzz",
  x %% 5 == 0 ~ "fizz",
  x %% 7 == 0 ~ "buzz",
  is.na(x) ~ "???",
  TRUE ~ as.character(x)
)

# 5.2.3 switch() statement ------------------------------------------------

x_option <- function(x) {
  if (x == "a") {
    "option 1"
  } else if (x == "b") {
    "option 2"
  } else if (x == "c") {
    "option 3"
  } else {
    stop("Invalid `x` value")
  }
}

x_option <- function(x) {
  switch (
    x,
    a = "option 1",
    b = "option 2",
    c = "option 3",
    stop("Invalid `x` code")
  )
}

(switch("c", a = 1, b = 2))

legs <- function(x) {
  switch (x,
    cow = ,
    horse = ,
    dog = 4,
    human = ,
    chicken = 2,
    plant = 0,
    stop("Unknown input")
  )
}

legs("cow")
legs("dog")

# 5.2.4 Exercises ---------------------------------------------------------

# What type of vector does each of the following calls to
# ifelse() return?

ifelse(TRUE, 1, "no")
ifelse(FALSE, 1, "no")
ifelse(NA, 1, "no")


# Read the documentation and write down the rules in your own words.

# Answer:

class(ifelse(TRUE, 1, "no"))
class(ifelse(FALSE, 1, "no"))
class(ifelse(NA, 1, "no"))

?ifelse
ifelse
class(NA)
# ifelse() returns numeric, character, logical. This is due to the
# returning yes, no or NA. class(NA) er logical...

# 2. Why does the following code work?
x <- 1:10
if (length(NULL)) "not empty" else "empty"
x <- numeric()
if (length(x)) "not empty" else "empty"
class(numeric())
# Ans. Because numeric() returns a numeric vector of length 0.

# 5.3 Loops ---------------------------------------------------------------

for (item in vector) perform_action

for (i in 1:3) {
  print(i)
}

i <- 100
for (i in 1:3) {}
i


for (i in 1:10) {
  if (i < 3)
    next

  print(i)

  if (i >= 5)
    break
}

# 5.3.1 Common pitfalls ---------------------------------------------------

means <- c(1, 50, 20)
out <- vector("list", length(means))
for (i in 1:length(means)) {
  out[[i]] <- rnorm(10, means[[i]])
}

means <- c()
out <- vector("list", length(means))
for (i in 1:length(means)) {
  out[[i]] <- rnorm(10, means[[i]])
}

1:length(means)

seq_along(means)

out <- vector("list", length(means))
for (i in seq_along(means)) {
  out[[i]] <- rnorm(10, means[[i]])
}
out

xs <- as.Date(c("2020-01-01", "2010-01-01"))
for (x in xs) {
  print(x)
}

for (i in seq_along(xs)) {
  print(xs[[i]])
}

# 5.3.2 Related tools -----------------------------------------------------

# while(condition) action: performs action while condition is TRUE.

# repeat(action): repeats action forever (i.e. until it encounters break).

# 5.3.3 Exercises ---------------------------------------------------------

# 1. Why does this code succeed without errors or warnings?
x <- numeric()
x
out <- vector("list", length(x))
for (i in 1:length(x)) {
  out[i] <- x[i] ^ 2
}
out

# Answer: x <- numeric() is really numeric(length = 0L), that is a
# numerical vector of length 0. This creates the object x, which simply
# associates a names to 0-dimensional numerical vector. Any numerical vector
# has numerical_vector[0] == numerical(0). It has no values specified for
# elements 1:N in x[1:N].
# Furthermore, any numerical_vector, where one calls elements out of bounds
# numerical_veltor[N+y] returns NA. Adding a numeric(0) to element 0 doesn't
# add anything. Therefore the for loop puts NA ^ 2 to out[1], which is NA,
# and then adds nothing in the x[0].
# This description is purely experimentally derived, I haven't seen it
# explicated in any documentation yet.

# 2. When the following code is evaluated, what can you say about the
# vector being iterated?

xs <- c(1, 2, 3)
for (x in xs) {
  xs <- c(xs, x * 2)
}
xs

# Ans. I expected this to become c(1, 2, 3, 6).
# I take the vector iterated to be c(xs, x * 2)
# This obviously associates c(xs, xs[1]*2, xs[2]*2, xs[3]*2) with the
# name xs. The entire for loop thus equals c(xs, xs*2).
# This would have to do with for (x in xs).
# Since seq_along(xs) and xs will do the same work, in this instance,
# And results become as I thought when changing the out vectors name to xz,
# this should mean that xs is simply updated. Which I should have thought
# of instantly...

# 3. What does the following code tell you about when the index is updated?

for (i in 1:3) {
    i <- i * 2
    print(i)
}

# I will answer this "in abstract" before checking out how it works out.
# This loop should print three figures. 2, 4 and 6.'
# And it did.
# The (i in 1:3) sends first 1 into i*2, then 2, then 3.

# Easy peasy.

## Quiz answers

# 1. What is the difference between if and ifelse()?

# Answer,

# ifelse is a vectorized version of if. It's done in a function.
# if can be paired with else, and is more flexible, escpecially if

# 2.  In the following code, what will the value of y be if x is
# TRUE? What if x is FALSE? What if x is NA?

y <- if (x) 3

if (T) 3
if (F) 3
if (NA) 3
                                                                                                    if (NA) 3
# Ans 3, no answer and error, for the cases x = T, x = F, nad x = NA.

# 3. What does switch("x", x = , y = 2, z = 3) return?

x <- c("x", "a", "y", "z", "a", "b", "c")
x2 <- lapply(x, switch, x = , y = 2, z = 3)
x2[unlist(lapply(x2, is.null))] <- list(NA)
unlist(x2)
?switch

# Ans. They will return, from vector x, 2 for values "x" and "y", and 3 for
# value "z".

# WRONG! switch() works with length 1 vectors. You will get 2!!!
