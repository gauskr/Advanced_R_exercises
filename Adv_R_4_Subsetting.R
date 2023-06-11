
# 4 Subsetting ------------------------------------------------------------


# 4.1 Introduction --------------------------------------------------------


# 4.2 Selecting multiple elements -----------------------------------------


# 4.2.1 Atomic vectors ----------------------------------------------------

x <- c(2.1, 4.2, 3.3, 5.4)

# Positive integers

x[c(3, 1)]
x[order(x)]
# Duplicate indices will duplicate values
x[c(1,1)]
# Real numbers are silently truncated to integers
x[c(2.1, 2.9)]

# Negative integers

x[-c(3, 1)]
x[c(-1, 2)] # Error

# Logical vectors

x[c(TRUE, TRUE, FALSE, FALSE)]

x[x > 3]

x[c(TRUE, FALSE)]
x[c(TRUE, FALSE, TRUE, FALSE)]

x[c(TRUE, TRUE, NA, FALSE)]

# Nothing

x[]

# Zero

x[0]

# character vectors
(x <- setNames(x, letters[1:4]))

y[c("d", "c", "a")]
y[c("a", "a", "a")]

z <- c(abc = 1, def = 2)
z[c("a", "d")]

y[factor("b")]

# 4.2.2 Lists -------------------------------------------------------------


# 4.2.3 Matrices and arrays -----------------------------------------------

a <- matrix(1:9, nrow = 3)
colnames(a) <- c("A", "B", "C")
a[1:2, ]
a[c(TRUE, FALSE, TRUE), c("B", "A")]
a[0, -2]

a[1, ]
a[1, 1]

vals <- outer(1:5, 1:5, FUN = "paste", sep = ",")
vals

vals[c(4, 15)]

select <- matrix(ncol = 2, byrow = TRUE, c(
  1, 1,
  3, 1,
  2, 4
))

vals[select]


# 4.2.4 Data frames and tibbles -------------------------------------------

df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
df[df$x == 2, ]
df[c(1, 3), ]

# There are two ways to select columns from a data frame
# Like a list

df[c("x", "z")]

# Like a matrix

df[, c("x", "z")]

# There's an important difference if you select a single
# column: matrix subsetting simplifies by default, list
# subsetting does not.

str(df["x"])
str(df[, "x"])

df <- tibble::tibble(x = 1:3, y = 3:1, z = letters[1:3])
str(df["x"])
str(df[, "x"])

# 4.2.5 Preserving dimensionality -----------------------------------------

a <- matrix(1:4, nrow = 2)
str(a[1, ])
str(a[1, , drop = FALSE])

df <- data.frame(a = 1:2, b = 1:2)
str(df[, "a"])
str(df[, "a", drop = FALSE])

z <- factor(c("a", "b"))
z[1]
z[1, drop = TRUE]

# 4.2.6 Exercises ---------------------------------------------------------

# 1. Fix each of the following common data frame subsetting errors:

mtcars[mtcars$cyl = 4, ]
mtcars[-1:4, ]
mtcars[mtcars$cyl <= 5]
mtcars[mtcars$cyl == 4 | 6, ]

# Answers:

mtcars[mtcars$cyl == 4, ]
mtcars[-c(1:4), ]
mtcars[mtcars$cyl <= 5, ]
mtcars[mtcars$cyl == 4 | mtcars$cyl == 6, ]
# OR:
mtcars[mtcars$cyl %in% c(4, 6), ]

# 2. Why does the following code yield five missing values?
# (Hint: why is it different from x[NA_real_]?)

x <- 1:5
x[NA]

# Ans:
x[NA_real_]
x[c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)]

# Here I had to "cheat", and consulted the solutions book. This
# turned out to having to do with NA being logical, and NA_real_
# being something else. NA recycles to the length of the vector,
# while NA_real_ doesn't recycle!

# 3. What does upper.tri() return?
# How does subsetting a matrix with it work?
# Do we need any additional subsetting rules to describe its behaviour?

x <- outer(1:5, 1:5, FUN = "*")
x[upper.tri(x)]

# Answers:
upper.tri(x)
# It returns a logical matrix, where the upper triangle (above the diagonal)
# is TRUE.

x[upper.tri(x), drop = FALSE]
str(x[upper.tri(x), drop = FALSE])
# Subsetting with a logical matrix returns the corresponding vector, and dropping
# dimensionality even when specifying drop = TRUE.

# This is, as far as I could remember, an extension of previous discussed
# subsetting rules.
upper.tri
?upper.tri

# 4. Why does mtcars[1:20] return an error?
# How does it differ from the similar mtcars[1:20, ]?

# ans.
mtcars[1:20]
# this snippet tries to select colums 1 through 20.
mtcars[1:20, ]
# this selects rows 1:20.

# 5. Implement your own function that extracts the diagonal entries from
# a matrix (it should behave like diag(x) where x is a matrix).

pull_diag <- function(m, return_matrix = FALSE) {
if (dim(m)[1] == dim(m)[2]) {
  select <- 1:dim(m)[1]^2
  select <- select[select %% (dim(m)[1] + 1) == 1]

  if (return_matrix == FALSE) {
  return(select)
  } else {
  m <-  matrix(rep(0, dim(m)[1]^2), nrow = dim(m)[1])
  m[select] <- select
  return(m)
  }
} else stop("This is NOT a rectangular matrix!")
}

# Test:

m <- matrix(1:25, nrow = 5)
m
pull_diag(m)
pull_diag(m, TRUE)
m2 <- matrix(1:100, nrow = 10)
m2
pull_diag(m2)
pull_diag(m2, return = T)
m3 <- matrix(1:30, nrow = 6)
pull_diag(m3)

# 6. What does df[is.na(df)] <- 0 do? How does it work?
# Ans. This codde should exchange every NA with 0.
# Test:

df <- data.frame(x = c(1, NA, 1), y = c(3, 2, NA), z = c(TRUE, FALSE, NA))
df
is.na(df)
df[is.na(df)] <- 0
df

# And it did!!

# 4.3 Selecting a single element ------------------------------------------

# 4.3.1 [[ ----------------------------------------------------------------

x <- list(1:3, "a", 4:6)
for (i in 2:length(x)) {
  out[i] <- fun(x[i], out[i - 1])
}

for (i in 2:length(x)) {
  out[[i]] <- fun(x[[i]], out[[i - 1]])
}

# 4.3.2 $ -----------------------------------------------------------------

var <- "cyl"
# Doesn't work - mtcars$var translated to mtcars[["var"]]
mtcars$var

# Instead use [[
mtcars[[var]]

x <- list(abc = 1)
x$a
x[["a"]]

options(warnPartialMatchDollar = TRUE)
x$a

# 4.3.3 Missing and out-of-bounds indices ---------------------------------

x <- list(a = c(1, 2, 3),
          b = c(3, 4, 5))

purrr::pluck(x, "a", 1)
purrr::pluck(x, "c", 1)
purrr::pluck(x, "c", 1, .default = NA)

# 4.3.4 @ and slot() ------------------------------------------------------

# Operators for S4 which is, appr., the same kinds of functions as $ and [[

# 4.3.5 Exercises ---------------------------------------------------------

# 1. Brainstorm as many ways as possible to extract the third value from
# the cyl variable in the mtcars dataset.

mtcars[[3, "cyl"]]
rownames(mtcars)[3]
mtcars[["Datsun 710", "cyl"]]
mtcars[[3,2]]
mtcars[["Datsun 710",2]]
cyl <- mtcars$cyl
cyl[3]

m <- matrix(rep(FALSE, dim(mtcars)[1]*dim(mtcars)[2]), nrow = dim(mtcars)[1])
m[3,2] <- TRUE
mtcars[m]


# 2. Given a linear model, e.g., mod <- lm(mpg ~ wt, data = mtcars),
# extract the residual degrees of freedom. Then extract the R squared from
# the model summary (summary(mod))

solve_assignment_2 <- function(...) {
mod <- lm(...)
modsum <- summary(mod)
dfresid <- modsum$df
cutetable <- tibble::tibble(
  df_resid = dfresid[2],
  r_squared = modsum$r.squared
)
cat("This table/dataframe solves assignment 2:")
return(cutetable)
}

solve_assignment_2(mpg ~ wt, data = mtcars)

# 4.4 Subsetting and assignment -------------------------------------------

x <- 1:5
x[c(1, 2)] <- c(101, 102)
x

x <- list(a = 1, b = 2)
x[["b"]] <- NULL
str(x)

y <- list(a = 1, b = 2)
y["b"] <- list(NULL)
str(y)

mtcars[] <- lapply(mtcars, as.integer)
is.data.frame(mtcars)

mtcars <- lapply(mtcars, is.integer)
is.data.frame(mtcars)

# 4.5 Applications --------------------------------------------------------

# 4.5.1 Lookup tables (character subsetting) ------------------------------

x <- c("m", "f", "u", "f", "f", "m", "m")
lookup <- c(m = "Male", f = "Female", "u" = NA)
lookup[x]
unname(lookup[x])

# 4.5.2 Matching and merging by hand (integer subsetting) -----------------

grades <- c(1, 2, 2, 3, 1)

info <- data.frame(
  grade = 3:1,
  desc = c("Exellent", "Good", "Poor"),
  fail = c(T, T, F)
)

id <- match(grades, info$grade)
id
info[id, ]

# 4.5.3 Random samples and bootstraps (integer subsetting) ----------------

df <- data.frame(x = c(1, 2, 3, 1, 2), y = 5:1, z = letters[1:5])
# Randomly reorder
df[sample(nrow(df)), ]

# Select 3 random rows
df[sample(nrow(df), 3), ]

# Select 6 bootstrap replicates
df[sample(nrow(df), 6, replace = TRUE), ]

# 4.5.4 Ordering (integer subsetting) -------------------------------------

x <- c("b", "c", "a")
order(x)
x[order(x)]

df2 <- df[sample(nrow(df)), 3:1]
df2[order(df2$x), ]
df2[ , order(names(df2))]

# 4.5.5 Expanding aggregated counts (integer subsetting) ------------------

df <- data.frame(x = c(2, 4, 1), y = c(9, 11, 6), n = c(3, 5, 1))
rep(1:nrow(df), df$n)
df[rep(1:nrow(df), df$n), ]

# 4.5.6 Removing columns from data frames (character ) --------------------

df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
df
df$z <- NULL
df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
df[c("x", "y")]
df[setdiff(names(df), "z")]

# 4.5.7 Selecting rows based on a condition (logical subsetting) ----------
rm(mtcars) # if mtcars is list...
mtcars[mtcars$gear == 5, ]
mtcars[mtcars$gear == 5 & mtcars$cyl == 4, ]

# 4.5.8 Boolean algebra versus sets (logical and integer ) ----------------

x <- sample(10) < 4
which(x)

unwhich <- function(x, n) {
  out <- rep_len(FALSE, n)
  out[x] <- TRUE
  out
}

unwhich(x, 10)

(x1 <- 1:10 %% 2 == 0)
(x2 <- which(x1))
(y1 <- 1:10 %% 5 == 0)
(y2 <- which(y1))

# X & Y <-> intersect(x, y)
x1 & y1
intersect(x2, y2)

# X | Y <-> union(x, y)
x1 | y1
union(x2, y2)

# X & !Y <-> setdiff(x, y)
x1 & !y1
setdiff(x2, y2)

# xor(X, Y) <-> setdiff(union(x, y), intersect(x, y))
xor(x1, y1)
setdiff(union(x2, y2), intersect(x2, y2))

# 4.5.9 Exercises

# 1. How would you randomly permute the columns of a data frame?
# (This is an important technique in random forests.)
# Can you simultaneously permute the rows and columns in one step?

# Answer:
df <- data.frame(x = 1:8, y = 8:1, z = letters[1:8], æ = letters[8:1])
df
# random rows and cols:
df[sample(nrow(df)), sample(ncol(df))]
# just columns:
df[, sample(ncol(df))]

# 2. How would you select a random sample of m rows from a data frame?
# What if the sample had to be contiguous (i.e., with an initial row, a final row, and
# every row in between)?

contiguous_row_sampling <- function(df) {
rbind(df[1,],
      df[-c(1, nrow(df)), ][sample(nrow(df)-2), ],
      df[nrow(df),])
}

contiguous_row_sampling(df)
contiguous_row_sampling(mtcars[1:5,])

# 3. How could you put the columns in a data frame in alphabetical order?

order_alphabetical_cols <- function(df) {
  df[, order(names(df))]
}

order_alphabetical_cols(mtcars[1:5,])

# DOING THE QUIZ ----------------------------------------------------------

# 1. What is the result of subsetting a vector with positive integers,
# negative integers,
# a logical vector,
# or a character vector?

# Answer. the result can be the same. If one knows the number of cols one wish to use,
# positive integers will do - they select the elements indicated by numbers.
# Negative integers will remove elements indicated by number.
# A logical vector will keep elements corresponding to TRUE, and remove elements
# corresponding to FALSE. While a character vector select elements by name, if they are
# named. This can also be done negatively, by using - before c()- vector.

vec %in% "c" # This is a logical vector!!

# 2. What’s the difference between [, [[, and $ when applied to a list?
# Answer: [ returns element as list.
# [[ returns element.
# The same does $

# 3. When should you use drop = FALSE?
# Ans. When you need to retain dimensionality!


# 4. If x is a matrix, what does x[] <- 0 do? How is it different from x <- 0?
# Answer: it sets every "cell" to zero

df <- as.matrix(df)
df[] <- 0
df

m <- matrix(1:9, nrow = 3)
m[] <- 0
m

# 5. How can you use a named vector to relabel categorical variables?
# Answer: like this

marry_fuck_kill <- stringr::str_split("m,m,m,m,f,f,k,k,k,f,m,f,f,k,k", ",")[[1]]
map_vec <- c(m = "Marry" , f = "Fuck", k = "Kill")
map_vec[marry_fuck_kill]
