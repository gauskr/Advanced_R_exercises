
# 3. Vectors --------------------------------------------------------------


# 3.1 Introduction --------------------------------------------------------


# 3.2 Atomic vectors ------------------------------------------------------


# 3.2.1 Scalars -----------------------------------------------------------


# 3.2.2 Making longer vectors with c() ------------------------------------

lgl_var <- c(TRUE, FALSE)
int_var <- c(1L, 6L, 10L)
dbl_var <- c(1, 2.5, 4.5)
chr_var <- c("these are", "some strings")

c(c(1, 2), c(3, 4))

typeof(lgl_var)
typeof(int_var)
typeof(dbl_var)
typeof(chr_var)


# 3.2.3 Missing values ----------------------------------------------------

NA > 5
10 * NA
!NA

NA ^ 0
NA | TRUE
NA & FALSE

x <- c(NA, 5, NA, 10)
x == NA

is.na(x)


# 3.2.4 Testing and coercion ----------------------------------------------

str(c("a", 1))
x <- c(FALSE, FALSE, TRUE)
as.numeric(x)

# Total number of TRUEs
sum(x)

# Proportion that are TRUE
mean(x)

as.integer(c("1", "1.5", "a"))
?as.integer


# 3.2.5 Exercises ---------------------------------------------------------

# How do you create raw and complex scalars? (See ?raw and ?complex.)
?raw
xx <- raw(2)
xx # "Empty" raw scalars within a raw vector.
xx[1] <- as.raw(40)     # NB, not just 40.
xx[2] <- charToRaw("A")
xx       ## 28 41   -- raw prints hexadecimals


as.numeric(xx)
rawToChar(xx)
x <- raw(2)
x[1] <- 28 # error

?complex

# Test your knowledge of the vector coercion rules by predicting the
# output of the following uses of c():

c(1, FALSE)
# pred c(1, 0)
c("a", 1)
# pred c("a", "1")
c(TRUE, 1L)
# c(1L, 1L)  # wrong.. 1 1
# and, of course, no c(x,x), just x x in print of objects...


# Why is 1 == "1" true? Why is -1 < FALSE true? Why is "one" < 2
# false?
"1" == "1"
-1 < 0
"one" < "2"
# Easy peasy

# Why is the default missing value, NA, a logical vector?
# What’s special about logical vectors?
# (Hint: think about c(FALSE, NA_character_).)
typeof(NA)
typeof(c(FALSE, NA_character_))
typeof(c(FALSE, NA_complex_))
typeof(c(FALSE, NA_integer_))
typeof(c(FALSE, NA_real_))
c(FALSE, NA_complex_)

# Precisely what do is.atomic(), is.numeric(), and is.vector() test
# for?
?is.atomic
# is.atomic is true for the atomic types
# ("logical", "integer", "numeric", "complex", "character" and "raw") and
# NULL.

?is.numeric
# is.numeric is a more general test of an object being interpretable as
# numbers.
is.numeric(2i)
is.numeric(3.2)
is.numeric(1L)

?is.vector
# is.vector(x) returns TRUE if x is a vector of the specified mode having
# no attributes other than names. For mode="any", see ‘Details’.
is.vector(4)
is.vector(4, mode = "character")
is.vector(4i)

# 3.3 Attributes


# 3.3.1 Getting and setting -----------------------------------------------

?attr

a <- 1:3
attr(a, "x") <- "abcdef"
attr(a, "x")

a <- 1:3
attr(a, "x") <- "abcdef"
attr(a, "x")

attr(a, "y") <- 4:6
str(attributes(a))

# Or equivalently
a <- structure(
  1:3,
  x = "abcdef",
  y = 4:6
)

b <- structure(
  1:3,
  x = "abcde",
  y = 4:6
)

str(attributes(b))

a == b
b

attributes(a[1])
attributes(sum(a))

# 3.3.2 Names -------------------------------------------------------------

x <- c(a = 1, b = 2, c = 3)
x

x <- 1:3
names(x) <- c("a", "b", "c")
x

x <- setNames(1:3, c("a", "b", "c"))
x


# 3.3.3 Dimensions --------------------------------------------------------

x <- matrix(1:6, nrow = 2, ncol = 3)
x

y <- array(1:12, c(2, 3, 2))
y

z <- 1:6
dim(z) <- c(3, 2)
z

?names
rownames
colnames
dimnames
length
nrow
ncol
dim
?c
rbind
cbind
?abind::abind
t
aperm
is.null(dim())
is.matrix
is.array

str(c(1:3))
str(matrix(1:3, ncol = 1))
str(matrix(1:3, nrow = 1))
str(array(1:3, 3))

# 3.3.4 Exercises ---------------------------------------------------------

# How is setNames() implemented? How is unname() implemented? Read the
# source code.
setNames
unname
# both is wrappers around names(object), and (in unname()) dimname(object)
#they do this (setNames())
names(object) <- nm

# and this (unname())
if (!is.null(names(obj)))
  names(obj) <- NULL
if (!is.null(dimnames(obj)) && (force || !is.data.frame(obj)))
  dimnames(obj) <- NULL
# and then return the objects

# 2. What does dim() return when applied to a 1-dimensional vector?
# When might you use NROW() or NCOL()?

# Ans, NULL
dim(c(1,2,3))

?NROW

# You can make useless functions like this:
dimx <- function(x) {
  if (!is.null(dim(x))) {
    return(dim(x))
  } else if (NROW(x) == 0) {
    return(NCOL(x))
  } else {
    return(NROW(x))
}
}
x <- 1:3
dimx(x)
dim(x)
dim(Orange)
dimx(Orange)
dimx(NULL)
# You can, therefore, use NROW() and/or NCOL() if you somehow need to treat
# dim(x) as something other then 0 for "zero-dimensional objects" like 1:3
# (which are, perhaps, intuitively thought of as 1D)

# 3. How would you describe the following three objects?
# What makes them different from 1:5?

x1 <- array(1:5, c(1, 1, 5))
x2 <- array(1:5, c(1, 5, 1))
x3 <- array(1:5, c(5, 1, 1))
# they are 3-dimensional objects, with "action" in different dimensions.
x1
x2
x3
dim(x1)
dim(x2)
dim(x3)
# all are different from x in the same way: they HAVE dimensions, while x
# do not (if mean(x == 1:5) == 1)
x <- c(1,2,3,4,5)

# An early draft used this code to illustrate structure():
structure(1:5, comment = "my attribute")
# But when you print that object you don’t see the comment attribute. Why?
# Is the attribute missing, or is there something else special about it?
# (Hint: try using help.)
?structure
# Contrary to other attributes, the comment is not printed (by print or
# print.default).
x <- matrix(1:12, 3, 4)
comment(x) <- c("This is my very important data from experiment #0234",
                "Jun 5, 1998")
x
comment(x)

has_comment <- function(x) mean(!is.na(comment(x))) != 0
has_comment(x)

# 3.4 S3 atomic vectors ---------------------------------------------------


