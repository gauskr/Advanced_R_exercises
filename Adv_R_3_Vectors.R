
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

has_comment <- function(x) {test <- mean(!is.na(comment(x))) != 0
if (test == TRUE) {
  TRUE
} else FALSE
}

has_comment2 <- function(x) !is.null(comment(x))

is.null(NULL)
has_comment(x)
has_comment(orange$Tree)
has_comment(orange$age)
has_comment2(orange$age)


#because
is.na(NULL)
logical(0)
logical(4)
?logical

# 3.4 S3 atomic vectors ---------------------------------------------------

# 3.4.1 Factors -----------------------------------------------------------

?factor
x <-  factor(c("a", "b", "b", "a"))
x

typeof(x)
attributes(x)

sex_char <- c("m", "m", "m")
sex_factor <- factor(sex_char, levels = c("m", "f"))

table(sex_char)
table(sex_factor)

grade <- ordered(c("b", "b", "a", "c"), levels = c("c", "b", "a"))
grade
table(grade)


# 3.4.2 Dates -------------------------------------------------------------

today <- Sys.Date()
typeof(today)
attributes(today)

date <- as.Date("1970-02-01")
unclass(date)


# 3.4.3 Date-times --------------------------------------------------------

now_ct <- as.POSIXct("2018-08-01 22:00", tz = "UTC")
now_ct
typeof(now_ct)
class(now_ct)

structure(now_ct, tzone = "Asia/Tokyo")
structure(now_ct, tzone = "America/New_York")
structure(now_ct, tzone = "Australia/Lord_Howe")
structure(now_ct, tzone = "Europe/Paris")
structure(now_ct, tzone = "Europe/Oslo")


# 3.4.4 Durations ---------------------------------------------------------

one_week_1 <- as.difftime(1, units = "weeks")
one_week_1

typeof(one_week_1)
attributes(one_week_1)

one_week_2 <- as.difftime(7, units = "days")
one_week_2

typeof(one_week_2)
attributes(one_week_2)


# 3.4.5 Exercises ---------------------------------------------------------

# 1. What sort of object does table() return?
# What is its type?
# What attributes does it have?
# How does the dimensionality change as you tabulate more variables?

object_table <- table(cars$speed)
 typeof(object_table)
 attributes(object_table)

object_table_2 <- table(cars$speed, cars$dist)
typeof(object_table_2)
attributes(object_table_2)

# 2. What happens to a factor when you modify its levels?

f1 <- factor(letters)
class(f1)
attributes(f1)
typeof(f1)

levels(f1) <- rev(levels(f1))

class(f1)
attributes(f1)
typeof(f1)

?levels
?gl
y <- gl(2, 4, 8)
y
y*2
levels(y) <- c("low", "high")
y
as.numeric(y)

# Ans. the level-attribute changes.

# 3. What does this code do?
# How do f2 and f3 differ from f1?

f2 <- rev(factor(letters)) # factors are same as f1, but vector is reverse

f3 <- factor(letters, levels = rev(letters)) # same vector, but reverse levels

##
f1 <- factor(letters)

f1
f3
f2


# 3.5 Lists ---------------------------------------------------------------


# 3.5.1 Creating ----------------------------------------------------------


l1 <- list(1:3,
           "a",
           c(TRUE, FALSE, TRUE),
           c(2.3, 5.9))

typeof(l1)
str(l1)

lobstr::obj_size(mtcars)
l2 <- list(mtcars, mtcars, mtcars, mtcars)
lobstr::obj_size(l2)

l3 <- list(list(list(1)))
str(l3)

l4 <- list(list(1, 2), c(3, 4))
l5 <- c(list(1,2), c(2,4))

str(l4)
str(l5)

# 3.5.2 Testing and coercion ----------------------------------------------

list(1:3)
as.list(1:3)

# 3.5.3 Matrices and arrays -----------------------------------------------

l <- list(1:3, "a", TRUE, 1.0)
dim(l) <- c(2,2)
l[[1,1]]

# 3.5.4 Exercises ---------------------------------------------------------

# 1. List all the ways that a list differs from an atomic vector.
# A list can have any kind of objekt. An atomic vector has only one type of content.
# Or... lists consist of references (same objects) to objects of any kind, while
# atomic vectors consist of references to one type of objects (raw, complex, logical,
# integer, double, character).
 l <- list(",", TRUE, 3.4)
attributes(l)

# 2. Why do you need to use unlist() to convert a list to an atomic vector?
# Why doesn’t as.vector() work?
?unlist
?as.vector
unlist(l)
as.vector(l)

df <- data.frame(x = 1:3, y = 5:7)
## Error:
try(as.vector(data.frame(x = 1:3, y = 5:7), mode = "numeric"))

x <- c(a = 1, b = 2)
is.vector(x)
as.vector(x)
all.equal(x, as.vector(x)) ## FALSE


###-- All the following are TRUE:
is.list(df)
! is.vector(df)
! is.vector(df, mode = "list")

is.vector(list(), mode = "list")
unlist
as.vector
?.Internal

## Ans. A vector in R is either an atomic vector i.e., one of the atomic types,
## see ‘Details’, or of type (typeof) or mode list or expression.
## Therefore, nothing happens when using as-vector() on a list. It already is a list!

# 3. Compare and contrast c() and unlist() when combining a date and date-time into
# a single vector.

ddate <- as.Date("2023-02-06")
ddatetime <- as.POSIXct("2023-02-06 22:00", tz = "UTC")

c(ddate, ddatetime) |> attributes() # both are Date-class
unlist(ddate, ddatetime) # only one. Hm. Why?

unlist(list(ddatetime, ddate)) # get it...
unlist(list(ddatetime, ddate)) |> attributes() # NULL

ellen.bugen@regnskap-kontor.no

?as.POSIXct
as.POSIXct
?as.Date()
as.Date


# 3.6 Data frames and tibbles ---------------------------------------------

df1 <- data.frame(x = 1:3, y = letters[1:3])
typeof(df1)
attributes(df1)

library(tibble)

df2 <- tibble(x = 1:3, y = letters[1:3])
typeof(df2)
attributes(df2)

# 3.6.1 Creating ----------------------------------------------------------

df <- data.frame(
  x = 1:3,
  y = c("a", "b", "c")
)
str(df)

df1 <- data.frame(
  x = 1:3,
  y = c("a", "b", "c"),
  stringsAsFactors = FALSE
)
str(df1)

df2 <- tibble(
  x = 1:3,
  y = c("a", "b", "c")
)
str(df2)

names(data.frame(`1` = 1))
names(tibble(`1` = 1))


data.frame(x = 1:4, y = 1:2)
data.frame(x = 1:4, y = 1:3) # ERROR
tibble(x = 1:4, y = 1)
tibble(x = 1:4, y = 1:2) # ERROR

tibble(
  x = 1:3,
  y = x*2
)

data.frame(
  x = 1:3,
  y = x*2
)


# 3.6.2 Row names ---------------------------------------------------------

df3 <- data.frame(
  age = c(35, 27, 18),
  hair = c("blonde", "brown", "black"),
  row.names = c("Bob", "Susan", "Sam")
)
df3

rownames(df3)

df3["Bob", ]
df3[c(1, 1, 1), ]
as_tibble(df3, rownames = "name")

# 3.6.3 Printing

dplyr::starwars

df1 <- data.frame(xyz = "a")
df2 <- tibble(xyz = "a")

str(df1$x)
str(df2$x)


# 3.6.5 Testing and coercing ----------------------------------------------

is.data.frame(df1)
is.data.frame(df2)

is_tibble(df1)
is_tibble(df2)


# 3.6.6 List columns ------------------------------------------------------

df <- data.frame(x = 1:3)
df$y <- list(1:2, 1:3, 1:4)

data.frame(x = 1:3,
           y = I(list(1:2, 1:3, 1:4)))

tibble(x = 1:3,
       y = list(1:2, 1:3, 1:4))


# 3.6.7 Matrix and data frame columns -------------------------------------

dfm <- data.frame(x = 1:3*10)
dfm$y <- matrix(1:9, nrow = 3)
dfm$z <- data.frame(a = 3:1, b = letters[1:3], stringsAsFactors = FALSE)

str(dfm)

dfm[1,]


# 3.6.8 Exercises ---------------------------------------------------------

# 1.Can you have a data frame with zero rows? What about zero columns?
names(dfm)
dfm[,-c("x", "y", "z")]

data.frame(NULL)
tibble(NULL)

# Answer: Yes.

# 2. What happens if you attempt to set rownames that are not unique?
df <- data.frame(x = 1:3,
           x = 4:6)
names(df)

df2 <- tibble(x = 1:3,
                 x = 4:6)

# data.frame() assigns a number which makes the name unique. tibble() throws
# an error.

# 3. If df is a data frame, what can you say about t(df), and t(t(df))?
# Perform some experiments, making sure to try different column types.

t(t(df)) == df

df$y <- letters[3:1]
df$z <- c(T,F,T)
df
t(df)
t(t(df))

df$y <- c(T,F,T)
df
t(df)
t(t(df))

# with t(df) the resulting columns are interpreted as variables. Thus rules
# for implicit coercion applies. This means, if the variables in df are of
# different types, then the entire data fram t(df) will be of the same type
# as the most complex variable in df. Therefore, t(t(df)) != df.

# What does as.matrix() do when applied to a data frame with columns of
# different types?

df <- data.frame(x = 1:3,
                 y = letters[4:6],
                 x = c(F,T,T),
                 stringsAsFactors = F
)
# How does it differ from data.matrix()?
as.matrix(df) # converts to most complex.
data.matrix(df) # treats non-factor character string as factor. Converts
# to numeric.
?data.matrix

