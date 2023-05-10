
# 2. Names and Values -----------------------------------------------------


# 2.1 Introduction ------------------------------------------------------------


library(lobstr)


# 2.2 Binding basics ------------------------------------------------------

x <- c(1, 2, 3)

y <- x

obj_addr(x)
obj_addr(y)


# 2.2.1 Non-syntactic names -----------------------------------------------

_abc <- 1
#> Error: unexpected input in "_"

if <- 10
#> Error: unexpected assignment in "if <-"

`_abc` <- 1
`_abc`
`if` <- 10
`if`

# 2.2.2 Exercises ---------------------------------------------------------


# 1. Explain the relationship between a, b, c and d in the following
# code:

a <- 1:10
b <- a
c <- b
d <- 1:10
lobstr::obj_addr(a)
lobstr::obj_addr(b) # same
lobstr::obj_addr(c) # same
lobstr::obj_addr(d) # a new object

# The following code accesses the mean function in multiple ways.
# Do they all point to the same underlying function object?
# Verify this with lobstr::obj_addr().

mean
base::mean
get("mean")
evalq(mean)
match.fun("mean")

lobstr::obj_addr(mean)
lobstr::obj_addr(get("mean")) # same
lobstr::obj_addr(evalq(mean)) # same
lobstr::obj_addr(match.fun("mean")) # same

# By default, base R data import functions, like read.csv(), will
# automatically convert non-syntactic names to syntactic ones. Why
# might this be problematic? What option allows you to suppress this
# behaviour?

?read.csv
test1 <- c(1:5, "6,7", "8,9,10")
tf <- tempfile()
writeLines(test1, tf)

df <- read.csv(tf, fill = TRUE)
names(df) <- "`_1`"
write.csv(df, file="df.csv")
read.csv("df.csv", check.names = FALSE)

# check.names can be set to FALSE. This might be useful if we need to
# use the original variable names.

# What rules does make.names() use to convert non-syntactic names
# into syntactic ones?

# he character "X" is prepended if necessary. All invalid characters
# are translated to ".". A missing value is translated to "NA".
# Names which match R keywords have a dot appended to them.
# Duplicated values are altered by make.unique.


?make.names
# I slightly simplified the rules that govern syntactic names.
# Why is .123e1 not a syntactic name? Read ?make.names for the full
# details.

# Because it's a dot followed by a number.


# 2.3 Copy-on-modify ------------------------------------------------------

x <- c(1, 2, 3)
y <- x

y[[3]] <- 4
x


# 2.3.1 tracemem() --------------------------------------------------------

x <- c(1, 2, 3)
cat(tracemem(x), "\n")

y <- x
y[[3]] <- 4L

y[[3]] <- 5L

untracemem(x)


# 2.3.2 Function calls ----------------------------------------------------

f <- function(a) {
  a
}

x <- c(1, 2, 3)
cat(tracemem(x), "\n")
z <- f(x)
# there's no copy here!

untracemem(x)


# 2.3.3 Lists -------------------------------------------------------------

l1 <- list(1, 2, 3)

l2 <- l1

l2[[3]] <- 4

ref(l1, l2)


# 2.3.4 Data frames -------------------------------------------------------

d1 <- data.frame(x = c(1, 5, 6), y = c(2, 4, 3))
d2 <- d1
d2[, 2] <- d2[, 2] * 2
d3 <- d1
d3[1, ] <- d3[1, ] * 3

ref(d1,d2)
ref(d1,d3)


# 2.3.5 Character vectors -------------------------------------------------

x <- c("a", "a", "abc", "d")
ref(x, character = TRUE)


# 2.3.6 Exercises ---------------------------------------------------------


# Why is tracemem(1:10) not useful?
# Because the object isn't saved

# Explain why tracemem() shows two copies when you run this code.
# Hint: carefully look at the difference between this code and the
# code shown earlier in the section.

x <- c(1L, 2L, 3L)
tracemem(x)
x[[3]] <- 4

# It's one copy for the copy-on, but then another for turning it into
# a double.

# Sketch out the relationship between the following objects:

a <- 1:10
b <- list(a, a)
c <- list(b, a, 1:10)

ref(a, b)
ref(a, c)
ref(b, c)

# Ans = look at the trees...

# What happens when you run this code?

x <- list(1:10)
x[[2]] <- x

# Draw a picture.
lobstr::ast(x[[2]] <- x)
x <- list(1:10)
ref(x)
x[[2]] <- x
ref(x)
x

# it creates a copy of the list(1:10), and alters initial x so that
# the copy-on-modyfied list includes two of the same lists as the
# name x originally was associated with. Puh! This is kind of
# philosophical!


# 2.4 Object size ---------------------------------------------------------

obj_size(letters)
obj_size(ggplot2::diamonds)

x <- runif(1e6)
obj_size(x)
y <- list(x, x, x)
obj_size(y)
?obj_size
obj_sizes(x,y)

obj_size(list(NULL, NULL, NULL))

banana <- "bananas bananas bananas"
obj_size(banana)
obj_size(rep(banana, 100))
obj_size(x, y)

obj_size(1:3)
obj_size(1:1e3)
obj_size(1:1e6)
obj_size(1:1e9)


# 2.4.1 Exercises ---------------------------------------------------------


# In the following example, why are object.size(y) and obj_size(y)
# so radically different? Consult the documentation of object.size().

y <- rep(list(runif(1e4)), 100)
object.size(y)
obj_size(y)
?object.size

# This function merely provides a rough indication: it should be
# reasonably accurate for atomic vectors, but does not detect if
# elements of a list are shared, for example. (Sharing amongst
# elements of a character vector is taken into account, but not that
# between character vectors in a single object.)

# Therefore, the difference is due to obj_size() more proper
# adjusting for shared elements.

# Take the following list. Why is its size somewhat misleading?

funs <- list(mean, sd, var)
obj_size(funs)
obj_size(mean)+
obj_size(sd)+
obj_size(var)
ref(mean)
ref(var)
ref(sd)
var
sd
mean
?obj_size

# My hypotheses is functions refer to objects/functions. sd should
# be bigger than var, but clearly isn't measured as such. This, then,
# means the other functions might be misleading also.

# Predict the output of the following code:

a <- runif(1e6)
obj_size(a)

b <- list(a, a)
obj_size(b)
obj_size(a, b)

b[[1]][[1]] <- 10
obj_size(b)
obj_size(a, b)

b[[2]][[1]] <- 10
obj_size(b)
obj_size(a, b)

# Yup!


# 2.5 Modify-in-place -----------------------------------------------------

# 2.5.1 Objects with a single binding -------------------------------------

v <- c(1, 2, 3)
v[[3]] <- 4

x <- data.frame(matrix(runif(5 * 1e4), ncol = 5))
medians <- vapply(x, median, numeric(1))

for (i in seq_along(medians)) {
  x[[i]] <- x[[i]] - medians[[i]]
}

cat(tracemem(x), "\n")

for (i in 1:5) {
  x[[i]] <- x[[i]] - medians[[i]]
}

untracemem(x)

y <- as.list(x)
cat(tracemem(y), "\n")

for (i in 1:5) {
  y[[i]] <- y[[i]] - medians[[i]]
}


# 2.5.2 Environments -------------------------------------------------------
e1 <- rlang::env(a = 1, b = 2, c = 3)
e2 <- e1

e1$c <- 4
e2$c

e <- rlang::env()
e$self <- e
ref(e) # environments can be self-referrential. This is unique
# for environments.


# 2.5.3 Exercises ---------------------------------------------------------


# Explain why the following code doesn’t create a circular list.

x <- list()
ref(x)
x[[1]] <- x
ref(x)

# Because only environments can contain themselves.
# And because the structure of "new x" is different from "old x".
# New x is a new list (copy-on-modify) which contains the object
# associated with the name/symbol "old x".

# Wrap the two methods for subtracting medians into two functions,
# then use the ‘bench’ package17 to carefully compare their speeds.
# How does performance change as the number of columns increase?

medians <- function(y) {
x <- data.frame(matrix(runif(y * 1e4), ncol = y))
median_func <- vapply(x, median, numeric(1))
median_func
}

medians(5)

median_method_1 <- function(y) {
  x <- data.frame(matrix(runif(y * 1e4), ncol = y))
  median_func <- vapply(x, median, numeric(1))

  for (i in seq_along(median_func)) {
  x[[i]] <- x[[i]] - median_func[[i]]
  }
as_tibble(x)
}
median_method_1(500)

median_method_2 <- function(y) {
  x <- data.frame(matrix(runif(y * 1e4), ncol = y))
  median_func <- vapply(x, median, numeric(1))
  x <- as_list(x)
  for (i in seq_along(median_func)) {
    x[[i]] <- x[[i]] - median_func[[i]]
  }
  as_tibble(x)
}
median_method_2(500)

col_num_vec <- c(1,10,100, 500, 1000, 2000, 4000, 7000, 10000)
process_1 <- vector("double", length(col_num_vec))
process_2 <- vector("double", length(col_num_vec))
real_1 <- vector("double", length(col_num_vec))
real_2 <- vector("double", length(col_num_vec))

for (i in seq_along(col_num_vec)) {
temp1 <- bench::bench_time(median_method_1(col_num_vec[i]))
process_1[[i]] <- temp1[1][[1]]
real_1[[i]] <- temp1[2][[1]]

temp2 <- bench::bench_time(median_method_2(col_num_vec[i]))
process_2[[i]] <- temp2[1][[1]]
real_2[[i]] <- temp2[2][[1]]
}

library(tidyverse)
g <- tibble(real_1 = real_1,
       real_2 = real_2,
       process_1 = process_1,
       process_2 = process_2,
       col_num_vec = log10(col_num_vec)) |> ggplot(aes(x = col_num_vec))

g + geom_line(aes(y = real_1), colour = "red") +
  geom_line(aes(y = real_2), colour = "blue")

g +
  geom_line(aes(y = process_1), colour = "orange") +
  geom_line(aes(y = process_2), colour = "green")


# What happens if you attempt to use tracemem() on an environment?

e1 <- env(a = 1, b = 2, c = 3)
tracemem(e1)

# Error in tracemem(e1) :
# 'tracemem' is not useful for promise and environment objects

# Probably made this way, because environments use modify-in-place.


# 2.6 Unbinding and the garbage collector ---------------------------------

x <- 1:3
x <- 2:4
rm(x)

gcinfo(TRUE)
gc()

mem_used()
