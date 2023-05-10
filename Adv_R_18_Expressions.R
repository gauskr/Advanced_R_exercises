rm(x)
y <- x * 10
#> Error in eval(expr, envir, enclos): object 'x' not found

z <- rlang::expr(y <- x * 10)
z

x <- 4
eval(z)
y

library(rlang)
library(lobstr)

lobstr::ast(f(x, "y", 1))
#> █─f
#> ├─x
#> ├─"y"
#> └─1

lobstr::ast(f(g(1, 2), h(3, 4, i())))

ast(
  f(x,  y)  # important!
)

lobstr::ast(y <- x)
lobstr::ast(y < -x)

y <- x * 10
y
`<-`(y, `*`(x, 10))
y

lobstr::ast(y <- x * 10)
lobstr::ast(`<-`(y, `*`(x, 10)))
expr(`<-`(y, `*`(x, 10)))

# 18.2.4 Exercises
# 1. Reconstruct the code represented by the trees below:

  #> █─f
  #> └─█─g
  #>   └─█─h

ast(f(g(h())))

#> █─`+`
#> ├─█─`+`
#> │ ├─1
#> │ └─2
#> └─3

ast(1+2+3)

#> █─`*`
#> ├─█─`(`
#> │ └─█─`+`
#> │   ├─x
#> │   └─y
#> └─z

ast((x + y) * z)

# 2. Draw the following trees by hand and then check your answers
# with lobstr::ast().

f(g(h(i(1, 2, 3))))
# █─g
# └─█─h
#   └─█─i
#     ├─1
#     ├─2
#     └─3
ast(f(g(h(i(1, 2, 3))))) ## Forgot f()...

f(1, g(2, h(3, i())))
# █─f
# ├─1
# └─█─g
#   ├─2
#   └─█─h
#     ├─3
#     └─█─i
ast(f(1, g(2, h(3, i())))) # good. adjusted the space.

f(g(1, 2), h(3, i(4, 5)))
## █─f
## └─█─g
## | ├─2
## | └─h
## └─█─h
## | ├─3
## | └─█─i
## |   ├─4
## |   └─5
ast(f(g(1, 2), h(3, i(4, 5))))

# What’s happening with the ASTs below? (Hint: carefully read ?"^".)

lobstr::ast(`x` + `y`)
#> █─`+`
#> ├─x
#> └─y
lobstr::ast(x ** y)
#> █─`^`
#> ├─x
#> └─y
ast(2 ** 3)

lobstr::ast(1 -> x)
#> █─`<-`
#> ├─x
#> └─1
?"^"
?"<-"
# Ans. ast returns the most basic form of equivalent syntax trees,
# when basic means "conventional practice". 1 -> x is, therefore,
# parsed as x <- 1. ** is, by convention equal to ^, although this
# was deprecated in S a long time ago.
?ast

# What is special about the AST below? (Hint: re-read Section 6.2.1.)

lobstr::ast(function(x = 1, y = 2) {})
#> █─`function`
#> ├─█─x = 1
#> │ └─y = 2
#> ├─█─`{`
#> └─<inline srcref>

# it includes a point which shows source reference, used for printing.
# unlike the body of the function, the scref (not shown in tree apart
# from the note 'inline scref') may have comments and other formating.

# What does the call tree of an if statement with multiple else if
# conditions look like? Why?

ast(if (a == 1) {
  y <- 3
} else if (a == 2) {
  y <- 2
} else if (a == 3) {
  y <- 1
} else {
  y <- NA
})

?"if"

# the syntax tree reflects the cond expr else structure of the
# function if (coupled with else). The else is, in fact, silent in
# the tree, but allows growing the three-clumped tree with a new
# 2-3 clumped if function as the third function in the root if...

ast(if (a == 1) {
  y <- 3
} else if (a == 2) {
  y <- 2
} else if (a == 3) {
  y <- 1
})

## Omit else, and it remains a cond expr- structure. That's why
## the tree builds up like it does.


# 18.3 Expressions -------------------------------------------------------------


# 18.3.1 Constants --------------------------------------------------------


identical(expr(TRUE), TRUE)
identical(expr(1), 1)
identical(expr(2L), 2L)
identical(expr("x"), "x")


# 18.3.2 Symbols ----------------------------------------------------------

expr(x)
sym("x")

as_string(expr(x))

str(expr(x))
is.symbol(expr(x))


# 18.3.3 Calls ------------------------------------------------------------

lobstr::ast(read.table("important.csv", row.names = FALSE))
x <- expr(read.table("important.csv", row.names = FALSE))
typeof(x)
is.call(x)


# 18.3.3.1 Subsetting -----------------------------------------------------

x[[1]]
is.symbol(x[[1]])

as.list(x[-1])

x[[2]]
x$row.names

length(x) - 1

rlang::call_standardise(x)

x$header <- TRUE
x


# 18.3.3.2 Function position ----------------------------------------------

lobstr::ast(foo())
lobstr::ast("foo"())

## Reference to functional P and R6 object... will need to come
## back...
