
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

funs <- list(
  half = function(x) x/2,
  double = function(x) x*2
)

funs$double(10)

# 6.2.4 Invoking a function -----------------------------------------------

args <- list(1:10, na.rm = TRUE)
do.call(mean, args)

# 6.2.5 Exercises ---------------------------------------------------------

# 1. Given a name, like "mean", match.fun() lets you find a function. Given a
# function, can you find its name? Why doesn’t that make sense in R?

balls <- function(x) x*2
nuts <- function(x) x*5
coocoo <- function(x) 4/x

# Ans: While this doesn't make much sense, because if you have a function you don't
# need it's exact name - you can just make a new name and assign the function to
# that name, it's not clear that it isn't helpful sometimes. For instance, when
# collaborating with other researchers one might have agreed on names on functions
# and listed them in i a package, or script, and then want to communicate about
# the function somehow, in relation to some specific issue. While this is unlikely
# to be a problem, it's not unthinkable. So what do we do then? Is there a way to
# find the function-name? Turns out there might be. Asking ChatGPT, for instance,
# can probably solve this in some instances, if the function is publicly known.
# Here I will try to find a way of remembering the name of a specific function
# stored in the global environment.
environment(nuts)
rfrf <- mtcars
utils::edit(rfrf)
findfuncname <- function(test) {
objs <- mget(ls(pos = 1L, all = TRUE), inherits = TRUE)
funs <- Filter(is.function, objs)
bodies <- lapply(funs, body)
findfunc <- function(fun) fun == body(test)
funs[unlist(lapply(bodies, findfunc))]
}

findfuncname(function(x) 4/x) # Cool!!

# Wow! That works! And it's fun! But so extremely useless!
# If I wanted to develop this further, then it's "useful" to be able to
# recognize functions that does the same. F.i.findfuncname(x*2) should
# recognize 2*x for 1.

# 2. It’s possible (although typically not useful) to call an anonymous
# function. Which of the two approaches below is correct? Why?

function(x) 3()
(function(x) 3)
# The second one is correct. This is because of the ().
?`(`
# Effectively, ( is semantically equivalent to the identity function(x) x.
# For (, the result of evaluating the argument. This has visibility set,
# so will auto-print if used at top-level.
# That's why I have used it only as a "print output" function, when doing stuff
# like
(a <- 2*6)
semantically_equivalent <- function(x) x
semantically_equivalent(4+7)
semantically_equivalent(function(x)3)()
# So in a sense () says return x. When x is a anonympus function, it returns
# a fully operational function, equal to the function, and also prints the same
# function. At least when explicitly called as function.
semantically_equivalent(mean(c(1,5)))() # Error: attempt to apply non-function
semantically_equivalent(function(x) mean(c(1,5)))() # 3

# 3. A good rule of thumb is that an anonymous function should fit on one line
# and shouldn’t need to use {}. Review your code. Where could you have used an
# anonymous function instead of a named function? Where should you have used
# a named function instead of an anonymous function?

# I haven't really used a lot of anonymous functions. Only in exercises like this.
# The first issue, that of actually using them, is the issue to review for me.
# Sometimes I make
luniq <- function(x) unique(length(x))
# Then I do things like
lapply(mtcars, luniq)
# I guess I could just have
lapply(mtcars, function(x) unique(length(x)))
# It's a bit more typing, but it will make the code more readable for cats that
# know R-code, but not necessarily have the time to read up on my wrappers.

# 4. What function allows you to tell if an object is a function? What function
# allows you to tell if a function is a primitive function?

# is.functions allows to tell wether a object is a function.
is.function(nuts)

# 5. This code makes a list of all functions in the base package.

objs <- mget(ls("package:base", all = TRUE), inherits = TRUE)
funs <- Filter(is.function, objs)

# Use it to answer the following questions:

# a. Which base function has the most arguments?

# b. How many base functions have no arguments?

# c. What’s special about those functions?

# d. How could you adapt the code to find all primitive functions?

# 6. What are the three important components of a function?

# 7. When does printing a function not show the environment it was created in?

