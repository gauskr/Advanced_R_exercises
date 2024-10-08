
# 8. Conditions -----------------------------------------------------------

# 8.1 Introduction --------------------------------------------------------

library(rlang)

# Quiz

# 1. What are the three most important types of condition?

# Answer: stop(), warning() and message()-

# There are three conditions that you can signal in code: errors, warnings,
# and messages.

# Errors are the most severe; they indicate that there is no way for a function
# to continue and execution must stop.

# Warnings fall somewhat in between errors and message, and typically indicate
# that something has gone wrong but the function has been able to at least
# partially recover.

# Messages are the mildest; they are way of informing users that some action
# has been performed on their behalf.

# 2. What function do you use to ignore errors in block of code?

# The easiest way to ignore errors would be to use try(). E.g. instead of

# function(x) {
# do_stuff(x)
# do_something_else
# return(something)
#}

# where do_stoff(x) might trhow an error, on can do

# function(x) {
# try(do_stuff(x)) # This is where I use try()
# do_something_else
# return(something)
#}

# Here, the function simply tries do_stuff(x). If it doesn't work, then it will move on to do_something_else
# instead of aborting.

# 3. What’s the main difference between tryCatch() and withCallingHandlers()?

# Answer:
# tryCatch() and withCallingHandlers differs in the kind of handlers they create.

# tryCatch() defines exiting handlers; after the condition is handled, control returns to the context where tryCatch() was called.
# This makes tryCatch() most suitable for working with errors and interrupts, as these have to exit anyway.

# withCallingHandlers() defines calling handlers; after the condition is captured control returns to the context where the condition
# was signalled. This makes it most suitable for working with non-error conditions.

# 4. Why might you want to create a custom error object?


# 8.2 Signalling conditions -----------------------------------------------

# There are three conditions that you can signal in code: errors, warnings,
# and messages.

# Errors are the most severe; they indicate that there is no way for a function
# to continue and execution must stop.

# Warnings fall somewhat in between errors and message, and typically indicate
# that something has gone wrong but the function has been able to at least
# partially recover.

# Messages are the mildest; they are way of informing users that some action
# has been performed on their behalf.

# There is a final condition that can only be generated interactively:
# an interrupt, which indicates that the user has interrupted execution by
# pressing Escape, Ctrl + Break, or Ctrl + C (depending on the platform).

stop("This is what an error looks like")
warning("This is what a warning looks like")
message("This is what a message looks like")


# 8.2.1 Errors ------------------------------------------------------------

f <- function() g()
g <- function() h()
h <- function() stop("This is an error!")

f()

# Good practice:
h <- function() stop("This is an error!", call. = FALSE)
f()

# 8.2.2 Warnings ----------------------------------------------------------

fw <- function() {
  cat("1\n")
  warning("W1")
  cat("2\n")
  warning("W2")
  cat("3\n")
  warning("W3")
}
fw()

# You can control this behaviour with the warn option:

# To make warnings appear immediately, set options(warn = 1).

# To turn warnings into errors, set options(warn = 2). This is usually the
# easiest way to debug a warning, as once it’s an error you can use tools
# like traceback() to find the source.

# Restore the default behaviour with options(warn = 0).

# In my opinion, base R tends to overuse warnings, and many warnings in base R
# would be better off as errors. For example, I think these warnings would be
# more helpful as errors:

formals(1)
file.remove("This-file-doesn't-exist")
lag(1:3, k = 1.5) # Actually throw an error now, my comm....
as.numeric(c("18", "30", "50+", "345,678"))

# Two situations where warnings are appropriate:
# When you deprecate a function.
# When you are reasonably certain you will solve any problems.

# 8.2.3 Messages ----------------------------------------------------------

fm <- function() {
    cat("1\n")
    message("M1")
    cat("2\n")
    message("M2")
    cat("3\n")
    message("M3")
}
fm()

cat("Hi!\n")
message("Hi!")

# 8.2.4 Exercises ---------------------------------------------------------

# 1. Write a wrapper around file.remove() that throws an error if the file to
# be deleted does not exist.

file_remove <- function(file) {

if (file.exists(file)) {
file.remove(file)
message("File is removed.")
} else {
  stop("File doesn't exist.")
}
}
file_remove("doddo.txt")
file.create("doddo.txt")
file_remove("doddo.txt")
file_remove("doddo.txt")

# 2. What does the appendLF argument to message() do?
# How is it related to cat()?

# Answer: To answer this, it might be instructive to first examine the function
# message
?message

# The function message(..., domain = NULL, appendLF = TRUE) generates a
# diagnostic message from its arguments.
# The argument appendLF	appears at the end of the argument-list (see below).
# It is a logical argument which defaults to TRUE, and determines whether
# messages given as a character string should have a newline appended.

# How, then, does it relate to cat(). Let's look "inside" (as far as inside is
# easily available to the user) the function.

message

# function (..., domain = NULL, appendLF = TRUE)
# {
#   cond <- if (...length() == 1L && inherits(..1, "condition")) {
#     if (nargs() > 1L)
#       warning("additional arguments ignored in message()")
#     ..1
#   }
#   else {
#     msg <- .makeMessage(..., domain = domain, appendLF = appendLF)
#     call <- sys.call()
#     simpleMessage(msg, call)
#   }
#   defaultHandler <- function(c) {
#     cat(conditionMessage(c), file = stderr(), sep = "")
#   }
#   withRestarts({
#     signalCondition(cond)
#     defaultHandler(cond)
#   }, muffleMessage = function() NULL)
#   invisible()
# }

message("Hello! ", "Goodbye! ", "This ", "is ", "a ", "complicated ", "way ", "of ", "messaging!")
# ... - argument means zero or more objects which can be coerced to character
# (and which are pasted together with no separator) or (for message only) a
# single condition object.

.makeMessage

?...length
...length # .Primitive("...length")
?.Primitive # .Primitive looks up by name a ‘primitive’
# (internally implemented) function.
.Primitive # .Primitive(".Primitive")
# Comment: Interesting structure.

cat
?...length
cat("Doesn't", "work", "or", "does", "it")

# Cat just prints everything w the objects separated by " ".

cat(str_c("generate bvfylke",1993:2021, " = substr(bvkomm", 1993:2021, ", 1, 2)\ndestring bvfylke", 1993:2021, ", force"), sep = "\n")

# because of the default sep-argument...

# My answer, which I'm not 100% saticfied with is that the appendLF-argument in message is set to TRUE, because of the makeMessage makes
# better use of it, or simply need it. Then cat is set to sep = "". Is it possible to tweak the function? Let's see:

message2 <- function (..., domain = NULL, appendLF = FALSE)
{
  cond <- if (...length() == 1L && inherits(..1, "condition")) {
    if (nargs() > 1L)
      warning("additional arguments ignored in message()")
    ..1
  }
  else {
    msg <- .makeMessage(..., domain = domain, appendLF = appendLF)
    call <- sys.call()
    simpleMessage(msg, call)
  }
  defaultHandler <- function(c) {
    cat(conditionMessage(c), file = stderr(), sep = " ")
  }
  withRestarts({
    signalCondition(cond)
    defaultHandler(cond)
  }, muffleMessage = function() NULL)
  invisible()
}

message("This", "is", "a", "message")
message2("This", "is", "a", "message")
message("This ", "is ", "a ", "\n", "message")
message2("This ", "is ", "a ", "\n", "message")

# Hm...

test1 <- function() {
  message("This", "is", "a", "message")
  message("This", "is", "also", "a", "message")
}

test2 <- function() {
  message2("This", "is", "a", "message")
  message2("This", "is", "also", "a", "message")
}

test1()
test2()

# Now I get it! The above theories is irelevant.
# What happen's is something akin to this:

cat(stringr::str_c("Line 1", "Line 2", sep = ""), sep = "") # message2 - case (appendLF = FALSE)
cat(stringr::str_c("Line 1", "Line 2", sep = "\n"), sep = "") # message.

message3 <- function (..., domain = NULL, appendLF = FALSE)
{
  cond <- if (...length() == 1L && inherits(..1, "condition")) {
    if (nargs() > 1L)
      warning("additional arguments ignored in message()")
    ..1
  }
  else {
    msg <- .makeMessage(..., domain = domain, appendLF = appendLF)
    call <- sys.call()
    simpleMessage(msg, call)
  }
  defaultHandler <- function(c) {
    cat(conditionMessage(c), file = stderr(), sep = "\n")
  }
  withRestarts({
    signalCondition(cond)
    defaultHandler(cond)
  }, muffleMessage = function() NULL)
  invisible()
}

test3 <- function() {
  message3("This", "is", "a", "message")
  message3("This", "is", "also", "a", "message")
}

test3() # The difference between message2 and message3 is that cat has a sep = "\n" in message3.
# appendLF = TRUE is more intuitive, as it draws attention to the operation in the argument-section.
# This is my final answer...

# 8.3 Ignoring conditions -------------------------------------------------

f1 <- function(x) {
  log(x)
  10
}
f1("x")

f2 <- function(x) {
  try(log(x))
  10
}
f2("a")

default <- NULL
try(default <- read.csv("possibly-bad-input.csv"), silent = TRUE)

suppressWarnings({
  warning("Uhoh!")
  warning("Another warning")
  1
})

suppressMessages({
  message("Hello there")
  2
})

suppressWarnings({
  message("You can still see me")
  3
})

# 8.4 Handling conditions -------------------------------------------------

tryCatch(
  error = function(cnd) {
    # code to run when error is thrown.
  },
  code_to_run_when_handlers_are_active
)

withCallingHandlers(
  warning = function(cnd) {
    # code to run when warning is signalled
  },
  message = function(cnd) {
    # code to run when message is signalled
  },
  code_to_run_while_handlers_are_active
)

# 8.4.1 Condition objects -------------------------------------------------

cnd <- catch_cnd(stop("An error!"))
str(cnd)

# 8.4.2 Exiting handlers --------------------------------------------------

f3 <- function(x) {
tryCatch(
  error = function(cnd) NA,
  log(x)
)
}

f3("x")

# General form of tryCatch() and withCallingHandlers()

#tryCatch(
#  error = function(cnd) {
#    # code to run when error is thrown
#  },
#  code_to_run_while_handlers_are_active
#)
#
#
#withCallingHandlers(
#  warning = function(cnd) {
#    # code to run when warning is signalled
#  },
#  message = function(cnd) {
#    # code to run when message is signalled
#  }
#  code_to_run_while_handlers_are_active
#)

# 8.4.1 Condition objects -------------------------------------------------

