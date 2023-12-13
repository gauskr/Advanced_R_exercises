
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

# 3. What’s the main difference between tryCatch() and withCallingHandlers()?

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
lag(1:3, k = 1.5)
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
# It is a logical argument which defaults to TRUE, and determines wether
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

message("Hello!", "Goodbye!", "This", "is", "a", "complicated", "way", "of", "messaging!")
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

