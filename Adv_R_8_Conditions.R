
# 8. Conditions -----------------------------------------------------------

# 8.1 Introduction --------------------------------------------------------

library(rlang)

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
