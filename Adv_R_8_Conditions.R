
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

tryCatch(
  error = function(cnd) 10,
  1 + 1
)

tryCatch(
  error = function(cnd) 10,
  {
    message("Hi")
    1 + 1
  }
)

tryCatch(
  message = function(cnd) "There",
  {
    message("Here")
    stop("This code is never run!")
  }
)

tryCatch(
  error = function(cnd) {
    paste0("--", conditionMessage(cnd), "--")
  },
  stop("This is an error")
)

path <- tempfile()
tryCatch(
  {
    writeLines("Hi!", path)
    # ...
  },
  finally = {
    # always run
    unlink(path)
  }
)

# 8.4.3 Calling handlers --------------------------------------------------

tryCatch(
  message = function(cnd) cat("Caught a message!\n"),
  {
  message("Someone there?")
  message("Why, yes!")
}
)

withCallingHandlers(
  message = function(cnd) cat("Caught a message!\n"),
  {
    message("Someone there?")
    message("Why, yes!")
  }
)

withCallingHandlers(
  message = function(cnd) cat("Caught a message!\n"),
  {
    message("Someone there?")
    message("Why, yes!")
  }
)

withCallingHandlers(
  message = function(cnd) message("Second message"),
  message("First message")
)

# Bubbles all the way up to default handler which generates
# the message
withCallingHandlers(
  message = function(cnd) cat("Level 2\n"),
  withCallingHandlers(
    message = function(cnd) cat("Level 1\n"),
    message("Hello")
  )
)

# Bubbles up to tryCatch
tryCatch(
  message = function(cnd) cat("Level 2\n"),
  withCallingHandlers(
    message = function(cnd) cat("Level 1\n"),
    message("Hello")
  )
)

# Muffles the default handler which prints the messages
withCallingHandlers(
  message = function(cnd) {
    cat("Level 2\n")
    cnd_muffle(cnd)
  },
  withCallingHandlers(
    message = function(cnd) cat("Level 1\n"),
    message("Hello")
  )
)

# Muffles level 2 handler and the default handler
withCallingHandlers(
  message = function(cnd) cat("Level 2\n"),
  withCallingHandlers(
    message = function(cnd) {
      cat("Level 1\n")
      cnd_muffle(cnd)
    },
    message("Hello")
  )
)

# 8.4.4 Call stacks -------------------------------------------------------

f <- function() g()
g <- function() h()
h <- function() message("!")
# Calling handlers are called in the context of the call that signalled the condition:
withCallingHandlers(f(), message = function(cnd) {
  lobstr::cst()
  cnd_muffle(cnd)
})
# Whereas exiting handlers are called in the context of the call to tryCatch():
tryCatch(f(), message = function(cnd) lobstr::cst())

# 8.4.5 Exercises ---------------------------------------------------------

# 1. What extra information does the condition generated by
# abort() contain compared to the condition generated by
# stop() i.e. what’s the difference between these two objects?
# Read the help for ?abort to learn more.

catch_cnd(stop("An error"))
catch_cnd(abort("An error"))

# Solving the question:
?abort
abort

# In short, the condition generated by abort() allows for adding metadata,
# and special handling of errors by subclass. I think this means that the conditions
# generated by abort is S3 - objects. But I don't understand the object oriented programming
# system yet.

# Here's a long answer from searching through the help-page:

# abort() and error_cnd() create errors of class "rlang_error". The differences with
# base errors are:

# Implementing conditionMessage() methods for subclasses of "rlang_error" is undefined
# behaviour. Instead, implement the cnd_header() method (and possibly cnd_body() and
# cnd_footer()). These methods return character vectors which are assembled by rlang
# when needed: when conditionMessage.rlang_error() is called (e.g. via try()), when
# the error is displayed through print() or format(), and of course when the error is
# displayed to the user by abort().
# cnd_header(), cnd_body(), and cnd_footer() methods can be overridden by storing
# closures in the header, body, and footer fields of the condition. This is useful to
# lazily generate messages based on state captured in the closure environment.
# [Experimental] The use_cli_format condition field instructs whether to use cli
# (or rlang's fallback method if cli is not installed) to format the error message at
# print time.
# In this case, the message field may be a character vector of header and bullets.
# These are formatted at the last moment to take the context into account (starting
# position on the screen and indentation).

# By using these help functions, by some algorithm I haven't figured out (haven't tried to)
# the condition objects can be enriched by information, which then can trigger nuanced
# handling. Me thinks.

# 2. Predict the results of evaluating the following code

# show_condition <- function(code) {
#  tryCatch(
#    error = function(cnd) "error",
#    warning = function(cnd) "warning",
#    message = function(cnd) "message",
#    {
#      code
#      NULL
#    }
#  )
#}
#
#show_condition(stop("!"))
#show_condition(10)
#show_condition(warning("?!"))
#show_condition({
#  10
#  message("?")
#  warning("?!")
#})

# Answer:

# Above there's five code-chunks. I will try to deal with them one by one.

# I.
# The first should return just null. That's because tryCatch sets up handlers for
# three situations: messages, warnings and errors. But none of those are triggered
# by the code run, which is just a NULL.

show_condition <- function(code) {
  tryCatch(
    error = function(cnd) "error",
    warning = function(cnd) "warning",
    message = function(cnd) "message",
    {
      code
      NULL
    }
  )
}

# Man, how stupid was that. This is just the function used... I was kind of tricked
# by the structure of the tryCatch - handler. Anyhow, lets see if my intuition was
# correct:

show_condition()

# It was wrong. The function returns "error".
# Let's experiment, and then guess!
show_condition("abc")
# Returns NULL
show_condition(1+1)
# I guess the next will return error:
show_condition(NA)
# It doesn't
# But now it will:
show_condition(apekatt)
# Yes...
# I'm ready.

# This will return "error"
show_condition(stop("!"))
# It does.
# The next will return NULL
show_condition(10)
# Yup
# The third will return "warning"
show_condition(warning("?!"))
# Correct
# The last will return
# NULL
# "message"
# "warning"
show_condition({
  10
  message("?")
  warning("?!")
})
# What?! It just returns message. Why??
# Here's from the solutions:

# The last example is the most interesting and makes us aware of the exiting
# qualities of tryCatch(); it will terminate the evaluation of the code as soon as it
# is called.

# So, in tryCatch, when the first handler is called, it terminates the rest, and returns
# only the condition-object. 10 didn't trigger anything, so the function moved on to
# message("?), and then terminated the code and returned "message".

# 3. Explain the results of running this code:

  withCallingHandlers(
    message = function(cnd) message("b"),
    withCallingHandlers(
      message = function(cnd) message("a"),
      message("c")
    )
  )

# The code nests two withCallingHandlers. message("c") is nested in two such handlers.
# When message("c") is called, this triggers a and b, in that order, however a also triggers
# b. The result is

# b
# a
# b
# c

# If this holds the following code should return c b a b c d
withCallingHandlers(
  message = function(cnd) message("c"),
  withCallingHandlers(
    message = function(cnd) message("b"),
    withCallingHandlers(
      message = function(cnd) message("a"),
      message("d")
    )
  )
)

# Nope. It returned
#c
#b
#c
#a
#c
#b
#c
#d

# Ok. So the highest level is triggered every time
# Simple message e
# lvl_1: de
# lvl_2: cdce
# lvl_3: bcbdbcbe
# lvl_4, then, should be: abacabadabacabae

withCallingHandlers(
  message = function(cnd) message("a"),
withCallingHandlers(
  message = function(cnd) message("b"),
  withCallingHandlers(
    message = function(cnd) message("c"),
    withCallingHandlers(
      message = function(cnd) message("d"),
      message("e")
    )
  )
))

# It returned abacabadabacabae. Just as I (finally) predicted.
# Thus, withCallingHandlers works in a nested fashion, letting the handler work once
# for each of the corresponding signals at lower levels of the nesting. This produces
# the result. Here's from the solutions:

# It’s a little tricky to untangle the flow here:
# First, message("c") is run, and it’s caught by (1). It then calls message("a"),
# which is caught by (2), which calls message("b"). message("b") isn’t caught by
# anything, so we see a b on the console, followed by a. But why do we get another b
# before we see c? That’s because we haven’t handled the message, so it bubbles up to
# the outer calling handler.

# Good point with the "bubbling". Although my answer is still ok.

# 4. Read the source code for catch_cnd() and explain how it works.
# Answer:

catch_cnd

# function (expr, classes = "condition")
# {
#  stopifnot(is_character(classes))
#  handlers <- rep_named(classes, list(identity))
#  eval_bare(rlang::expr(tryCatch(!!!handlers, {
#    force(expr)
#    return(NULL)
#  })))
#}

# function (expr, classes = "condition")
# The function have two arguments, expr, and classes.
# stopifnot(is_character(classes))
# If classes, which defaults to "conditions", is not a character vector, the function
# stops.
# handlers <- rep_named(classes, list(identity))
# Hm. Let us see what this does:
handlers <- rep_named("condition", list(identity))
handlers
class(handlers) # It's a list.
identity("conditions") # It's just character.
list(identity) # this is just a list with [[1]] function(x) x
rep_named
?`%||%`
# If x is NULL, this funciont will return y; otherwise returns x.
# This line generates list(identity), and names the list using the
# classes-argument.
# Finally, here's the action:
#  eval_bare(rlang::expr(tryCatch(!!!handlers, {
#    force(expr)
#    return(NULL)
#  })))
?`!!!`
# This is the splice operator from rlang.
# In this context it takes a list of handlers and
# just puts them inside tryCatch. I think.
?rlang::expr
# This takes the expression and returns it as expression.
# This is probably necessary to stabilize the code.
?eval_bare
# This evaluates (runs) the expression. It's stack-sensitive...
?catch_cnd
# catch_cnd is a wrapper around tryCatch().
catch_cnd(10)
catch_cnd(abort("an error"))
catch_cnd(signal("my_condition", message = "a condition"))

# Let's attempt to break them down!
classes <- "conditions"
stopifnot(is_character(classes))
handlers <- rep_named(classes, list(identity))
handlers
eval_bare(rlang::expr(tryCatch(!!!handlers, {
force(10)
NULL
})))
eval_bare(rlang::expr(tryCatch(!!!handlers, {
  force(abort("Just stop"))
  NULL
})))

?tryCatch

# Ok... I don't quite get it... that's a bomb... Let's see the solutions:

# catch_cnd() is a simple wrapper around tryCatch(). If a condition is signalled,
# it’s caught and returned. If no condition is signalled, execution proceeds
# sequentially and the function returns NULL.

# The current version of catch_cnd() is a little more complex because it allows you
# to specify which classes of condition you want to capture. This requires some
# manual code generation because the interface of tryCatch() provides condition
# classes as argument names.

# My comment: This "explanation" doesn't solve the riddle! How can the function work
# as it does? I guess this will become clearer after learning about object oriented
# programming!

# 5. How could you rewrite show_condition() to use a single handler?
# Answer: just remove the two others, like this:
#function(code) {
#  tryCatch(
#    error = function(cnd) "error"
#    {
#      code
#      NULL
#    }
#  )
#}

# Argh!! I misinterpreted the question!!!

# show_condition() was defined in one of the previous questions. Let’s use the condition argument of tryCatch() as shown in rlang::catch_cond() above for our re-implementation:

  show_condition2 <- function(code) {
    tryCatch(
      condition = function(cnd) {
        if (inherits(cnd, "error"))   return("error")
        if (inherits(cnd, "warning")) return("warning")
        if (inherits(cnd, "message")) return("message")
      },
      {
        code
        NULL
      }
    )
  }
# Here's the same funciont, but with just one handler... which covers all events
# similarly to the original function. Boy this chapter is boring... that's probably
# why so few people have "taken full advantage of the powerful condition-system",
# according to Wickham. They couldn't stand the pain! Off we go, to

# 8.5 Custom conditions ---------------------------------------------------

abort(
  "error_not_found",
  message = "Path `blah.cvs not found",
  path = "blah.csv"
)

# 8.5.1 Motivation --------------------------------------------------------

log(letters)
log(1:10, base = letters)

my_log <- function(x, base = exp(1)) {
 if (!is.numeric(x)) {
   abort(paste0(
     "`x` must be a numeric vector; not ", typeof(x), "."
   ))
 }
 if (!is.numeric(base)) {
   abort(paste0(
     "`base` must be a numeric vector; not ", typeof(base), "."
   ))
 }
 base::log(x, base = base)
}

my_log(letters)
my_log(1:10, base = letters)

# This is better. But will only work in interactive settings, not programmatically...

# 8.5.2 Signalling --------------------------------------------------------

abort_bad_argument <- function(arg, must, not = NULL) {
  msg <- glue::glue("`{arg}` must {must}")
  if (!is.null(not)) {
    not <- typeof(not)
    msg <- glue::glue("{msg}; not {not}.")
  }

  abort("error_bad_argument",
        message = msg,
        arg = arg,
        must = must,
        not = not)
}

stop_custom <- function(.subclass, message, call = NULL, ...) {
err <- structure(
  list(
    message = message,
    call = call,
    ...
  ),
  class = c(.subclass, "error", "condition")
)
stop(err)
}


err <- catch_cnd(
  stop_custom("error_new", "This is a custom error", x = 10)
)

class(err)
err$x

my_log <- function(x, base = exp(1)) {
  if (!is.numeric(x)) {
    abort_bad_argument("x", must = "be numeric", not = x)
  }
  if (!is.numeric(base)) {
    abort_bad_argument("base", must = "be numeric", not = base)
  }
  base::log(x, base = base)
}

my_log(letters)
my_log(1:10, base = letters)

# 8.5.3 Handling ----------------------------------------------------------

library(testthat)
err <- catch_cnd(my_log("a"))
expect_s3_class(err, "error_bad_argument")
expect_equal(err$arg, "x")
expect_equal(err$not, "character")

tryCatch(
  error_bad_argument = function(cnd) "bad_argument",
  error = function(cnd) "other error",
  my_log("a")
)

tryCatch(
  error = function(cnd) "other error",
  error_bad_argument = function(cnd) "bad_argument",
  my_log("a")
)


# 8.5.4 Exercises ---------------------------------------------------------

# 1. Inside a package, it’s occasionally useful to check that a package is
# installed before using it. Write a function that checks if a package is
# installed (with requireNamespace("pkg", quietly = FALSE)) and if not,
# throws a custom condition that includes the package name in the metadata.

# Answer:
pcg <- "Bongo"

test <- function(pcg) {
result <- requireNamespace(pcg,quietly = T)
if (!result) {
inform(message = glue::glue("package ", pcg, " is not installed."), class = c("pck_not_istalled", pcg))
pcg
}
}

test("bongo")
class(catch_cnd(test("bongo")))

# Define a custom function to check if a package is installed
check_package_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    # Define a custom condition
    condition <- structure(
      list(message = paste("Package", pkg, "is not installed."),
           pkg = pkg), # Adding package name to metadata
      class = c("packageNotInstalledError", "error", "condition", pkg)
    )
    # Throw the custom error condition
    stop(condition)
  } else {
  # Return TRUE if the package is installed
  TRUE
  }
}

class(catch_cnd(check_package_installed("bongo")))

# I actually solved this one...

# 2. Inside a package you often need to stop with an error when something
# is not right. Other packages that depend on your package might be tempted
# to check these errors in their unit tests. How could you help these packages
# to avoid relying on the error message which is part of the user interface
# rather than the API and might change without notice?

# From solutions:
# Instead of returning an error it might be preferable to throw a customised
# condition and place a standardised error message inside the metadata.
# Then the downstream package could check for the class of the condition,
# rather than inspecting the message.


# 8.6 Applications --------------------------------------------------------

# 8.6.1 Failure value -----------------------------------------------------

fail_with <- function(expr, value = NULL) {
  tryCatch(
    error = function(cnd) value,
    expr
  )
}

fail_with(log(10), NA_real_)
fail_with(log("x"), NA_real_)

try2 <- function(expr, silent = FALSE) {
  tryCatch(
    error = function(cnd) {
      msg <- conditionMessage(cnd)
      if (!silent) {
        message("Error: ", msg)
      }
      structure(msg, class = "try-error")
    },
    expr
  )
}

try2(1)
try2(stop("Hi"))
try2(stop("Hi"), silent = TRUE)

# 8.6.2 Success and failure values ----------------------------------------

