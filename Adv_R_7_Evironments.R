
# 7.1 Introduction --------------------------------------------------------


library(rlang)

# 7.2 Environment basics --------------------------------------------------

# 7.2.1 Basics

e1 <- rlang::env(
  a = FALSE,
  b = "a",
  c = 2.3,
  d = 1:3
)

e1$d <- e1 # e1 now contains itself.

e1 # not very useful

rlang::env_print(e1) # more useful

rlang::env_names(e1)
names(e1) # R 3.2 or higher.


# 7.2.2 Important environments --------------------------------------------

identical(current_env(), global_env())

# current_env() == global_env() # ERROR. Because == is a vectorized function.


# 7.2.3 Parents -----------------------------------------------------------

e2a <- env(d = 4, e = 5)
e2b <- env(e2a, a = 1, b = 2, c = 3)

rlang::env_parent(e2a)
rlang::env_parent(e2b)

e2c <- env(empty_env(), d = 4, e = 5)
e2d <- env(e2c, a = 1, b = 2, c = 3)

env_parents(e2d)
env_parents(e2b)

env_parents(e2b, last = empty_env())

# 7.2.4 Super assignment, <<- ---------------------------------------------

x <- 0

f <- function() {
  x <<- 1
}

f()
x

# 7.2.5 Getting and setting -----------------------------------------------

e3 <- env(x = 1, y = 2)
e3$x
e3$z <- 3
e3[["z"]]

# e3[[1]] # Error in e3[[1]] : wrong arguments for subsetting an environment
# e3[c("x", "y")] # Error in e3[c("x", "y")] :
# object of type 'environment' is not subsettable

e3$xyz # returns NULL
# env_get(e3, "xyz") # throws an error
env_get(e3, "xyz", default = NA) # returns a specific value

env_poke(e3, "a", 100)
e3$a

env_bind(e3, a = 10, b = 20)
env_names(e3)

e3$a <- NULL
env_has(e3, "a")

env_unbind(e3, "a")
env_has(e3, "a")

# 7.2.6 Advanced bindings -------------------------------------------------

env_bind_lazy(current_env(), b = {Sys.sleep(3); 1})
system.time(runif(100000000))
system.time(print(b)) # Doesn't work the same way anymore... maybe "sleep" also freezes the time-track-function?
system.time(b)

env_bind_active(current_env(), z1 = function() runif(1))
z1
z1

# 7.2.7 Exercises ---------------------------------------------------------

# 1. List three ways in which an environment differs from a list.

# Answer:
# An environment is similar to a list, in the sense that it contains name - objects- bindings of any kind. However there are some differences.

# Firstly, an environment is copied in place. It doesn't get copied, modified and then given a new address. It retains the same address throughout the operation.

# List:

l <- list(a = 1, b = "2", c = TRUE)
l
obj_address(l)
l$d <- "new"
l
obj_address(l)

# As you can see, the object has changed it's adress.

# Environment:

e <- rlang::env(a = 1, b = "2", c = TRUE)
env_print(e)
e$d <- "new"

# The address is exactly the same.

# Secondly, a list is a vector. This is not true of an environment, which is why this works with lists:
library(tidyverse)
map_chr(l, as.character)
# but not for environments:
# map_chr(e, as.character) # Error in `map_chr()`:
# ! `.x` must be a vector, not an environment.
# Run `rlang::last_trace()` to see where the error occurred.

# An environment can contain itself:

l$l <- l
str(l) # stops.
obj_address(l)
obj_address(l$l) # not same

e$e <- e
e$e$e$e$e$e$e$e$e$e$e$e # infinitly
e # same address!

# An environment has a parent

parent.env(e)
# parent.env(l) # Error in parent.env(l) : argument is not an environment

# Generally, an environment is similar to a named list, with four important exceptions:

# Every name must be unique.

e2 <- env(a = 1, a = 3)
names(e2)
l2 <- list(a = 1, a = 3)
str(l2)

# The names in an environment are not ordered.

# e2[[2]] # Error in e2[[2]] : wrong arguments for subsetting an environment
l2[[2]]

# 2. Create an environment as illustrated by this picture.

# Answer, I already did. Here's again:

e2$e2 <- e2
e2$e2$e2$e2
e2
names(e2)
names(e2$e2$e2)

# 3. Create a pair of environments as illustrated by this picture.

e2$e <- e
e$e2 <- e2

env_parent(e$e2)
names(e)
e$e2$e$e2
e2
env_parent(e2$e)
e2$e$e2$e
e

# 4. Explain why e[[1]] and e[c("a", "b")] don’t make sense when e is an environment.

# Ans. This is because an environment isn't ordered (in the first place), and because it isn't subsettable in both cases.

# 5. Create a version of env_poke() that will only bind new names, never re-bind old names. Some programming languages only do this, and are known as single assignment languages.

env_poke_no_new_names <- function(env = caller_env(), nm, value, ...) {
if (nm %in% names(env)) {
  stop("This version of env_poke() only bind new names, never re-bind old names.")
}
env_poke(env = env, nm = nm, value = value, ...)
}

env_poke_no_new_names(e, "a", 100)
env_poke_no_new_names(e, "g", 100)
env_has(e2$e, "g") # TRUE

# 6. What does this function do? How does it differ from <<- and why might you prefer it?

  rebind <- function(name, value, env = caller_env()) {
    if (identical(env, empty_env())) {
      stop("Can't find `", name, "`", call. = FALSE)
    } else if (env_has(env, name)) {
      env_poke(env, name, value)
    } else {
      rebind(name, value, env_parent(env))
    }
  }

rebind("a", 10)

a <- 5
rebind("a", 10)
a

# Answer:
# rebind() looks for a name in current_env(), in this case this means the global environment, and rebinds the name with a new value. If the name
# isn't found in caller_env(), the process starts a new, but with the parent of caller_env() as environment. This process continues until name is found, or until
# the function reaches empty_env() - the empty environment. If the function reaches the empty environment, the function throws an error-message.

# The operators <<- and ->>, on the other hand, cause a search to be made through parent environments for an existing definition of the variable being assigned. If such a
# variable is found (and its binding is not locked) then its value is redefined, otherwise assignment takes place in the global environment.

# rebind() would be preferred if the value to be rebind() is at a higher level than parent_env(), for instance in functions within functions. Which I personally have struggled
# with.

# Once more:

truls <- env(a =1, b = "c")
trine <- env(a = 5, b = 7)

truls$truls <- truls
truls$truls$truls$truls
truls
trine$trine <- trine
trine$truls <- truls
truls$trine <- trine

trine$truls$trine$truls
truls

