library(rlang)
library(lobstr)

expr(mean(x, na.rm = TRUE))
expr(10 + 30 + 100)

capture_it <- function(x) {
  expr(x)
}

capture_it(10 + 100 + 1000)

capture_it <- function(x) {
  enexpr(x)
}

capture_it(10 + 100 + 1000)

f <- expr(f(x = 1, y = 2))
f
f$z <- 3
f
f[2] <- NULL
f

lobstr::ast(mean(c(1:5, 7,9)*3, na.rm = TRUE))

call2("f", 1, 2, 3)
call2("+", 1, call2("*", 2, 3))

xx <- expr(x + x)
yy <- expr(y + y)
expr(!!xx/!!yy)

cv <- function(var){
  var <- enexpr(var)
  expr(sd(!!var)/mean(!!var))
}

cv2 <- function(var){
  var <- enexpr(var)
  expr(sd(!!var)/mean(!!var))
}

x <- 1
cv(3)
cv_ex
(cv_ex <- cv(34+2*x))

eval(expr(cv_ex), env(x = 1)) # NA... dunno why

cv(`)`)

eval(expr(x + y), env(x = 1, y = 10))
eval(expr(x + y), env(x = 2, y = 100))

x <- 10
y <- 100
eval(expr(x + y))

string_math <- function(x) {
  e <- env(
    caller_env(),
    `+` = function(x, y) paste0(x, y),
    `*` = function(x, y) strrep(x, y)
  )

  eval(enexpr(x), e)
}

name <- "Hadley"
string_math("Hello " + name)
string_math(("x" * 2 + "-y") * 3)

library(dplyr)
con <- DBI::dbConnect(RSQLite::SQLite(), filename = ":memory:")
mtcars_db <- copy_to(con, mtcars)

mtcars_db %>%
  filter(cyl > 2) %>%
  select(mpg:hp) %>%
  head(10) %>%
  show_query()

DBI::dbDisconnect(con)

df <- data.frame(x = 1:5, y = sample(5))
eval_tidy(expr(x + y), df)

with2 <- function(df, expr) {
  eval_tidy(enexpr(expr), df)
}

with2(df, x + y)

with2 <- function(df, expr) {
  a <- 1000
  eval_tidy(enexpr(expr), df)
}

df <- data.frame(x = 1:3)
a <- 10
with2(df, x + a)

with2 <- function(df, expr) {
  a <- 1000
  eval_tidy(enquo(expr), df)
}

with2(df, x + a)

