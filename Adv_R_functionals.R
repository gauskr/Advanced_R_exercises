randomise <- function(f) f(runif(1e3))
randomise(mean)
randomise(sum)
randomise(max)
randomise(min)

library(purrr)

vignette(package = "purrr")

triple <- function(x) x*3
content <- map(1:4, triple)
unlist(content)

map_unlist <- function(vec, func) {
  unlist(map(vec, func))
}
library(inst)
map_unlist(1:4, triple)

c(1:4)*3

info <- function(x) {
n_length <- function(x) length(unique(x))
info <-   rbind(map_chr(x, typeof),
  map_chr(x, class),
  map_int(x, n_length)
  )
row.names(info) <- c("typeof", "class", "n_length")
return(info)
}

data()
info(mtcars)

iris[map_lgl(iris, is.factor)] |> head(10)
map_dbl(iris[map_lgl(iris, is.numeric)], mean) |> as.data.frame()
rappdf$rapp
map_chr(rappdf$rapp, str_split(pattern = " "))

simple_map <- function(x, f, ...) {
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    out[[i]] <- f(x[[i]], ...)
  }
  out
}

pair <- function(x) c(x, x)
pair(2)
pair(1:3)
library(tidyverse)
map_dbl(1:2, pair)

map_chr(1:2, as.character)

map(1:2, pair)
map(1:2, as.character)

map_dbl(mtcars, function(x) length(unique(x)))
map_dbl(mtcars, ~length(unique(.x)))
as_mapper(~length(unique(.x)))

x <- map(1:3, ~rnorm(2))
str(x)

