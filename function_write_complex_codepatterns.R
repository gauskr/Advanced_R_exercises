write_complex_codepatterns <- function(file, add) {
code <- readLines("Temp.txt")
has_year <- stringr::str_detect(code, "[0-9][0-9][0-9][0-9]")
skeleton <- stringr::str_split(code, "[0-9][0-9][0-9][0-9]")
num_of_tweaks <- sum(has_year)

for (i in 1:num_of_tweaks) {
years <- stringr::str_extract_all(code[has_year][i], "[0-9][0-9][0-9][0-9]")
years <- unlist(years)
years <- as.integer(years) + add
num_of_years <- length(years)

first <- stringr::str_c("skeleton[has_year][",i ,"][[1]][", 1:num_of_years, "], years[", 1:num_of_years,"]")
first <- stringr::str_c(first, collapse = ", ")
body <- stringr::str_c(first, ", skeleton[has_year][",i,"][[1]][", num_of_years + 1,"]")
make_code <- str_c("stringr::str_c(", body, ")")
make_code <- rlang::parse_expr(make_code)
new_code <- eval(make_code)
code[has_year][i] <- new_code
}

return(code)
}
