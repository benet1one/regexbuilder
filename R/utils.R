
library(glue)
library(rlang)
library(stringr)

#' @import glue
#' @import rlang
#' @import stringr

inside <- function(expr) {
    if (!is_expression(expr))
        return(expr)
    wrapper <- format(expr[[1L]])
    if (wrapper == "{" || wrapper == "(")
        return(inside(expr[[2L]]))
    expr
}

parse_times <- function(times, env = caller_env(2L)) {
    if (is_integerish(times, n = 1L))
        return(times)
    if (is_character(times, n = 1L))
        if (times %in% c("?", "*", "+")) return(times)
        else stop("'times' must be one of '?', '*', '+', or be an integer or an integer range (a:b)")
    if (!is_symbolic(times))
        stop("'times' must be either an integer, an integer range (a:b), or a character ('+').")

    times <- inside(times)

    if (times[[1L]] == quote(`:`)) {
        tmin <- eval(times[[2L]], env)
        tmax <- eval(times[[3L]], env)
        stopifnot(
            is_integerish(tmin, n = 1L), tmin >= 0L,
            is_integerish(tmax, n = 1L), tmax >= 0L, tmax >= tmin
        )
        if (tmax == tmin) return(tmax)
        else return(c(tmin, tmax))
    }

    eval(times, env)
}

validate_pattern <- function(pattern) {
    if (!is_character(pattern, n = 1L))
        stop("'pattern' must be a length one character string.")
    if (pattern == "")
        warning("'pattern' is an empty string (\"\").")

    check <- try(grep(pattern, ""), silent = TRUE) |> suppressWarnings()
    if (inherits(check, "try-error"))
        stop(str_remove(check, r"(.+\n\s+)"))
}
validate_pattern <- Vectorize(validate_pattern)

# UNUSED
enclose_patterns <- function(pattern) {
    if (str_detect(pattern, "^\\(") || is_pattern_atomic(pattern)) pattern
    else glue("(?:{pattern})")
}
enclose_patterns <- Vectorize(enclose_patterns, USE.NAMES = FALSE)

is_pattern_atomic <- function(pattern) {
    if (nchar(pattern) == 1L)
        return(TRUE)
    pattern %in% atomic_patterns
}
is_pattern_atomic <- Vectorize(is_pattern_atomic)
atomic_patterns <- {paste0("\\", c(letters, LETTERS)) |> c(
    "[:alnum:]",
    "[:alpha:]",
    "[:blank:]",
    "[:cntrl:]",
    "[:digit:]",
    "[:graph:]",
    "[:lower:]",
    "[:print:]",
    "[:punct:]⁠",
    "[:space:]",
    "[:upper:]",
    "[:xdigit:]⁠"
)}

meta_sequence <- function(letter, times = 1L, negate = FALSE) {
    if (negate)  letter <- toupper(letter)
    pattern <- glue("\\", letter)
    times(pattern, n)
}
new_meta_function <- function(letter) {
    substitute(
        function(times = 1L, negate = FALSE) {
            meta_sequence(letter, times, negate)
        },
        env = environment()
    )
}
