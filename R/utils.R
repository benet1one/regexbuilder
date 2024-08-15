
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

parse_times <- function(n, env = caller_env(2L)) {
    if (is_symbol(n))
        n <- eval(n, env)
    if (is_integerish(n, n = 1L))
        return(n)
    if (is_character(n, n = 1L))
        if (n %in% c("?", "*", "+")) return(n)
        else stop("'n' must be one of '?', '*', '+', or be an integer or an integer range (a:b)")
    if (!is_symbolic(n))
        stop("'n' must be either an integer, an integer range (a:b), or a character ('+').")

    n <- inside(n)

    if (n[[1L]] == quote(`:`)) {
        nmin <- eval(n[[2L]], env)
        nmax <- eval(n[[3L]], env)
        stopifnot(
            is_integerish(nmin, n = 1L), nmin >= 0L,
            is_integerish(nmax, n = 1L), nmax >= 0L, nmax >= nmin
        )
        if (nmax == nmin) return(nmax)
        else return(c(nmin, nmax))
    }

    eval(n, env)
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

enclose_patterns <- function(pattern) {
    if (str_detect(pattern, "^\\(") || is_pattern_atomic(pattern)) pattern
    else glue("(?:{pattern})")
}
enclose_patterns <- Vectorize(enclose_patterns, USE.NAMES = FALSE)

is_pattern_atomic <- function(pattern) {
    nchar(pattern) == 1L  ||
        (str_detect(pattern, "^\\(") && str_detect(pattern, "\\)$"))  ||
        (str_detect(pattern, "^\\[") && str_detect(pattern, "\\]$"))  ||
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
special_characters <- {c(
    ".", r"(\)", "|",
    "+", "*", "?",
    "[", "]", "(", ")", "{", "}",
    "^", "$"
)}

mess <- list(
    capture_arg_error = "'capture' must be TRUE, FALSE, or a string indicating the name of the capture group."
)
