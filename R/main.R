
#' @export
glue <- glue::glue

# Matching functions -------------------------------

#' Match a string literally.
#' @description
#' Escapes all regex special characters.
#' @export
literally <- fixed <- function(string) {
    str_replace_all(
        string,
        pattern = r"(([\.\|\(\)\[\]\{\}\^\$\?\*\+\\]))",
        replacement = r"(\\\0)"
    ) |> as_glue()
}
#' Create a capture group.
#' @description
#' Encloses the string in parentheses \code{(...)}, and optionally
#' names the capture group.
#' @param name Optionally, a string indicating the name of the capture group.
#' @export
capture <- function(pattern, name = NULL) {
    validate_pattern(pattern)
    if (is.null(name))
        return(glue("({pattern})"))
    if (str_detect(name, r"(\W)"))
        stop("Capture group names can only contain letters and underscores.")

    glue("(?<{name}>{pattern})")
}

#' Match a pattern \code{n} times.
#' @description
#' Adds an appropriate suffix after the pattern.
#'
#' @param pattern String indicating the pattern to match.
#' @param n How many times to match the pattern.
#' Can be an integer \code{a}, to match the pattern exactly \code{a} times;
#' an integer range \code{a:b}, to match the pattern between \code{a} and
#' \code{b} times, both inclusive. \code{b} can be \code{Inf};
#' or one of the following characters: \code{?, +, *}.
#'
#' @param capture Logical indicating whether to capture the pattern, or
#' a string indicating the name of the capture group.
#'
#' Beware this is not equivalent to using \code{capture(times(pattern, n))}.
#' See \link{examples}.
#'
#' @export
#' @examples
#' times("A", 3L)
#' times("[a-z]", 0:Inf)
#' times("hey ?", "+")
#'
#' ## Notice the difference
#' times("\\d", 0:1, capture = TRUE)
#' capture(times("\\d", 0:1))
times <- function(pattern, n = 1:Inf, capture = FALSE) {
    validate_pattern(pattern)
    n <- parse_times(enexpr(n))
    suffix <- if (is_character(n))
        n
    else if (length(n) == 1L) {
        if (n == 1L) ""
        else paste0("{", n, "}")
    } else if (all(n == c(0L, 1L))) {
        "?"
    } else if (all(n == c(0L, Inf))) {
        "*"
    } else if (all(n == c(1L, Inf))) {
        "+"
    } else if (n[1L] == 0L) {
        paste0("{,", n[2L], "}")
    } else if (n[2L] == Inf) {
        paste0("{", n[1L], ",}")
    } else {
        paste0("{", n[1L], ",", n[2L], "}")
    }

    if (is_true(capture))
        glue("({pattern}){suffix}")
    else if (is_character(capture, 1L))
        glue("(?<{capture}>{pattern}){suffix}")
    else if (is_false(capture) && is_pattern_atomic(pattern))
        glue("{pattern}{suffix}")
    else if (is_false(capture))
        glue("(?:{pattern}){suffix}")
    else stop(mess$capture_arg_error)

}
#' Maybe match a pattern.
#' @description
#' Alias for \code{times(pattern, n = 0:nmax)}
#' @param pattern String indicating the pattern to match.
#' @param nmax Maximum number of times to match the pattern.
#' @param capture Logical indicating whether to capture the pattern, or
#' string indicating the name of the capture group.
#' @seealso [times()]
#' @export
maybe <- function(pattern, nmax = 1L, capture = FALSE) {
    times(pattern, n = 0:nmax, capture)
}

any_char <- function(n = 1L, include_newline = FALSE) {
    pattern <- ifelse(include_newline, no = ".", yes = "(?:.|\\n)")
    times(pattern, !!enexpr(n))
}
any_of <- function(characters, n = 1L, negate = FALSE) {
    for (k in seq_along(characters))
        if (!is_pattern_atomic(characters))
            warning("Pattern '", characters[k], " is not atomic. ",
                    "Result may be unexpected.")
    neg <- ifelse(negate, yes = '^', no = '')
    any <- glue("[", neg, glue_collapse(characters), "]")
    times(any, !!enexpr(n))
}
none_of <- function(characters, n = 1L, negate = TRUE) {
    any_of(characters, !!enexpr(n), negate)
}
option <- function(patterns, capture = FALSE) {
    validate_pattern(patterns)
    options <- glue_collapse(patterns, sep = "|")
    if (is_true(capture))
        glue("({options})")
    else if (is_false(capture))
        glue("(?:{options})")
    else if (is_character(capture, 1L))
        capture(options, name = capture)
    else stop(mess$capture_arg_error)
}

meta_sequence <- function(letter, n = 1L, negate = FALSE) {
    if (negate)  letter <- toupper(letter)
    pattern <- glue("\\", letter)
    times(pattern, !!enexpr(n))
}
new_meta_function <- function(letter) {
    call <- substitute(
        function(n = 1L, negate = FALSE) {
            meta_sequence(letter, !!enexpr(n), negate)
        },
        env = environment()
    )
    call[[4L]] <- NULL
    eval(call)
}

word <- new_meta_function("w")
digit <- new_meta_function("d")
whitespace <- new_meta_function("s")
newline <- new_meta_function("n")

start <- function() '^'
end <- function() '$'

# Substitution Functions -----------------------------

full_match <- function() r"(\0)"
capture_reference <- function(index) {
    stopifnot(is_integerish(index, n = 1L), index >= 1L)
    glue("\\", index)
}

