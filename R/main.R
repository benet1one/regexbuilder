
build_pattern <- function(expr, env = caller_env()) {
    env <- as_environment(rb, parent = env)
    res <- eval(expr, env)

}

#' @export
rb <- within(list(), {
    literally <- fixed <- function(string) {
        str_replace_all(
            string,
            pattern = r"(([\.\|\(\)\[\]\{\}\^\$\?\*\+\\]))",
            replacement = r"(\\\0)"
        ) |> as_glue()
    }
    times <- function(pattern, times = 1:Inf, capture = FALSE) {
        validate_pattern(pattern)
        times <- parse_times(enexpr(times))
        suffix <- if (is_character(times))
            times
        else if (length(times) == 1L) {
            if (times == 1L) ""
            else paste0("{", times, "}")
        } else if (all(times == c(0L, 1L))) {
            "?"
        } else if (all(times == c(0L, Inf))) {
            "*"
        } else if (all(times == c(1L, Inf))) {
            "+"
        } else if (times[2L] == Inf) {
            paste0("{", times[1L], ",}")
        } else {
            paste0("{", times[1L], ",", times[2L], "}")
        }

        if (capture)
            glue("({pattern}){suffix}")
        else if (nchar(pattern) == 1L)
            glue("{pattern}{suffix}")
        else
            glue("(?:{pattern}){suffix}")

    }
    capture <- function(pattern, name = NULL) {
        validate_pattern(pattern)
        if (is.null(name))
            return(glue("({pattern})"))
        if (str_detect(name, r"(\W)"))
            stop("Capture group names can only contain letters and underscores.")

        glue("(?<{name}>{pattern})")
    }

    any <- function(include_newline = FALSE) {
        ifelse(include_newline, no = ".", yes = "(?:.|\\n)")
    }
    any_of <- function(characters, times = 1L, negate = FALSE) {
        for (k in seq_along(characters))
            if (!is_pattern_atomic(characters))
                warning("Pattern '", characters[k], " is not atomic. ",
                        "Result may be unexpected.")
        neg <- ifelse(negate, yes = '^', no = '')
        any <- glue("[", neg, glue_collapse(characters), "]")
        times(any, times)
    }
    none_of <- function(characters, times = 1L, negate = TRUE) {
        any_of(characters, times, negate)
    }
    option <- function(patterns, capture = FALSE) {
        validate_pattern(patterns)
        options <- glue_collapse(patterns, sep = "|")
        if (is_true(capture))
            glue("({options})")
        else if (is_false(capture))
            glue("(?:{options})")
        else if (is_character(capture, n = 1L))
            capture(options, name = capture)
        else stop("Capture must be TRUE, FALSE, or a character with the name of the capture group.")
    }

    word <- new_meta_function("w")
    digit <- new_meta_function("d")
    whitespace <- new_meta_function("s")
    newline <- new_meta_function("n")

    start <- function() '^'
    end <- function() '$'
})


