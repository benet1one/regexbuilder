
library(testthat, pos = 4)

decimal_number <- {
    num <- digit('+')
    dot <- literally('.')
    glue(num, maybe(glue(dot, num)))
}

test_that("compound", {
    expect_equal(decimal_number, r"(\d+(?:\.\d+)?)")
})

test_that("simple", {
    expect_equal(word(), r"(\w)")
    expect_equal(word("?"), r"(\w?)")
    expect_equal(word(0:1, negate=TRUE), r"(\W?)")
    expect_equal(word(0:Inf), r"(\w*)")
    expect_equal(word(1:Inf), r"(\w+)")
    expect_equal(word(3), r"(\w{3})")
    expect_equal(word(0:3), r"(\w{,3})")
    expect_equal(word(3:Inf), r"(\w{3,})")

    expect_equal(maybe("a"), "a?")
    expect_equal(maybe("a", capture = TRUE), "(a)?")
    expect_equal(maybe("a", capture = "name"), "(?<name>a)?")

    expect_equal(capture("a"), "(a)")
    expect_equal(capture("a", name = "cap"), "(?<cap>a)")

    expect_equal(any_char(0:Inf, include_newline = TRUE), r"((?:.|\n)*)")
})

test_that("incorrect simple", {
    expect_error(word("++"))
    expect_error(word("-"))
    expect_error(word(-1:1))
    expect_error(word(3:2))
})
