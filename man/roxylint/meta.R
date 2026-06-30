list(
    linters = list(
        title = roxylint::tidy_title,
        param = function(x, ...) {
            roxylint::lint_full_stop(x, ...)
        },
        typed = function(x, ...) {
            roxylint::lint_full_stop(x, ...)
        },
        return = roxylint::tidy_return,
        seealso = roxylint::tidy_seealso
    )
)
