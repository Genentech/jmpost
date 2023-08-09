

test_that("Basic Jinjar syntax works with `decorated_render()`", {
    string <- "multi
{{ myvar }}
{% if myvar == 5%}hello{% endif %}
line
"
    observed_1 <- decorated_render(.x = string, myvar = 5)
    observed_2 <- decorated_render(.x = string, myvar = 6)
    expected_1 <- "multi\n5\nhello\nline\n"
    expected_2 <- "multi\n6\n\nline\n"

    expect_equal(observed_1, expected_1)
    expect_equal(observed_2, expected_2)
})


test_that("Global variables defined by `decorated_render() are usable", {
    string <- "{{ var1 }} hi there {{ var2 }} and {{ machine_double_eps }}"
    observed <- decorated_render(.x = string, var1 = 5, var2 = "bob")
    expected <- sprintf(
        "%d hi there %s and %s",
        5,
        "bob",
        scales::scientific(sqrt(.Machine$double.eps), digits = 8)
    )
    expect_equal(observed, expected)
})
