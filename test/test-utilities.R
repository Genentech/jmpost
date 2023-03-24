

library(survival)

test_that("get_missing_rownumbers works as expected", {
    df <- data.frame(
        time = c(1, 2, 3, NA),
        event = c(0, NA, 1, 1),
        x = c(NA, NA, NA, NA),
        y = c(NA, 5, 3, 7),
        z = c(1, 4, 3, NA)
    )

    actual <- get_missing_rownumbers(df, Surv(time, event) ~ y + z)
    expected <- c(1, 2, 4)
    expect_equal(actual, expected)
})



test_that("remove_missing_rows works as expected", {
    
    df <- data.frame(
        time =  c(1 ,   NA,   3,   5,   5,   4,   5,   5),
        event = c(0 ,    0,  NA,   1,   0,   1,   1,   0),
        y =     c(NA,    5,   3,  NA,   3,   4,   3,   3),
        z =     c(NA,    4,   3,   2,  NA,   2,   4,   3),
        x =     c(NA,   NA,  NA,  NA,  NA,  NA,  NA,  NA),
        w =     c("a", "b", "c", "d", "f", "h", "i",  NA_character_)
    )

    expected <- data.frame(
        time =  c( 4, 5),
        event = c( 1, 1),
        y = c( 4, 3), 
        z = c( 2, 4), 
        x = c(NA, NA),
        w = c("h", "i")
    )
    suppressMessages({
        actual <- remove_missing_rows(df, Surv(time, event) ~ y, extra_vars = c("z", "w"))
    })
    
    rownames(expected) <- NULL
    rownames(actual) <- NULL
    
    expect_equal(expected, actual)
    
})

