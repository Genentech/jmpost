
test_that("SimGroup() works as expected", {
    sim_group <- SimGroup(100, "Arm-A", "Study-X")
    expect_s4_class(sim_group, "SimGroup")
    expect_equal(sim_group@n, 100)
    expect_equal(sim_group@arm, "Arm-A")
    expect_equal(sim_group@study, "Study-X")

    expect_error(
        SimGroup(c(100, 200), "Arm-A", "Study-X"),
        regexp = "`n` must be a length 1 integer"
    )
    expect_error(
        SimGroup(100, c("Arm-A", "Arm-b"), "Study-X"),
        regexp = "`arm` must be a length 1 string"
    )
    expect_error(
        SimGroup(100, "Arm-A", c("Study-X", "Study-Z")),
        regexp = "`study` must be a length 1 string"
    )
})
