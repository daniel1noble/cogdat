context("Evaluate data frame")

test_that("Check data structure var, Date, ID", {
	data(dataEg1.rda)
	x <- processDat(dataEg1)
	
	expect_is(x, "data.frame")
})