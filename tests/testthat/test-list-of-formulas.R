test_that("list_formulas class generation works", {

	x <- X(output) ~ input
	y <- apples + bananas ~ orange + (1 | peels)
	z <- ~ garbage_in
	f <- list(x, y, z)
	fl_1 <- list_of_formulas(x, y, z)
	fl_2 <- list_of_formulas(f)
	expect_equal(fl_1, fl_2)
	expect_s3_class(fl_1, "list_of_formulas")

	# Conversion
	expect_type(formula(fl_1), "list")

})

