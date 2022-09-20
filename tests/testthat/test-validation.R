test_that("class validation works", {
	expect_error(validate_class(rx(), "spell"))
	expect_true(validate_class(rx(), "rune"))
})
