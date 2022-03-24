test_that("an atomic path vector/record can be made", {

	# Blank or initialization
	p0 <- paths()
	expect_length(p0, 0)
	expect_output(print(p0), "0")

	# Using characters
	p1 <- paths(x = "X", y = "Y")
	p2 <- paths(x = "M", y = "Y")
	pv <- c(p1, p2)

	expect_s3_class(p1, "paths")
	expect_length(p1, 1)
	expect_output(print(p1), "paths")
	expect_output(print(p1), "Y ~ X")
	expect_length(pv, 2)
	expect_output(print(pv), "2")

	# Tibble
	if (isTRUE(requireNamespace("tibble", quietly = TRUE))) {
		tibble::tibble(pv) |>
			print() |>
			expect_output("<px>")
	}

	# Expecting errors
	expect_error(paths(1))

})
