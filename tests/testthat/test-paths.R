test_that("an atomic path vector/record can be made", {

	# Blank or initialization
	p0 <- paths()
	expect_length(p0, 0)
	expect_output(print(p0), "0")

	# Using characters
	p1 <- paths(x = "X", to = "Y")
	p2 <- paths(x = "M", to = "Y")
	p3 <- paths(x = "X", to = "M")
	pv <- c(p1, p2, p3)

	expect_s3_class(p1, "paths")
	expect_length(p1, 1)
	expect_output(print(p1), "paths")
	expect_output(print(p1), "Y ~ X")
	expect_length(pv, 3)
	expect_output(print(pv), "2")

	# Tibble
	if (isTRUE(requireNamespace("tibble", quietly = TRUE))) {
		tibble::tibble(pv) |>
			print() |>
			expect_output("<pt>")
	}

	# Expecting errors
	expect_error(paths(1))

})

test_that("formula can break down into a path", {

})

test_that("coercion is appropriate", {

	# Characters
	p1 <- paths(x = "X", to = "Y")
	p2 <- paths(x = "M", to = "Y")
	p3 <- paths(x = "X", to = "M")
	pv <- c(p1, p2, p3)

	expect_type(as.character(pv), "character")
	expect_length(as.character(pv), 3)
	expect_s3_class(formula(pv), "formula")
	expect_length(formula(pv), 3)


})
