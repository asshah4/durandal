test_that("an atomic path vector/record can be made", {

	# Blank or initialization
	p0 <- paths()
	expect_length(p0, 0)
	expect_output(print(p0), "0")
	expect_output(print(paths()), "<paths\\[0\\]>")
	expect_length(format(paths()), 0)

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
	expect_output(print(pv), "<paths\\[3\\]>")

	# Tibble
	if (isTRUE(requireNamespace("tibble", quietly = TRUE))) {
		tibble::tibble(pv) |>
			print() |>
			expect_output("<pt>")
	}

	# Expecting errors
	expect_error(paths(1))

})

test_that("simple and complex formulas can break down into a path", {

	# Formulas
	x <- mpg ~ wt + hp
	p <- paths(x)
	expect_length(p, 2)

	x <- mpg + wt ~ hp + am + vs + cyl
	p <- paths(x)
	expect_length(p, 8)

	# Scripts
	x <- rx(mpg + wt ~ X(hp) + am)
	p <- paths(x)
	expect_length(p, 4)

	x <- rx(mpg + wt ~ X(hp) + M(cyl) + qsec)
	p <- paths(x)
	expect_length(p, 8)

	x <- rx(mpg + wt ~ X(hp) + M(cyl) + qsec + gear, pattern = "sequential")
	p <- paths(x)
	expect_length(p, 11)

})

test_that("coercion is appropriate", {

	# Characters
	p1 <- paths(x = "X", to = "Y")
	p2 <- paths(x = "M", to = "Y")
	p3 <- paths(x = "X", to = "M")
	pv <- c(p1, p2, p3)
	pc <- c(pv, "test")

	# Base R vectorization will just return a list
	expect_type(c("test", pv), "list")
	cp <- vec_c("test", pv)
	expect_type(cp, "character")

	expect_type(as.character(pv), "character")
	expect_length(as.character(pv), 3)
	expect_s3_class(formula(pv)[[1]], "formula")
	expect_length(formula(pv), 3)
	expect_type(pc, "character")


})

