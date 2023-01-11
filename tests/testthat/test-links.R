test_that("appropriate input to get paths", {

	t <- tm(y ~ .x(x))
	left <- t[1]
	right <- t[2]
	l1 <- link(left, right)
	l2 <- link(right, left)

	expect_length(l1, 1)
	expect_length(new_link(), 0)
	expect_output(print(l1), "y ~ x")
	expect_output(print(link()), "<links\\[0\\]>")
	expect_length(c(l1, l2), 2)
	expect_s3_class(l1, "link")
	expect_type(as.character(l1), "character")



})
