test_that("new_path_stack() can be generated and printed", {

	# Empty arguments
	from <- to <- term_archetype()
	formula <- formula_archetype()
	model <- character()
	estimate <- statistic <- p.value <- conf.low <- conf.high <- numeric()

	# Combine into tibble
	x <- tibble::tibble(
		from = from,
		to = to,
		formula = formula,
		model = model,
		estimate = estimate,
		statistic = statistic,
		p.value = p.value,
		conf.low = conf.low,
		conf.high = conf.high
	)

	# New stack
	x <- new_path_stack()

})

test_that("path stack can be made from paths", {

	# Vector of paths
	p1 <- paths(
		x = "X1",
		to = "Y",
		role = list(X1 ~ "exposure", Y ~ "outcome"),
		label = list(X1 ~ "Independent Variable", Y ~ "Dependent Variable")
	)
	p2 <- paths(
		x = "M",
		to = "Y",
		role = list(M ~ "mediator", Y ~ "confounder"),
		label = list(M ~ "Intermediary Variable", Y ~ "Dependent Variable")
	)
	p3 <- paths(
		x = "X2",
		to = "Y",
		role = list(Y ~ "outcome"),
		label = list(M ~ "Potential Confounder", Y ~ "Dependent Variable")
	)
	x <- c(p1, p2, p3)

	s <- path_stack(x)
	expect_length(ps, 3)
	expect_output(print(ps), "path_stack")

	# Tibble output check for abbreviations
	# Tibble
	if (isTRUE(requireNamespace("tibble", quietly = TRUE))) {
		tibble::tibble(ps) |>
			print() |>
			expect_output("<pth_stk>")
	}

})

test_that("path stacks error appropriately", {

	# Empty
	expect_length(path_stack(), 0)
	expect_error(path_stack("test"))
})
