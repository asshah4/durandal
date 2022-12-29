test_that("new `tm` can be made from character/atomic components", {
	ty <- tm(
		x = "Y",
		role = "outcome",
		label = "Dependent Variable",
		description = "Artificially created",
		distribution = "normal",
		type = "continuous"
	)

	tx <- tm(
		"X",
		role = "exposure",
		label = "Independent Variable",
		description = "Artificially created",
		distribution = "normal",
		type = "continuous"
	)

	tm <- tm(
		"M",
		role = "mediator",
		label = "Independent Variable",
		description = "Artificially created",
		distribution = "normal",
		type = "continuous"
	)

	tc <- tm(
		"C",
		role = "confounder",
		label = "Confounder",
		description = "Artificially created",
		distribution = "ordinal",
		type = "categorical"
	)

	tp <- tm(
		"P",
		role = "predictor",
		label = "Independent Variable",
		description = "Artificially created",
		distribution = "ordinal",
		type = "categorical"
	)

	ts <- tm(
		"S",
		role = "strata",
		label = "Stratification Variable",
		description = "Levels for data set",
		distribution = "binary",
		type = "categorical",
	)

	ti <- tm(
		"I",
		role = "interaction",
		label = "Interaction Variable",
		description = "Interaction for the exposure variable",
		distribution = "binary",
		type = "categorical",
	)

	t <- c(ty, tx, tm, tp, tc, ts, ti)

	expect_length(t, 7)
	expect_true(is_tm(t))

})

