test_that("inputs are appropriate for grouped forest tables", {
	object <-
		sx(mpg ~ X(wt) + S(vs) + S(am), pattern = "direct") |>
		fmls(
			label = list(mpg ~ "Mileage", vs ~ "Vroom Sounds", am ~ "Automatic Transmission"),
			order = 2
		) |>
		fit(.fit = lm, data = mtcars, archetype = TRUE) |>
		mdls()

	tbl_1 <- tbl_group_forests(
		object,
		formula = mpg ~ wt,
		vars = c("vs", "am"),
		type = "subgroup",
		columns = list(beta ~ "Estimates", conf ~ "95% CI", n ~ "Number"),
		axis = list(
			lab ~ "B (95% CI)",
			lim ~ c(0, 0.1),
			breaks ~ c(0, .02, .05, .1),
			scale ~ "continuous",
			title ~ "Forest Plots"
		)
	)
	expect_s3_class(tbl_1, "gt_tbl")
})

test_that("subgroup models can be made, with a forest plot", {

	# Outcome = am
	# Exposure = wt (and binary)
	# Adjustment = mpg
	# Subgroup = vs
	test_data <-
		mtcars |>
		dplyr::mutate(heavyweight = dplyr::if_else(wt < median(wt), 0, 1))

	x <- sx(mpg ~ X(wt) + S(vs) + S(am), pattern = "direct")
	f <- fmls(
		x,
		label = list(mpg ~ "Mileage", vs ~ "Vroom Sounds", am ~ "Automatic Transmission"),
		order = 2
	)
	fits <- fit(f, .fit = lm, data = test_data, archetype = TRUE)
	m <- mdls(fits)

	object = m
	formula = mpg ~ wt
	vars = c("vs", "am")
	columns = list(beta ~ "Estimates", conf ~ "95% CI", n ~ "Number", p ~ "P Value")
	axis = list(lab ~ "B (95% CI)", lim ~ c(0, 0.1), breaks ~ c(0, .02, .05, .1), scale ~ "continuous", title ~ "Forest Plot")

	tbl <- tbl_group_forests(
		object = object,
		formula = formula,
		vars = vars,
		columns = columns,
		axis = axis,
		type = "subgroup"
	)

	expect_s3_class(tbl, "gt_tbl")

	# TODO modify how axes are made
	# TODO Ability to relabel the levels of stratified groups

})

test_that("interaction terms can be used for forest plots", {

	x <- sx(mpg ~ X(wt) + hp + vs + am + In(am), pattern = "direct")
	f <- fmls(
		x,
		label = list(mpg ~ "Mileage", vs ~ "Vroom Sounds", am ~ "Automatic Transmission"),
		order = 2
	)
	fits <- fit(f, .fit = lm, data = mtcars, archetype = TRUE)
	m <- mdls(fits)

	object = m
	formula = mpg ~ wt
	vars = c("am")
	columns = list(beta ~ "Estimates", conf ~ "95% CI", n ~ "Number", p ~ "p-value")
	axis = list(lab ~ "B (95% CI)", lim ~ c(0, 0.1), breaks ~ c(0, .02, .05, .1), scale ~ "continuous", title ~ "Plots")
	level = list(am ~ c("Manual", "Automatic"))

	tbl <- tbl_group_forests(
		object = m,
		formula = formula,
		vars = vars,
		level = level,
		columns = columns,
		axis = axis,
		type = "interaction",
		flip = FALSE
	)
	expect_s3_class(tbl, "gt_tbl")

	expect_error(tbl_group_forests(
		object = m,
		formula = formula,
		vars = vars,
		level = level,
		columns = columns,
		axis = axis,
		type = "bad_type",
		flip = FALSE
	))

	expect_message(tbl_group_forests(
		object = m,
		formula = formula,
		vars = c("am", "hp"),
		level = level,
		columns = columns,
		axis = axis,
		type = "interaction",
		flip = FALSE
	))

})

test_that("level relabeling works", {

	m1 <-
		sx(mpg ~ X(wt) + hp + vs + am + In(am), pattern = "direct") |>
		fmls(label = list(am ~ "Automatic Transmission"), order = 2) |>
		fit(.fit = lm, data = mtcars, archetype = TRUE)

	m2 <-
		sx(mpg ~ X(wt) + hp + In(vs) + am, pattern = "direct") |>
		fmls(label = list(vs ~ "Vroom Sounds"), order = 2) |>
		fit(.fit = lm, data = mtcars, archetype = TRUE)

	m <- mdls(m1, m2)

	object = m
	formula = mpg ~ wt
	vars = c("am", "vs")
	columns = list(beta ~ "Estimates", conf ~ "95% CI", n ~ "Number", p ~ "p-value")
	axis = list(lab ~ "B (95% CI)", lim ~ c(0, 0.1), breaks ~ c(0, .02, .05, .1), scale ~ "continuous", title ~ "Plots")
	level = list(am ~ c("Manual", "Automatic"), 0 ~ "Absent")

	tbl <- tbl_group_forests(
		object = m,
		formula = formula,
		vars = vars,
		level = level,
		columns = columns,
		axis = axis,
		type = "interaction",
		flip = FALSE
	)

	expect_true(all(c("Manual", "Automatic", "Absent") %in% tbl[["_stub_df"]]$rowname))

})

# Skipping on running tests
test_that("survival models can be made into forest plots", {
	skip("Manual build of test only")

	# External data set in forge format
	object <- readRDS("../mims/_targets/objects/subgroup_models")
	formula <- Surv(death_timeto, death_cv_yn) ~ hf_stress_rest_delta_zn
	vars <- c("age_median", "female_bl", "blackrace", "hx_diabetes_bl", "gensini_median", "simi", "lvef")
	columns <- list(beta ~ "Hazard Ratio", conf ~ "95% CI", n ~ "No.", p ~ "P value")
	axis <- list(lim ~ c(0,10), lab ~ "HR (95% CI)", title ~ "Increasing Hazard", breaks ~ c(0,1, 2, 5, 10), int ~ 1, scale ~ "log")


	x <- tbl_group_forests(
		object,
		formula = formula,
		vars = vars,
		columns = columns,
		axis = axis,
		flip = TRUE,
		type = "interaction"
	)

})


