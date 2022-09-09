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
	columns = list(beta ~ "Estimates", conf ~ "95% CI", n ~ "Number")
	axis = list(lab ~ "B (95% CI)", lim ~ c(0, 0.1), breaks ~ c(0, .02, .05, .1), scale ~ "continuous")

	tbl <- tbl_forest.forge(
		object = object,
		formula = formula,
		vars = vars,
		columns = columns,
		axis = axis
	)

	expect_s3_class(tbl, "gt_tbl")

	# TODO modify how axes are made
	# TODO Ability to relabel the levels of stratified groups

})

test_that("interaction terms can be used for forest plots", {

	test_data <-
		mtcars |>
		dplyr::mutate(heavyweight = dplyr::if_else(wt < median(wt), 0, 1))

	x <- sx(mpg ~ X(wt) + vs + am + In(am), pattern = "direct")
	f <- fmls(
		x,
		label = list(mpg ~ "Mileage", vs ~ "Vroom Sounds", am ~ "Automatic Transmission"),
		order = 2
	)
	fits <- fit(f, .fit = lm, data = test_data, archetype = TRUE)
	m <- mdls(fits)

	object = m
	formula = mpg ~ wt
	vars = c("am")
	columns = list(beta ~ "Estimates", conf ~ "95% CI", n ~ "Number", p ~ "p-value")
	axis = list(lab ~ "B (95% CI)", lim ~ c(0, 0.1), breaks ~ c(0, .02, .05, .1), scale ~ "continuous", title ~ "Plots")

	tbl <- tbl_forest(
		object = m,
		formula = formula,
		vars = vars,
		columns = columns,
		axis = axis,
		interaction = TRUE,
		flip = FALSE
	)

	expect_s3_class(tbl, "gt_tbl")
})

# Skipping on running tests
test_that("survival models can be made into forest plots", {
	skip("Manual build of test only")

	# External data set in forge format
	object <- readRDS("../mims/_targets/objects/subgroup_models")
	formula <- Surv(death_timeto, death_cv_yn) ~ hf_stress_rest_delta_zn
	vars <- c("age_median", "female_bl", "blackrace", "hx_diabetes_bl", "gensini_median", "simi", "lvef")
	columns <- list(beta ~ "Hazard Ratio", conf ~ "95% CI", n ~ "No.", p ~ "p-value")
	axis <- list(lim ~ c(0,10), lab ~ "HR (95% CI)", title ~ "Increasing Hazard", breaks ~ c(0,1, 2, 5, 10), int ~ 1, scale ~ "log")


	tbl_forest(
		object,
		formula = formula,
		vars = vars,
		columns = columns,
		axis = axis,
		flip = TRUE,
		interaction = TRUE
	)

})

