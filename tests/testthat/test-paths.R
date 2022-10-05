test_that("appropriate input to get paths", {

	m <-
		rx(am ~ X(wt) + mpg + cyl,
			 label = list(am ~ "Automatic Transmission",
			 						 wt ~ "Weight",
			 						 cyl ~ "Cylinder")) |>
		sx(pattern = "sequential") |>
		fmls(order = 2:4) |>
		fit(.fit = glm,
				data = mtcars,
				archetype = TRUE)

	x <- mdls(m)



})
