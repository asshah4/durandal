test_that("hazard works", {

	object <-
		targets::tar_read(cox_mdls, store = '../mims/_targets') |>
		dplyr::filter(name == 'parsimonious')

	data <-
		targets::tar_read(clinical, store = '../mims/_targets') |> mutate(lf_delta_bin = factor(
			lf_delta_bin,
			levels = c(1, 0),
			labels = c('Yes', 'No')
		)) |> mutate(lf_rest_quartile = factor(
			lf_rest_quartile,
			levels = c(1, 0),
			labels = c('Yes', 'No')
		))

	events <-
		list('death_cv_yn' ~ 'Cardiovascular mortality',
				 'death_any_yn' ~ 'All-cause mortality')

	followup <- 'death_timeto'

	terms <- list(lf_delta_bin ~ '')

	adjustment <-
		list(
			2 ~ 'Rate per 100 person-years',
			5 ~ 'Adjusted for demo',
			7 ~ 'Adjust for above + clinical',
			8 ~ 'Adjust for above + stress testing'
		)


})
