#' Model map and its paths
#' @export
model_map <- function(x) {
	UseMethod("model_map", object = x)
}

#' @export
model_map.forge <- function(x) {

	# Trim to only models that have been run
	y <-
		x |>
		dplyr::filter(run == TRUE)

	# Unique exposure and outcome combinations (which are order = 1 formulas)
	paths <-
		y |>
		dplyr::select(terms, outcome, exposure) |>
		dplyr::rowwise() |>
		dplyr::mutate(
			outcome = get_runes(terms, field = "runes", value = outcome),
			exposure = get_runes(terms, field = "runes", value = exposure)
		) |>
		dplyr::select(-terms) |>
		dplyr::ungroup() |>
		tibble::tibble() |>
		unique()

	# Model types
	model_types <-
		y |>
		dplyr::select(type, subtype) |>
		tibble::tibble() |>
		unique()

	# Strata or subgroups (or interactions)

	# Sequential or parallel adjustments


}
