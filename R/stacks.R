### Relationship Stack ----

# Example:
#		y ~ x
#		y ~ c
# 	y ~ m
#		x ~ c
# 	m ~ x

# Requirements
# - Needs a list or vector of paths
# - Have potential characteristics of roles and potential labels
# - Roles and labels of terms are emergent properties when creating a path stack

#' Stacks
#'
#' @export
#' @name path_stack
path_stack <- function(...) {

	# Validation and early break
	dots <- list(...)
	if (length(dots) == 0) {
		new_path_stack()
	}


	# Initialized variables
	from_list <- to_list <- term_archetype()
	formula_list <- formula_archetype()
	model_list <- character()
	est_list <- stat_list <- p_list <- conf_low_list <- conf_high_list <- numeric()

	for (i in seq_along(dots)) {
		validate_class(dots[i], c("paths", "model_archetype"))

		if (class(dots[i]) == "paths") {

		}
	}

	# Create a path stack object for each member of hte path
	new_path_stack(
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

}

# Stack Table Definition ------------------------------------------------------------

#' paths records
#' @keywords internal
#' @noRd
new_path_stack <- function(from = term_archetype(),
													 to = term_archetype(),
													 formula = formula_archetype(),
													 model = character(),
													 estimate = numeric(),
													 statistic = numeric(),
													 p.value = numeric(),
													 conf.low = numeric(),
													 conf.high = numeric()) {

	vec_assert(from, ptype = term_archetype())
	vec_assert(to, ptype = term_archetype())
	vec_assert(formula, ptype = formula_archetype())
	vec_assert(model, ptype = character())
	vec_assert(estimate, ptype = numeric())
	vec_assert(statistic, ptype = numeric())
	vec_assert(p.value, ptype = numeric())
	vec_assert(conf.low, ptype = numeric())
	vec_assert(conf.high, ptype = numeric())

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

	# Validate composition
	stopifnot(is.data.frame(x))

	# Create new class
	tibble::new_tibble(
		x,
		class = "path_stack",
		nrow = nrow(x)
	)

}

#' @keywords internal
#' @noRd
methods::setOldClass(c("path_stack", "vctrs_vctr"))

# Output -----------------------------------------------------------------------

#' @export
print.path_stack <- function(x, ....) {
	cat(sprintf("<%s>\n", class(x)[[1]]))
	cli::cat_line(format(x)[-1])
}

#' @export
vec_ptype_full.path_stack <- function(x, ...) {
	"path_stack"
}

#' @export
vec_ptype_abbr.path_stack <- function(x, ...) {
	"pth_stk"
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.path_stack <- function(x, ...) {
	out <- format(x)
	pillar::new_pillar_shaft_simple(out, align = "left")
}

# Casting and coercion ---------------------------------------------------------

#' @export
pth_stk_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {
	tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}

#' @export
pth_stk_cast <- function(x, to, ..., x_arg = "", to_arg = "") {
	tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}

#' @export
vec_ptype2.path_stack.path_stack <- function(x, y, ...) {
	pth_stk_ptype2(x, y, ...)
}

#' @export
vec_cast.path_stack.path_stack <- function(x, to, ...) {
	pth_stk_cast(x, to, ...)
}

