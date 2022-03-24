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
path_stack <- function(x = unspecified(), ...) {
	UseMethod("path_stack", object = x)
}

#' @rdname path_stack
#' @export
path_stack.paths <- function(x = paths(),
														 ...) {

	# Create list of formulas from the paths
	fl <- list()
	for (i in seq_along(x)) {
		f <-
			paste(paste(field(x[i], "to")),
						paste(field(x[i], "from")),
						sep = " ~ ") |>
			stats::as.formula()

		fl <- append(fl, f)
	}
	lof <- fmls(fl)

	# Extract terms from the list
	tm <- term()
	for (i in seq_along(x)) {
		tm <- append(tm, field(x[i], "from"))
		tm <- append(tm, field(x[i], "to"))
	}
	tm <-
		vec_data(tm) |>
		unique() |>
		vec_restore(term())

	new_path_stack(
		path_stack = lof,
		terms = tm
	)

}

#' @rdname path_stack
#' @export
path_stack.default <- function(x = unspecified(), ...) {
	# Early break
	if (length(x) == 0) {
		return(new_path_stack())
	}

	stop("`term()` is not defined for a `",
			 class(x)[1],
			 "` object.",
			 call. = FALSE)

}
# List Of Definition ------------------------------------------------------------

#' paths records
#' @keywords internal
#' @noRd
new_path_stack <- function(path_stack = formula_list(),
													 terms = term()) {

	# Validation
	validate_class(path_stack, "formula_list")
	vec_assert(terms, ptype = term())

	new_rcrd(
		fields = list(
			"paths" = path_stack
		),
		terms = terms,
		class = "path_stack"
	)

}

#' @keywords internal
#' @noRd
methods::setOldClass(c("path_stack", "vctrs_vctr"))

# Output -----------------------------------------------------------------------

#' @export
format.path_stack <- function(x, ...) {

	fmt_ps <- character()

	if (vec_size(x) == 0) {
		fmt_ps <- new_path_stack()
	} else {
		fmt_ps <-
			vec_data(x)$paths |>
			format()
	}

	# Return
	fmt_ps
}

#' @export
obj_print_data.path_stack <- function(x) {

	if (vec_size(x) == 0) {
		new_path_stack()
	} else if (vec_size(x) > 1) {
		cat(format(x), sep = "\n")
	} else {
		cat(format(x))
	}
}

#' @export
vec_ptype_full.path_stack <- function(x, ...) {
	"path_stack"
}

#' @export
vec_ptype_abbr.path_stack <- function(x, ...) {
	"pth_stk"
}

# Casting and coercion ---------------------------------------------------------

#' @export
vec_ptype2.path_stack.path_stack <- function(x, y, ...) {
	x
}

#' @export
vec_cast.path_stack.path_stack <- function(x, y, ...) {
	x
}
