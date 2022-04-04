# Paths ------------------------------------------------------------------------

# Example:
#		y ~ x

#' Pathways
#'
#' @param x An object of the following types
#'
#' @inheritParams archetypes::term_archetype
#'
#' @section List~Formula Arguments:
#'
#' The __role__ and __label__ arguments should be provided as a list of formulas.
#' @export
#' @name paths
paths <- function(x = unspecified(), ...) {
	UseMethod("paths", object = x)
}

#' @rdname paths
#' @export
paths.character <- function(x = character(),
														to = character(),
														role = list(),
														label = list(),
														family = character(),
														...) {

	# Transform into terms, with expectation of Y ~ X
	from <-
		term_archetype(x,
				 side = "right",
				 role = "unknown")

	to <-
		term_archetype(to,
				 side = "left",
				 role = "unknown")

	tm <-
		c(from, to) |>
		set_roles(roles = archetypes:::formula_args_to_list(role)) |>
		set_labels(labels = archetypes:::formula_args_to_list(label))


	new_paths(
		from = tm[1],
		to = tm[2]
	)

}

#' @rdname paths
#' @export
paths.default <- function(x = unspecified(), ...) {
	# Early break
	if (length(x) == 0) {
		return(new_paths())
	}

	stop("`paths()` is not defined for a `",
			 class(x)[1],
			 "` object.",
			 call. = FALSE)

}

# Record Definition ------------------------------------------------------------

#' paths records
#' @keywords internal
#' @noRd
new_paths <- function(from = term_archetype(),
											to = term_archetype(),
											track = formula_archetype()) {
	# Terms
	vec_assert(from, ptype = term_archetype())
	vec_assert(to, ptype = term_archetype())
	vec_assert(track, ptype = formula_archetype())

	new_rcrd(fields = list(
		"from" = from,
		"to" = to,
		"track" = track
	),
	class = "paths")

}

#' @keywords internal
#' @noRd
methods::setOldClass(c("paths", "vctrs_vctr"))

# Output -----------------------------------------------------------------------

#' @export
format.paths <- function(x, ...) {

	fmt <- character()

	if (vec_size(x) == 0) {
		fmt <- new_paths()
	} else {

		for (i in seq_along(x)) {
			# Obtain components
			f <- field(x[i], "from")
			t <- field(x[i], "to")

			# Formula notation
			fmt <- append(fmt, paste(format(t), "~", format(f)))
		}
	}

	# Return
	fmt
}

#' @export
obj_print_data.paths <- function(x) {

	if (vec_size(x) == 0) {
		new_paths()
	} else if (vec_size(x) > 1) {
		cat(format(x), sep = "\n")
	} else {
		cat(format(x))
	}
}

#' @export
vec_ptype_full.paths <- function(x, ...) {
	"paths"
}

#' @export
vec_ptype_abbr.paths <- function(x, ...) {
	"pt"
}

# Casting and coercion ---------------------------------------------------------

#' @export
vec_ptype2.paths.paths <- function(x, y, ...) {
	x
}

#' @export
vec_cast.paths.paths <- function(x, y, ...) {
	x
}

#' @export
vec_ptype2.paths.character <- function(x, y, ...) {
	y
}

#' @export
vec_ptype2.character.paths <- function(x, y, ...) {
	x
}

#' @export
vec_cast.character.paths <- function(x, to, ...) {
	format(x)
}

#' @export
formula.paths <- function(x, ...) {
	lapply(x, FUN = stats::as.formula)
}
