# Paths ------------------------------------------------------------------------

# Example:
#		y ~ x

#' paths vector
#' @export
#' @name paths
paths <- function(x = unspecified(), ...) {
	UseMethod("paths", object = x)
}

#' @rdname paths
#' @export
paths.character <- function(x = character(),
														y = character(),
														role = list(),
														label = list(),
														...) {

	# Transform into terms, with expectation of Y ~ X
	from <-
		term(x,
				 side = "right")

	to <-
		term(y,
				 side = "right")

	new_paths(
		from = from,
		to = to,
		direction = direction
	)

}

#' @rdname paths
#' @export
paths.default <- function(x = unspecified(), ...) {
	# Early break
	if (length(x) == 0) {
		return(new_paths())
	}

	stop("`term()` is not defined for a `",
			 class(x)[1],
			 "` object.",
			 call. = FALSE)

}
# Record Definition ------------------------------------------------------------

#' paths records
#' @keywords internal
#' @noRd
new_paths <- function(from = term(),
											to = term()) {
	# Terms
	vec_assert(from, ptype = term())
	vec_assert(to, ptype = term())

	new_rcrd(list(
			"from" = from,
			"to" = to,
			"direction" = direction
		),
		class = "paths"
	)

}

#' @keywords internal
#' @noRd
methods::setOldClass(c("paths", "vctrs_vctr"))

# Output -----------------------------------------------------------------------

#' @export
format.paths <- function(x, ...) {

	fmt_px <- character()

	if (vec_size(x) == 0) {
		fmt_px <- new_paths()
	} else {

		for (i in seq_along(x)) {
			# Obtain components
			f <- field(x[i], "from") |> as.character()
			t <- field(x[i], "to") |> as.character()

			# Formula notation
			fmt_px <- append(fmt_px, paste(t, "~", f))
		}
	}

	# Return
	fmt_px
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
	"px"
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
