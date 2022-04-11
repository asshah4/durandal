# Paths ------------------------------------------------------------------------

# Example:
# 		y ~ x

#' Pathways
#'
#' @param x An object of the following types:
#'
#'   * `character` where __x__ is the start of the _path_ and __to__ is the end
#'   of the _path_
#'
#'   * `formula` from [stats::formula()]
#'
#'   * `script` object from [archetypes::prescribe()]
#'
#' @inheritParams archetypes::term_archetype
#'
#' @section List~Formula Arguments:
#'
#' The __role__ and __label__ arguments should be provided as a list of
#' formulas. For the arguments that would be dispatched for objects that are
#' plural, e.g. containing multiple terms such as a `formula` object, the input
#' should be wrapped within a `list()`.
#'
#' For example, for the __role__ argument, it would be written:
#'
#' `role = list(X ~ "exposure", Y ~ "outcome", M ~ "mediator")`
#'
#' This applies for all others plural objects and arguments.
#'
#' @export
#' @name paths
paths <- function(x = unspecified(), ...) {
  UseMethod("paths", object = x)
}

#' @rdname paths
#' @export
paths.character <- function(x,
                            to = character(),
                            role = list(),
														tier = list(),
                            label = list(),
                            ...) {

  # Transform into terms, with expectation of Y ~ X
  from <-
    term_archetype(x,
      side = "right",
      role = "unknown"
    )

  to <-
    term_archetype(to,
      side = "left",
      role = "unknown"
    )

  tm <-
    c(from, to) |>
    set_roles(roles = archetypes:::formula_args_to_list(role)) |>
    set_tier(tiers = archetypes:::formula_args_to_list(tier)) |>
    set_labels(labels = archetypes:::formula_args_to_list(label))

  # Will need to trace underlying source or family for the function
  f <- formula_archetype(tm)

  new_paths(
    from = tm[1],
    to = tm[2],
    trace = f
  )
}


#' @rdname paths
#' @export
paths.formula <- function(x,
													role = list(),
													tier = list(),
													label = list(),
                          ...) {

  # Obtain formula components
  t <-
    tm(x) |>
    set_roles(roles = archetypes:::formula_args_to_list(role)) |>
    set_tier(tiers = archetypes:::formula_args_to_list(tier)) |>
    set_labels(labels = archetypes:::formula_args_to_list(label))
  td <- vec_data(t)
  f <- fmls(x)
  fd <- vec_data(f)

  # Break into individual paths
  left <-
    td[td$side == "left", ] |>
    vec_restore(to = term_archetype())

  right <-
    td[td$side == "right", ] |>
    vec_restore(to = term_archetype())

  # Create combination paths
  pl <- paths()

  for (i in seq_along(left)) {
    for (j in seq_along(right)) {
      p <- new_paths(
        from = right[j],
        to = left[i],
        trace = f
      )
      pl <- append(pl, p)
    }
  }

  # Return
  pl
}

#' @rdname paths
#' @export
paths.script <- function(x,
												 ...) {

	t <- field(x, "terms")[[1]]
	td <- vec_data(t)
	lof <- fmls(x, order = 1)

	# New potential path lists
  pl <- paths()

	for (i in seq_along(lof)) {
		f <- lof[i]
		left <- lhs(f)
		right <- rhs(f)
		for (j in left) {
			left_term <-
				td[td$terms == j, ] |>
				vec_restore(to = term_archetype())
			for (k in right) {
				right_term <-
					td[td$terms == k, ] |>
					vec_restore(to = term_archetype())

				# New path
				p <- new_paths(
					from = right_term,
					to = left_term,
					trace = f
				)

	      pl <- append(pl, p)

			}
		}
	}

  # Return
  pl

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
    call. = FALSE
  )
}

# Record Definition ------------------------------------------------------------

#' paths records
#' @keywords internal
#' @noRd
new_paths <- function(from = term_archetype(),
                      to = term_archetype(),
                      trace = formula_archetype()) {
  # Terms
  vec_assert(from, ptype = term_archetype())
  vec_assert(to, ptype = term_archetype())
  vec_assert(trace, ptype = formula_archetype())

  new_rcrd(
    fields = list(
      "from" = from,
      "to" = to,
      "trace" = trace
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
  # The trace may or may not have the binary/unit-based formula
  # Formula should be created from actual data but converted to character
	# Will return a list of formulas
  lapply(x, FUN = function(.x) {
    format(.x) |>
      stats::as.formula()
  })
}
