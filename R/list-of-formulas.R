#' List of formulas
#'
#' Vectorized subclass of the `list_of` class from {vctrs::list_of()}
#'
#' # Pluralized Arguments
#'
#' For the arguments that would be dispatched for objects that are plural,
#' e.g. containing multiple terms such as a `formula` object, the input should
#' be wrapped within a `list()`.
#'
#' For example, for the __role__ argument, it would be written:
#'
#' `role = list(X ~ "exposure", M ~ "mediator", C ~ "confounder")`
#'
#' This applies for all others plural objects and arguments.
#'
#' @name list_of_formulas
#' @export
list_of_formulas <- function(...) {

	# Early break
	if (missing(..1)) {
		return(new_list_of_formulas())
	}

	if (inherits(..1, "list") & ...length() == 1) {
		f <- list(...)[[1]]
	} else {
		f <- list(...)
	}

	lof <- lapply(
		f,
		FUN = function(.x) {
			stopifnot(inherits(.x, "formula"))
			.y <- deparse1(.x)
			gsub("\"", "", .y)

		}
	)

	#left <- get_left_side(x)
	##right <- get_right_side(x)
	#stats::reformulate(termlabels = right, response = left)

	new_list_of_formulas(lof)

}

#' @keywords internal
#' @noRd
new_list_of_formulas <- function(...) {

	new_list_of(
		x = list(...),
		ptype = character(),
		class = "list_of_formulas"
	)

}

#' @keywords internal
#' @noRd
methods::setOldClass(c("list_of_formulas", "vctrs_list_of"))


#' @export
obj_print_data.list_of_formulas <- function(x, ...) {
	if (vec_size(x) == 0) {
		new_list_of_formulas()
	}

	if (vec_size(x) >= 1) {
		cat(format(x), sep = "\n")
	} else {
		cat(format(x))
	}
}

#' @export
format.list_of_formulas <- function(x, ...) {

	fmt <- character()

	# Character representation of formula
	if (vec_size(x) == 0) {
		return()
	} else {
		fmt <- sapply(x, FUN = as.character)
	}

	# Return
	fmt
}

#' @export
vec_ptype_full.list_of_formulas <- function(x, ...) {
	"list_of_formulas"
}

#' @export
vec_ptype_full.list_of_formulas <- function(x, ...) {
	"lst_fmls"
}

#' @export
vec_ptype2.list_of_formulas.list_of_forulas <- function(x, y, ...) {
	new_list_of_formulas()
}

#' @export
vec_cast.list_of_formulas.list_of_formulas <- function(x, to, ...) {
	x
}

# Formula Helpers ----

#' @export
formula.list_of_formulas <- function(x, ...) {

	y <- list()
	for (i in seq_along(x)) {
		y <- append(y, x[[i]])
	}
	lapply(y, FUN = stats::formula)

}

#' Tools for working with formulas
#' @name formula_helpers
#' @export
get_left_vars <- function(x) {
	if (inherits(x, "formula")) {
		if (length(x) == 2) {
			res <- character()
		} else {
			res <- all.vars(x[[2]])
		}
	}

	res
}

#' @rdname formula_helpers
#' @export
get_left_side <- function(x) {
	if (inherits(x, "formula")) {
		if (length(x) == 2) {
			res <- character()
		} else {
			res <- deparse1(x[[2]])
		}
	}

	res
}

#' @rdname formula_helpers
#' @export
get_right_vars <- function(x) {
	if (inherits(x, "formula")) {
		if (length(x) == 2) {
			res <- all.vars(x)
		} else {
			res <- all.vars(x[[3]])
		}
	}

	res
}

#' @rdname formula_helpers
#' @export
get_right_side <- function(x) {
	if (inherits(x, "formula")) {
		res <-
			x[[length(x)]] |>
			deparse1() |>
			strsplit("\\+|-") |>
			unlist() |>
			trimws()
	}

	res
}
