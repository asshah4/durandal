get_rhs <- function(x) {
	if (inherits(x, "formula")) {
		rhs <-
			x |>
			stats::terms() |>
			labels()
	} else {
		cl <- class(x)
		err <- sprintf("get_rhs() only works for class `formula`, and not for `%s`", cl)
		stop(err, call. = FALSE)
	}

	rhs
}


get_lhs <- function(x) {
	if (inherits(x, "formula")) {
		lhs <- as.character(x[[2]])
	} else {
		cl <- class(x)
		err <- sprintf("get_lhs() only works for class `formula`, and not for `%s`", cl)
		stop(err, call. = FALSE)
	}

	lhs
}
