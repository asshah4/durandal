#' @keywords internal
#' @noRd
validate_class <- function(x, what) {
	if (!inherits(x, what)) {
		stop(
			deparse(substitute(x)),
			" needs to inherit from `",
			paste("c(", paste(what, collapse = ", "), ")", sep = ""),
			"`, but is of class `",
			class(x),
			"`.",
			call. = FALSE
		)
	}
	invisible(TRUE)
}

#' Validate arguments for term creation
#' @keywords internal
#' @noRd
validate_classes <- function(x, what) {

	varnames <- names(x)

	lapply(
		varnames,
		FUN = function(.x) {
			if (!inherits(x[[.x]], what)) {
				stop(
					"`",
					.x,
					"` needs to inherit from `",
					paste("c(", paste(what, collapse = ", "),
								")",
								sep = ""
					),
					"`.",
					call. = FALSE
				)
			}
		}
	)

	invisible(TRUE)

}

