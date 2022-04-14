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

	for (i in seq_along(dots)) {
		validate_class(dots[i], c("paths", "model_archetype"))
	}

	t <- tibble::tibble(
		from = field(p, "from"),
		to = field(p, "to"),
		formula = field(p, "trace")
	)

	# Create a path stack object for each member of hte path
	new_path_stack(t)

}

# Stack Table Definition ------------------------------------------------------------

#' paths records
#' @keywords internal
#' @noRd
new_path_stack <- function(x = data.frame()) {

	stopifnot(is.data.frame(x))
	tibble::new_tibble(
		x,
		class = "path_stack",
		nrow = length(x)
	)

}

#' @keywords internal
#' @noRd
methods::setOldClass(c("path_stack", "vctrs_vctr"))

# Output -----------------------------------------------------------------------

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.path_stack <- function(x, ...) {
	out <- format(x)
	pillar::new_pillar_shaft_simple(out, align = "left")
}

#' @export
vec_ptype_full.path_stack <- function(x, ...) {
	"path_stack"
}

#' @export
vec_ptype_abbr.path_stack <- function(x, ...) {
	"pth_stk"
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
