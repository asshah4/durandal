### Atomic Relationship ----

# Example:
#		y ~ x

#' @keywords internal
#' @noRd
new_path <- function(from = character(), to = character()) {
	vec_assert(from, character())
	vec_assert(to, character())

	new_rcrd(list("from" = from, "to" = to), class = "vctrs_path")
}

#' Path vector
#' @export
#' @name path
path <- function(f, ...) {
	UseMethod(f, "path")
}

#' @rdname path
path.formula <- function(f, ...) {
	lhs <- f[[2]]
	rhs <- labels(stats::terms(f))

	# Ensure only single element on RHS
	n <- length(rhs)
	if (n > 1) {
		err <- sprintf("An atomic path vector cannot be defined if there are %d terms on the RHS", n)
		stop(err, call. = FALSE)
	}

	# Return new path
	from = as.character(rhs)
	to = as.character(lhs)
	new_path(from = from, to = to)
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("vctrs_path", "vctrs_vctr"))

#' @keywords internal
#' @noRd
format.vctrs_path <- function(x, ...) {
	paste(field(x, "to"), "~", field(x, "from"))
}

#' @keywords internal
#' @noRd
obj_print_data.vctrs_path <- function(x) {
	cat(format(x), sep = "\n")
}

#' @export
is_path <- function(x) {
	inherits(x, "vctrs_path")
}

#' @keywords internal
#' @noRd
vec_ptype_abbr.vctrs_path <- function(x, ...) {
	"path"
}

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

#' @keywords internal
#' @noRd
new_path_stack <- function(stack = list(),
													 lhs = character(),
													 rhs = character(),
													 terms = character(),
													 roles = list(),
													 labels = list()) {
	vec_assert(stack = list())
	vec_assert(lhs = character())
	vec_assert(rhs = character())
	vec_assert(terms = character())
	vec_assert(roles = list())
	vec_assert(labels = list())

	new_rcrd(list(
		"stack" = stack,
		"lhs" = lhs,
		"rhs" - rhs,
		"terms" = terms,
		"roles" = roles,
		"labels" = labels
	),
	class = "path_stack")
}

#' Path stack vector
#' @name path_stack
#' @export
stack_paths <- function(f, ...) {
	UseMethod(f, "stack_paths")
}

#' @rdname path_stack
#' @export
stack_paths.vctrs_path <- function(f, roles, labels, ...) {

	# Length of vector
	n <- length(f)
	stack <- f

	# Terms
	lhs <- field(f, "from")
	rhs <- field(f, "to")
	terms <- unique(c(lhs, rhs))

	# Roles


	# Labels

	new_path_stack(
		stack = stack,
		lhs = lhs,
		rhs = rhs,
		terms = terms
	)

}

#' @export
is_path_stack <- function(x) {
	inherits(x, "path_stack")
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("path_stack", "vctrs_vctr"))
