### Class definition -----------------------------------------------------------

#' @export
link <- function(left = tm(),
								 right = tm(),
								 ...) {

	new_link(left = left, right = right)

}

#' link record
#' @keywords internal
#' @noRd
new_link <- function(left = tm(), right = tm()) {

	vec_assert(left, ptype = tm())
	vec_assert(right, ptype = tm())

	new_rcrd(list(left = left, right = right),
					 class = "link")
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("link", "vctrs_vctr"))

#' @export
format.link <- function(x, ...) {
	left <- field(x, "left")
	right <- field(x, "right")

	out <- paste0(left, " ~ ", right)

	out
}

#' @export
obj_print_data.link <- function(x, ...) {

	if (vec_size(x) == 0) {
		fmt <- new_link()
	} else {
		fmt <- format(x)
	}

	if (length(fmt) > 1) {
		cat(format(fmt), sep = "\n")
	} else {
		cat(format(fmt))
	}

}

#' @export
vec_ptype_full.link <- function(x, ...) "links"

#' @export
vec_ptype_abbr.link <- function(x, ...) "lnk"

#' @export
vec_ptype2.link.link <- function(x, y, ...) x

#' @export
vec_cast.link.link <- function(x, to, ...) x

#' @export
vec_ptype2.link.character <- function(x, y, ...) y

#' @export
vec_ptype2.character.link <- function(x, y, ...) x

#' @export
vec_cast.character.link <- function(x, to, ...) {
	paste0(field(x, "left"), " ~ ", field(x, "right"))
}
