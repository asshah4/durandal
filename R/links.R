### Class definition -----------------------------------------------------------

#' Link together two terms
#' @name links
#' @export
link <- function(x = unspecified(), ...) {
	UseMethod("link", object = x)
}

#' @rdname links
#' @export
link.character <- function(x, to, ...) {

	# Early break if needed
	stopifnot("Missing/NA value not accepted for `link` object" = !is.na(x))
	if (length(x) == 0) {
		return(new_link())
	}

	x <- tm(x, side = "right")
	to <- tm(to, side = "left")

	new_link(left = to, right = x)

}

#' @rdname links
#' @export
link.tm <- function(x, to, ...) {

	# Early break if needed
	stopifnot("Missing/NA value not accepted for `link` object" = !is.na(x))
	if (length(x) == 0) {
		return(new_link())
	}

	new_link(left = to, right = x)

}

#' @rdname links
#' @export
link.default <- function(x = unspecified(), ...) {
	if (length(x) == 0) {
		return(new_link())
	}

	stop("`link()` is not defined for a `",
			 class(x)[1],
			 "` object.",
			 call. = FALSE)
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
