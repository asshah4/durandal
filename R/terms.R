### Class Definition ----------------------------------------------------------

#' Create vectorized terms
#'
#' `r lifecycle::badge('experimental')`
#'
#' A vectorized term object that allows for additional information to be carried
#' with the variable name.
#'
#' This is not meant to replace traditional [stats::terms()], but to supplement
#' it using additional information that is more informative for causal modeling.
#'
#' # Roles
#'
#' Specific roles the variable plays within the formula. These are of particular
#' importance, as they serve as special terms that can effect how a formula is
#' interpreted.
#'
#' | Role | Shortcut | Description |
#' | --- | --- | --- |
#' | outcome | `.o(...)` | exposure &rarr; __outcome__ |
#' | exposure | `.x(...)` | __exposure__ &rarr; outcome |
#' | predictor | `.p(...)` | exposure &plus; __predictor__ &rarr; outcome |
#' | confounder | `.c(...)` | exposure &larr; __confounder__ &rarr; outcome |
#' | mediator | `.m(...)` | exposure &rarr; __mediator__ &rarr; outcome |
#' | interaction | `.i(...)` | exposure &times; __interaction__ &rarr; outcome |
#' | strata | `.s(...)` | exposure &divide; __strata__ &rarr; outcome |
#' | _unknown_ |  | not yet assigned |
#'
#' Formulas can be condensed by applying their specific role to individual runes
#' as a function/wrapper. For example, `y ~ .x(x1) + x2 + x3`. This would
#' signify that `x1` has the specific role of an exposure.
#'
#' @inheritSection list_of_formulas Pluralized Arguments
#'
#' @param x An object that can be coerced to a `tm` object.
#'
#' @param role Specific roles the variable plays within the formula. These are
#'   of particular importance, as they serve as special terms that can effect
#'   how a formula is interpreted. Please see the _Roles_ section below for
#'   further details. The options for roles are as below:
#'
#'   * outcome
#'
#'   * exposure
#'
#'   * predictor
#'
#'   * confounder
#'
#'   * mediator
#'
#'   * interaction
#'
#'   * strata
#'
#'   * unknown
#'
#' @param label Display-quality label describing the variable
#'
#' @param level Display-quality levels of categorical variables, defined in a
#'   `list_of_formula` object
#'
#' @param group Grouping variable name for modeling or placing terms together
#'
#' @param type Type of variable, either categorical (qualitative) or
#'   continuous (quantitative)
#'
#' @param distribution How the variable itself is more specifically
#'   subcategorized, e.g. ordinal, continuous, dichotomous, etc
#'
#' @param description Option for further descriptions or definitions needed for
#'   the tm, potentially part of a data dictionary
#'
#' @param transformation Modification of the term to be applied when
#'   combining with data
#'
#' @name terms
#' @export
tm <- function(x = unspecified(), ...) {
	UseMethod("tm", object = x)
}

#' @rdname terms
#' @export
tm.character <- function(x,
												 role = character(),
												 label = character(),
												 level = character(),
												 group = character(),
												 type = character(),
												 distribution = character(),
												 description = character(),
												 transformation = character(),
												 ...) {

	# Early Break if needed
	stopifnot("Missing/NA value not accepted for `tm` object" = !is.na(x))
	if (length(x) == 0) {
		return(new_tm())
	}

	# Redefine empty variables
	if (length(role) == 0) role <- "unknown"
	if (length(label) == 0) label <- NA
	if (length(level) == 0) level <- NA
	if (length(group) == 0) group <- NA
	if (length(type) == 0) type <- NA
	if (length(distribution) == 0) distribution <- NA
	if (length(description) == 0) description <- NA
	if (length(transformation) == 0) transformation <- NA

	# Casting
	x <- vec_cast(x, character())
	role <- vec_cast(role, character())
	label <- vec_cast(label, character())
	level <- vec_cast(level, character())
	group <- vec_cast(group, character())
	description <- vec_cast(description, character())
	type <- vec_cast(type, character())
	distribution <- vec_cast(distribution, character())
	transformation <- vec_cast(transformation, character())

	new_tm(
		term = x,
		role = role,
		label = label,
		level = level,
		group = group,
		description = description,
		type = type,
		distribution = distribution,
		transformation = transformation,
	)
}

#' @rdname terms
#' @export
tm.formula <- function(x,
											 role = formula(),
											 label = formula(),
											 level = formula(),
											 group = formula(),
											 type = formula(),
											 distribution = formula(),
											 description = formula(),
											 transformation = formula(),
											 ...) {

	# Early Break if needed
	if (length(x) == 0) {
		return(new_tm())
	}

}

#' @noRd
old_formula_to_runes_function <- function(x,
											 role = formula(),
											 label = formula(),
											 level = formula(),
											 group = formula(),
											 type = formula(),
											 distribution = formula(),
											 description = formula(),
											 transformation = formula(),
											 ...) {


	# Early Break if needed
	if (length(x) == 0) {
		return(new_tm())
	}

	# Validate arguments and coerce into original assignments
	# Requires zeallot for more compact code
	# Arguments
	all_args <- c(as.list(environment()), list(...))
	formal_names <- methods::formalArgs(tm.formula)[-c(1, 10)]
	named_args <- all_args[which(names(all_args) %in% formal_names)]
	validate_classes(named_args, what = c("list", "formula"))
	mod_args <- lapply(named_args, list_of_formulas)
	zeallot::`%<-%`(c(role, label, level, group, type, distribution, description, transformation), mod_args)


	# All terms are needed to build rx record
	left <- lhs(x)
	right <- rhs(x, tidy = TRUE)
	all <- c(left, right)
	n <- length(all)

	# Roles and operations need to be identified (on which terms they apply)
	right_ops <-
		x |>
		all.names() |>
		{
			\(.x) {
				# These will be named roles
				var_names <- character()
				var_roles <- character()
				for (i in seq_along(.x)) {
					if (.x[i] %in% template_shortcuts) {
						var_names <- append(var_names, .x[i + 1])
						var_roles <- append(var_roles, .x[i])
					}
				}

				names(var_roles) <- var_names
				var_roles |>
					as.list()
			}
		}()

	# Warn and validate for interaction (as needs exposure variable)
	if ("In" %in% right_ops & !("X" %in% right_ops)) {
		warning("As a specific interaction term was included, an exposure variable must be included as well or this cannot later be expanded to an appropriate formula.")
	}

	# check to see if it is a "role" or a data transformation
	which_ops <- right_ops %in% template_shortcuts
	role_ops <- right_ops[which_ops]
	data_ops <- right_ops[!which_ops]

	other <- right[!(right %in% names(role_ops))]
	other_ops <- rep("predictor", length(other))
	names(other_ops) <- other
	other_ops <- as.list(other_ops)

	left_ops <- rep("outcome", length(left))
	names(left_ops) <- left
	left_ops <- as.list(left_ops)

	role_ops <- c(role_ops, left_ops, other_ops)

	# Interaction term is already included by name
	for (i in seq_along(role_ops)) {
		if (role_ops[[i]] == "O") {
			role_ops[[i]] <- "outcome"
		}
		if (role_ops[[i]] == "X") {
			role_ops[[i]] <- "exposure"
		}

		if (role_ops[[i]] == "M") {
			role_ops[[i]] <- "mediator"
		}

		if (role_ops[[i]] == "C") {
			role_ops[[i]] <- "confounder"
		}

		if (role_ops[[i]] == "S") {
			role_ops[[i]] <- "strata"
		}

		if (role_ops[[i]] == "In") {
			role_ops[[i]] <- "interaction"
		}
	}

	# create tm
	tm_vector <- new_tm()

	for (i in 1:n) {
		# make parameters
		t <- all[i]

		# Sides and meta tm
		side <- if (t %in% names(role_ops[role_ops == "strata"])) {
			"meta"
		} else if (t %in% left) {
			"left"
		} else if (t %in% right) {
			"right"
		}

		# Data transforms
		op <- if (t %in% names(data_ops)) {
			data_ops[[t]]
		} else {
			NA
		}

		# Roles
		role <- if (t %in% names(role_ops)) {
			role_ops[[t]]
		} else {
			NA
		}

		# Tiers
		tier <-
			if (t %in% names(tiers) & t %in% names(role_ops[role_ops %in% c("exposure", "mediator", "strata", "outcome")])) {
				message(
					"The tm `",
					t,
					"` cannot be given a tier as it is not an ordinary predictor."
				)
			} else if (t %in% names(tiers)) {
				tiers[[t]]
			} else {
				NA
			}

		# Labels
		lab <- if (t %in% names(labels)) {
			labels[[t]]
		} else {
			NA
		}

		# place into rx list after casting appropriate classes
		rn <- rx.character(
			x = vec_cast(t, character()),
			side = vec_cast(side, character()),
			role = vec_cast(role, character()),
			tier = vec_cast(tier, character()),
			operation = vec_cast(op, character()),
			label = vec_cast(lab, character())
		)

		tm_vector <- append(tm_vector, rn)

	}

	# return as a record of tm
	tm_vector
}

#' @rdname terms
#' @export
tm.default <- function(x = unspecified(), ...) {
	# Early break
	if (length(x) == 0) {
		return(new_tm())
	}

	stop("`tm()` is not defined for a `",
			 class(x)[1],
			 "` object.",
			 call. = FALSE
	)
}

#' record of formula tm
#' @keywords internal
#' @noRd
new_tm <- function(term = character(),
									 role = character(),
									 label = character(),
									 level = character(),
									 group = character(),
									 type = character(),
									 distribution = character(),
									 description = character(),
									 transformation = character(),
									 order = integer()) {

	# Validation
	vec_assert(term, ptype = character())
	vec_assert(role, ptype = character())
	vec_assert(label, ptype = character())
	vec_assert(description, ptype = character())
	vec_assert(type, ptype = character())
	vec_assert(distribution, ptype = character())
	vec_assert(transformation, ptype = character())
	vec_assert(order, ptype = integer())

	# Forced order
	if (length(term) > 0) {
		order <- 0L
	}

	new_rcrd(
		list(
			"term" = term,
			"role" = role,
			"label" = label,
			"description" = description,
			"type" = type,
			"distribution" = distribution,
			"transformation" = transformation,
			"order" = order
		),
		class = "tm"
	)
}

#' @rdname terms
#' @export
is_tm <- function(x) {
	inherits(x, "tm")
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("tm", "rcrds_rcrd"))

#' @export
format.tm <- function(x, ...) {
	tms <- vec_data(x)
	fmt <- character()

	if (vec_size(x) == 0) {
		fmt <- new_tm()
	} else if (has_cli() & vec_size(x) > 0) {
		for (i in 1:nrow(tms)) {
			if (tms$role[i] == "outcome") {
				t <- tms$term[i]
				fmt <- append(fmt, cli::col_yellow(t))
			}

			if (tms$role[i] == "exposure") {
				t <- tms$term[i]
				fmt <- append(fmt, cli::col_blue(t))
			}

			if (tms$role[i] == "predictor") {
				t <- tms$term[i]
				fmt <- append(fmt, cli::col_black())
			}

			if (tms$role[i] == "mediator") {
				t <- tms$term[i]
				fmt <- append(fmt, cli::col_cyan(t))
			}

			if (tms$role[i] == "confounder") {
				t <- tms$term[i]
				fmt <- append(fmt, cli::col_green(t))
			}

			if (tms$role[i] == "strata") {
				t <- tms$term[i]
				fmt <- append(fmt, cli::col_br_white(t))
			}

			if (tms$role[i] == "interaction") {
				t <- tms$term[i]
				fmt <- append(fmt, cli::col_br_blue(t))
			}

			if (tms$role[i] == "unknown") {
				t <- tms$term[i]
				fmt <- append(fmt, cli::col_br_black())
			}

		}
	} else {
		for (i in 1:nrow(tms)) {
			fmt <- append(fmt, tms$term[i])
		}
	}

	# return
	fmt
}

#' @export
obj_print_data.tm <- function(x, ...) {
	if (vec_size(x) == 0) {
		new_tm()
	} else if (vec_size(x) > 1) {
		cat(format(x), sep = "\n")
	} else {
		cat(format(x))
	}
}

#' @export
vec_ptype_full.tm <- function(x, ...) {
	"tm"
}

#' @export
vec_ptype_abbr.tm <- function(x, ...) {
	"tm"
}


#' @export
vec_ptype2.tm.tm <- function(x, y, ...) x

#' @export
vec_cast.tm.tm <- function(x, to, ...) x

### character() ###

#' @export
vec_ptype2.tm.character <- function(x, y, ...) y # X = tm

#' @export
vec_ptype2.character.tm <- function(x, y, ...) x # X = character

#' @export
vec_cast.tm.character <- function(x, to, ...) {
	# order is flipped, such that `x` is character
	# Cast from character into terms
	attributes(x) <- NULL
	x[[1]]
}

#' @export
vec_cast.character.tm <- function(x, to, ...) {
	# order is flipped, such that `x` is tm
	attributes(x) <- NULL
	x[[1]]
}

