### Class Definition ----------------------------------------------------------

#' Create vectorized terms
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param x An object that can be coerced to a `.terms` object.
#'
#' @param side states the side of the formula the variable belongs on:
#'
#'   * __left__: for variables that are intended to be dependent
#'
#'   * __right__: for variables that are intended to be independent
#'
#'   * __meta__: for variables that are intended to explain relationships
#'
#'   * __unknown__: for variables that have unknown or undetermined sides, such
#'   as unknown position between other variables (e.g. potential mediators,
#'   conditioning variables, etc)
#'
#' @param role Specific roles the variable plays within the formula. These are
#'   of particular importance, as they serve as special terms that can effect
#'   how a formula is interpreted. Please see the _Roles_ section below for
#'   further details. The options for roles are as below:
#'
#'   * __outcome__: outcome/dependent variable that serves as an individual
#'   variable in the \eqn{exposure -> outcome} relationship (DEFAULT for LHS variables)
#'
#'   * __predictor__: predictors of the outcomes (DEFAULT for RHS variables)

#'   * __exposure__: predictor variable that serves as a primary or key
#'   variable in the \eqn{exposure -> outcome} relationship
#'
#'   * __confounder__: predictor variable that is thought to be a confounder of
#'   the causal relationship in the \eqn{exposure <- confounder -> outcome}
#'   pathway, normally thought of as an adjustment or controlling variable
#'
#'   * __mediator__: predictor variable that is thought to be a causal
#'   intermediary in the \eqn{exposure -> mediator -> outcome} pathway
#'
#'   * __interaction__: predictor variable that is proposed as an interaction
#'   term with a exposure variable, and currently only supported if exposure
#'   variables are declared
#'
#'   * __unknown__: default role of a variable that has not yet been assigned a
#'   place, such as a potential intermediary object
#'
#' @param tier Grouping variable names for covariates or __confounders__ for
#'   modeling terms together
#'
#' @param label Display-quality label describing the variable
#'
#' @param description Option for further descriptions or definitions needed for
#'   the .terms, potentially part of a data dictionary
#'
#' @param distribution If its associated with a data vector, describes the
#'   distribution pattern of the original .terms
#'
#' @param class Class of the variable itself, either expected or measured, such
#'   as `character` or `numeric` or `factor`
#'
#' @param type Type of variable, either categorical (qualitative) or
#'   continuous (quantitative)
#'
#' @param subtype How the variable itself is more specifically subcategorized,
#'   e.g. ordinal, continuous, dichotomous, etc
#'
#' @param operation Modification of the term to be applied when
#'   combining with data
#'
#' @section Pluralized Arguments:
#'
#'   For the arguments that would be dispatched for objects that are plural,
#'   e.g. containing multiple terms such as a `formula` object, the input should
#'   be wrapped within a `list()`.
#'
#'   For example, for the __role__ argument, it would be written:
#'
#'   `role = list(X ~ "exposure", M ~ "mediator", C ~ "confounder")`
#'
#'   This applies for all others plural objects and arguments.
#'
#' @name terms
#' @export
.terms <- function()

### Record definition ---------------------------------------------------------

#' record of formula rune
#' @keywords internal
#' @noRd
new_terms <- function(x = character(),
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
	vec_assert(runes, ptype = character())
	vec_assert(role, ptype = character())
	vec_assert(label, ptype = character())
	vec_assert(description, ptype = character())
	vec_assert(type, ptype = character())
	vec_assert(distribution, ptype = character())
	vec_assert(transformation, ptype = character())
	vec_assert(order, ptype = integer())

	# Forced order
	if (length(runes) > 0) {
		order <- 0L
	}

	new_rcrd(
		list(
			"runes" = runes,
			"role" = role,
			"label" = label,
			"description" = description,
			"type" = type,
			"distribution" = distribution,
			"transformation" = transformation,
			"order" = order
		),
		class = ".terms"
	)
}

#' @keywords internal
#' @noRd
methods::setOldClass(c(".terms", "rcrds_rcrd"))
