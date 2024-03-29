#' Grouped Forest Plots
#'
#' @param object A `forge` object that contains or represents model data, with
#'   required grouping variables (such as strata or interaction terms).
#'
#' @param formula Identifies the relationships of interest, with LHS
#'   representing the outcome, and RHS representing the exposure.
#'
#' @param vars Character vector of the variables of interest. They can either be
#'   grouping variables, or can be performed as interaction terms (based on the
#'   models included originally). If the parameter __interaction__ is changed to
#'   `TRUE`, then will create joint confidence intervals for the presence or
#'   absence of the interaction term.
#'
#' @param type Character vector to identify if the model was using subgroups or
#'   interaction terms. The difference is that confidence intervals for
#'   interaction can be calculated in multiple ways, and the effect size is
#'   dependent on the presence or absence of the interaction variable.
#'
#'   * __interaction__ = assumes an interaction term that is binary/dichotomous
#'
#'   * __subgroup__ = assumes a categorical variable that was used to group the
#'   original dataset
#'
#' @param level List of formulas. Each list-element is a formula with the LHS
#'   reflecting either the variable to re-label or a specific level, and the RHS
#'   reflecting what the new level should be called (for display). If there are
#'   conflicting labels, the most recent will be used.
#'
#'   For example, `list(am ~ c("Manual", "Automatic")` would take, from the
#'   `mtcars` dataset, the `am` variable, which consists of `c(0, 1)`, and
#'   relabel them in the order described. They are sorted in ascendiing order
#'   prior to re-labeling.
#'
#'   The alternative approach is to use the specific level itself and have it
#'   re-labeled. `list(0 ~ "Absent")` would take all levels that are zero, and
#'   change their value.
#'
#' @param columns Additional columns that help to describe the subgroup models.
#'   At least one column should be selected from this list. The sequence listed
#'   will reflect the sequence shown in the table. The current options are:
#'
#'   * beta = point estimate value, such as odds ratio or hazard ratio
#'
#'   * conf = inclusion of the confidence interval (presumed to be 95%-ile)
#'
#'   * n = number of observations in each model group
#'
#'   * p = p.value for model or interaction term
#'
#'   For example: `list(beta ~ "Hazard", conf ~ "95% CI" n ~ "No.")"`
#'
#' @param flip Determine if the odds or hazard ratio should be shown as the
#'   reciprocal values. Instead of a decreasing hazard for every unit increase,
#'   it describes an increasing hazard for every unit decrease.
#'
#' @param axis Argument to help modify the forest plot itself. This is a
#'   list-formula of the following parameters. If they are not named, the
#'   function will attempt to "guess" the optimal parameters. The options are:
#'
#'   * title = label or title for the column describing the forest plot
#'
#'   * lim = x-axis limits
#'
#'   * breaks = x-axis tick marks or break points that should be numbers
#'
#'   * int = x-axis intercept
#'
#'   * lab = label for the x-axis
#'
#'   * scale = defaults to continuous, but may also use a log transformation as
#'   well `c("continuous", "log")`
#'
#'   For example: `list(title ~ "Decreasing Hazard", lab ~ "HR (95% CI))`
#'
#' @param width Describes the width of each column in a list of two-sided
#'   formulas. The RHS is a decimal reflecting the percent each column should
#'   take of the entire table. The forest plot is usually given 30% of the
#'   width.
#'
#' For example: `list(n ~ .1, forest ~ 0.3)`
#'
#' @import ggplot2
#' @import gt
#' @export
tbl_group_forests <- function(object,
															formula,
															type,
															vars,
															level = list(),
															columns = list(beta ~ "Estimate",
																						 conf ~ "95% CI",
																						 n ~ "No."),
															flip = FALSE,
															axis = list(scale ~ "continuous"),
															width = list()) {


	# TODO revise how this function works for forge objects versus standard data tables
	# TODO add ability to extract or filter by formulas from forge objects
	# TODO how to decide which LEVEL or number to pick in terms of adjusted models

	validate_class(object, "forge")

	# Validate formula
	if (!inherits(formula, "formula")) {
		stop("Object should be of type `formula`, not `", class(formula)[1], "`")
	}
	y <- lhs(formula)
	x <- rhs(formula)

	# Validate type and if strata variables are appropriate
	if (!type %in% c("interaction", "subgroup")) {
		stop("A single string for type of 'interaction' or 'subgroup' must be selected.")
	} else {
		vars_in_models <- vars %in% unlist(object[c("strata", "interaction")], use.names = FALSE)
		if (!any(vars_in_models)) {
			stop("The variables selected for ", type, " are not available in the model objects.")
		} else if (!all(vars_in_models)) {
			message("The variables `", paste0(vars[!vars_in_models], collapse = ", "), "` are not avaiilable in the model object.")
		}
	}

	# Get labels and/or levels
	lvl <- list()
	if (length(level) > 0) {
		lvl <- formula_to_named_list(level)
	}

	# Rename selecting columns (for both parameter estimates and model info)
	cols <- formula_to_named_list(columns)
	est_vars <- character()
	mod_vars <- character()
	if ("beta" %in% names(cols)) {
		est_vars <- append(est_vars, "estimate")
	}
	if ("conf" %in% names(cols)) {
		est_vars <- append(est_vars, c("conf.low", "conf.high"))
	}
	if ("p" %in% names(cols)) {
		est_vars <- append(est_vars, "p.value")
	}
	if ("n" %in% names(cols)) {
		mod_vars <- append(mod_vars, "nobs")
	}

	if (type == "interaction") {
		# Basic table by interaction
		tbl_present <-
			object |>
			dplyr::filter(interaction %in% vars) |>
			dplyr::filter(outcome == y & exposure == x) |>
			dplyr::mutate(joint_term = paste0(exposure, ":", interaction)) |>
			dplyr::rowwise() |>
			dplyr::mutate(p.value = parameter_estimates$p.value[parameter_estimates$term == joint_term]) |>
			dplyr::mutate(pars = list(interaction_estimates(model, exposure, interaction, present = TRUE)))

		tbl_absent <-
			object |>
			dplyr::filter(interaction %in% vars) |>
			dplyr::filter(outcome == y & exposure == x) |>
			dplyr::mutate(joint_term = paste0(exposure, ":", interaction)) |>
			dplyr::rowwise() |>
			dplyr::mutate(p.value = parameter_estimates$p.value[parameter_estimates$term == joint_term]) |>
			dplyr::mutate(pars = list(interaction_estimates(model, exposure, interaction, present = FALSE)))

		tbl <-
			dplyr::bind_rows(tbl_present, tbl_absent) |>
			dplyr::mutate(
				estimate = pars[["estimate"]],
				conf.low = pars[["conf.low"]],
				conf.high = pars[["conf.high"]],
				nobs = pars[["nobs"]],
				level = pars[["level"]]
			) |>
			dplyr::select(exposure, interaction, level, exposure, terms, all_of(est_vars), all_of(mod_vars)) |>
			dplyr::rename(strata = interaction,
										term = exposure) |>
			dplyr::group_by(strata, level) |>
			dplyr::arrange(strata)

		# Relabel if needed
		if (length(lvl) > 0) {
			for (i in seq_along(lvl)) {
				if (names(lvl)[i] %in% tbl$strata) {
					tbl$level[tbl$strata == names(lvl[i])] <- lvl[[i]]
				} else if (names(lvl)[i] %in% tbl$level) {
					tbl$level[tbl$level == names(lvl)[i]] <- lvl[[i]]
				}
			}
		}
	} else {
		# Basic table by strata
		tbl <-
			object |>
			temper() |>
			dplyr::filter(strata %in% vars) |>
			dplyr::filter(outcome == y & exposure == x) |>
			dplyr::group_by(strata, level) |>
			dplyr::filter(number == max(number)) |>
			dplyr::filter(term == x) |>
			dplyr::ungroup() |>
			dplyr::select(strata, level, term, terms,
										all_of(est_vars), all_of(mod_vars))

	}


	# Reciprocal odds or hazard if needed
	if (flip) {
		tbl <-
			dplyr::mutate(tbl, across(
				c(all_of(est_vars), -p.value),
				~ 1 / .x
			))

		if ("conf.low" %in% est_vars) {
			tbl <-
				dplyr::rename(tbl, conf.high = conf.low, conf.low = conf.high)
		}
	}

	# Setup plotting variables from the axis argument
	x_vars <- formula_to_named_list(axis)

	if ("lim" %in% names(x_vars)) {
		lim_val <- eval(x_vars$lim)
		xmin <- min(lim_val)
		xmax <- max(lim_val)
	} else {
		xmin <- min(tbl$conf.low, na.rm = TRUE)
		xmax <- min(tbl$conf.high, na.rm = TRUE)
	}

	if ("int" %in% names(x_vars)) {
		xint <- eval(x_vars$int)
	} else {
		xint <- dplyr::case_when(
			xmin < -1 & xmax <= 0 ~ -1,
			xmin > -1 & xmax <= 0 ~ 0,
			xmin < 0 & xmax > 0 ~ 0,
			xmin >= 0 & xmax <= 1 ~ 0,
			xmin >= 0 & xmax > 1 ~ 1
		)
	}

	if ("breaks" %in% names(x_vars)) {
		breaks <- eval(x_vars$breaks)
	} else {
		breaks <- ggplot2::waiver()
	}

	if ("lab" %in% names(x_vars)) {
		lab <- x_vars$lab
	} else {
		lab <- NULL
	}

	if ("scale" %in% names(x_vars)) {
		scale <- x_vars$scale
	} else {
		scale <- "continuous"
	}

	# Make appropriate plots
	plots <-
		tbl |>
		dplyr::group_by(strata, level) |>
		tidyr::nest() |>
		dplyr::mutate(gg = purrr::map(data, ~ {
			ggplot(.x, aes(x = estimate, y = 0)) +
				geom_point(size = 50) +
				geom_linerange(aes(xmax = conf.high, xmin = conf.low, size = 5)) +
				geom_vline(xintercept = xint, linetype = 3, linewidth = 5) +
				theme_minimal() +
				theme(
					axis.text.y = element_blank(),
					axis.title.y = element_blank(),
					axis.text.x = element_blank(),
					axis.title.x = element_blank(),
					axis.line.x = element_blank(),
					legend.position = "none",
					panel.grid.major = element_blank(),
					panel.grid.minor = element_blank()
				) +
				{
					if(scale == "log") {
						scale_x_continuous(
							trans = scales::pseudo_log_trans(sigma = 0.1, base = exp(1)),
							breaks = breaks,
							limits = c(xmin, xmax),
							oob = scales::oob_squish
						)
					} else {
						scale_x_continuous(
							breaks = breaks,
							limits = c(xmin, xmax),
							oob = scales::oob_squish
						)
					}
				}
		})) |>
		tidyr::unnest(data) |>
		dplyr::ungroup() |>
		dplyr::add_row()

	tmp <- plots$gg[[1]]
	tmp$layers[1:2] <- NULL
	btm_axis <-
		tmp +
		xlab(lab) +
		theme(
			axis.text.x = element_text(size = 100, margin = margin(10, 0 , 0 , 0)),
			axis.ticks.x = element_line(linewidth = 5),
			axis.ticks.length.x = unit(30, "pt"),
			axis.title.x = element_text(size = 150, margin = margin(10, 0, 0 , 0)),
			axis.line.x = element_line(
				linewidth = 5,
				arrow = grid::arrow(
					length = grid::unit(50, "pt"),
					ends = "both",
					type = "closed"
				)
			)
		)

	plots$gg[nrow(plots)] <- list(btm_axis)


	# To use to help filter for which variables to modify in grouped rows
	lowest_lvls <-
		subset(tbl, select = c(strata, level)) |>
		dplyr::group_by(strata) |>
		dplyr::slice_tail() |>
		dplyr::pull(level) |>
		unique()

	# Create table
	tbl |>
		dplyr::rowwise() |>
		dplyr::mutate(strata = dplyr::case_when(
			strata %in% names(labels(terms)) ~ vec_data(get_runes(terms, field = "runes", value = strata))$label,
			!(strata %in% names(labels(terms))) ~ strata
		)) |>
		dplyr::ungroup() |>
		dplyr::mutate(ggplots = NA) |>
		dplyr::add_row() |>
		dplyr::select(level, strata, all_of(mod_vars), all_of(est_vars), ggplots) |>
		gt(rowname_col = "level", groupname_col = "strata") |>
		# Estimates and confidence intervals
		{\(.) {
			if (all(c("estimate", "conf.low", "conf.high") %in% est_vars)) {
				. |>
					cols_merge(columns = est_vars[1:3],
												 pattern = "{1} ({2}, {3})") |>
					cols_width(estimate ~ pct(40)) |>
					cols_label(estimate = cols$beta)
			} else {
				.
			}
		}}() |>
		# Number of observations
		{\(.) {
			if (all(c("nobs") %in% mod_vars)) {
				. |>
					cols_width(nobs ~ pct(10)) |>
					cols_label(nobs = cols$n)
			} else {
				.
			}
		}}() |>
		# P.value is included for interactions
		{\(.) {
			if (all(c("p.value") %in% est_vars & isTRUE(type == "interaction"))) {
				. |>
					cols_move_to_end(p.value) |>
					tab_style(
						style = cell_text(color = "white", size = px(0)),
						locations = cells_body(columns = p.value,
																	 rows = level %in% lowest_lvls)
					) |>
					tab_style(
						style = cell_text(v_align = "bottom"),
						locations = cells_body(columns = p.value,
																	 rows = level > min(level, na.rm = TRUE))
					) |>
					tab_style(
						style = cell_text(weight = "bold"),
						locations = cells_body(columns = p.value,
																	 rows = p.value < 0.05)
					) |>
					cols_label(p.value = cols$p)
			} else {
				.
			}
		}}() |>
		# P value included for general groups
		{\(.) {
			if (all(c("p.value") %in% est_vars)) {
				. |>
					cols_move_to_end(p.value) |>
					tab_style(
						style = cell_text(weight = "bold"),
						locations = cells_body(columns = p.value,
																	 rows = p.value < 0.05)
					) |>
					cols_label(p.value = cols$p)
			} else {
				.
			}
		}}() |>
		tab_style(
			style = list(
				cell_borders(sides = "all", color = NULL)
			),
			locations = list(
				cells_body(columns = c(all_of(mod_vars), all_of(est_vars))),
				cells_stub(rows = everything())
			)
		) |>
		fmt_number(
			columns = where(is.numeric),
			drop_trailing_zeros = TRUE,
			n_sigfig = 2
		) |>
		cols_width(ggplots ~ pct(50)) |>
		opt_vertical_padding(scale = 0) |>
		opt_table_outline(style = "none") |>
		tab_options(
			data_row.padding = px(0),
			table_body.border.bottom.width = px(0),
			table_body.border.top.width = px(0),
			column_labels.border.top.width = px(0)
		) |>
		tab_style(
			style = list(
				cell_text(color = "white", size = px(0)),
				cell_borders(sides = "all", color = NULL)
			),
			locations = list(
				cells_body(columns = ggplots),
				cells_row_groups(groups = "NA"),
				cells_stub(rows = is.na(level))
			)
		) |>
		tab_style(
			style = list(
				cell_text(color = "white", size = px(0))
			),
			locations = list(
				cells_body(columns = c(all_of(mod_vars), all_of(est_vars)),
											 rows = is.na(level))
			)
		) |>
		cols_label(
			ggplots = x_vars$title,
		) |>
		text_transform(
			locations = cells_body(columns = ggplots),
			fn = function(x) {
				purrr::map(plots$gg,
									 ggplot_image,
									 height = px(50),
									 aspect_ratio = 5)
			}
		)

}


#' Compact and minimal theme for `gt` tables
#'
#' This theme was used for placing somewhat larger tables into `xaringan` slides
#' by making the spacing more compact and decreasing the font size. The exposed
#' variables are to control font size and table width, but any option from the
#' `gt` package is allowed.
#'
#' @inheritParams gt::tab_options
#' @param ... For passing additional arguments to the [tab_options()]
#'   function
#' @family visualizers
#' @importFrom gt tab_options px pct
#' @export
theme_gt_compact <- function(data,
														 table.font.size = pct(80),
														 table.width = pct(90),
														 ...) {

	validate_class(data, "gt_tbl")

	data |>
		tab_options(
			# Preset
			table.margin.left = px(1),
			table.margin.right = px(1),
			row_group.padding = px(1),
			data_row.padding = px(1),
			footnotes.padding = px(1),
			source_notes.padding = px(1),
			stub.border.width = px(1),
			# User supplied
			table.width = table.width,
			table.font.size = table.font.size,
			...
		)

}
