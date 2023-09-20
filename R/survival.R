#' Make complex regression table
#' @examples
#' object <- targets::tar_read(cox_mdls, store = '../mims/_targets') |>
#' dplyr::filter(name == 'parsimonious')
# data <- targets::tar_read(clinical, store = '../mims/_targets') |>
# mutate(lf_delta_bin = factor(lf_delta_bin, levels = c(1, 0), labels = c('Yes', 'No'))) |> mutate(lf_rest_quartile = factor(lf_rest_quartile, levels = c(1, 0), labels = c('Yes', 'No')))
#'
#' events <- list('death_cv_yn' ~ 'Cardiovascular mortality', 'death_any_yn' ~
#' 'All-cause mortality')
#'
#' followup <- 'death_timeto'
#'
#' terms <- list(lf_delta_bin ~ '')
#'
#' adjustment <- list(2 ~ 'Rate per 100 person-years', 5 ~ 'Adjusted for demo',
#' 7 ~ 'Adjust for above + clinical', 8 ~ 'Adjust for above + stress testing')

#' @export
tbl_categorical_hazard <- function(object,
																	 data,
																	 events = formula(),
																	 followup = character(),
																	 terms = formula(),
																	 adjustment = formula(),
																	 ...) {

	# Ensure only one model family is present
	if (length(unique(object$name)) > 1) {
		stop('Cannot combine models from different datasets or regressions into a table safely.')
	}

	# Get relevant filtering variables
	## Outcomes
	out <- volundr::formulas_to_named_list(events)
	out_nms <- names(out)
	out_lab <- unlist(unname(out))
	## Terms
	tms <- volundr::formulas_to_named_list(terms)
	tms_nms <- names(tms)
	tms_lab <- unlist(unname(tms))
	## Adjustment
	lvl <- volundr::formulas_to_named_list(adjustment)
	lvl_nms <- names(lvl)
	lvl_lab <- unlist(unname(lvl))

	# Create subset of model_table
	obj <-
		object |>
		dplyr::filter(grepl(paste0(out_nms, collapse = '|'), outcome)) |>
		volundr::flatten_table() |>
		dplyr::filter(grepl(paste0(tms_nms, collapse = '|'), term)) |>
		dplyr::select(number, outcome, term, estimate, conf_low, conf_high, p_value)

	# Incidence rates
	dat <- data[c(out_nms, followup, tms_nms)]

	# Number of tables to make depends on...
	#		Number of outcomes
	# 	Number of terms

	# Get tables ready to be stored

	tbl_tms <- list()
	tbl_out <- list()

	for (t in tms_nms) {
		for (o in out_nms) {

			# Adjusted hazards first
			adj <-
				obj |>
				dplyr::filter(grepl(t, term)) |>
				dplyr::filter(grepl(o, outcome)) |>
				tidyr::pivot_wider(
					names_from = term,
					values_from = c(estimate, conf_low, conf_high, p_value),
					names_glue = '{term}_{.value}'
				) |>
				dplyr::filter(number %in% as.numeric(lvl_nms)) |>
				dplyr::mutate(outcome = o) |>
				tibble::add_row(number = 0, outcome = o, .before = 1)

			# Person years into table
			py <-
				survival::pyears(survival::Surv(dat[[followup]], dat[[o]]) ~ dat[[t]])

			py$pyears <- py$pyears / 100

			rates <-
				rbind(n = py$n, events = py$event, risk = py$event / py$pyears) |>
				as.data.frame() |>
				rownames_to_column(var = 'label')

			# Save number per group for later in the table
			n <- rates[1, -1]

			# Make compatible with binding
			unadj <-
				rates[-1, ] |>
				dplyr::mutate(label = ifelse(label == 'risk', lvl_nms[1], label)) |>
				tibble::add_row(label = as.character(adj$number[-c(1:2)]))

			# Bind the tables together
			# Order of columns is that reference is on left
			# Each level is then with incidence rate and to the right is hazard

			lvls <- levels(dat[[t]]) # Reference is first
			vars <- unique(grep(t, obj$term, value = TRUE))

			stopifnot(
				'Levels of variables are not consistent within the specificed term'
				= length(lvls)  == length(vars) + 1
			)

			# Basic table merge
			tbl <-
				bind_cols(unadj, adj) |>
				dplyr::select(-number)

			# Organize columns
			for (i in seq_along(vars)) {
				tbl <-
					tbl |>
					dplyr::relocate(contains(vars[i]), .after = lvls[i + 1])
			}

			# Rename columns
			names(tbl)[names(tbl) %in% names(n)] <-
				paste0(names(n), ' (n = ', n, ')')

			# We are rotating through each outcome for a single term
			tbl_out[[o]] <- tbl

		}

		# Now the outcomes can be bound together
		# Don't want to duplicate the outcome and label lines so will drop if dups
		if (length(tbl_tms) == 0) {
			tbl_tms[[t]] <- dplyr::bind_rows(tbl_out)
		} else {
			tbl_tms[[t]] <- dplyr::bind_rows(tbl_out) |>
				dplyr::select(-label, -outcome)
		}

	}

	# Now the terms are bound together and clean up
	tab <-
		dplyr::bind_cols(tbl_tms) |>
		dplyr::mutate(label = as.character(factor(
			label,
			levels = c('events', lvl_nms),
			labels = c('Total No. of events', lvl_lab)
		))) |>
		dplyr::mutate(outcome = factor(outcome, levels = out_nms, labels = out_lab))

	# Convert into gt table
	gtbl <-
		gt(tab, rowname_col = 'label', groupname_col = 'outcome') |>
		cols_hide(columns = contains('p_value')) |>
		sub_missing(missing_text = '') |>
		fmt_number(drop_trailing_zeros = TRUE)


	# Merge columns programmatically
	for (i in vars) {
		gtbl <-
			gtbl |>
			cols_merge(columns = starts_with(i),
								 pattern = '{1} ({2}, {3})',
								 rows = contains(lvl_lab))
	}

	# Rename hazard columns
	col_lab <- rep('HR (95% CI)', length(vars))
	names(col_lab) <- paste0(vars, '_estimate')
	gtbl <- gtbl |> cols_label(.list = col_lab)


	# Visual modifications
	# 	Indent adjustment lines
	# 	Align headings
	gtbl <-
		gtbl |>
		tab_stub_indent(rows = contains(lvl_lab), indent = 3) |>
		opt_align_table_header('left') |>
		tab_style(
			style = cell_text(align = 'left'),
			locations = cells_column_spanners()
		) |>
		tab_style(
			style = cell_text(align = 'left'),
			locations = cells_column_labels()
		) |>
		tab_style(
			style = cell_text(align = 'left'),
			locations = cells_body()
		)

	# Return gt table
	gtbl
}


