## code to prepare `templates` dataset goes here
template_models <- c("model_fit",
										 "lm",
										 "glm",
										 "coxph")

template_patterns <- c("fundamental",
											 "direct",
											 "sequential",
											 "parallel")

template_shortcuts <- c(".o",
												".x",
												".c",
												".m",
												".s",
												".i")

template_roles <- c("outcome",
										"exposure",
										"confounder",
										"mediator",
										"strata",
										"interaction")

template_operations <- c("log")

usethis::use_data(template_models,
									template_patterns,
									template_shortcuts,
									template_roles,
									template_operations,
									internal = TRUE,
									overwrite = TRUE)


usethis::use_data(templates, overwrite = TRUE)
