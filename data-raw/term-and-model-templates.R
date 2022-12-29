## code to prepare `templates` dataset goes here
.models <- c("model_fit",
						 "lm",
						 "glm",
						 "coxph")

.patterns <- c("fundamental",
							 "direct",
							 "sequential",
							 "parallel")

.shortcuts <- c(".o",
								".x",
								".p",
								".c",
								".m",
								".s",
								".i")

.roles <- c("outcome",
						"exposure",
						"predictor",
						"confounder",
						"mediator",
						"strata",
						"interaction")

usethis::use_data(.models,
									.patterns,
									.shortcuts,
									.roles,
									internal = TRUE,
									overwrite = TRUE)
