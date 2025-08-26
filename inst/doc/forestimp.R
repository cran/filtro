## -----------------------------------------------------------------------------
#| include: false
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## -----------------------------------------------------------------------------
#| label: start
#| include: false

library(filtro)
library(dplyr)
library(modeldata)


## -----------------------------------------------------------------------------
#| label: setup
library(filtro)
library(dplyr)
library(modeldata)


## -----------------------------------------------------------------------------
#| eval: false
# score_imp_rf
# score_imp_rf_conditional
# score_imp_rf_oblique


## -----------------------------------------------------------------------------
#| echo: false
score_imp_rf@engine
score_imp_rf_conditional@engine
score_imp_rf_oblique@engine


## -----------------------------------------------------------------------------
cells_subset <- modeldata::cells |> 
  # Use a small example for efficiency
  dplyr::slice(1:50)
cells_subset$case <- NULL

# cells_subset |> str() # Uncomment to see the structure of the data


## -----------------------------------------------------------------------------
# Specify random forest and fit score
cells_imp_rf_res <- score_imp_rf |>
  fit(
    class ~ .,
    data = cells_subset, 
    seed = 42 
  )


## -----------------------------------------------------------------------------
cells_imp_rf_res@results


## -----------------------------------------------------------------------------
#| eval: false
# # Set hyperparameters
# cells_imp_rf_res <- score_imp_rf |>
#   fit(
#     class ~ .,
#     data = cells_subset,
#     trees = 100,
#     mtry = 2,
#     min_n = 1
#   )


## -----------------------------------------------------------------------------
#| eval: false
# cells_imp_rf_res <- score_imp_rf |>
#   fit(
#     class ~ .,
#     data = cells_subset,
#     trees = 100,
#     mtry = 2,
#     min_n = 1,
#     seed = 42 # Set seed for reproducibility
#   )


## -----------------------------------------------------------------------------
#| eval: false
# cells_imp_rf_res <- score_imp_rf |>
#   fit(
#     class ~ .,
#     data = cells_subset,
#     num.trees = 100,
#     mtry = 2,
#     min.node.size = 1,
#     seed = 42
#   )


## -----------------------------------------------------------------------------
# Set seed for reproducibility
set.seed(42)

# Specify conditional random forest and fit score
cells_imp_rf_conditional_res <- score_imp_rf_conditional |>
  fit(class ~ ., data = cells_subset, trees = 100)
cells_imp_rf_conditional_res@results


## -----------------------------------------------------------------------------
# Set seed for reproducibility
set.seed(42)

# Specify oblique random forest and fit score
cells_imp_rf_oblique_res <- score_imp_rf_oblique |>
  fit(class ~ ., data = cells_subset, trees = 100, mtry = 2)
cells_imp_rf_oblique_res@results


## -----------------------------------------------------------------------------
#| echo: false
#| message: false
knitr::kable(
  data.frame(
    "object" = c("`score_imp_rf`", "`score_imp_rf_conditional`", "`score_imp_rf_oblique`"),
    "engine" = c("`ranger::ranger`", "`partykit::cforest`", "`aorsf::orsf`"),
    "task" = rep(c("regression, classification"), 3)
  )
)


