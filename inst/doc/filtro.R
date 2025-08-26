## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## -----------------------------------------------------------------------------
#| label: start
#| include: false
library(filtro)
library(desirability2)
library(dplyr)
library(modeldata)


## -----------------------------------------------------------------------------
#| label: setup
library(filtro)
library(desirability2)
library(dplyr)
library(modeldata)


## -----------------------------------------------------------------------------
ames <- modeldata::ames
ames <- ames |>
  dplyr::mutate(Sale_Price = log10(Sale_Price))

# ames |> str() # uncomment to see the structure of the data


## -----------------------------------------------------------------------------
ames_aov_pval_res <-
  score_aov_pval |>
  fit(Sale_Price ~ ., data = ames)


## -----------------------------------------------------------------------------
ames_aov_pval_res@results


## -----------------------------------------------------------------------------
# Show best score, based on proportion of predictors
ames_aov_pval_res |> show_best_score_prop(prop_terms = 0.2)


## -----------------------------------------------------------------------------
# ANOVA raw p-value 
natrual_units <- score_aov_pval |> dont_log_pvalues()
ames_aov_pval_natrual_res <-
  natrual_units |>
  fit(Sale_Price ~ ., data = ames)

# Pearson correlation
ames_cor_pearson_res <-
  score_cor_pearson |>
  fit(Sale_Price ~ ., data = ames)

# Forest importance
ames_imp_rf_reg_res <-
  score_imp_rf |>
  fit(Sale_Price ~ ., data = ames, seed = 42)

# Information gain
ames_info_gain_reg_res <-
  score_info_gain |>
  fit(Sale_Price ~ ., data = ames)


## -----------------------------------------------------------------------------
# Create a list
class_score_list <- list(
  ames_aov_pval_natrual_res, 
  ames_cor_pearson_res,
  ames_imp_rf_reg_res,
  ames_info_gain_reg_res
)


## -----------------------------------------------------------------------------
# Fill safe values
ames_scores_results <- class_score_list |>
  fill_safe_values() |>
  # Remove outcome
  dplyr::select(-outcome)
ames_scores_results


## -----------------------------------------------------------------------------
# Optimize correlation alone
ames_scores_results |>
  show_best_desirability_prop(
    maximize(cor_pearson, low = 0, high = 1)
  ) |> 
  # Show predictor and desirability only
  dplyr::select(predictor, starts_with(".d_"))

# Optimize correlation and forest importance
ames_scores_results |>
  show_best_desirability_prop(
    maximize(cor_pearson, low = 0, high = 1),
    maximize(imp_rf)
  ) |> 
  dplyr::select(predictor, starts_with(".d_"))

# Optimize correlation, forest importance and information gain
ames_scores_results |>
  show_best_desirability_prop(
    maximize(cor_pearson, low = 0, high = 1),
    maximize(imp_rf),
    maximize(infogain)
  ) |> 
  dplyr::select(predictor, starts_with(".d_"))


## -----------------------------------------------------------------------------
# Same as above, but retain only a proportion of predictors
ames_scores_results |>
  show_best_desirability_prop(
    maximize(cor_pearson, low = 0, high = 1),
    maximize(imp_rf),
    maximize(infogain),
    prop_terms = 0.2
  ) |>
  dplyr::select(predictor, starts_with(".d_"))


## -----------------------------------------------------------------------------
ames_scores_results |>
  show_best_desirability_prop(
    minimize(aov_pval, low = 0, high = 1)
  ) |> 
  dplyr::select(predictor, starts_with(".d_"))

ames_scores_results |>
  show_best_desirability_prop(
    target(cor_pearson, low = 0.2, target = 0.255, high = 0.9)
  ) |> 
  dplyr::select(predictor, starts_with(".d_"))

ames_scores_results |>
  show_best_desirability_prop(
    constrain(cor_pearson, low = 0.2, high = 1)
  ) |> 
  dplyr::select(predictor, starts_with(".d_"))


## -----------------------------------------------------------------------------
#| echo: false
grep("^score_", ls("package:filtro"), value = TRUE)


## -----------------------------------------------------------------------------
#| echo: false
grep("^show_best_score_", ls("package:filtro"), value = TRUE)


## -----------------------------------------------------------------------------
#| echo: false
grep("^show_best_desirability_", ls("package:filtro"), value = TRUE)

