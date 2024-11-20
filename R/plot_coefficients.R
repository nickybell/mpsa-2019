#' Extract coefficients from linear model or generalized linear model
#'
#' @param model A object of class `lm` or `glm`, usually a result of a call to [lm()] or [glm()].
#'
#' @return A data frame containing the results of [summary.lm()] or [summary.glm()].
extract_coefficients <- function(model) {
  out <- as.data.frame(stats::coef(summary(model)))
  out
}

#' Generate confidence intervals from a set of estimates and standard errors
#'
#' @param .data A data frame.
#' @param estimates <[`data-masking`][rlang::args_data_masking]> The variable containing the estimates.
#' @param std_errors <[`data-masking`][rlang::args_data_masking]> The variable containing the standard errors.
#' @param alpha The desired confidence level. Defaults to .05 (95% confidence intervals).
#'
#' @return The original data frame with additional columns `ci_lower` and `ci_upper`.
#' @export
generate_confidence_intervals <- function(.data, estimates, std_errors, alpha = .05) {
  cv <- stats::qnorm(alpha/2, mean = 0, sd = 1, lower.tail = FALSE) # critical value for alpha
  out <-
    .data |>
    dplyr::mutate(ci_lower = {{estimates}} - cv * {{std_errors}},
                  ci_upper = {{estimates}} + cv * {{std_errors}})
  out
}

#' Plot coefficients from linear model or generalized linear model
#'
#' @inheritParams extract_coefficients
#' @inheritParams generate_confidence_intervals
#' @inheritParams dplyr::case_match
#' @param rename_covariates Optional. A named list of covariates to rename, e.g., `list(old_value ~ "new_value")`. Unnamed covariates will be removed from the plot.
#' @param covariate_order Optional. A vector of character strings indicating the order (top to bottom) of the covariates (or renamed covariates, if applicable) in the plot. Unordered covariates will be placed in their existing order after ordered covariates.
#' @param dv_name Optional. A character string indicating the name of the dependent variable for use in the plot title.
#' @param odds_ratio Optional. If the link function is logit, setting `odds_ratio` to TRUE will plot the odds ratios. Default is FALSE.
#' @param transparency The transparency ([scales::alpha]) to apply to non-significant coefficients.
#'
#' @return A ggplot object.
#' @export
plot_coefficients <- function(model, rename_covariates = NULL, covariate_order = NULL, dv_name = NULL, odds_ratio = FALSE, alpha = .05, transparency = 0.3) {
  if ("family" %in% names(model) & odds_ratio == TRUE) {
    if(model$family$link == "logit") {
      or <- TRUE
    } else {
      or <- FALSE
    }
  } else {
    or <- FALSE
  }
  
  if(is.character(dv_name)) {
    dv <- dv_name
  } else {
    dv <- all.vars(model$call)[1]
  }
  
  plot_df <-
    extract_coefficients(model) |>
    generate_confidence_intervals(Estimate, `Std. Error`, alpha) |>
    tibble::rownames_to_column(var = "var") |>
    dplyr::mutate(sign = factor(dplyr::case_when(ci_lower > 0 ~ "+",
                                                 ci_upper < 0 ~ "-",
                                                 TRUE ~ "0"),
                                levels = c("+", "-", "0"))) |>
    dplyr::filter(var != "(Intercept)")
  
  if (or) {
    plot_df <-
      plot_df |>
      dplyr::mutate(dplyr::across(tidyselect::all_of(c("Estimate", "ci_lower", "ci_upper")), exp))
  }
  
  if (length(rename_covariates) > 0) {
    plot_df <-
      plot_df |>
      dplyr::rowwise() |>
      dplyr::mutate(var = ifelse(is.null(rename_covariates[[match(var, names(rename_covariates))]]), NA_character_, rename_covariates[[match(var, names(rename_covariates))]])) |>
      dplyr::ungroup() |>
      dplyr::filter(!is.na(var))
  }
  
  if (is.character(covariate_order)) {
    plot_df <-
      plot_df |>
      dplyr::mutate(var = forcats::fct_rev(forcats::fct_relevel(factor(var, levels = unique(var)), covariate_order)))
  }
  
  position <- ggplot2::position_dodge(.9)
  # Setting the limits while maintaining symmetry
  if (or) {
    furthest_low <- max(abs(1-min(plot_df$ci_lower)))
    furthest_high <- max(abs(1-max(plot_df$ci_upper)))
    limits <- c(1-max(furthest_low, furthest_high), 1+max(furthest_low, furthest_high))
  } else {
    furthest_low <- max(abs(min(plot_df$ci_lower)))
    furthest_high <- max(abs(max(plot_df$ci_upper)))
    limits <- c(-max(furthest_low, furthest_high), max(furthest_low, furthest_high))
  }
  
  plot <-
    ggplot2::ggplot(plot_df, ggplot2::aes(x = Estimate, y = var, alpha = sign, color = sign)) +
    ggplot2::geom_point(size = 2, position = position) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci_lower, xmax = ci_upper), height = .2, position = position) +
    ggplot2::geom_vline(xintercept = ifelse(or, 1, 0), linetype = "dashed") +
    ggplot2::scale_x_continuous(limits = limits) +
    ggplot2::scale_alpha_manual(values = c("+" = 1, "-" = 1, "0" = transparency)) +
    ggplot2::scale_color_manual(values = c("+" = "blue", "-" = "red", "0" = "black")) +
    ggplot2::labs(x = ifelse(or, "Odds Ratio", "Estimate"),
                  caption = paste0("Note: ", round((1-alpha)*100), "% confidence intervals."),
                  title = paste0("Regression of ", dv, " on Covariates")) +
    ggplot2::theme_minimal() +
    ggplot2::theme( axis.title.y = ggplot2::element_blank(),
                   legend.position = "none",
                   plot.title = ggplot2::element_text(hjust = .5),
                   plot.background = ggplot2::element_rect(fill = "white", color = "white"))
  
  plot
}