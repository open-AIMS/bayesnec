#' @param brms_fit A \code{\link[brms]{brmsfit}} object.
#' @param bayesnecformula A \code{\link{bayesnecformula}} formula object.
#'
#' @return A \code{\link[base]{data.frame}}.
#'
#' @importFrom dplyr %>% mutate select
#' @importFrom rlang .data
#' @importFrom stats model.frame
#'
#' @noRd
prep_raw_data <- function(brms_fit, bayesnecformula) {
  r_df <- brms_fit$data
  mod_dat <- model.frame(bayesnecformula, data = r_df)
  y_var <- attr(mod_dat, "bnec_pop")[["y_var"]]
  x_var <- attr(mod_dat, "bnec_pop")[["x_var"]]
  family <- brms_fit$family
  custom_name <- check_custom_name(family)
  if (family$family == "binomial" | custom_name == "beta_binomial2") {
    trials_var <- attr(mod_dat, "bnec_pop")[["trials_var"]]
    r_df[[y_var]] <- r_df[[y_var]] / r_df[[trials_var]]
  } else {
    r_df[[y_var]] <- r_df[[y_var]]
  }
  r_df %>%
    mutate(x_e = NA, y_e = NA, y_ci = NA, x_r = .data[[x_var]],
           y_r = .data[[y_var]]) %>%
    select(.data$x_e, .data$y_e, .data$y_ci, .data$x_r, .data$y_r)
}

#' @param data A \code{\link[base]{data.frame}}.
#' @param nec_vals A \code{\link[base]{numeric}} vector containing the mean,
#' and 95% credible intervals of NEC values.
#' @param xform A function to apply to the returned estimated concentration
#' values.
#'
#' @return A \code{\link[base]{data.frame}}.
#'
#' @noRd
bind_nec <- function(data, nec_vals, xform = NA) {
  data$nec_vals <- NA
  data$nec_labs <- NA
  data$nec_labs_l <- NA
  data$nec_labs_u <- NA
  df <- data[1:3, ]
  df[ ] <- NA
  if (inherits(xform, "function")) {
    nec_vals <- xform(nec_vals)
  }
  df$nec_vals <- nec_vals
  df$nec_labs[1] <- rounded(nec_vals[[1]], 2)
  df$nec_labs_l[1] <- rounded(nec_vals[[2]], 2)
  df$nec_labs_u[1] <- rounded(nec_vals[[3]], 2)
  rbind(data, df)
}

#' @param data A \code{\link[base]{data.frame}}.
#' @param ecx_vals A \code{\link[base]{numeric}} vector containing the mean,
#' and 95% credible intervals of ECx values.
#'
#' @return A \code{\link[base]{data.frame}}.
#'
#' @noRd
bind_ecx <- function(data, ecx_vals) {
  data$ecx_vals <- NA
  data$ecx_int <- NA
  data$ecx_labs <- NA
  data$ecx_labs_l <- NA
  data$ecx_labs_u <- NA
  df <- data[1:3, ]
  df[ ] <- NA
  df$ecx_vals <- ecx_vals
  df$ecx_int[1] <- strsplit(names(ecx_vals)[1], "_", fixed = TRUE)[[1]][2]
  df$ecx_labs[1] <- rounded(ecx_vals[[1]], 2)
  df$ecx_labs_l[1] <- rounded(ecx_vals[[2]], 2)
  df$ecx_labs_u[1] <- rounded(ecx_vals[[3]], 2)
  rbind(data, df)
}

#' Creates the data.frame for plotting with \code{\link{autoplot}}.
#'
#' @param x An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}, as returned by function \code{\link{bnec}}.
#' @param add_nec Should NEC values be added to the plot? Defaults to TRUE.
#' @param add_ecx Should ECx values be added to the plot? Defaults to FALSE.
#' @param force_x A \code{\link[base]{logical}} value indicating if the argument
#' \code{xform} should be forced on the predictor values. This is useful when
#' the user transforms the predictor beforehand
#' (e.g. when using a non-standard base function).
#' @param xform A function to apply to the returned estimated concentration
#' values.
#' @param ... Additional arguments to be passed to \code{\link{ecx}}. By
#' default, function \code{\link{ecx}} returns EC10.
#'
#' @return A \code{\link[base]{data.frame}}.
#'
#' @examples
#' \donttest{
#' library(bayesnec)
#' options(mc.cores = 2)
#' data(manec_example)
#'
#' ggbnec_data(manec_example)
#' ggbnec_data(manec_example, add_ecx = TRUE, ecx_val = 50)
#' }
#'
#' @export
ggbnec_data <- function(x, add_nec = TRUE, add_ecx = FALSE, force_x = FALSE,
                        xform = NA, ...) {
  UseMethod("ggbnec_data")
}

#' Creates the data.frame for plotting with \code{\link{autoplot}}.
#'
#' @inheritParams ggbnec_data
#'
#' @inherit ggbnec_data return examples
#'
#' @importFrom dplyr %>% mutate
#' @importFrom brms conditional_effects
#' @importFrom rlang .data
#'
#' @noRd
#'
#' @export
ggbnec_data.default <- function(x, add_nec = TRUE, add_ecx = FALSE,
                                force_x = FALSE, xform = NA, ...) {
  brms_fit <- x$fit
  plot_obj <- brms_fit %>%
    conditional_effects(method = "posterior_epred") %>%
    plot(plot = FALSE)
  e_df <- plot_obj[[1]]$data
  e_df <- data.frame(x_e = c(e_df$effect1__, rev(e_df$effect1__)),
                     y_e = c(e_df$estimate__, rep(NA, nrow(e_df))),
                     y_ci = c(e_df$lower__, rev(e_df$upper__)),
                     x_r = NA, y_r = NA)
  r_df <- prep_raw_data(brms_fit, x$bayesnecformula)
  out <- rbind(e_df, r_df)
  if (force_x) {
    if (!inherits(xform, "function")) {
      stop("You need to specify a function through `xform` when",
           " `force_x = TRUE`.")
    }
    out <- out %>%
      mutate(x_e = xform(.data$x_e), x_r = xform(.data$x_r))
  }
  if (add_nec) {
    out <- bind_nec(out, x$nec, xform = xform)
  }
  if (add_ecx) {
    ecx_vals <- ecx(x, xform = xform, ...)
    out <- bind_ecx(out, ecx_vals)
  }
  out
}

#' Creates the data.frame for plotting with \code{\link{autoplot}}.
#'
#' @inheritParams ggbnec_data
#'
#' @param x An object of class \code{\link{bayesnecfit}}, as returned by
#' function \code{\link{bnec}}.
#'
#' @inherit ggbnec_data return examples
#'
#' @noRd
#'
#' @export
ggbnec_data.bayesnecfit <- function(x, ...) {
  ggbnec_data.default(x, ...)
}

#' Creates the data.frame for plotting with \code{\link{autoplot}}.
#'
#' @inheritParams ggbnec_data
#'
#' @param x An object of class \code{\link{bayesmanecfit}}, as returned by
#' function \code{\link{bnec}}.
#'
#' @inherit ggbnec_data return examples
#'
#' @importFrom dplyr %>% mutate
#' @importFrom rlang .data
#'
#' @noRd
#'
#' @export
ggbnec_data.bayesmanecfit <- function(x, add_nec = TRUE, add_ecx = FALSE,
                                      force_x = FALSE, xform = NA, ...) {
  e_df <- x$w_pred_vals$data
  e_df <- data.frame(x_e = c(e_df$x, rev(e_df$x)),
                     y_e = c(e_df$Estimate, rep(NA, nrow(e_df))),
                     y_ci = c(e_df$Q2.5, rev(e_df$Q97.5)),
                     x_r = NA, y_r = NA)
  r_df <- prep_raw_data(x$mod_fits[[1]]$fit, x$mod_fits[[1]]$bayesnecformula)
  out <- rbind(e_df, r_df)
  if (force_x) {
    if (!inherits(xform, "function")) {
      stop("You need to specify a function through `xform` when",
           " `force_x = TRUE`.")
    }
    out <- out %>%
      mutate(x_e = xform(.data$x_e), x_r = xform(.data$x_r))
  }
  if (add_nec) {
    out <- bind_nec(out, x$w_nec, xform = xform)
  }
  if (add_ecx) {
    ecx_vals <- ecx(x, xform = xform, ...)
    out <- bind_ecx(out, ecx_vals)
  }
  out
}

#' ggbnec
#'
#' \code{\link[bayesnec:bayesnec-package]{bayesnec}} standard \pkg{ggplot2}
#' plotting method.
#'
#' @inheritParams autoplot.bayesnecfit
#'
#' @param x A \code{\link[base]{data.frame}} created by function
#' \code{\link{ggbnec_data}}.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @importFrom ggplot2 ggplot geom_polygon aes geom_line geom_point
#' @importFrom ggplot2 geom_vline geom_text theme_classic facet_wrap theme
#' @importFrom ggplot2 element_text element_blank element_rect labs
#' @importFrom dplyr %>% filter
#' @importFrom rlang .data
#'
#' @noRd
ggbnec <- function(x, nec = TRUE, ecx = FALSE) {
  out <- ggplot() +
    geom_polygon(data = x %>% filter(!is.na(.data$y_ci)),
                 mapping = aes(x = .data$x_e, y = .data$y_ci),
                 fill = "grey75", alpha = 0.5) +
    geom_line(data = x %>% filter(!is.na(.data$y_e)),
              mapping = aes(x = .data$x_e, y = .data$y_e),
              colour = "black", linetype = 2) +
    geom_point(data = x %>% filter(!is.na(.data$y_r)),
               mapping = aes(x = .data$x_r, y = .data$y_r), fill = "grey30",
               shape = 21)
  if (nec) {
    ltys <- rep(c(1, 2, 2), length(unique(x$model)))
    lwds <- rep(c(0.5, 0.2, 0.2), length(unique(x$model)))
    out <- out +
      geom_vline(data = x %>% filter(!is.na(.data$nec_vals)),
                 mapping = aes(xintercept = .data$nec_vals),
                 linetype = ltys, colour = "grey50",
                 lwd = lwds) +
      geom_text(data = x %>% filter(!is.na(.data$nec_labs)),
                mapping = aes(label = paste0("NEC: ", .data$nec_labs, " (",
                                             .data$nec_labs_l, "-",
                                             .data$nec_labs_u, ")")),
                x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, size = 3,
                colour = "grey50")
  }
  if (ecx) {
    ltys <- rep(c(1, 2, 2), length(unique(x$model)))
    lwds <- rep(c(0.5, 0.2, 0.2), length(unique(x$model)))
    out <- out +
      geom_vline(data = x %>% filter(!is.na(.data$ecx_vals)),
                 mapping = aes(xintercept = .data$ecx_vals),
                 linetype = ltys, colour = "dodgerblue4",
                 lwd = lwds) +
      geom_text(data = x %>% filter(!is.na(.data$ecx_labs)),
                mapping = aes(label = paste0("EC[", .data$ecx_int, "]", ": ",
                                             .data$ecx_labs, " (",
                                             .data$ecx_labs_l, "-",
                                             .data$ecx_labs_u, ")")),
                x = Inf, y = Inf, hjust = 1.1, vjust = 5.5, size = 3,
                colour = "dodgerblue4")
  }
  out +
    theme_classic() +
    facet_wrap(~.data$model, scales = "free", ncol = 2) +
    theme(strip.text = element_text(hjust = 0),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(colour = NA, fill = NA)) +
    labs(x = "Predictor",
         y = "Response")
}

#' autoplot.bayesnecfit
#'
#' \code{\link[bayesnec:bayesnec-package]{bayesnec}} standard \pkg{ggplot2}
#' plotting method.
#'
#' @param object An object of class \code{\link{bayesnecfit}} as returned by
#' function \code{\link{bnec}}.
#' @param ... Additional arguments to be passed to \code{\link{ggbnec_data}}.
#' @param nec Should NEC values be added to the plot? Defaults to TRUE.
#' @param ecx Should ECx values be added to the plot? Defaults to FALSE.
#' @param force_x A \code{\link[base]{logical}} value indicating if the argument
#' \code{xform} should be forced on the predictor values. This is useful when
#' the user transforms the predictor beforehand
#' (e.g. when using a non-standard base function).
#' @param xform A function to apply to the returned estimated concentration
#' values.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @importFrom dplyr mutate
#' @family autoplot methods
#'
#' @examples
#' \donttest{
#' library(brms)
#' library(bayesnec)
#' options(mc.cores = 2)
#' data(nec_data)
#'
#' necs <- bnec(y ~ crf(x, c("nec3param", "nec4param")), data = nec_data,
#'              iter = 2e2, family = Beta(link = "identity"))
#' nec3param <- pull_out(necs, "nec3param")
#' autoplot(nec3param)
#' autoplot(nec3param, nec = FALSE)
#' autoplot(nec3param, ecx = TRUE, ecx_val = 50)
#'
#' # plot model averaged predictions
#' autoplot(necs)
#' # plot all panels together
#' autoplot(necs, ecx = TRUE, ecx_val = 50, all_models = TRUE)
#' # plots multiple models, one at a time, with interactive prompt
#' autoplot(necs, ecx = TRUE, ecx_val = 50, all_models = TRUE,
#'          multi_facet = FALSE)
#' }
#' @export
autoplot.bayesnecfit <- function(object, ..., nec = TRUE, ecx = FALSE,
                                 force_x = FALSE, xform = NA) {
  x <- object
  ggbnec_data(x, add_nec = nec, add_ecx = ecx, force_x = force_x,
              xform = xform, ...) %>%
    mutate(model = x$model) %>%
    ggbnec(nec = nec, ecx = ecx)
}

#' autoplot.bayesmanecfit
#'
#' \code{\link[bayesnec:bayesnec-package]{bayesnec}} standard \pkg{ggplot2}
#' plotting method.
#'
#' @inheritParams autoplot.bayesnecfit
#'
#' @param object An object of class \code{\link{bayesmanecfit}} as returned by
#' function \code{\link{bnec}}.
#' @param ... Additional arguments to be passed to \code{\link{ggbnec_data}}.
#' @param all_models Should all individual models be plotted separately\
#' (defaults to FALSE) or should model averaged predictions be plotted instead?
#' @param plot Should output \code{\link[ggplot2]{ggplot}} output be plotted?
#' Only relevant if \code{all = TRUE} and \code{multi_facet = FALSE}.
#' @param ask Indicates if the user is prompted before a new page is plotted.
#' Only relevant if \code{plot = TRUE} and \code{multi_facet = FALSE}.
#' @param newpage Indicates if the first set of plots should be plotted to a
#' new page. Only relevant if \code{plot = TRUE} and
#' \code{multi_facet = FALSE}.
#' @param multi_facet Should all plots be plotted in one single panel via
#' facets? Defaults to TRUE.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @inherit autoplot.bayesnecfit examples
#' @family autoplot methods
#'
#' @importFrom dplyr %>% mutate
#' @importFrom purrr map_dfr
#' @importFrom grDevices devAskNewPage
#' @export
autoplot.bayesmanecfit <- function(object, ..., nec = TRUE, ecx = FALSE,
                                   force_x = FALSE, xform = NA,
                                   all_models = FALSE, plot = TRUE, ask = TRUE,
                                   newpage = TRUE, multi_facet = TRUE) {
  x <- object
  if (all_models) {
    all_fits <- lapply(x$success_models, pull_out, manec = x) %>%
      suppressMessages %>%
      suppressWarnings
    if (multi_facet) {
      names(all_fits) <- x$success_models
      map_dfr(all_fits, ggbnec_data, add_nec = nec, add_ecx = ecx,
              force_x = force_x, xform = xform, ..., .id = "model") %>%
        ggbnec(nec = nec, ecx = ecx)
    } else {
      if (plot) {
        default_ask <- devAskNewPage()
        on.exit(devAskNewPage(default_ask))
        devAskNewPage(ask = FALSE)
      }
      plots <- vector(mode = "list", length = length(all_fits))
      for (i in seq_along(all_fits)) {
        plots[[i]] <- ggbnec_data(all_fits[[i]], add_nec = nec, add_ecx = ecx,
                                  force_x = force_x, xform = xform, ...) %>%
          mutate(model = x$success_models[i]) %>%
          ggbnec(nec = nec, ecx = ecx)
        plot(plots[[i]], newpage = newpage || i > 1)
        if (i == 1) {
          devAskNewPage(ask = ask)
        }
      }
      invisible(plots)
    }
  } else {
    ggbnec_data(x, add_nec = nec, add_ecx = ecx, force_x = force_x,
                xform = xform, ...) %>%
      mutate(model = "Model averaged predictions") %>%
      ggbnec(nec = nec, ecx = ecx)
  }
}
