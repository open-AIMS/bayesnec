#' bayesnec standard ggplot2 plotting method
#'
#' \code{\link[bayesnec:bayesnec-package]{bayesnec}} standard \pkg{ggplot2}
#' plotting method.
#'
#' @name autoplot
#' @order 1
#'
#' @param object An object of class \code{\link{bayesnecfit}},
#' \code{\link{bayesmanecfit}}, \code{\link{bayesnecgroupfit}} or
#' \code{\link{bayesnecjointfit}}.
#' @param ... Additional arguments to be passed to \code{\link{ggbnec_data}}.
#' @param nec Should NEC values be added to the plot? Defaults to TRUE.
#' @param ecx Should ECx values be added to the plot? Defaults to FALSE..
#' @param xform \code{NULL}, the default, or a function. See
#' \code{\link{ggbnec_data}}.
#' @param x_scale \code{"recorded"} (the default) draws the predictor as
#' supplied to \code{bnec()}. \code{"fitted"} draws it as transformed inside
#' \code{crf()}, with axis labels in the supplied units. It has no effect
#' when \code{crf()} names an untransformed predictor.
#' @param group An optional character string naming a categorical variable in
#' the data supplied to \code{\link{bnec}}. Observations are joined through
#' their per-level means when the variable occurs as a group-level term in the
#' fitted formula; a level recorded at one predictor value is marked at its
#' mean instead. A variable not fitted by the model can be shown only with
#' \code{group_aes = "colour"}; the curve and credible band are pooled over
#' that variable, and the legend identifies it as not fitted. A
#' \code{bayesnecgroupfit} uses its fitted grouping variable automatically and
#' draws one panel per level, as does a \code{\link{bayesnecjointfit}}, whose
#' levels are coefficients of one posterior rather than separate fits.
#' @param group_aes How should the grouping selected by \code{group} be shown?
#' \code{"line"} (the default) draws grey per-level means. \code{"colour"}
#' also maps the grouping to the observation fill and the per-level mean marks.
#' The argument is ignored when \code{group} is \code{NULL}. Only
#' \code{"colour"} is available when \code{group} is not a group-level term
#' in the fitted formula.
#' @param point_alpha The opacity of the observation points, a single number
#' from 0 (transparent) to 1 (opaque, the default). It does not change the
#' credible band or the per-level means drawn for \code{group}.
#' @param which The curve to draw for a \code{\link{bayesnecfit}} with a
#' two-block family (\code{"hurdle_gamma"}, \code{"zero_inflated_beta"},
#' \code{"hurdle_poisson"} or \code{"hurdle_negbinomial"}), as returned by
#' \code{\link{bnec}} or by \code{\link{bnec_joint}} on a
#' \code{\link{bnec_hurdle}} fit: \code{"combined"} (the default), the
#' expected response per individual exposed; \code{"growth"}, the response of
#' survivors, which \code{dpar = "mu"} selects in \code{\link{ecx}};
#' \code{"survival"}, the probability of a non-zero response, which
#' \code{dpar = "hu"} (\code{"zi"} for \code{zero_inflated_beta}) selects; or
#' \code{"all"} for a panel of each. The panels and their titles are those
#' \code{\link{autoplot.bayesnechurdlefit}} draws for a factorised fit, and
#' each title names the equation of its block. A growth or survival panel marks
#' the \code{nec} parameter of its block where the block's equation has one,
#' and otherwise the NSEC of its curve, and draws the ECx that
#' \code{\link{ecx}} returns with the matching \code{dpar}. A model set, a
#' \code{\link{bayesnecgroupfit}}, a \code{\link{bayesnecjointfit}} and a fit
#' of any other family draw one curve and refuse any value but
#' \code{"combined"}.
#' @param xlab The x-axis label. Defaults to \code{"Predictor"}.
#' @param ylab The y-axis label. Defaults to \code{NULL}, which gives
#' \code{"Response"}, or for a two-block family the title of the curve drawn.
#' Where \code{which = "all"}, the panel titles name the curves and
#' \code{NULL} leaves the axis unlabelled.
#'
#' @details Where a group-level term spans the predictor, as a plate does when
#' each plate is tested at every concentration, the curve and credible band
#' are those of the population curve, with every group deviation set to zero.
#' The band is therefore the 95% credible interval of the expected response
#' of a typical level, and it does not include the variation between levels:
#' the observations of a level whose deviation is large can lie outside it.
#' The NEC, NSEC and ECx lines are read from the observed levels instead (see
#' \code{pool} in \code{\link{ecx}}), so they need not fall where the plotted
#' curve equals the corresponding response.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object, or, for a
#' \code{\link{bayesmanecfit}} with \code{multi_facet = FALSE}, a
#' \code{\link[base]{list}} of them, returned invisibly, as described under
#' \code{multi_facet}.
#'
#' @examples
#' \dontrun{
#' library(brms)
#' nec4param <- pull_out(manec_example, "nec4param")
#' autoplot(nec4param)
#' autoplot(nec4param, nec = FALSE)
#' autoplot(nec4param, ecx = TRUE, ecx_val = 50, xlab = "Concentration")
#'
#' # plot model averaged predictions
#' autoplot(manec_example)
#' # plot the model average and each equation of the set, a facet each
#' autoplot(manec_example, ecx = TRUE, ecx_val = 50,
#'          model = c("nec4param", "ecx4param"))
#' # plot the equations alone, without the model average
#' autoplot(manec_example, model = manec_example$success_models,
#'          average = FALSE)
#' }
#' \dontrun{
#' # plots multiple models, one at a time, with interactive prompt
#' autoplot(manec_example, ecx = TRUE, ecx_val = 50,
#'          model = manec_example$success_models, average = FALSE,
#'          multi_facet = FALSE)
#' }
NULL

#' @rdname autoplot
#' @order 2
#'
#' @method autoplot bayesnecfit
#'
#' @inherit autoplot description return examples
#'
#' @importFrom dplyr mutate
#' @importFrom chk chk_lgl
#' @importFrom rlang .env
#'
#' @export
autoplot.bayesnecfit <- function(object, ..., nec = TRUE, ecx = FALSE,
                                 xform = NULL, group = NULL,
                                 group_aes = c("line", "colour"),
                                 point_alpha = 1,
                                 x_scale = c("recorded", "fitted"),
                                 which = "combined", xlab = "Predictor",
                                 ylab = NULL) {
  x <- object
  chk_lgl(nec)
  chk_lgl(ecx)
  group_aes <- match.arg(group_aes)
  check_xform(xform)
  check_point_alpha(point_alpha)
  check_plot_dpar(list(...), x)
  blocks <- plot_blocks(x, which)
  x_scale <- plot_x_scale(x, x_scale, xform)
  summ <- summary(x, ecx = FALSE, xform = identity) |>
    suppressWarnings() |>
    suppressMessages()
  dat <- ggbnec_data(x, add_nec = nec, add_ecx = ecx,
                     xform = xform, x_scale = x_scale, group = group,
                     which = which, ...)
  x_axis <- attr(dat, "x_axis")
  group_label <- plot_group_label(dat, group, group_aes)
  labels <- plot_block_labels(x, blocks, rownames(summ$nec_vals)[1])
  # A two-block fit's frame names each row's curve in `panel`; map it to the
  # title and annotation label of that curve.
  key <- if (is.null(dat$panel)) {
    rep("combined", nrow(dat))
  } else {
    names(hurdle_panel_names)[match(as.character(dat$panel),
                                    hurdle_panel_names)]
  }
  # A factor keeps several panels in the order of `which`; facet_wrap() would
  # sort a character column.
  dat$model <- unname(labels$title[key])
  if (length(blocks) > 1) {
    dat$model <- factor(dat$model, levels = unname(labels$title))
  }
  dat$tag <- unname(labels$tag[key])
  # Several curves of one fit share the predictor, so they are laid out as
  # autoplot.bayesnechurdlefit() lays them out: one row, the x axis shared.
  several <- length(blocks) > 1
  ggbnec(dat, nec = nec, ecx = ecx, group = !is.null(group),
         group_aes = group_aes, group_label = group_label,
         group_fitted = !identical(attr(dat, "group_fitted"), FALSE),
         point_alpha = point_alpha, x_axis = x_axis, xlab = xlab,
         ylab = plot_ylab(ylab, x, names(blocks)),
         facet_scales = if (several) "free_y" else "free",
         facet_ncol = if (several) NULL else 2)
}

#' @rdname autoplot
#' @order 2
#'
#' @param model \code{NULL} (the default), or a \code{\link[base]{character}}
#' vector naming equations of the model set. Each named equation is drawn in a
#' panel of its own, which shows the same curve and annotations as
#' \code{autoplot()} gives for the \code{\link{bayesnecfit}} that
#' \code{\link{pull_out}} returns for it. A name that is not an equation of
#' the set is refused.
#' @param average Should the model averaged predictions be plotted? Defaults
#' to TRUE. Where \code{model} is also given, the model average takes the
#' first panel. \code{average = FALSE} requires \code{model}.
#' @param plot Should output \code{\link[ggplot2]{ggplot}} output be plotted?
#' Only relevant if \code{model} is given and \code{multi_facet = FALSE}.
#' @param ask Indicates if the user is prompted before a new page is plotted.
#' Only relevant if \code{plot = TRUE} and \code{multi_facet = FALSE}.
#' @param newpage Indicates if the first set of plots should be plotted to a
#' new page. Only relevant if \code{plot = TRUE} and
#' \code{multi_facet = FALSE}.
#' @param multi_facet Should all plots be plotted in one single panel via
#' facets? Defaults to TRUE, in which case the facets of the named equations
#' are ordered by name. Only relevant if \code{model} is given. With
#' \code{multi_facet = FALSE}, one \code{\link[ggplot2]{ggplot}} is built for
#' each panel, drawn in turn only where \code{plot = TRUE}, and the list of
#' them is returned invisibly, the model average
#' first where it is requested and then the equations in the order given in
#' \code{model}.
#' @param all_models Deprecated, and to be removed in a later release; use
#' \code{model} and \code{average}. \code{all_models = TRUE} is
#' \code{model = object$success_models, average = FALSE}, and
#' \code{all_models = FALSE} is the default. Supplying it gives a warning, and
#' supplying it together with \code{model} or \code{average} is an error.
#'
#' @method autoplot bayesmanecfit
#'
#' @inherit autoplot description return examples
#'
#' @importFrom dplyr mutate left_join bind_rows
#' @importFrom purrr map_dfr
#' @importFrom tibble rownames_to_column
#' @importFrom grDevices devAskNewPage
#' @importFrom chk chk_lgl
#' @importFrom rlang .env
#'
#' @export
autoplot.bayesmanecfit <- function(object, ..., nec = TRUE, ecx = FALSE,
                                   xform = NULL, model = NULL,
                                   average = TRUE, plot = TRUE, ask = TRUE,
                                   newpage = TRUE, multi_facet = TRUE,
                                   group = NULL,
                                   group_aes = c("line", "colour"),
                                   point_alpha = 1, all_models = NULL,
                                   x_scale = c("recorded", "fitted"),
                                   xlab = "Predictor", ylab = NULL) {
  x <- object
  chk_lgl(nec)
  chk_lgl(ecx)
  group_aes <- match.arg(group_aes)
  check_xform(xform)
  check_point_alpha(point_alpha)
  # Check before the equations are pulled out: a pulled-out equation would
  # accept `which` and draw its blocks inside the model's one panel.
  dots <- list(...)
  check_plot_single_curve(dots[["which"]], x)
  check_plot_dpar(dots, x)
  if (ecx) {
    check_plot_relative(dots, x)
  }
  ylab <- plot_ylab(ylab, x)
  x_scale <- plot_x_scale(x, x_scale, xform)
  shown <- resolve_model_average(
    x, model, average, all_models,
    new_supplied = !missing(model) || !missing(average)
  )
  chk_lgl(plot)
  chk_lgl(ask)
  chk_lgl(newpage)
  chk_lgl(multi_facet)
  if (is.null(shown$model)) {
    avg <- manec_average_plot_data(x, nec = nec, ecx = ecx, xform = xform,
                                   x_scale = x_scale, group = group,
                                   group_aes = group_aes, ...)
    return(ggbnec(avg$data, nec = nec, ecx = ecx, group = !is.null(group),
                  group_aes = group_aes, group_label = avg$group_label,
                  group_fitted = avg$group_fitted,
                  point_alpha = point_alpha, x_axis = avg$x_axis,
                  xlab = xlab, ylab = ylab))
  }
  all_fits <- lapply(shown$model, pull_out, manec = x) |>
    suppressMessages() |>
    suppressWarnings()
  # Title each equation as autoplot() titles it once pulled out, which for a
  # two-block fit names the equation of each block.
  titles <- vapply(all_fits, plot_title, character(1))
  names(titles) <- shown$model
  if (multi_facet) {
    names(all_fits) <- shown$model
    nec_labs <- map_dfr(all_fits, function(x) {
      summ <- summary(x, ecx = FALSE, xform = identity) |>
        suppressWarnings() |>
        suppressMessages()
      summ$nec_vals |>
        data.frame() |>
        rownames_to_column(var = "tag")
    }, .id = "model")
    plot_data <- lapply(all_fits, function(fit) {
      ggbnec_data(fit, add_nec = nec, add_ecx = ecx,
                  xform = xform, x_scale = x_scale, group = group, ...)
    })
    group_label <- plot_group_label(plot_data[[1]], group, group_aes)
    x_axis <- attr(plot_data[[1]], "x_axis")
    group_fitted <- !identical(
      attr(plot_data[[1]], "group_fitted"), FALSE
    )
    dat <- map_dfr(plot_data, identity, .id = "model") |>
      left_join(y = nec_labs, by = "model")
    # Join the labels by equation name before the titles replace it.
    dat$model <- unname(titles[dat$model])
    if (shown$average) {
      avg <- manec_average_plot_data(x, nec = nec, ecx = ecx, xform = xform,
                                     x_scale = x_scale, group = group,
                                     group_aes = group_aes,
                                     ...)
      # A factor puts the model average in the first facet. The equations
      # follow in alphabetical order, as facet_wrap() orders a character column.
      dat <- bind_rows(avg$data, dat)
      dat$model <- factor(dat$model,
                          levels = c(manec_average_label,
                                     unname(titles[sort(unique(shown$model))])))
    }
    ggbnec(dat, nec = nec, ecx = ecx, group = !is.null(group),
           group_aes = group_aes, group_label = group_label,
           group_fitted = group_fitted, point_alpha = point_alpha,
           x_axis = x_axis, xlab = xlab, ylab = ylab)
  } else {
    if (plot) {
      default_ask <- devAskNewPage()
      on.exit(devAskNewPage(default_ask))
      devAskNewPage(ask = FALSE)
    }
    n_average <- as.integer(shown$average)
    plots <- vector(mode = "list", length = length(all_fits) + n_average)
    for (i in seq_along(plots)) {
      if (i <= n_average) {
        avg <- manec_average_plot_data(x, nec = nec, ecx = ecx,
                                       xform = xform, x_scale = x_scale,
                                       group = group,
                                       group_aes = group_aes, ...)
        plots[[i]] <- ggbnec(avg$data, nec = nec, ecx = ecx,
                             group = !is.null(group), group_aes = group_aes,
                             group_label = avg$group_label,
                             group_fitted = avg$group_fitted,
                             point_alpha = point_alpha,
                             x_axis = avg$x_axis, xlab = xlab, ylab = ylab)
      } else {
        j <- i - n_average
        summ_j <- summary(all_fits[[j]], ecx = FALSE, xform = identity) |>
          suppressWarnings() |>
          suppressMessages()
        dat_j <- ggbnec_data(all_fits[[j]], add_nec = nec, add_ecx = ecx,
                             xform = xform, x_scale = x_scale,
                             group = group, ...)
        x_axis <- attr(dat_j, "x_axis")
        group_label <- plot_group_label(dat_j, group, group_aes)
        plots[[i]] <- dat_j |>
          mutate(model = unname(titles[j]),
                 tag = rownames(.env$summ_j$nec_vals)) |>
          ggbnec(nec = nec, ecx = ecx, group = !is.null(group),
                 group_aes = group_aes, group_label = group_label,
                 group_fitted = !identical(
                   attr(dat_j, "group_fitted"), FALSE
                 ), point_alpha = point_alpha, x_axis = x_axis,
                 xlab = xlab, ylab = ylab)
      }
      # Draw each page as soon as it is built, and only where `plot` is TRUE,
      # the flag that also guards the device's prompt setting above.
      if (plot) {
        plot(plots[[i]], newpage = newpage || i > 1)
        if (i == 1) {
          devAskNewPage(ask = ask)
        }
      }
    }
    invisible(plots)
  }
}

# The label of the model average, shared by the plot that shows it alone,
# the facet it takes beside named equations and its panel in plot().
manec_average_label <- "Model averaged predictions"

#' The plotting frame of a model set's model-averaged predictions
#'
#' Separated from \code{autoplot.bayesmanecfit()} so that one frame serves
#' the plot of the model average alone, its facet beside named equations,
#' and its page when \code{multi_facet = FALSE}.
#'
#' @return A list: \code{data}, the \code{\link{ggbnec_data}} frame with
#' \code{model} and \code{tag} columns added; \code{group_label} and
#' \code{group_fitted}, as \code{ggbnec()} takes them.
#'
#' @importFrom dplyr mutate
#' @importFrom rlang .env
#'
#' @noRd
manec_average_plot_data <- function(x, nec, ecx, xform, x_scale, group, group_aes,
                                    ...) {
  summ <- summary(x, ecx = FALSE, xform = identity) |>
    suppressWarnings() |>
    suppressMessages()
  dat <- ggbnec_data(x, add_nec = nec, add_ecx = ecx, xform = xform,
                     x_scale = x_scale, group = group, ...)
  group_label <- plot_group_label(dat, group, group_aes)
  list(
    data = dat |>
      mutate(model = manec_average_label,
             tag = rownames(.env$summ$nec_vals)),
    group_label = group_label,
    group_fitted = !identical(attr(dat, "group_fitted"), FALSE),
    x_axis = attr(dat, "x_axis")
  )
}

#' @param brms_fit A \code{\link[brms]{brmsfit}} object.
#' @param bayesnecformula A \code{\link{bayesnecformula}} formula object.
#'
#' @return A \code{\link[base]{data.frame}}.
#'
#' @param group An optional character string naming a categorical variable in
#' the data supplied to \code{\link{bnec}}.
#' @param retained_data A data frame of columns omitted from
#' \code{brms_fit$data}, stored in fitted-row order on the bayesnec object.
#'
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @importFrom stats model.frame
#'
#' @noRd
prep_raw_data <- function(brms_fit, bayesnecformula, group = NULL,
                          retained_data = NULL) {
  r_df <- brms_fit$data
  mod_dat <- model.frame(bayesnecformula, data = r_df)
  if (!is.null(group) &&
      (!is.character(group) || length(group) != 1 || is.na(group) ||
       !nzchar(group))) {
    stop("`group` must be NULL or one non-empty column name.", call. = FALSE)
  }
  group_fitted <- NULL
  group_values <- NULL
  if (!is.null(group)) {
    group_vars <- attr(mod_dat, "bnec_group")
    group_vars <- group_vars[!is.na(group_vars)]
    available <- union(names(r_df), names(retained_data))
    if (!group %in% available) {
      stop("`group` must name a group-level variable in the fitted formula ",
           "or a column retained from the data supplied to bnec(). Available ",
           "columns are: ", paste0("\"", available, "\"", collapse = ", "),
           ".", call. = FALSE)
    }
    group_fitted <- group %in% group_vars
    group_values <- if (group %in% names(r_df)) {
      r_df[[group]]
    } else {
      retained_data[[group]]
    }
    if (!group_fitted &&
        !(is.factor(group_values) || is.character(group_values) ||
          is.logical(group_values))) {
      stop("`group` must be categorical when it is not a group-level ",
           "variable in the fitted formula; convert \"", group,
           "\" to a factor before fitting.", call. = FALSE)
    }
    if (anyNA(group_values)) {
      stop("`group` contains missing values in \"", group,
           "\"; remove or define them before fitting.", call. = FALSE)
    }
  }
  y_var <- attr(mod_dat, "bnec_pop")[["y_var"]]
  x_var <- attr(mod_dat, "bnec_pop")[["x_var"]]
  family <- brms_fit$family
  rate_var <- unname(attr(mod_dat, "bnec_pop")["rate_var"])
  if (family$family == "binomial" | family$family == "beta_binomial") {
    trials_var <- attr(mod_dat, "bnec_pop")[["trials_var"]]
    r_df[[y_var]] <- r_df[[y_var]] / r_df[[trials_var]]
  } else if (!is.na(rate_var)) {
    # Rate scale, matching the grid -- see the same branch in plot.R.
    r_df[[y_var]] <- r_df[[y_var]] / r_df[[rate_var]]
  }
  out <- r_df |>
    mutate(x_e = NA, y_e = NA, y_ci = NA, x_r = .data[[x_var]],
           y_r = .data[[y_var]])
  keep <- c("x_e", "y_e", "y_ci", "x_r", "y_r")
  if (!is.null(group)) {
    # A stable output name lets downstream code use the same mapping whichever
    # source column was selected. The source name remains on the result as an
    # attribute for code that labels the grouping.
    out$group <- group_values
    keep <- c(keep, "group")
  }
  out <- out[, keep, drop = FALSE]
  if (!is.null(group)) {
    attr(out, "group_fitted") <- group_fitted
  }
  out
}

#' Label and validate a grouping selected for autoplot
#'
#' A grouping absent from the fitted formula describes observations only. It
#' is labelled as such and is not allowed to draw the fitted-group mean lines,
#' which would otherwise suggest that the model estimated a per-level curve.
#'
#' @noRd
plot_group_label <- function(data, group, group_aes) {
  if (is.null(group)) {
    return(NULL)
  }
  if (identical(attr(data, "group_fitted"), FALSE)) {
    if (!identical(group_aes, "colour")) {
      stop("A `group` that is not a group-level variable in the fitted ",
           "formula can only use `group_aes = \"colour\"`.", call. = FALSE)
    }
    return(paste0(group, " (not fitted)"))
  }
  group
}

#' @param data A \code{\link[base]{data.frame}}.
#' @param nec_vals A \code{\link[base]{numeric}} vector containing the mean,
#' and 95% credible intervals of NEC values, already on the axis scale.
#'
#' @return A \code{\link[base]{data.frame}}.
#'
#' @noRd
bind_nec <- function(data, nec_vals, label_vals = nec_vals) {
  data$nec_vals <- NA
  data$nec_labs <- NA
  data$nec_labs_l <- NA
  data$nec_labs_u <- NA
  df <- data[1:3, ]
  df[ ] <- NA

  # nec_vals is already on the axis scale; see to_axis_scale(). Transforming
  # here would reorder the three entries under a decreasing map while the
  # censoring marks stayed where they were.
  cens <- attr(nec_vals, "censored_summary")

  df$nec_vals <- nec_vals
  # A censored entry is the end of the prediction range, not a quantile, so
  # its label carries the bound's prefix. The line is still drawn there,
  # where the estimate is known to lie at or beyond.
  df$nec_labs[1] <- censored_label(label_vals, cens, 1)
  df$nec_labs_l[1] <- censored_label(label_vals, cens, 2)
  df$nec_labs_u[1] <- censored_label(label_vals, cens, 3)
  rbind(data, df)
}

#' One annotation label, marked where the entry is a bound
#'
#' @param values A summarised estimate.
#' @param cens Its \code{"censored_summary"} attribute, or \code{NULL}.
#' @param i The entry to label.
#'
#' @return A \code{\link[base]{character}} value.
#' @noRd
censored_label <- function(values, cens, i) {
  lab <- rounded(values[[i]], 2)
  if (is.null(cens) || !nzchar(cens$bound[i])) {
    return(lab)
  }
  paste0(cens$bound[i], lab)
}

#' @param data A \code{\link[base]{data.frame}}.
#' @param ecx_vals A \code{\link[base]{numeric}} vector containing the mean,
#' and 95% credible intervals of ECx values.
#'
#' @return A \code{\link[base]{data.frame}}.
#'
#' @noRd
bind_ecx <- function(data, ecx_vals, label_vals = ecx_vals) {
  data$ecx_vals <- NA
  data$ecx_int <- NA
  data$ecx_labs <- NA
  data$ecx_labs_l <- NA
  data$ecx_labs_u <- NA
  df <- data[1:3, ]
  df[ ] <- NA
  cens <- attr(ecx_vals, "censored_summary")
  df$ecx_vals <- ecx_vals
  df$ecx_int[1] <- attr(ecx_vals, "ecx_val")
  df$ecx_labs[1] <- censored_label(label_vals, cens, 1)
  df$ecx_labs_l[1] <- censored_label(label_vals, cens, 2)
  df$ecx_labs_u[1] <- censored_label(label_vals, cens, 3)
  rbind(data, df)
}

#' Recorded values for labelling estimates drawn on the fitted axis
#'
#' The fitted axis is labelled in recorded units, so an estimate placed on it
#' is labelled in those units too.
#'
#' @param values Estimates on the fitted scale.
#' @param object The fit whose formula transforms the predictor.
#' @param x_grid_raw Recorded predictor values spanning the data.
#' @return \code{values} mapped back to the recorded scale.
#' @noRd
fitted_axis_labels <- function(values, object, x_grid_raw) {
  inverse <- recorded_x_map(scale_source_fit(object)$bayesnecformula,
                            x_grid_raw, inline_x_transform(object))
  out <- as.numeric(inverse(as.numeric(values)))
  names(out) <- names(values)
  out
}

#' Creates the data.frame for plotting with \code{\link{autoplot}}.
#'
#' @param x An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}, as returned by function \code{\link{bnec}},
#' a \code{\link{bayesnecgroupfit}} returned by \code{\link{bnec_group}}, or
#' a \code{\link{bayesnecjointfit}} returned by \code{\link{bnec_joint}}.
#' @param add_nec Should NEC values be added to the plot? Defaults to TRUE.
#' @param add_ecx Should ECx values be added to the plot? Defaults to FALSE.
#' @param xform \code{NULL}, the default, or a function. Where \code{crf()}
#' does not transform the predictor inline, a function is applied to the
#' predictor and to the estimates. Where it does, the predictor is on the
#' recorded scale, and the estimates are those \code{\link{nec}} and
#' \code{\link{ecx}} return by default, which are on that scale. A function
#' other than \code{identity} is then applied to the estimates on the
#' transformed scale instead, as the estimators apply it.
#' @param x_scale \code{"recorded"} (the default) returns the predictor in
#' supplied units. \code{"fitted"} returns the scale transformed inside
#' \code{crf()} and attaches \code{"x_axis"}, a list of fitted-scale breaks
#' and labels in supplied units. A fitted axis cannot be combined with a
#' non-identity \code{xform}.
#' @param group An optional character string naming a categorical variable in
#' the data supplied to \code{\link{bnec}}. When supplied, its values are
#' returned in a column named \code{group}, with the source name in attribute
#' \code{"group_var"}. Attribute \code{"group_fitted"} states whether the
#' fit represents the grouping: either the fitted formula includes it as a
#' group-level variable, or independently fitted \code{bayesnecgroupfit}
#' panels use it. A
#' \code{bayesnecgroupfit} returns its fitted grouping variable automatically.
#' @param ... Additional arguments to be passed to \code{\link{ecx}}. By
#' default, function \code{\link{ecx}} returns EC10. \code{dpar} is refused:
#' select a block with \code{which}.
#' @param which For a \code{\link{bayesnecfit}} with a two-block family, the
#' curve or curves to return: \code{"combined"} (the default),
#' \code{"growth"}, \code{"survival"} or \code{"all"}, as described for
#' \code{\link{autoplot}}. Every other class and family has one curve, and
#' refuses any value but \code{"combined"}.
#'
#' @details The curve and credible band are those \code{\link{autoplot}}
#' draws: where a group-level term spans the predictor, the population curve,
#' with every group deviation set to zero. See the Details of
#' \code{\link{autoplot}}.
#'
#' @return A \code{\link[base]{data.frame}}. When \code{group} is supplied, or
#' \code{x} is a \code{bayesnecgroupfit} or a \code{bayesnecjointfit}, the
#' frame includes a \code{group} column and a \code{"group_var"} attribute.
#' Those two classes also include a \code{panel} column containing the level
#' drawn in each panel. For a \code{bayesnecfit} with a two-block family, the
#' \code{panel} column holds the title of the curve each row belongs to, a
#' factor whose levels follow \code{which}.
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
# Keep which on the generic: the bayesnecfit method is @noRd, so the generic's
# usage is the only place it can be documented.
ggbnec_data <- function(x, add_nec = TRUE, add_ecx = FALSE,
                        xform = NULL, ..., group = NULL,
                        x_scale = c("recorded", "fitted"),
                        which = "combined") {
  UseMethod("ggbnec_data")
}

#' @noRd
plot_x_scale <- function(object, x_scale, xform) {
  x_scale <- match.arg(x_scale, c("recorded", "fitted"))
  if (x_scale == "fitted" && !is.null(inline_x_transform(object))) {
    if (!is.null(xform) && !identical(xform, identity)) {
      stop("`x_scale = \"fitted\"` cannot be combined with a non-identity ",
           "`xform`.", call. = FALSE)
    }
    return(x_scale)
  }
  "recorded"
}

#' @noRd
plot_fitted_grid <- function(object, resolution = 1000, x_range = NA) {
  fit <- scale_source_fit(object)
  formula <- fit$bayesnecformula
  if (length(x_range) == 1 && is.na(x_range) &&
      !inherits(object, "bayesnechurdlefit")) {
    stored <- if (inherits(object, "bayesmanecfit")) {
      object$w_pred_vals$data$x
    } else {
      fit$pred_vals$data$x
    }
    x_range <- range(stored, na.rm = TRUE)
  }
  grid <- newdata_eval(object, resolution = resolution, x_range = x_range)
  recorded <- grid$x_vec
  fitted <- as.numeric(sub_x_transformation(recorded, formula))
  if (any(!is.finite(fitted))) {
    stop("The fitted predictor axis is not finite over the plotted range.",
         call. = FALSE)
  }
  if (!all(diff(fitted) > 0) && !all(diff(fitted) < 0)) {
    stop("The fitted predictor axis must be strictly monotone over the ",
         "plotted range.", call. = FALSE)
  }
  x <- seq(min(fitted), max(fitted), length.out = resolution)
  inverse <- recorded_x_map(formula, recorded, inline_x_transform(object))
  raw <- as.numeric(inverse(x))
  x_var <- attr(model.frame(formula, fit$fit$data), "bnec_pop")[["x_var"]]
  grid$newdata[[x_var]] <- raw
  list(x = x, recorded = raw, newdata = grid$newdata)
}

#' @noRd
plot_fitted_curve <- function(object) {
  plot_curve(object, plot_fitted_grid(object))
}

#' The credible band of a fit, or of one block of a joint hurdle fit, on a grid
#'
#' @param object A \code{\link{bayesnecfit}} or \code{\link{bayesmanecfit}}.
#' @param grid A list holding \code{x}, the axis values, and \code{newdata},
#' the prediction rows on the recorded scale.
#' @param dpar \code{NULL} for the expected response, or one block of a joint
#' hurdle fit, as \code{block_epred()} takes it.
#'
#' @return A \code{\link[base]{data.frame}} with columns \code{x},
#' \code{Estimate}, \code{Q2.5} and \code{Q97.5}.
#' @noRd
plot_curve <- function(object, grid, dpar = NULL) {
  # The population curve, as in the stored band.
  grid_draws <- function(fit, formula) {
    block_epred(fit, formula, grid$newdata, re_formula = NA, dpar = dpar)
  }
  if (inherits(object, "bayesmanecfit")) {
    models <- names(object$mod_fits)
    draw_index <- pull_draw_index(object, models,
                                  set_draw_count(object$mod_fits))
    # Retain only the draws contributing to the model average before
    # predicting the next equation, so a large set does not hold every full
    # grid posterior.
    pieces <- lapply(models, function(model) {
      fit <- object$mod_fits[[model]]
      draws <- grid_draws(fit$fit, fit$bayesnecformula)
      draws[draw_index[[model]], , drop = FALSE]
    })
    draws <- do.call(rbind, pieces)
  } else {
    draws <- grid_draws(object$fit, object$bayesnecformula)
  }
  summaries <- t(apply(draws, 2, estimates_summary))
  data.frame(x = grid$x, Estimate = summaries[, "Estimate"],
             Q2.5 = summaries[, "Q2.5"], Q97.5 = summaries[, "Q97.5"])
}

#' @noRd
plot_x_axis <- function(recorded, formula) {
  observed <- sort(unique(recorded[is.finite(recorded)]))
  positive <- observed[observed > 0]
  if (length(positive) && diff(range(positive)) > 0 &&
      diff(log10(range(positive))) >= 1) {
    ticks <- 10^pretty(log10(range(positive)), n = 4)
  } else {
    ticks <- pretty(range(observed), n = 6)
  }
  ticks <- ticks[ticks >= min(observed) & ticks <= max(observed)]
  if (0 %in% observed) ticks <- c(0, ticks)
  ticks <- sort(unique(ticks))
  breaks <- as.numeric(sub_x_transformation(ticks, formula))
  keep <- is.finite(breaks)
  breaks <- breaks[keep]
  labels <- vapply(ticks[keep], function(value) {
    format(signif(value, 3), trim = TRUE, scientific = FALSE)
  }, character(1))
  order <- order(breaks)
  list(breaks = breaks[order], labels = labels[order])
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
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#'
#' @noRd
#'
#' @export
ggbnec_data.bayesnecfit <- function(x, add_nec = TRUE, add_ecx = FALSE,
                                    xform = NULL, ..., group = NULL,
                                    x_scale = c("recorded", "fitted"),
                                    which = "combined") {
  chk_lgl(add_nec)
  chk_lgl(add_ecx)
  check_xform(xform)
  check_plot_dpar(list(...), x)
  blocks <- plot_blocks(x, which)
  warn_stale_observed_display(x)
  warn_stale_ne_type(x)
  x_scale <- plot_x_scale(x, x_scale, xform)
  r_df <- prep_raw_data(x$fit, x$bayesnecformula, group = group,
                        retained_data = x$retained_data)
  bdat <- model.frame(x$bayesnecformula, data = x$fit$data, run_par_checks = TRUE)
  # On the recorded axis with no xform, draw what the estimators return.
  recorded <- x_scale == "recorded" && plot_reads_recorded(bdat, xform)
  axis_xform <- if (is.null(xform)) identity else xform
  x_grid_raw <- x$pred_vals$data$x
  two_block <- is_hurdle_family(x$fit$family)
  if (add_ecx && "combined" %in% names(blocks)) {
    check_plot_relative(list(...), x)
  }
  pieces <- lapply(names(blocks), function(w) {
    dpar <- if (is.na(blocks[[w]])) NULL else blocks[[w]]
    curve <- plot_block_curve(x, dpar, x_scale)
    e_df <- data.frame(x_e = c(curve$x, rev(curve$x)),
                       y_e = c(curve$Estimate, rep(NA, nrow(curve))),
                       y_ci = c(curve$Q2.5, rev(curve$Q97.5)),
                       x_r = NA, y_r = NA)
    raw <- plot_block_raw(r_df, w)
    if (!is.null(group)) {
      e_df$group <- raw$group[rep(NA_integer_, nrow(e_df))]
    }
    out <- rbind(e_df, raw)
    if (x_scale == "fitted") {
      out$x_r <- as.numeric(sub_x_transformation(out$x_r,
                                                 x$bayesnecformula))
    }
    if (!pop_var_is_transformed(bdat, "x_var")) {
      out <- out |>
        mutate(x_e = axis_xform(.data$x_e), x_r = axis_xform(.data$x_r))
    }
    if (add_nec) {
      ne_fit <- if (is.null(dpar)) x else plot_block_ne_fit(x, dpar)
      # to_axis_scale() puts the estimates and the censoring marks onto the
      # axis scale together, reversing both under a decreasing map.
      ne_vals <- if (recorded) {
        summary_ne_vals(ne_fit, NULL)
      } else if (x_scale == "fitted") {
        ne_fit$ne
      } else {
        to_axis_scale(ne_fit$ne, bdat, x$bayesnecformula, x_grid_raw,
                      axis_xform)
      }
      out <- bind_nec(out, ne_vals, if (x_scale == "fitted") {
        fitted_axis_labels(ne_vals, x, x_grid_raw)
      } else {
        ne_vals
      })
    }
    if (add_ecx) {
      ecx_dots <- list(...)
      ecx_dots$dpar <- dpar
      ecx_vals <- plot_ecx(x, x$fit$family$family, ecx_dots,
                           if (recorded) NULL else identity)
      if (!recorded && x_scale != "fitted") {
        ecx_vals <- to_axis_scale(ecx_vals, bdat, x$bayesnecformula,
                                  x_grid_raw, axis_xform)
      }
      out <- bind_ecx(out, ecx_vals, if (x_scale == "fitted") {
        fitted_axis_labels(ecx_vals, x, x_grid_raw)
      } else {
        ecx_vals
      })
    }
    if (two_block) {
      out$panel <- hurdle_panel_names[[w]]
    }
    out
  })
  if (length(pieces) == 1) {
    out <- pieces[[1]]
  } else {
    out <- do.call(rbind, pieces)
    rownames(out) <- NULL
  }
  if (two_block) {
    out$panel <- factor(out$panel,
                        levels = unname(hurdle_panel_names[names(blocks)]))
  }
  if (!is.null(group)) {
    attr(out, "group_var") <- group
    attr(out, "group_fitted") <- attr(r_df, "group_fitted")
  }
  if (x_scale == "fitted") {
    attr(out, "x_axis") <- plot_x_axis(r_df$x_r, x$bayesnecformula)
  }
  out
}

#' The curves a plot of a single fit draws
#'
#' @param object A \code{\link{bayesnecfit}}.
#' @param which The value supplied to \code{autoplot()} or
#' \code{ggbnec_data()}.
#'
#' @return A named \code{\link[base]{character}} vector with one element per
#' curve, named by the value of \code{which} that selects the curve. Each
#' element is the block the curve reads, or \code{NA} for the expected
#' response.
#' @noRd
plot_blocks <- function(object, which) {
  which <- hurdle_check_plot_which(which)
  family <- object$fit$family
  if (!is_hurdle_family(family)) {
    check_plot_single_curve(which, object)
    return(c(combined = NA_character_))
  }
  blocks <- c(combined = NA_character_, growth = "mu",
              survival = hurdle_dpar(family))
  if (identical(which, "all")) blocks else blocks[which]
}

#' Refuse a curve other than the expected response
#'
#' Only a single fit with a two-block family draws its blocks. Without this
#' check \code{which} falls into \code{...}, where it is ignored, or reaches
#' \code{ecx()}, which refuses it with a message about the estimators.
#'
#' @param which The value supplied, or \code{NULL}.
#' @param object The object being plotted.
#'
#' @return \code{NULL}, invisibly, or an error.
#' @noRd
check_plot_single_curve <- function(which, object) {
  if (is.null(which) ||
      identical(hurdle_check_plot_which(which), "combined")) {
    return(invisible(NULL))
  }
  stop(plot_curve_refusal(object, "which"), call. = FALSE)
}

#' Refuse a dpar passed to a plotting method
#'
#' Passed through \code{...}, it would reach \code{ecx()} alone, which would
#' then mark the ECx of one block on the curve of another.
#'
#' @param dots The calling method's \code{list(...)}.
#' @param object The object being plotted.
#'
#' @return \code{NULL}, invisibly, or an error.
#' @noRd
check_plot_dpar <- function(dots, object) {
  if ("dpar" %in% names(dots)) {
    stop(plot_curve_refusal(object, "dpar"), call. = FALSE)
  }
  invisible(NULL)
}

#' Refuse a relative ECx on the combined curve of a two-block fit
#'
#' \code{ecx()} refuses it too, but its message names \code{dpar}, which a
#' plot refuses.
#'
#' @param dots The calling method's \code{list(...)}, which \code{ecx()}
#' receives.
#' @param object The object being plotted.
#'
#' @return \code{NULL}, invisibly, or an error.
#' @noRd
check_plot_relative <- function(dots, object) {
  if (!identical(dots[["type"]], "relative") ||
      !is_hurdle_family(scale_source_fit(object)$fit$family)) {
    return(invisible(NULL))
  }
  remedy <- if (is_bayesnecfit(object)) {
    paste0("Draw one block with `which = \"growth\"` or `which = ",
           "\"survival\"`, whose ECx is measured towards the block's own ",
           "asymptote, or use type = \"absolute\" or type = \"range\".")
  } else {
    "Use type = \"absolute\" or type = \"range\"."
  }
  stop("type = \"relative\" is not defined for the combined curve of a ",
       "two-block fit, whose asymptote is the product of the two blocks' ",
       "asymptotes rather than a fitted parameter. ", remedy, call. = FALSE)
}

#' Why a plot cannot select a curve with which or dpar
#'
#' @param object The object being plotted.
#' @param arg The argument supplied, \code{"which"} or \code{"dpar"}.
#'
#' @return A \code{\link[base]{character}} message.
#' @noRd
plot_curve_refusal <- function(object, arg) {
  if (is_bayesnechurdlefit(object)) {
    return(paste0("A bnec_hurdle() fit selects its curve with `which`, not ",
                  "`dpar`: `which = \"growth\"`, `\"survival\"`, ",
                  "`\"combined\"` or `\"all\"`."))
  }
  family <- scale_source_fit(object)$fit$family
  if (!is_hurdle_family(family)) {
    return(paste0("This fit has a ", family$family, " family, which has one ",
                  "curve, so `", arg, "` has no block to select. Only a ",
                  "joint two-block fit (family \"hurdle_gamma\", ",
                  "\"zero_inflated_beta\", \"hurdle_poisson\" or ",
                  "\"hurdle_negbinomial\") has blocks, which a plot selects ",
                  "with `which`."))
  }
  if (is_bayesnecfit(object)) {
    return(paste0("A plot selects a block of a joint two-block fit with ",
                  "`which`, not `dpar`: `which = \"growth\"` draws the ",
                  "response block, `which = \"survival\"` the survival ",
                  "block, and `which = \"all\"` both beside the combined ",
                  "curve."))
  }
  if (inherits(object, "bayesmanecfit")) {
    return(paste0("A model set is drawn as its model-averaged combined ",
                  "curve, so `", arg, "` cannot select a block. Draw the ",
                  "blocks of one equation with autoplot(pull_out(x, model = ",
                  "\"", names(object$mod_fits)[1], "\"), which = \"all\")."))
  }
  paste0("A ", class(object)[1], " is drawn as the combined curve of each ",
         "level, so `", arg, "` cannot select a block. Estimate a block ",
         "with ecx() or nsec() and `dpar`.")
}

#' The y-axis label of a plot drawn by ggbnec()
#'
#' @param ylab The label supplied, or \code{NULL}.
#' @param object The object being plotted.
#' @param curves The values of \code{which} the plot draws.
#'
#' @return The label, or \code{NULL} where the panel titles name several
#' curves.
#' @noRd
plot_ylab <- function(ylab, object, curves = "combined") {
  if (!is.null(ylab)) {
    return(ylab)
  }
  if (!is_hurdle_family(scale_source_fit(object)$fit$family)) {
    return("Response")
  }
  if (length(curves) > 1) {
    return(NULL)
  }
  hurdle_panel_names[[curves]]
}

#' Panel titles and no-effect labels of a plot of a single fit
#'
#' A one-block fit is titled by its equation. Each curve of a two-block fit is
#' titled by the equation of each block it reads, after the curve's own title
#' where several curves are drawn.
#'
#' @param object A \code{\link{bayesnecfit}}.
#' @param blocks The curves drawn, as \code{plot_blocks()} returns them.
#' @param combined_tag The label \code{summary()} gives the stored no-effect
#' estimate.
#'
#' @return A list of two named \code{\link[base]{character}} vectors,
#' \code{title} and \code{tag}, named as \code{blocks}.
#' @noRd
plot_block_labels <- function(object, blocks, combined_tag) {
  curves <- names(blocks)
  if (!is_hurdle_family(object$fit$family)) {
    return(list(title = c(combined = object$model),
                tag = c(combined = combined_tag)))
  }
  dpar <- hurdle_dpar(object$fit$family)
  survival <- dpar_models(object)[[dpar]]
  if (is.na(survival)) {
    survival <- paste(dpar, "block")
  }
  equations <- c(combined = paste(object$model, survival, sep = " / "),
                 growth = object$model, survival = survival)[curves]
  title <- if (length(curves) > 1) {
    paste0(hurdle_panel_names[curves], "\n", equations)
  } else {
    unname(equations)
  }
  names(title) <- curves
  tag <- vapply(curves, function(w) {
    if (identical(w, "combined")) {
      combined_tag
    } else if (block_has_nec(object, blocks[[w]])) {
      "NEC"
    } else {
      "NSEC"
    }
  }, character(1))
  list(title = title, tag = tag)
}

#' The panel title of a single fit's combined curve
#'
#' @param object A \code{\link{bayesnecfit}}.
#'
#' @return A \code{\link[base]{character}} value.
#' @noRd
plot_title <- function(object) {
  plot_block_labels(object, c(combined = NA_character_), "")$title[[1]]
}

#' The nec parameter of one block of a joint two-block fit
#'
#' @param dpar \code{"mu"}, \code{"hu"} or \code{"zi"}.
#'
#' @return The name of the non-linear parameter.
#' @noRd
block_nec_nlpar <- function(dpar) {
  if (identical(dpar, "mu")) "nec" else paste0(dpar, "nec")
}

#' Whether one block of a joint two-block fit estimates a nec parameter
#'
#' @param object A \code{\link{bayesnecfit}} with a two-block family.
#' @param dpar \code{"mu"}, \code{"hu"} or \code{"zi"}.
#'
#' @return A \code{\link[base]{logical}} value.
#'
#' @importFrom brms variables
#' @noRd
block_has_nec <- function(object, dpar) {
  paste0("b_", block_nec_nlpar(dpar), "_Intercept") %in%
    variables(object$fit)
}

#' The credible band of one curve of a single fit
#'
#' @param object A \code{\link{bayesnecfit}}.
#' @param dpar \code{NULL} for the expected response, or one block of a joint
#' two-block fit.
#' @param x_scale \code{"recorded"} or \code{"fitted"}.
#'
#' @return A \code{\link[base]{data.frame}} with columns \code{x},
#' \code{Estimate}, \code{Q2.5} and \code{Q97.5}.
#' @noRd
plot_block_curve <- function(object, dpar, x_scale) {
  if (is.null(dpar)) {
    # The stored grid keeps rate() curves on the response scale.
    return(if (x_scale == "fitted") {
      plot_fitted_curve(object)
    } else {
      object$pred_vals$data
    })
  }
  grid <- if (x_scale == "fitted") {
    plot_fitted_grid(object)
  } else {
    # The grid of the stored band, so that every panel spans the same range.
    x <- object$pred_vals$data$x
    list(x = x, newdata = bnec_newdata(object, resolution = length(x),
                                       x_range = range(x)))
  }
  plot_curve(object, grid, dpar)
}

#' The observations drawn with one curve of a single fit
#'
#' As \code{autoplot.bayesnechurdlefit()} draws them: the survivors for the
#' growth curve, and the proportion of non-zero responses at each predictor
#' value for the survival curve, within each level of \code{group} where one
#' was selected.
#'
#' @param raw The frame \code{prep_raw_data()} returns.
#' @param which \code{"combined"}, \code{"growth"} or \code{"survival"}.
#'
#' @return A frame with the columns of \code{raw}.
#' @noRd
plot_block_raw <- function(raw, which) {
  if (identical(which, "growth")) {
    return(raw[raw$y_r > 0, , drop = FALSE])
  }
  if (!identical(which, "survival")) {
    return(raw)
  }
  rows <- if (is.null(raw$group)) {
    list(seq_len(nrow(raw)))
  } else {
    split(seq_len(nrow(raw)), raw$group, drop = TRUE)
  }
  parts <- lapply(rows, function(i) {
    obs <- survival_by_x(raw$x_r[i], raw$y_r[i])
    out <- data.frame(x_e = NA, y_e = NA, y_ci = NA, x_r = obs$x,
                      y_r = obs$y)
    if (!is.null(raw$group)) {
      out$group <- raw$group[i[rep(1L, length(obs$x))]]
    }
    out
  })
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out
}

#' A single fit whose no-effect estimate is that of one block
#'
#' @param object A \code{\link{bayesnecfit}} with a two-block family.
#' @param dpar \code{"mu"}, \code{"hu"} or \code{"zi"}.
#'
#' @return \code{object} with \code{ne} and \code{ne_posterior} replaced.
#' @noRd
plot_block_ne_fit <- function(object, dpar) {
  post <- joint_block_ne(object, dpar)
  object$ne_posterior <- post
  object$ne <- estimates_summary(post)
  object
}

#' The no-effect posterior of one block of a joint two-block fit
#'
#' The block's \code{nec} parameter where its equation has one, and otherwise
#' the NSEC of its curve, read as \code{nec(which = "growth")} reads one
#' component of a \code{\link{bayesnechurdlefit}}. A group term whose levels
#' span concentrations is read at each observed level, and the levels are
#' mixed by observation count, as for the stored combined estimate.
#'
#' @param object A \code{\link{bayesnecfit}} with a two-block family.
#' @param dpar \code{"mu"}, \code{"hu"} or \code{"zi"}.
#'
#' @return Draws on the fitted scale, with a \code{"censored"} record.
#'
#' @importFrom brms as_draws_df posterior_linpred
#' @noRd
joint_block_ne <- function(object, dpar) {
  formula <- object$bayesnecformula
  if (!block_has_nec(object, dpar)) {
    args <- recorded_expand_args(list(object))
    return(suppressMessages(nsec(
      object, sig_val = args$sig_val, resolution = args$resolution,
      x_range = args$x_range, xform = identity, posterior = TRUE,
      dpar = dpar
    )))
  }
  nlpar <- block_nec_nlpar(dpar)
  with_record <- function(draws, recorded_x) {
    bounds <- sub_x_transformation(recorded_x, formula)
    bounds <- range(bounds[is.finite(bounds)])
    attr(draws, "censored") <- censoring_record(
      bounds[2], bounds[1], draws >= bounds[2], draws <= bounds[1]
    )
    draws
  }
  spec <- observed_level_spec(object)
  if (is.null(spec)) {
    name <- paste0("b_", nlpar, "_Intercept")
    draws <- as.numeric(as_draws_df(object$fit, variable = name)[[name]])
    return(with_record(draws, object$pred_vals$data$x))
  }
  parts <- lapply(seq_len(nrow(spec$combinations)), function(i) {
    level <- observed_level_fit(object, spec, i)
    nd <- newdata_eval(level, resolution = 2, x_range = NA)$newdata[1, ,
                                                                  drop = FALSE]
    draws <- as.numeric(posterior_linpred(
      object$fit, newdata = nd, nlpar = nlpar,
      re_formula = spec$re_formula, transform = TRUE
    )[, 1])
    with_record(draws, range(level$level_x))
  })
  pool_observed_draws(parts, spec)
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
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#'
#' @noRd
#'
#' @export
ggbnec_data.bayesmanecfit <- function(x, add_nec = TRUE, add_ecx = FALSE,
                                      xform = NULL, ..., group = NULL,
                                      x_scale = c("recorded", "fitted"),
                                      which = "combined") {
  chk_lgl(add_nec)
  chk_lgl(add_ecx)
  # Check xform as the bayesnecfit method does. Otherwise mutate() fails with
  # an error that names neither the argument nor the remedy.
  check_xform(xform)
  check_plot_single_curve(which, x)
  check_plot_dpar(list(...), x)
  if (add_ecx) {
    check_plot_relative(list(...), x)
  }
  warn_stale_observed_display(x)
  warn_stale_ne_type(x)
  x_scale <- plot_x_scale(x, x_scale, xform)
  e_df <- if (x_scale == "fitted") plot_fitted_curve(x) else x$w_pred_vals$data
  e_df <- data.frame(x_e = c(e_df$x, rev(e_df$x)),
                     y_e = c(e_df$Estimate, rep(NA, nrow(e_df))),
                     y_ci = c(e_df$Q2.5, rev(e_df$Q97.5)),
                     x_r = NA, y_r = NA)
  r_df <- prep_raw_data(x$mod_fits[[1]]$fit,
                        x$mod_fits[[1]]$bayesnecformula, group = group,
                        retained_data = x$retained_data)
  if (!is.null(group)) {
    e_df$group <- r_df$group[rep(NA_integer_, nrow(e_df))]
  }
  bdat <- model.frame(x$mod_fits[[1]]$bayesnecformula, 
                      data = x$mod_fits[[1]]$fit$data, 
                      run_par_checks = TRUE)
  manec_formula <- x$mod_fits[[1]]$bayesnecformula
  x_grid_raw <- x$w_pred_vals$data$x
  out <- rbind(e_df, r_df)
  if (x_scale == "fitted") {
    out$x_r <- as.numeric(sub_x_transformation(out$x_r, manec_formula))
  }
  # As in the bayesnecfit method.
  recorded <- x_scale == "recorded" && plot_reads_recorded(bdat, xform)
  axis_xform <- if (is.null(xform)) identity else xform
  if (!pop_var_is_transformed(bdat, "x_var")) {
    out <- out |>
      mutate(x_e = axis_xform(.data$x_e), x_r = axis_xform(.data$x_r))
  }
  if (add_nec) {
    ne_vals <- if (recorded) {
      summary_ne_vals(x, NULL)
    } else if (x_scale == "fitted") {
      x$w_ne
    } else {
      to_axis_scale(x$w_ne, bdat, manec_formula, x_grid_raw, axis_xform)
    }
    out <- bind_nec(out, ne_vals, if (x_scale == "fitted") {
      fitted_axis_labels(ne_vals, x, x_grid_raw)
    } else {
      ne_vals
    })
  }
  if (add_ecx) {
    ecx_vals <- plot_ecx(x, x$mod_fits[[1]]$fit$family$family, list(...),
                         if (recorded) NULL else identity)
    if (!recorded && x_scale != "fitted") {
      ecx_vals <- to_axis_scale(ecx_vals, bdat, manec_formula, x_grid_raw,
                                axis_xform)
    }
    out <- bind_ecx(out, ecx_vals, if (x_scale == "fitted") {
      fitted_axis_labels(ecx_vals, x, x_grid_raw)
    } else {
      ecx_vals
    })
  }
  if (!is.null(group)) {
    attr(out, "group_var") <- group
    attr(out, "group_fitted") <- attr(r_df, "group_fitted")
  }
  if (x_scale == "fitted") {
    attr(out, "x_axis") <- plot_x_axis(r_df$x_r, manec_formula)
  }
  out
}

#' Creates the data.frame for plotting a grouped set of fits
#'
#' @inheritParams ggbnec_data
#'
#' @param x An object of class \code{\link{bayesnecgroupfit}}, as returned by
#' \code{\link{bnec_group}}.
#'
#' @inherit ggbnec_data return examples
#'
#' @method ggbnec_data bayesnecgroupfit
#'
#' @export
ggbnec_data.bayesnecgroupfit <- function(x, add_nec = TRUE, add_ecx = FALSE,
                                         xform = NULL, ..., group = NULL,
                                         x_scale = c("recorded", "fitted"),
                                         which = "combined") {
  check_plot_single_curve(which, x)
  check_plot_dpar(list(...), x)
  if (isTRUE(add_ecx)) {
    check_plot_relative(list(...), x)
  }
  x_scale <- plot_x_scale(x, x_scale, xform)
  panel_group <- is.null(group) || identical(group, x$group_var)
  pieces <- once_stale_ne_warning(x, lapply(seq_along(x$fits), function(i) {
    out <- ggbnec_data(x$fits[[i]], add_nec = add_nec, add_ecx = add_ecx,
                       xform = xform, x_scale = x_scale, ...,
                       group = if (panel_group) NULL else group)
    out$panel <- factor(rep(x$levels[i], nrow(out)), levels = x$levels)
    if (panel_group) {
      out$group <- out$panel
    }
    out
  }))
  group_fitted <- if (panel_group) TRUE else attr(pieces[[1]], "group_fitted")
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  attr(out, "group_var") <- if (panel_group) x$group_var else group
  attr(out, "group_fitted") <- group_fitted
  attr(out, "panel_var") <- x$group_var
  if (x_scale == "fitted") {
    source <- scale_source_fit(x)
    variable <- inline_x_transform(x)$variable
    observed <- unlist(lapply(x$fits, function(part) {
      fit <- scale_source_fit(part)
      fit$fit$data[[variable]]
    }))
    attr(out, "x_axis") <- plot_x_axis(observed, source$bayesnecformula)
  }
  out
}

#' @rdname autoplot
#' @order 4
#'
#' @method autoplot bayesnecgroupfit
#'
#' @inherit autoplot description return examples
#'
#' @importFrom chk chk_lgl
#'
#' @export
autoplot.bayesnecgroupfit <- function(object, ..., nec = TRUE, ecx = FALSE,
                                      xform = NULL, group = NULL,
                                      group_aes = c("line", "colour"),
                                      point_alpha = 1,
                                      x_scale = c("recorded", "fitted"),
                                      xlab = "Predictor", ylab = NULL) {
  chk_lgl(nec)
  chk_lgl(ecx)
  group_aes <- match.arg(group_aes)
  check_xform(xform)
  check_point_alpha(point_alpha)
  x_scale <- plot_x_scale(object, x_scale, xform)
  dat <- ggbnec_data(object, add_nec = nec, add_ecx = ecx,
                     xform = xform, x_scale = x_scale, group = group, ...)
  x_axis <- attr(dat, "x_axis")
  tags <- vapply(object$fits, function(fit) {
    summ <- summary(fit, ecx = FALSE, xform = identity) |>
      suppressWarnings() |>
      suppressMessages()
    rownames(summ$nec_vals)[1]
  }, character(1))
  names(tags) <- object$levels
  panel_labs <- paste0(object$group_var, " = ", object$levels)
  names(panel_labs) <- object$levels
  level <- as.character(dat$panel)
  dat$model <- factor(unname(panel_labs[level]), levels = unname(panel_labs))
  dat$tag <- unname(tags[level])
  show_group <- !is.null(group) && !identical(group, object$group_var)
  group_label <- if (show_group) {
    plot_group_label(dat, group, group_aes)
  } else {
    NULL
  }
  ggbnec(dat, nec = nec, ecx = ecx, group = show_group,
         group_aes = group_aes, group_label = group_label,
         group_fitted = !identical(attr(dat, "group_fitted"), FALSE),
         point_alpha = point_alpha, x_axis = x_axis, xlab = xlab,
         ylab = plot_ylab(ylab, object))
}

#' Refuse a point_alpha that is not a single number from 0 to 1
#'
#' @param point_alpha The value supplied to \code{autoplot()}.
#'
#' @return \code{NULL}, invisibly.
#'
#' @importFrom chk chk_number chk_range
#'
#' @noRd
check_point_alpha <- function(point_alpha) {
  chk_number(point_alpha)
  chk_range(point_alpha, c(0, 1))
  invisible(NULL)
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
#' @importFrom ggplot2 ggplot geom_polygon aes geom_line geom_point stat_summary
#' @importFrom ggplot2 geom_vline geom_text theme_classic facet_wrap theme
#' @importFrom ggplot2 element_text element_blank element_rect labs
#' @importFrom ggplot2 scale_colour_discrete scale_fill_discrete
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @noRd
ggbnec <- function(x, nec = TRUE, ecx = FALSE, group = FALSE,
                   group_aes = c("line", "colour"), group_label = NULL,
                   group_fitted = TRUE, point_alpha = 1,
                   x_axis = NULL, xlab = "Predictor", ylab = "Response",
                   facet_scales = "free", facet_ncol = 2) {
  group_aes <- match.arg(group_aes)
  out <- ggplot() +
    geom_polygon(data = x |> filter(!is.na(.data$y_ci)),
                 mapping = aes(x = .data$x_e, y = .data$y_ci),
                 fill = "grey75", alpha = 0.5)
  raw <- x |> filter(!is.na(.data$y_r))
  if (group && group_fitted) {
    # Character values omit unused factor levels. Keeping them would add NA to
    # the selected names below and could pass an absent level into a layer.
    n_x <- tapply(raw$x_r, as.character(raw$group), function(value) {
      length(unique(value))
    })
    spanning <- names(n_x)[n_x > 1]
    single_x <- names(n_x)[n_x <= 1]
    if (length(spanning) > 0) {
      spanning_data <- raw[
        as.character(raw$group) %in% spanning, , drop = FALSE
      ]
      if (group_aes == "colour") {
        out <- out + stat_summary(
          data = spanning_data,
          mapping = aes(x = .data$x_r, y = .data$y_r,
                        group = .data$group, colour = .data$group),
          fun = mean, geom = "line", linewidth = 0.3
        )
      } else {
        out <- out + stat_summary(
          data = spanning_data,
          mapping = aes(x = .data$x_r, y = .data$y_r,
                        group = .data$group),
          fun = mean, geom = "line", colour = "grey40", linewidth = 0.3
        )
      }
    }
    if (length(single_x) > 0) {
      single_x_data <- raw[
        as.character(raw$group) %in% single_x, , drop = FALSE
      ]
      if (group_aes == "colour") {
        out <- out + stat_summary(
          data = single_x_data,
          mapping = aes(x = .data$x_r, y = .data$y_r,
                        group = .data$group, fill = .data$group),
          fun = mean, geom = "point", shape = 23, size = 2
        )
      } else {
        out <- out + stat_summary(
          data = single_x_data,
          mapping = aes(x = .data$x_r, y = .data$y_r,
                        group = .data$group),
          fun = mean, geom = "point", shape = 23, fill = "white", size = 2
        )
      }
    }
  }
  out <- out +
    geom_line(data = x |> filter(!is.na(.data$y_e)),
              mapping = aes(x = .data$x_e, y = .data$y_e),
              colour = "black", linetype = 2)
  if (group && group_aes == "colour") {
    group_levels <- if (is.factor(raw$group)) {
      levels(droplevels(raw$group))
    } else {
      sort(unique(as.character(raw$group)), na.last = NA)
    }
    out <- out +
      geom_point(data = raw,
                 mapping = aes(x = .data$x_r, y = .data$y_r,
                               fill = .data$group), shape = 21,
                 alpha = point_alpha) +
      scale_colour_discrete(limits = group_levels) +
      scale_fill_discrete(limits = group_levels) +
      labs(colour = group_label, fill = group_label)
  } else {
    out <- out +
      geom_point(data = raw,
                 mapping = aes(x = .data$x_r, y = .data$y_r),
                 fill = "grey30", shape = 21, alpha = point_alpha)
  }
  if (nec) {
    ltys <- rep(c(1, 2, 2), length(unique(x$model)))
    lwds <- rep(c(0.5, 0.2, 0.2), length(unique(x$model)))
    out <- out +
      geom_vline(data = x |> filter(!is.na(.data$nec_vals)),
                 mapping = aes(xintercept = .data$nec_vals),
                 linetype = ltys, colour = "grey50",
                 lwd = lwds) +
      geom_text(data = x |> filter(!is.na(.data$nec_labs)),
                mapping = aes(
                  label = paste0(
                    .data$tag, ": ", .data$nec_labs, " (", .data$nec_labs_l,
                    "-", .data$nec_labs_u, ")"
                  )
                ), x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, size = 3,
                colour = "grey50")
  }
  if (ecx) {
    ltys <- rep(c(1, 2, 2), length(unique(x$model)))
    lwds <- rep(c(0.5, 0.2, 0.2), length(unique(x$model)))
    out <- out +
      geom_vline(data = x |> filter(!is.na(.data$ecx_vals)),
                 mapping = aes(xintercept = .data$ecx_vals),
                 linetype = ltys, colour = "dodgerblue4",
                 lwd = lwds) +
      geom_text(data = x |> filter(!is.na(.data$ecx_labs)),
                mapping = aes(label = paste0("EC[", .data$ecx_int, "]", ": ",
                                             .data$ecx_labs, " (",
                                             .data$ecx_labs_l, "-",
                                             .data$ecx_labs_u, ")")),
                x = Inf, y = Inf, hjust = 1.1, vjust = 5.5, size = 3,
                colour = "dodgerblue4")
  }
  axis_scale <- if (is.null(x_axis)) {
    scale_x_continuous(labels = function(x) signif(x, 2))
  } else {
    scale_x_continuous(breaks = x_axis$breaks, labels = x_axis$labels)
  }
  out +
    axis_scale +
    theme_classic() +
    facet_wrap(~.data$model, scales = facet_scales, ncol = facet_ncol) +
    theme(strip.text = element_text(hjust = 0),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(colour = NA, fill = NA)) +
    labs(x = xlab, y = ylab)
}
