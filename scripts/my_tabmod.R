mytabmod <- function (..., transform, show.intercept = TRUE, show.est = TRUE, 
    show.ci = 0.95, show.ci50 = FALSE, show.se = NULL, show.std = NULL, 
    show.p = TRUE, show.stat = FALSE, show.df = FALSE, show.zeroinf = TRUE, 
    show.r2 = TRUE, show.icc = TRUE, show.re.var = TRUE, show.ngroups = TRUE, 
    show.fstat = FALSE, show.aic = FALSE, show.aicc = FALSE, 
    show.dev = FALSE, show.loglik = FALSE, show.obs = TRUE, show.reflvl = FALSE, 
    terms = NULL, rm.terms = NULL, order.terms = NULL, keep = NULL, 
    drop = NULL, title = NULL, pred.labels = NULL, dv.labels = NULL, 
    wrap.labels = 25, bootstrap = FALSE, iterations = 1000, seed = NULL, 
    robust = FALSE, vcov.fun = NULL, vcov.type = c("HC3", "const", 
        "HC", "HC0", "HC1", "HC2", "HC4", "HC4m", "HC5", "CR0", 
        "CR1", "CR1p", "CR1S", "CR2", "CR3"), vcov.args = NULL, 
    string.pred = "Predictors", string.est = "Estimate", string.std = "std. Beta", 
    string.ci = "CI", string.se = "std. Error", string.std_se = "standardized std. Error", 
    string.std_ci = "standardized CI", string.p = "p", string.std.p = "std. p", 
    string.df = "df", string.stat = "Statistic", string.std.stat = "std. Statistic", 
    string.resp = "Response", string.intercept = "(Intercept)", 
    strings = NULL, ci.hyphen = "&nbsp;&ndash;&nbsp;", minus.sign = "&#45;", 
    collapse.ci = FALSE, collapse.se = FALSE, linebreak = TRUE, 
    col.order = c("est", "se", "std.est", "std.se", "ci", "std.ci", 
        "ci.inner", "ci.outer", "stat", "std.stat", "p", "std.p", 
        "df.error", "response.level"), digits = 2, digits.p = 3, 
    digits.rsq = 3, digits.re = 2, emph.p = TRUE, p.val = NULL, 
    df.method = NULL, p.style = c("numeric", "stars", "numeric_stars", 
        "scientific", "scientific_stars"), p.threshold = c(0.05, 
        0.01, 0.001), p.adjust = NULL, case = "parsed", auto.label = TRUE, 
    prefix.labels = c("none", "varname", "label"), bpe = "median", 
    CSS = css_theme("regression"), file = NULL, use.viewer = TRUE, 
    encoding = "UTF-8", r2.fun = performance::r2) {
    
    if (!missing(df.method)) {
        p.val <- df.method
    }
    if (!is.null(p.val)) {
        p.val <- match.arg(p.val, choices = c("wald", "profile", 
            "kenward", "kr", "satterthwaite", "ml1", "betwithin"))
    }
    p.style <- match.arg(p.style)
    prefix.labels <- match.arg(prefix.labels)
    vcov.type <- match.arg(vcov.type)
    change_string_est <- !missing(string.est)
    if (missing(case)) {
        if (prefix.labels == "none" && !show.reflvl) 
            case <- "parsed"
        else case <- NULL
    }
    if (p.style == "stars") 
        show.p <- FALSE
    if (isTRUE(robust)) {
        vcov.type <- "HC3"
        vcov.fun <- "vcovHC"
    }
    vcov.fun <- sjPlot:::check_se_argument(se = vcov.fun, type = NULL)
    models <- list(...)
    if (length(class(models[[1]])) == 1 && class(models[[1]]) == 
        "list") 
        models <- lapply(models[[1]], function(x) x)
    names(models) <- unlist(lapply(match.call(expand.dots = F)$..., 
        function(.x) deparse(.x, width.cutoff = 500L)))
    auto.transform <- missing(transform)
    ci.lvl <- ifelse(is.null(show.ci), 0.95, show.ci)
    copos <- which("est" == col.order)
    if (!sjmisc::is_empty(copos)) 
        col.order[copos] <- "estimate"
    copos <- which("se" == col.order)
    if (!sjmisc::is_empty(copos)) 
        col.order[copos] <- "std.error"
    copos <- which("ci" == col.order)
    if (!sjmisc::is_empty(copos)) 
        col.order[copos] <- "conf.int"
    copos <- which("std.est" == col.order)
    if (!sjmisc::is_empty(copos)) 
        col.order[copos] <- "std.estimate"
    copos <- which("std.se" == col.order)
    if (!sjmisc::is_empty(copos)) 
        col.order[copos] <- "std.se"
    copos <- which("std.ci" == col.order)
    if (!sjmisc::is_empty(copos)) 
        col.order[copos] <- "std.conf.int"
    copos <- which("p" == col.order)
    if (!sjmisc::is_empty(copos)) 
        col.order[copos] <- "p.value"
    copos <- which("std.p" == col.order)
    if (!sjmisc::is_empty(copos)) 
        col.order[copos] <- "std.p.value"
    copos <- which("stat" == col.order)
    if (!sjmisc::is_empty(copos)) 
        col.order[copos] <- "statistic"
    copos <- which("std.stat" == col.order)
    if (!sjmisc::is_empty(copos)) 
        col.order[copos] <- "std.statistic"
    if (!sjmisc::is_empty(strings) && !is.null(names(strings))) {
        s.names <- names(strings)
        if ("pred" %in% s.names) 
            string.pred <- strings[["pred"]]
        if ("est" %in% s.names) 
            string.est <- strings[["est"]]
        if ("std" %in% s.names) 
            string.std <- strings[["std"]]
        if ("ci" %in% s.names) 
            string.ci <- strings[["ci"]]
        if ("se" %in% s.names) 
            string.se <- strings[["se"]]
        if ("std_se" %in% s.names) 
            string.std_se <- strings[["std_se"]]
        if ("std_ci" %in% s.names) 
            string.std_ci <- strings[["std_ci"]]
        if ("p" %in% s.names) 
            string.p <- strings[["p"]]
        if ("std.p" %in% s.names) 
            string.std.p <- strings[["std.p"]]
        if ("df" %in% s.names) 
            string.df <- strings[["df"]]
        if ("stat" %in% s.names) 
            string.stat <- strings[["stat"]]
        if ("std.stat" %in% s.names) 
            string.std.stat <- strings[["std.stat"]]
        if ("resp" %in% s.names) 
            string.resp <- strings[["resp"]]
        if ("intercept" %in% s.names) 
            string.intercept <- strings[["intercept"]]
    }
    model.list <- purrr::map2(models, 1:length(models), function(model, 
        i) {
        fam.info <- insight::model_info(model)
        if (insight::is_multivariate(model)) 
            fam.info <- fam.info[[1]]
        if (auto.transform) {
            if (fam.info$is_linear || identical(fam.info$link_function, 
                "identity")) 
                transform <- NULL
            else transform <- "exp"
        }
        dat <- sjPlot:::tidy_model(model = model, ci.lvl = ci.lvl, tf = transform, 
            type = "est", bpe = bpe, robust = list(vcov.fun = vcov.fun, 
                vcov.type = vcov.type, vcov.args = vcov.args), 
            facets = FALSE, show.zeroinf = show.zeroinf, p.val = p.val, 
            bootstrap = bootstrap, iterations = iterations, seed = seed, 
            p_adjust = p.adjust, keep = keep, drop = drop)
        if (!sjPlot:::is.stan(model) && !is.null(transform)) {
            funtrans <- match.fun(transform)
            dat[["estimate"]] <- funtrans(dat[["estimate"]])
            dat[["conf.low"]] <- funtrans(dat[["conf.low"]])
            dat[["conf.high"]] <- funtrans(dat[["conf.high"]])
            dat[["std.error"]] <- dat[["std.error"]] * dat[["estimate"]]
        }
        dat <- dat %>% dplyr::mutate(conf.int = sprintf("%.*f%s%.*f", 
            digits, .data$conf.low, ci.hyphen, digits, .data$conf.high)) %>% 
            dplyr::select(-.data$conf.low, -.data$conf.high)
        if (sjPlot:::is.stan(model)) {
            dat <- dat %>% sjmisc::var_rename(conf.int = "ci.outer") %>% 
                dplyr::mutate(ci.inner = sprintf("%.*f%s%.*f", 
                  digits, .data$conf.low50, ci.hyphen, digits, 
                  .data$conf.high50)) %>% dplyr::select(-.data$conf.low50, 
                -.data$conf.high50)
        }
        if (!is.null(show.std) && !sjPlot:::is.stan(model)) {
            std_method <- switch(show.std, std = "refit", std2 = "2sd", 
                "")
            tmp_dat <- sjPlot:::tidy_model(model = model, ci.lvl = ci.lvl, 
                tf = transform, type = "est", bpe = bpe, robust = list(vcov.fun = vcov.fun, 
                  vcov.type = vcov.type, vcov.args = vcov.args), 
                facets = FALSE, show.zeroinf = show.zeroinf, 
                p.val = p.val, p_adjust = p.adjust, standardize = std_method, 
                bootstrap = bootstrap, iterations = iterations, 
                seed = seed, keep = keep, drop = drop) %>% sjPlot:::format_p_values(p.style, 
                digits.p, emph.p, p.threshold) %>% sjmisc::var_rename(estimate = "std.estimate", 
                std.error = "std.se", conf.low = "std.conf.low", 
                conf.high = "std.conf.high", p.value = "std.p.value", 
                statistic = "std.statistic", p.stars = "std.p.stars") %>% 
                dplyr::select(-1)
            if (!sjPlot:::is.stan(model) && !is.null(transform)) {
                funtrans <- match.fun(transform)
                tmp_dat[["std.estimate"]] <- funtrans(tmp_dat[["std.estimate"]])
                tmp_dat[["std.conf.low"]] <- funtrans(tmp_dat[["std.conf.low"]])
                tmp_dat[["std.conf.high"]] <- funtrans(tmp_dat[["std.conf.high"]])
                tmp_dat[["std.se"]] <- tmp_dat[["std.se"]] * 
                  tmp_dat[["std.estimate"]]
            }
            dat <- tmp_dat %>% sjmisc::add_columns(dat) %>% dplyr::mutate(std.conf.int = sprintf("%.*f%s%.*f", 
                digits, .data$std.conf.low, ci.hyphen, digits, 
                .data$std.conf.high)) %>% dplyr::select(-.data$std.conf.low, 
                -.data$std.conf.high)
            if (all(round(dat$statistic[-1], 3) == round(dat$std.statistic[-1], 
                3))) {
                dat <- dat %>% dplyr::select(-.data$std.statistic, 
                  -.data$std.p.value)
            }
        }
        dat <- sjPlot:::format_p_values(dat, p.style, digits.p, emph.p, 
            p.threshold)
        if (grepl("stars", p.style)) {
            if (obj_has_name(dat, "estimate")) 
                dat$estimate <- sprintf("%.*f <sup>%s</sup>", 
                  digits, dat$estimate, dat$p.stars)
            if (!show.est && obj_has_name(dat, "std.estimate")) {
                dat$std.estimate <- sprintf("%.*f <sup>%s</sup>", 
                  digits, dat$std.estimate, dat$std.p.stars)
                dat <- dplyr::select(dat, -.data$std.p.stars)
            }
        }
        dat <- dplyr::select(dat, -.data$p.stars)
        dat <- dat[, sjPlot:::sort_columns(colnames(dat), sjPlot:::is.stan(model), 
            col.order)]
        cn <- colnames(dat)[2:ncol(dat)]
        colnames(dat)[2:ncol(dat)] <- sprintf("%s_%i", cn, i)
        dat <- dat %>% purrr::map_if(is.numeric, ~sprintf("%.*f", 
            digits, .x)) %>% as.data.frame(stringsAsFactors = FALSE)
        if (!show.ci50) 
            dat <- dplyr::select(dat, -sjPlot:::string_starts_with("ci.inner", 
                colnames(dat)))
        if (collapse.ci) {
            if (linebreak) 
                lb <- "<br>"
            else lb <- " "
            est.cols <- sjPlot:::string_starts_with("estimate", x = colnames(dat))
            dat[[est.cols]] <- sprintf("%s%s(%s)", dat[[est.cols]], 
                lb, dat[[est.cols + 2]])
            if (!sjmisc::is_empty(sjPlot:::string_starts_with("ci", x = colnames(dat)))) {
                if (isTRUE(show.ci50)) {
                  dat <- dplyr::select(dat, -sjPlot:::string_starts_with("ci.inner", 
                    x = colnames(dat)))
                  dat[[est.cols]] <- sprintf("%s%s(%s)", dat[[est.cols]], 
                    lb, dat[[est.cols + 2]])
                }
                dat <- dplyr::select(dat, -sjPlot:::string_starts_with("ci.outer", 
                  x = colnames(dat)))
            }
            else {
                dat <- dplyr::select(dat, -sjPlot:::string_starts_with("conf.int", 
                  x = colnames(dat)))
            }
            std.cols <- sjPlot:::string_starts_with("std.estimate", x = colnames(dat))
            if (!sjmisc::is_empty(std.cols)) {
                dat[[std.cols]] <- sprintf("%s%s(%s)", dat[[std.cols]], 
                  lb, dat[[std.cols + 2]])
                dat <- dplyr::select(dat, -sjPlot:::string_starts_with("std.conf.int", 
                  x = colnames(dat)))
            }
        }
        if (collapse.se) {
            if (linebreak) 
                lb <- "<br>"
            else lb <- " "
            est.cols <- sjPlot:::string_starts_with("estimate", x = colnames(dat))
            dat[[est.cols]] <- sprintf("%s%s(%s)", dat[[est.cols]], 
                lb, dat[[est.cols + 1]])
            dat <- dplyr::select(dat, -sjPlot:::string_starts_with("std.error", 
                x = colnames(dat)))
            std.cols <- sjPlot:::string_starts_with("std.estimate", x = colnames(dat))
            if (!sjmisc::is_empty(std.cols)) {
                dat[[std.cols]] <- sprintf("%s%s(%s)", dat[[std.cols]], 
                  lb, dat[[std.cols + 1]])
                dat <- dplyr::select(dat, -sjPlot:::string_starts_with("std.se", 
                  x = colnames(dat)))
            }
        }
        dat[] <- lapply(dat, function(i) gsub("-(\\d)(.*)", paste0(minus.sign, 
            "\\1\\2"), i))
        zidat <- NULL
        wf <- sjPlot:::string_starts_with("wrap.facet", x = colnames(dat))
        if (!sjmisc::is_empty(wf)) {
            zi <- which(dat[[wf]] %in% c("Zero-Inflated Model", 
                "Zero Inflation Model", "zero_inflated", "zi"))
            if (show.zeroinf && !sjmisc::is_empty(zi)) {
                zidat <- dat %>% dplyr::slice(!!zi) %>% dplyr::select(!!-wf)
            }
            if (!sjmisc::is_empty(zi)) 
                dat <- dplyr::slice(dat, !!-zi)
            dat <- dplyr::select(dat, !!-wf)
        }
        n_obs <- NULL
        if (show.obs) {
            n_obs <- sjPlot:::get_observations(model)
        }
        vars <- vars_brms <- NULL
        if ((show.icc || show.re.var || show.r2) && sjPlot:::is_mixed_model(model)) {
            if (inherits(model, "brmsfit")) {
                vars <- suppressWarnings(insight::get_variance(model))
                if (is.null(vars)) {
                  vars_brms <- tryCatch({
                    performance::variance_decomposition(model)
                  }, error = function(e) {
                    NULL
                  })
                  if (!is.null(vars_brms)) {
                    vars$var.intercept <- attr(vars_brms, "var_rand_intercept")
                    vars$var.residual <- attr(vars_brms, "var_residual")
                  }
                }
            }
            else {
                vars <- suppressWarnings(insight::get_variance(model))
            }
        }
        else {
            vars <- NULL
        }
        if (!is.null(vars) && length(vars) == 1 && is.na(vars)) 
            vars <- NULL
        icc <- NULL
        if (show.icc && sjPlot:::is_mixed_model(model) && !is.null(vars) && 
            !all(is.na(vars))) {
            if (inherits(model, "brmsfit") && !is.null(vars_brms)) {
                icc <- list(icc.adjusted = vars_brms$ICC_decomposed)
            }
            else {
                icc <- list(icc.adjusted = vars$var.random/(vars$var.random + 
                  vars$var.residual))
            }
        }
        rsq <- NULL
        if (show.r2 && !insight::is_multivariate(model)) {
            if (sjPlot:::is_mixed_model(model)) {
                if (inherits(model, "brmsfit")) {
                  rsqdummy <- tryCatch({
                    suppressWarnings(performance::r2(model))
                  }, error = function(x) {
                    NULL
                  })
                  if (!is.null(rsqdummy)) {
                    rsq <- list(`Marginal R2` = rsqdummy$R2_Bayes_marginal, 
                      `Conditional R2` = rsqdummy$R2_Bayes)
                  }
                }
                else if (is.list(r2.fun)) {
                  rsq <- tryCatch({
                    suppressWarnings(r2.fun[[i]](model))
                  }, error = function(x) {
                    NULL
                  })
                  if (!is.null(rsq)) {
                    rnames <- sub("_", " ", names(rsq))
                    names(rsq) <- rnames
                  }
                }
                else if (!is.null(vars)) {
                  if (is.null(vars$var.random)) {
                    rsq <- list(`Marginal R2` = vars$var.fixed/(vars$var.fixed + 
                      vars$var.residual), `Conditional R2` = NA)
                  }
                  else {
                    rsq <- list(`Marginal R2` = vars$var.fixed/(vars$var.fixed + 
                      vars$var.random + vars$var.residual), `Conditional R2` = (vars$var.fixed + 
                      vars$var.random)/(vars$var.fixed + vars$var.random + 
                      vars$var.residual))
                  }
                }
            }
            else if (is.list(r2.fun)) {
                rsq <- tryCatch({
                  suppressWarnings(r2.fun[[i]](model))
                }, error = function(x) {
                  NULL
                })
                if (!is.null(rsq)) {
                  rnames <- sub("_", " ", names(rsq))
                  names(rsq) <- rnames
                }
            }
            else {
                rsq <- tryCatch({
                  suppressWarnings(r2.fun(model))
                }, error = function(x) {
                  NULL
                })
                if (!is.null(rsq)) {
                  rnames <- sub("_", " ", names(rsq))
                  names(rsq) <- rnames
                }
            }
        }
        n_re_grps <- NULL
        if (show.ngroups && sjPlot:::is_mixed_model(model)) {
            rand_eff <- insight::get_data(model)[, insight::find_random(model, 
                split_nested = TRUE, flatten = TRUE), drop = FALSE]
            n_re_grps <- sapply(rand_eff, function(.i) length(unique(.i, 
                na.rm = TRUE)))
            names(n_re_grps) <- sprintf("ngrps.%s", names(n_re_grps))
        }
        dev <- NULL
        if (show.dev) 
            dev <- model_deviance(model)
        aic <- NULL
        if (show.aic) 
            aic <- sjPlot:::model_aic(model)
        aicc <- NULL
        if (show.aicc) 
            aicc <- sjPlot:::model_aicc(model)
        loglik <- NULL
        if (show.loglik) 
            loglik <- model_loglik(model)
        if (inherits(model, "brmsfit")) {
            dat$term <- gsub("^b_", "", dat$term)
            if (!is.null(zidat)) 
                zidat$term <- gsub("^b_", "", zidat$term)
        }
        if (string.intercept != "(Intercept)") {
            intercepts <- which(dat$term == "(Intercept)")
            if (!sjmisc::is_empty(intercepts)) {
                dat$term[intercepts] <- string.intercept
            }
            if (!is.null(zidat)) {
                intercepts <- which(zidat$term == "(Intercept)")
                if (!sjmisc::is_empty(intercepts)) {
                  zidat$term[intercepts] <- string.intercept
                }
            }
        }
        list(dat = dat, transform = transform, zeroinf = zidat, 
            rsq = rsq, n_obs = n_obs, icc = icc, dev = dev, aic = aic, 
            variances = vars, n_re_grps = n_re_grps, loglik = loglik, 
            aicc = aicc)
    })
    na.vals <- c("NA", sprintf("NA%sNA", ci.hyphen), sprintf("NA (NA%sNA)", 
        ci.hyphen), sprintf("NA (NA%sNA) (NA)", ci.hyphen))
    model.data <- purrr::map(model.list, ~.x[[1]])
    transform.data <- purrr::map(model.list, ~.x[[2]])
    zeroinf.data <- purrr::map(model.list, ~.x[[3]])
    rsq.data <- purrr::map(model.list, ~.x[[4]])
    n_obs.data <- purrr::map(model.list, ~.x[[5]])
    icc.data <- purrr::map(model.list, ~.x[[6]])
    dev.data <- purrr::map(model.list, ~.x[[7]])
    aic.data <- purrr::map(model.list, ~.x[[8]])
    variance.data <- purrr::map(model.list, ~.x[[9]])
    ngrps.data <- purrr::map(model.list, ~.x[[10]])
    loglik.data <- purrr::map(model.list, ~.x[[11]])
    aicc.data <- purrr::map(model.list, ~.x[[12]])
    is.zeroinf <- purrr::map_lgl(model.list, ~!is.null(.x[[3]]))
    zeroinf.data <- purrr::compact(zeroinf.data)
    if (!show.zeroinf) 
        zeroinf.data <- NULL
    model.data <- purrr::map(model.data, function(.x) {
        resp.col <- sjPlot:::string_starts_with("response.level", x = colnames(.x))
        if (!sjmisc::is_empty(resp.col)) 
            .x[order(match(.x[[resp.col]], unique(.x[[resp.col]]))), 
                ]
        else .x
    })
    show.response <- TRUE
    if (length(model.data) == 1) {
        fi <- insight::model_info(models[[1]])
        if (insight::is_multivariate(models[[1]])) 
            fi <- fi[[1]]
        if (insight::is_multivariate(models[[1]]) || fi$is_categorical) {
            show.response <- FALSE
            if (fi$is_categorical) {
                dv.labels <- sprintf("%s: %s", insight::find_response(models[[1]]), 
                  unique(model.data[[1]][["response.level_1"]]))
                model.data <- split(model.data[[1]], model.data[[1]]["response.level_1"])
            }
            else {
                dv.labels <- insight::find_response(models[[1]])
                model.data <- split(model.data[[1]], model.data[[1]]["response.level_1"])
                dv.labels <- dv.labels[match(names(dv.labels), 
                  names(model.data))]
                dv.labels <- sjmisc::word_wrap(dv.labels, wrap = wrap.labels, 
                  linesep = "<br>")
            }
            model.data <- purrr::map2(model.data, 1:length(model.data), 
                function(x, y) {
                  colnames(x) <- gsub(pattern = "_1", replacement = sprintf("_%i", 
                    y), x = colnames(x))
                  x
                })
        }
    }
    dat <- model.data %>% purrr::reduce(~dplyr::full_join(.x, 
        .y, by = "term")) %>% purrr::map_df(~dplyr::if_else(.x %in% 
        na.vals | is.na(.x), "", .x))
    dat <- sjPlot:::remove_unwanted(dat, show.intercept, show.est, show.std, 
        show.ci, show.se, show.stat, show.p, show.df, show.response, 
        terms, rm.terms)
    zeroinf <- NULL
    if (!sjmisc::is_empty(zeroinf.data)) {
        zeroinf <- zeroinf.data %>% purrr::reduce(~dplyr::full_join(.x, 
            .y, by = "term")) %>% purrr::map_df(~dplyr::if_else(.x %in% 
            na.vals | is.na(.x), "", .x))
        zeroinf <- sjPlot:::remove_unwanted(zeroinf, show.intercept, show.est, 
            show.std, show.ci, show.se, show.stat, show.p, show.df, 
            show.response, terms, rm.terms)
    }
    if (isTRUE(auto.label) && sjmisc::is_empty(pred.labels)) {
        if (sjPlot:::.labelled_model_data(models) || any(sapply(models, 
            sjPlot:::is.stan)) || isTRUE(show.reflvl)) {
            pred.labels <- sjlabelled::term_labels(models, case = case, 
                mark.cat = TRUE, prefix = prefix.labels)
            category.values <- attr(pred.labels, "category.value")
            re_terms <- unlist(sapply(models, insight::find_predictors, 
                effects = "random", component = "all", flatten = TRUE))
            if (!is.null(re_terms)) {
                pred.labels.tmp <- sjlabelled::term_labels(models, 
                  case = case, mark.cat = TRUE, prefix = "varname")
                for (.re in re_terms) {
                  found <- grepl(paste0("^", .re, ":"), pred.labels.tmp)
                  if (any(found)) {
                    pred.labels <- pred.labels[!found]
                    category.values <- category.values[!found]
                    pred.labels.tmp <- pred.labels.tmp[!found]
                  }
                }
            }
            no.dupes <- !duplicated(names(pred.labels))
            pred.labels <- prepare.labels(x = pred.labels[no.dupes], 
                grp = show.reflvl, categorical = category.values[no.dupes], 
                models = models)
        }
        else {
            pred.labels <- NULL
            for (pl_counter in 1:length(models)) {
                pred.labels <- c(pred.labels, parameters::format_parameters(models[[pl_counter]]))
            }
            pred.labels <- pred.labels[!duplicated(names(pred.labels))]
            show.reflvl <- FALSE
        }
    }
    else {
        show.reflvl <- FALSE
    }
    if (!sjmisc::is_empty(pred.labels)) {
        if (!is.null(names(pred.labels))) {
            labs <- sjmisc::word_wrap(pred.labels, wrap = wrap.labels, 
                linesep = "<br>")
            if (show.reflvl) {
                pl <- pred.labels
                dupes <- which(pred.labels == names(pred.labels))
                if (!sjmisc::is_empty(dupes)) 
                  pl <- pl[-dupes]
                dat <- merge(dat, data.frame(term = names(pl)), 
                  by = "term", all = TRUE)
                found <- match(names(pl), dat$term)
                dat[sort(found), ] <- dat[found, ]
                refs <- is.na(dat[, 2])
            }
            else {
                refs <- NULL
            }
            tr <- 1:nrow(dat)
            find.matches <- match(dat$term, names(pred.labels))
            find.na <- which(is.na(find.matches))
            if (!sjmisc::is_empty(find.na)) 
                tr <- tr[-find.na]
            rp <- as.vector(stats::na.omit(find.matches))
            dat$term[tr] <- unname(labs[rp])
            if (!is.null(refs)) {
                dat[refs, 2:ncol(dat)] <- ""
                est.cols <- if (show.est) 
                  grepl("^estimate", colnames(dat))
                else if (show.std) 
                  grepl("^std.estimate", colnames(dat))
                else NULL
                if (!is.null(est.cols)) 
                  dat[refs, est.cols] <- "<em>Reference</em>"
            }
            if (!is.null(zeroinf)) {
                tr <- 1:nrow(zeroinf)
                find.matches <- match(zeroinf$term, names(pred.labels))
                find.na <- which(is.na(find.matches))
                if (!sjmisc::is_empty(find.na)) 
                  tr <- tr[-find.na]
                rp <- as.vector(stats::na.omit(find.matches))
                zeroinf$term[tr] <- unname(labs[rp])
            }
        }
        else {
            if (length(pred.labels) == nrow(dat)) 
                dat$term <- pred.labels
            else message("Length of `pred.labels` does not equal number of predictors, no labelling applied.")
        }
    }
    if (isTRUE(auto.label) && is.null(dv.labels)) {
        dv.labels <- sjmisc::word_wrap(sjlabelled::response_labels(models, 
            case = case), wrap = wrap.labels, linesep = "<br>")
    }
    else if (is.null(dv.labels)) {
        dv.labels <- purrr::map(models, insight::find_response) %>% 
            purrr::flatten_chr()
    }
    if (!is.null(order.terms)) {
        if (length(order.terms) == nrow(dat)) {
            dat <- dat[order.terms, ]
        }
        else {
            message("Number of values in `order.terms` does not match number of terms. Terms are not sorted.")
        }
    }
    col.header <- purrr::map_chr(colnames(dat), function(x) {
        pos <- grep("^estimate_", x)
        if (!sjmisc::is_empty(pos)) {
            i <- as.numeric(sub("estimate_", "", x = x, fixed = T))
            if (insight::is_multivariate(models[[1]])) 
                mr <- i
            else mr <- NULL
            if (change_string_est && !sjmisc::is_empty(string.est)) {
                x <- string.est
            }
            else if (i <= length(models)) {
                x <- sjPlot:::estimate_axis_title(models[[i]], axis.title = NULL, 
                  type = "est", transform = transform.data[[i]], 
                  multi.resp = mr, include.zeroinf = FALSE)
            }
            else if (length(models) == 1) {
                x <- sjPlot:::estimate_axis_title(models[[1]], axis.title = NULL, 
                  type = "est", transform = transform.data[[1]], 
                  multi.resp = mr, include.zeroinf = FALSE)
            }
            else {
                x <- string.est
            }
        }
        pos <- grep("^term", x)
        if (!sjmisc::is_empty(pos)) 
            x <- string.pred
        pos <- grep("^conf.int", x)
        if (!sjmisc::is_empty(pos)) 
            x <- string.ci
        pos <- grep("^std.error", x)
        if (!sjmisc::is_empty(pos)) 
            x <- string.se
        pos <- grep("^std.estimate", x)
        if (!sjmisc::is_empty(pos)) 
            x <- string.std
        pos <- grep("^std.se", x)
        if (!sjmisc::is_empty(pos)) 
            x <- string.std_se
        pos <- grep("^std.conf.int", x)
        if (!sjmisc::is_empty(pos)) 
            x <- string.std_ci
        pos <- grep("^p.value", x)
        if (!sjmisc::is_empty(pos)) 
            x <- string.p
        pos <- grep("^std.p.value", x)
        if (!sjmisc::is_empty(pos)) 
            x <- string.std.p
        pos <- grep("^df", x)
        if (!sjmisc::is_empty(pos)) 
            x <- string.df
        pos <- grep("^statistic", x)
        if (!sjmisc::is_empty(pos)) 
            x <- string.stat
        pos <- grep("^std.statistic", x)
        if (!sjmisc::is_empty(pos)) 
            x <- string.std.stat
        pos <- grep("^response.level", x)
        if (!sjmisc::is_empty(pos)) 
            x <- string.resp
        pos <- grep("^ci.inner", x)
        if (!sjmisc::is_empty(pos)) 
            x <- "CI (50%)"
        pos <- grep("^ci.outer", x)
        if (!sjmisc::is_empty(pos)) 
            x <- sprintf("CI (%i%%)", round(100 * show.ci))
        x
    })
    if (grepl("stars", p.style)) 
        footnote <- sprintf("* p&lt;%s&nbsp;&nbsp;&nbsp;** p&lt;%s&nbsp;&nbsp;&nbsp;*** p&lt;%s", 
            format(p.threshold[1]), format(p.threshold[2]), format(p.threshold[3]))
    else footnote <- NULL
    sjPlot:::tab_model_df(x = dat, zeroinf = zeroinf, is.zeroinf = is.zeroinf, 
        title = title, col.header = col.header, dv.labels = dv.labels, 
        rsq.list = rsq.data, n_obs.list = n_obs.data, icc.list = icc.data, 
        dev.list = dev.data, aic.list = aic.data, aicc.list = aicc.data, 
        variance.list = variance.data, ngrps.list = ngrps.data, 
        loglik.list = loglik.data, n.models = length(model.list), 
        show.re.var = show.re.var, show.icc = show.icc, CSS = CSS, 
        file = file, use.viewer = use.viewer, footnote = footnote, 
        digits.rsq = digits.rsq, digits.re = digits.re, encoding = encoding)
}
