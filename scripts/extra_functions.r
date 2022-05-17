require(magrittr)
require(dplyr)
# Functions to add to their own file

# Tell summary functions to group by species or not
group_vars <- function(by_species = FALSE) {
  if(by_species) {
    alist(spp, treatment, year)
  } else {
    alist(treatment, year)
  }
}

# color table by groups function, only works if table is
# properly arranged.
color_groups <- function(x, caption = "", background = "#EEE9E9", digits = 3) {
  even_groups <- x %>%
    rownames_to_column() %>%
    mutate(rowname = as.numeric(rowname), first_row = first(rowname)) %>%
    ungroup() %>%
    mutate(group_id = dense_rank(first_row)) %>%
    filter(group_id %% 2 == 0) %>%
    pull(rowname)
  x %>%
    kableExtra::kbl(caption = caption, digits = digits) %>%
    kableExtra::kable_styling(full_width = FALSE) %>%
    kableExtra::row_spec(even_groups, background = background)
}

# Outlier Function
out <- function(x, mag = 1.5) {
  qu <- quantile(x, c(.25, .75), na.rm = TRUE)
  bounds <- c(lower = qu[[1]] - mag * IQR(x, na.rm = TRUE),
              upper = qu[[2]] + mag * IQR(x, na.rm = TRUE))
  outliers <- x < bounds["lower"] | x > bounds["upper"]
  outliers[is.na(outliers)] <- FALSE
  outliers
}

# Plot ht vs dbh with square root transformation and
# highlight outliers
plot_ht <- function(data, mag = 1.5) {
  mod <- lm(ht ~ sqrt(dbh), data = data)
  plot(data$dbh, data$ht, col = "gray30")
  curve(coef(mod)[1] + coef(mod)[2] * sqrt(x), add = TRUE, col = "deeppink")
  out_resid <- as.numeric(names(resid(mod))[out(resid(mod), mag = mag)])
  points(data$dbh[out_resid], data$ht[out_resid], col = "hotpink")
  title(paste(format(formula(mod)), "\n", "residuals >", mag, "x IQR"))
  invisible(out_resid)
}

# function to filter condition values with OR logic

get_cond <- function(..., str = FALSE) {
  vals <- unlist(list(...))
  cond <- evalq(cond, parent.frame())
  cond <- str_split(cond, ",")
  if (str == TRUE) {
    map_chr(cond, ~ toString(.x[.x %in% vals]))
  } else {
    map_lgl(cond, ~ any(as.numeric(.x) %in% vals))
  }
}

#' Calculate XY coordinates based on distance and bearing
#'
#' @description Function converts distance and bearing into XY coordinates.
#' It is useful to generate stem maps (forestry) based on field data
#' (in forestry usually distance and bearing to a tree from a plot center are
#' collected to define tree locations).
#' Optionally center coordinates of the plot may be provided.
#'
#' @param distance a vector of distances.
#' @param bearing a vector of bearings, in degrees (0 - 360).
#' @param center.x An x coordinate of a plot center
#' @param center.y An y coordinate of a plot center
#' @return a data frame with x and y columns
#' @examples
#' d <- runif(10,min = 0.5,max=10)
#' b <- runif(10,min = 0,max=360)
#' plot(circular2xy(d,b))
#' plot(circular2xy(d,b,1000,2500))
#' @export


circular2xy <- function(distance, bearing, center.x = 0, center.y = 0) {

  #check inputs
  if(!is.numeric(distance) | !is.numeric(bearing)) stop("Input data not numeric")
  if(any(bearing > 360)) stop("bearing value(s) exceed 360")

  bearing.rad <- bearing * (pi / 180) #convert bearing to radians
  delta.x <- distance * sin(bearing.rad)
  delta.y <- distance * cos(bearing.rad)
  tree.x <- center.x + delta.x
  tree.y <- center.y + delta.y
  return(data.frame(x = tree.x, y = tree.y))
}

# get equations for models
lm_eqn <- function(m){
  r2 <- round(summary(m)$r.squared, digits = 2)
  a <- round(unname(coef(m)[1]), digits = 3)
  b <- round(unname(coef(m)[2]), digits = 3)
  n <- nrow(m$model)
  eqn <- as.character(
    as.expression(bquote(italic(y) == .(a) + .(b) %.% italic(x)))
  )
  r_sq <- as.character(as.expression(bquote(italic(r)^2~"="~.(r2))))
  n <- as.character(as.expression(bquote(italic(N)~"="~.(n)))) 
  data.frame(eqn = eqn, r_sq = r_sq, n = n)
}

# Scale data to min/max (0 - 1) for easy plotting
npc <- function(dat, x) {
  range <- max(dat, na.rm = TRUE) - min(dat, na.rm = TRUE)
  low <- min(dat, na.rm = TRUE)
  x * range + low
}



# here is a function to to get a an AIC from a formula
# optionally split by species
formula_aic <- function(form, data, method = "lmm"){
  if(method == "lmm") {
    mod <- lmer(form, data = data, REML = FALSE)
    rmse <- sqrt(mean(resid(mod, type = "pearson")^2))
    r2 <- round(unlist(performance::r2_nakagawa(mod)), 2)
    return(list(
    formula = deparse1(form),
    aicc = AICc(mod),
    rmse = rmse,
    r2 = paste(r2[[2]], r2[[1]], sep = " / ")
    ))
  }
  if(method == "glm") {
    mod <- glmer(form, data = data, family = Gamma(link = "log"))
    rmse <- sqrt(mean(resid(mod, type = "pearson")^2))
  }
  if(method == "lm") {
    mod <- lm(form, data = data)
    rmse <- sqrt(mean(resid(mod)^2))
  }
  list(
    formula = deparse1(form),
    aicc = AICc(mod),
    rmse = rmse
  )
}

# get AICs from list of formulas and calculate AIC weights
aic_weights <- function(formlist, delta_max = 6, ...) {
  map_dfr(formlist, formula_aic, .id = "row", ...) %>%
  arrange(aicc) %>%
  mutate(delta = aicc - first(aicc)) %>%
  filter(delta < delta_max) %>%
  mutate(wi = round(exp(-.5 * delta) / sum(exp(-.5 * delta)), 3))
}

get_aic <- function(formlist, ...) {
  map_dfr(formlist, formula_aic, .id = "row", ...) %>%
  arrange(aicc) %>%
  mutate(aicc = round(aicc, 1), rmse = round(rmse, 3))
}


update_no_simplify <- function(object, ...) {
  UseMethod("update_no_simplify")
}

update_no_simplify.formula <- function(old, new) {
  tmp <- .Call(stats:::C_updateform, as.formula(old), as.formula(new))
  formula(terms.formula(tmp, simplify = FALSE))
}

update_no_simplify.default <- function (object, formula., ..., evaluate = TRUE) {
  if (is.null(call <- getCall(object))) 
    stop("need an object with call component")
  extras <- match.call(expand.dots = FALSE)$...
  if (!missing(formula.)) 
    call$formula <- update_no_simplify.formula(formula(object), formula.)
  if (length(extras)) {
    existing <- !is.na(match(names(extras), names(call)))
    for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
    if (any(!existing)) {
      call <- c(as.list(call), extras[!existing])
      call <- as.call(call)
    }
  }
  if (evaluate) 
    eval(call, parent.frame())
  else call
}


# here is a function to relevel and relabel treatments and relabel measures
# for tables and figures data for the publication

relevel_treatment <- function(data, treatment = treatment) {
  mutate(data,
    "{{treatment}}" := factor({{ treatment }},
      levels = c("H40", "L40", "H80", "L80", "C"),
      labels = c("C40", "L40", "C80", "L80", "Control")
    )
  )   
}

relabel_measure <- function(data, measure = measure, fig = FALSE) {
  if(fig) {
    mutate(data,
      "{{measure}}" := recode_factor({{ measure }},
        dbh = "QMD~(cm)",
        ht_p = "Height~(m)",
        density = "Stems%.%ha^-1",
        ba = "BA~(m^2~ha^-1)",
        ba_inc2 = "BAI",
        mort = "New~mortality~(tph)",
        dom_dbh = "Dominant~DBH~(cm)",
        dom_ht = "Tallest~trees~(m)",
        sdi = "SDI"
      )
    ) %>%
    arrange({{ measure }})
  } else {
    data <- mutate(data,
      "{{measure}}" := recode_factor({{ measure }},
        dbh = "QMD (cm)",
        ht_p = "Height (m)",
        density = "Density (stems ha^-1^)",
        ba = "Basal area (m^2^/ha)",
        mort = "New mortality (tph)",
        dom_dbh = "Dominant DBH (cm)",
        dom_ht = "Dominant height (m)",
        sdi = "SDI"
      )
    ) %>%
    arrange({{ measure }})
  }
}


# Convenience function for printing tables

kbl2 <- function(.x, ...) {
    kableExtra::kbl(.x, ...) %>%
    kableExtra::kable_styling(full_width = FALSE)
}

# Update _site.yml based on numbered rmd files in project folder
# and the titles of each file

update_yml <- function() {
  files <- list.files(pattern = "^\\d+.*rmd$")

  get_file_title <- function(x) {
    frnt <- readLines(x, n = 10)
    match_title <- grepl("title: ", frnt)
    if(!any(match_title)) {
      title <- sub("(.*)\\..*$", "\\1", x[1])
    } else {
      title <- frnt[match_title][1]
      title <- gsub("title: (.*)", "\\1", title)
      title <- gsub("\\\"", "", title)
    }
    title
  }

  html_names <- gsub(".rmd", ".html", files)
  navbar_entries <- sapply(files, get_file_title)
  names(navbar_entries) <- html_names
  
  navbar <- c(
    "navbar: ",
    "  title: A-972 10-year Study",
    "  left: ",
    rbind(
      paste("    - text:", navbar_entries),
      paste("      href:", names(navbar_entries))
    )
  )

  cur_yml <- readLines("_site.yml", warn = FALSE)
  
  new_yml <- c(
    cur_yml[1:grep("navbar:", cur_yml) - 1],
    navbar
  )

  cat(new_yml, file = "_site.yml", sep = "\n")
}

random_plot <- function(data) {
  plot_year <- paste(data$plot, data$year)
  subset(data, plot_year == sample(plot_year))
}


# Confidence interval for average treatment estimate
ci <- function(x) {
  t_crit <- qt(0.975, length(x))
  LB <- mean(x) - t_crit * sd(x) / sqrt(length(x) - 1)
  UB <- mean(x) + t_crit * sd(x) / sqrt(length(x) - 1)
  data.frame(LB = LB, UB = UB)
}


fig_w <- function(col, unit) {
  w <- if_else(col == 1, 8.84, 18.2)
  if_else(unit == "in", w / 2.54, w)
}


