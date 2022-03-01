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
color_groups <- function(x, caption = "", background = "#EEE9E9") {
  even_groups <- x %>%
    rownames_to_column() %>%
    mutate(rowname = as.numeric(rowname), first_row = first(rowname)) %>%
    ungroup() %>%
    mutate(group_id = dense_rank(first_row)) %>%
    filter(group_id %% 2 == 0) %>%
    pull(rowname)
  x %>%
    kableExtra::kbl(caption = caption) %>%
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

# These are the functions for calculating the SCI metric
# for structural heterogeity
cross_product <- function(a, b) {
  if(length(a)!=3 || length(b)!=3){
        stop("Cross product is only defined for 3D vectors.")
    }
  i1 <- c(2, 3, 1)
  i2 <- c(3, 1, 2)
  a[i1] * b[i2] - a[i2] * b[i1]
}

# area is the magnitude of two vectors
tri_area <- function(tri, points) {
  apply(tri, 1, function(point) {
    AB <- points[point[2], ] - points[point[1], ]
    AC <- points[point[3], ] - points[point[1], ]
    sqrt(sum(cross_product(AB, AC)^2))
  })
}

sci_metric <- function(x, y, z) {
  points <- as.matrix(cbind(x, y, z))
  deln_obj <- geometry::delaunayn(points[, 1:2], output.options = "Fa")
  area3d <- sum(tri_area(deln_obj$tri, points))
  area2d <- sum(deln_obj$areas)
  area3d / area2d
}

# here is a function to to get a an AIC from a formula
# optionally split by species
formula_aic <- function(form, data, method = "lmm"){
  if(method == "lmm") {
    mod <- lmer(form, data = data, REML = FALSE)
    rmse <- sqrt(mean(resid(mod, type = "pearson")^2))
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
