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
    filter(cur_group_id() %% 2 == 0) %>%
    pull(rowname) %>%
    as.numeric()
  x %>%
    kbl(caption = caption) %>%
    kable_styling(full_width = FALSE) %>%
    row_spec(even_groups, background = background)
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
get_cond <- function(...) {
  vals <- unlist(list(...))
  cond <- evalq(cond, parent.frame())
  map_lgl(str_split(cond, ","), ~ any(as.numeric(.x) %in% vals))
}