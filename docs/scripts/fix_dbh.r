fix_dbh <- function(.data) {
  key <- paste(.data$year, .data$tree_id) %in% do.call(paste, remove_id)
  .data %>%
    group_by(tree_id) %>%
    mutate(p1 = nth(dbh - lag(dbh), 2), p2 = nth(dbh - lag(dbh), 3)) %>%
    group_by(plot, spp) %>%
    mutate(p1_mean = mean(p1, na.rm = TRUE),
           p2_mean = mean(p2, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(key = key) %>%
    mutate(
      problem = case_when(
        tree_id %in% anti_join(out_d[[1]], out_d[[2]])$tree_id ~ "p1",
        tree_id %in% anti_join(out_d[[2]], out_d[[1]])$tree_id ~ "p2"
      )
    ) %>%
    mutate(
      dbh = case_when(
        !key ~ dbh,
        year == "08" & p2 < 0 ~ (lead(dbh) + lead(dbh, 2)) / 2,
        year == "08" & problem != "p2" & !is.na(p2) ~ lead(dbh) - p2,
        year == "08" & problem == "p2" ~ lead(dbh) - p2_mean,
        year == "18" ~ lag(dbh) + p1,
      )
    ) %>% filter(key)
}

d_l %>%
  mutate(key = fix_dbh(.), tree_id = tree_id)
