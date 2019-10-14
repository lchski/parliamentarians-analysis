cabinet_ministers <- ministers %>%
  filter(in_cabinet)

summarize_roles_by_category <- function(dataset, ...) {
  dataset %>%
    group_by(...) %>%
    summarize(
      first_appearance = min(StartDate, na.rm = FALSE),
      last_appearance = max(EndDate, na.rm = FALSE),
      category_occupied_yrs = sum(time_length(period_in_office, "years")),
      category_lifespan_yrs = time_length(interval(first_appearance, last_appearance), "years"),
      count = n(),
      avg_length_mos =
        mean(
          time_length(period_in_office, unit = "months")
        ),
      min_length_mos = 
        min(
          time_length(period_in_office, unit = "months")
        ),
      max_length_mos = 
        max(
          time_length(period_in_office, unit = "months")
        )
    )
}

cabinet_ministers %>%
  summarize_roles_by_category(OrganizationLongEn) %>%
  View()


## calculate number of years a position is actually occupied
## (or # of days, if we drop the division at the end)
## doesn't work for roles where n = 1 (lists!)
seq_date_vectorized <- Vectorize(seq.Date, vectorize.args = c("from", "to"))
cabinet_ministers %>%
  filter(OrganizationLongEn == "Forestry and Rural Development") %>%
  select(StartDate, EndDate, period_in_office) %>%
  mutate(dates = seq_date_vectorized(StartDate, EndDate, by = "days")) %>%
  summarize(dates = length(unique(do.call(c, dates))) / 365.25)

