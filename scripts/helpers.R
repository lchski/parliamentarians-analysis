vectorize_json = Vectorize(fromJSON)

## Find columns that are "empty" (only one unique variable)
identify_empty_columns <- function(dataset) {
  dataset %>%
    gather() %>%
    group_by(key) %>%
    unique() %>%
    summarize(count = n()) %>%
    filter(count == 1) %>%
    select(key) %>%
    pull()
}

## Find columns that provide French values
identify_fr_columns <- function(dataset) {
  dataset %>%
    names() %>%
    tibble::enframe(name = NULL) %>%
    filter(str_detect(value, "Fr$")) %>%
    pull()
}

remove_extra_columns <- function(dataset) {
  dataset %>%
    select(-one_of(identify_empty_columns(.))) %>%
    select(-one_of(identify_fr_columns(.)))
}

## 
count_group <- function(dataset) {
  dataset %>%
    summarize(count = n()) %>%
    arrange(-count)
}

focus_role_columns <- function(role_tibble) {
  role_tibble %>%
    select(id, StartDate, EndDate, NameEn, OrganizationLongEn, ToBeStyledAsEn) %>%
    arrange(id, StartDate, EndDate)
}
