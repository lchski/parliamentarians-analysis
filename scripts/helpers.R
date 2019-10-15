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
count_group <- function(dataset, ...) {
  dataset %>%
    group_by(...) %>%
    summarize(count = n()) %>%
    arrange(-count)
}

pull_count <- function(dataset) {
  dataset %>%
    summarize(count = n()) %>%
    pull(count)
}

pull_count_unique_people <- function(dataset) {
  dataset %>%
    distinct(Person.PersonId) %>%
    pull_count()
}

focus_role_columns <- function(role_tibble, ...) {
  role_tibble %>%
    select(Person.PersonId, StartDate, EndDate, NameEn, OrganizationLongEn, ToBeStyledAsEn, ...) %>%
    arrange(Person.PersonId, StartDate, EndDate)
}

## find details on a person
lookup_person <- function(PersonId, ...) {
  parliamentarians %>%
    filter(Person.PersonId == PersonId) %>%
    select(Person.DisplayName, ...)
}

party_colour_mappings = tribble(
  ~party_simple,~colour,
  #--|--|----
  "liberal","red",
  "conservative","blue"
)

## note that we need to return a `list`
## ref: https://stackoverflow.com/questions/58072649/using-geoms-inside-a-function
colour_block_by_party <- function() {
  list(
      geom_rect(
      data = ministries,
      alpha = 0.1,
      mapping = aes(
        xmin = start_date,
        xmax = end_date,
        ymin = -Inf,
        ymax = Inf,
        fill = party_simple
      )
    ),
    scale_fill_manual(
      values = c(
        "conservative" = "blue",
        "liberal" = "red"
      )
    )
  )
}
