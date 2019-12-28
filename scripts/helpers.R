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
    mutate(count_prop = count / sum(count)) %>%
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
) %>%
  rename(name = party_simple)

gender_colour_mappings = tribble(
  ~Person.Gender,~colour,
  #--|--|----,
  "F","red",
  "M","blue"
) %>%
  rename(name = Person.Gender)

generic_colour_mappings = rbind(
  party_colour_mappings,
  gender_colour_mappings
)

generic_colour_mappings_v <- generic_colour_mappings$colour
names(generic_colour_mappings_v) <- generic_colour_mappings$name

## note that we need to return a `list`
## ref: https://stackoverflow.com/questions/58072649/using-geoms-inside-a-function
colour_block_by_party <- function(party_bg_alpha = 0.1) {
  list(
    geom_rect(
      data = ministries,
      inherit.aes = FALSE,
      alpha = party_bg_alpha,
      mapping = aes(
        xmin = start_date,
        xmax = end_date,
        ymin = -Inf,
        ymax = Inf,
        fill = party_simple
      )
    ),
    scale_fill_manual(
      values = generic_colour_mappings_v
    )
  )
}

## for calculating number of days a position occupied
seq_date_vectorized <- Vectorize(seq.Date, vectorize.args = c("from", "to"))
