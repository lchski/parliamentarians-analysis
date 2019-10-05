library(tidyverse)
library(readtext)
library(jsonlite)
library(lubridate)

source("scripts/helpers.R")

parliamentarians <- as_tibble(readtext::readtext("data/members/", verbosity = 0)) %>%
  mutate(doc_id = str_split(doc_id, fixed("?callback=jQuery33107266187344061623_1569968990479&_=1569968990480"))) %>%
  unnest(cols = c(doc_id)) %>%
  filter(doc_id != "") %>%
  mutate(text = str_split(text, fixed("/**/ typeof jQuery33107266187344061623_1569968990479 === 'function' && jQuery33107266187344061623_1569968990479("))) %>%
  unnest(cols = c(text)) %>%
  mutate(text = str_split(text, fixed("});"))) %>%
  unnest(cols = c(text)) %>%
  filter(text != "") %>%
  mutate(text = paste0("[", text, "}]")) %>% # TODO: use only up to this point to export JSON versions of the files
  mutate(contents = vectorize_json(text, flatten = TRUE)) %>%
  select(contents) %>%
  bind_rows(.$contents) %>%
  filter(! is.na(Person.PersonId)) %>%
  select(-one_of(identify_empty_columns(.)))

## Just get Senators
parliamentarians %>%
  mutate(
    was_senator = map(
      Person.Roles,
      ~ as_tibble(.) %>%
        filter(NameEn == "Senator") %>%
        summarize(count = n()) %>%
        mutate(was_senator = count > 0) %>%
        pull(was_senator)
    )
  ) %>%
  filter(was_senator)

parliamentarians %>%
  mutate(degree_count = map_dbl(Person.Education,
                            ~ as_tibble(.) %>%
                              summarize(count = n()) %>%
                              pull(count))
         ) %>%
  select(Person.PersonId, Person.DateOfBirth, Person.DisplayName, Person.Education, degree_count)

roles <- parliamentarians %>%
  select(Person.PersonId, Person.Roles) %>%
  unnest()

roles %>%
  filter(OrganizationTypeEn == "Cabinet Committee") %>%
  mutate(decade = year(floor_date(date(StartDate), unit = "10 years"))) %>%
  group_by(OrganizationLongEn, decade) %>%
  count_group() %>%
  View()

roles %>%
  filter(OrganizationTypeEn == "Cabinet Committee") %>%
  group_by(OrganizationLongEn, ParliamentNumber) %>%
  count_group() %>%
  View()

## how many Ministers of Veterans Affairs had military experience?
vac_ministers <- parliamentarians %>%
  mutate(
    minister_of_va = map_lgl(
      Person.Roles,
      ~ as_tibble(.) %>%
        filter(NameEn == "Minister") %>%
        filter(OrganizationLongEn %in% c("Veterans Affairs", "Soldiers' Civil Re-establishment")) %>%
        summarize(count = n()) %>%
        mutate(had_role = count > 0) %>%
        pull(had_role)
    ),
    had_military_experience = map_lgl(
      MilitaryExperience,
      ~ as_tibble(.) %>%
        summarize(count = n()) %>%
        mutate(had_role = count > 0) %>%
        pull(had_role)
    )
  ) %>%
  filter(minister_of_va)

vac_ministers %>%
  mutate(earliest_date_as_minister = map(
    Person.Roles,
    ~ as_tibble(.) %>%
      filter(NameEn == "Minister") %>%
      filter(OrganizationLongEn %in% c("Veterans Affairs", "Soldiers' Civil Re-establishment")) %>%
      summarize(date = max(date(StartDate))) %>%
      pull(date)
  ) %>% reduce(c)) %>%
  select(
    Person.PersonId,
    Person.DisplayName,
    Person.DateOfBirth,
    earliest_date_as_minister,
    had_military_experience,
    MilitaryExperience
  ) %>%
  arrange(earliest_date_as_minister)

## more nuanced capture for ministers
ministers <- roles %>%
  filter(GroupingTitleEn == "Cabinet") %>%
  filter(
    ! OrganizationTypeEn %in% 
      c("Province",
        "Municipal Government",
        "Regional Government",
        "Party"
      )
  ) %>%
  remove_extra_columns(.) %>%
  mutate(
    StartDate = date(StartDate),
    EndDate = date(EndDate),
    period_in_office = interval(StartDate, EndDate)
  )
	

## Cabinet Size! Replicating: https://lop.parl.ca/sites/ParlInfo/default/en_CA/People/primeMinisters/Cabinet
### Peeps
ministers %>%
  mutate(in_range = date("1997-04-26") %within% period_in_office) %>%
  filter(in_range) %>%
  left_join(parliamentarians) %>%
  select(Person.PersonId, Person.DisplayName, StartDate, EndDate, NameEn, OrganizationLongEn) %>%
  View()

### Count unique peeps
### (good parallel for "Cabinet Size" figure—add 1 to this and you get it,
### except when the PM was counted, e.g., "1993-06-25", when Kim Campbell
### was both PM and Minister responsible for Federal-Provincial Relations.)
ministers %>%
  mutate(in_range = date("1997-04-26") %within% period_in_office) %>%
  filter(in_range) %>%
  distinct(Person.PersonId) %>%
  summarize(count = n())



ministers %>%
  mutate(in_range = date("1993-06-25") %within% period_in_office) %>%
  filter(in_range) %>%
  left_join(parliamentarians) %>%
  select(Person.PersonId, Person.DisplayName, StartDate, EndDate, NameEn, OrganizationLongEn, ToBeStyledAsEn) %>%
  View()

ministers %>%
  mutate(in_range = date("1993-01-03") %within% period_in_office) %>%
  filter(in_range) %>%
  distinct(Person.PersonId) %>%
  summarize(count = n())






departments <- as_tibble(readtext::readtext("../lop-departments-data/data/")) %>%
  mutate(text = str_split(text, fixed("/**/ typeof jQuery33105631282387368284_1570190165107 === 'function' && jQuery33105631282387368284_1570190165107(["))) %>%
  unnest(cols = c(text)) %>%
  mutate(text = str_split(text, fixed("}]);"))) %>%
  unnest(cols = c(text)) %>%
  filter(text != "") %>%
  mutate(text = paste0("[", text, "}]")) %>%
  mutate(contents = vectorize_json(text, flatten = TRUE)) %>%
  select(contents) %>%
  bind_rows(.$contents)
