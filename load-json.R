library(tidyverse)
library(jsonlite)
library(lubridate)
library(manifestoR) ## TODO: Actually make use of this.

# LOAD THE DATAZ

## Get list of data files
files_path <- "data/lop/"
files <- tibble(filename = dir(files_path, pattern = "*.json"))

## Pull rows from each file into a big table
## (this var used generally just for debug)
data_by_file_nested <- files %>%
  mutate(
    contents = map(filename,
                   ~ as_tibble(fromJSON(file.path(files_path, .), flatten = TRUE))
                   )
  )

## Separate
data_by_file <- data_by_file_nested %>%
  mutate(contents = map(contents, ~
                          .x %>% mutate(
                            EthnicityLongEn = as.character(EthnicityLongEn),
                            EthnicityLongFr = as.character(EthnicityLongFr),
                            FamilyRelations = as.list(FamilyRelations),
                            Languages = as.list(Languages)
                          ))) %>%
  unnest(cols = c(contents))


## Get rid of the filename column
parliamentarians <- data_by_file %>%
  select(-filename)

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

## Version of the table _without_ list columns (makes it easier to identify distinct entries)
parliamentarians_fixed <- parliamentarians %>%
  select(-one_of(identify_empty_columns(.))) %>%
  select(
    -Professions,
    -Languages,
    -FamilyRelations,
    -Pictures,
    -Roles,
    -YearsOfServiceSegments
  ) %>%
  distinct() %>%
  mutate(id = PersonId)

## Remove columns that differ across the data sources (because each contributes something different)
## (NB: Removing Age is a cop-out, because there's one weird exception in my dataset because Jack Iyerak Anawak's
##      birthday was the same as the day I downloaded the data, so the auto-calculated age column reflects that.
##      We can calculate it ourselves, instead.)
parliamentarians_backgrounds <- parliamentarians_fixed %>%
  select(
    -PrimeMinisterEn:-AssistantCriticOfFr,
    -PremiershipExperienceEn:-MinistryDurationFr,
    -EthnicityLongEn:-EthnicityLongFr,
    -IsPrimeMinister
  ) %>%
  select(-Age) %>%
  distinct()

## Table of roles
roles <- parliamentarians %>%
  select(id = PersonId, Roles) %>%
  unnest(cols = c(Roles)) %>%
  select(-Classes) %>%
  distinct() %>%
  select(-one_of(identify_empty_columns(.))) %>%
  mutate(
    StartDate = date(StartDate),
    EndDate = if_else(
      is.na(EndDate),
      today(),
      date(EndDate)
    )
  ) %>%
  distinct()

## Table of professions
### FIXME: Doesn't work??
professions <- parliamentarians %>%
  select(id = PersonId, Professions) %>%
  unnest(cols = c(Professions)) %>%
  distinct() %>%
  select(-one_of(identify_empty_columns(.)))

## Table of years of service
### TODO: confirm assumption that multiple rows indicate a break in service (e.g. resigning as MP, then running a year later)
years_of_service <- parliamentarians %>%
  select(id = PersonId, YearsOfServiceSegments) %>%
  unnest(cols = c(YearsOfServiceSegments)) %>%
  distinct() %>%
  select(-one_of(identify_empty_columns(.)))




### count of each role
roles %>%
  select(NameEn) %>%
  group_by(NameEn) %>%
  summarize(count = n()) %>%
  arrange(-count) %>%
  View()

### understand minister styling vs organization names
ministers <- roles %>%
  filter(NameEn == "Minister")

min_to_be_styled <- ministers %>%
  group_by(ToBeStyledAsEn) %>%
  summarize(count = n()) %>%
  arrange(-count)
min_org <- ministers %>%
  group_by(OrganizationLongEn) %>%
  summarize(count = n()) %>%
  arrange(-count)

multi_ministers <- ministers %>%
  group_by(id) %>%
  summarize(count = n()) %>%
  filter(count > 1) %>%
  pull(id)

ministers %>%
  filter(id %in% multi_ministers) %>%
  #filter(str_detect(ToBeStyledAsEn, "and|,")) %>%
  focus_role_columns() %>%
  View()

focus_role_columns <- function(role_tibble) {
  role_tibble %>%
    select(id, StartDate, EndDate, NameEn, OrganizationLongEn, ToBeStyledAsEn) %>%
    arrange(id, StartDate, EndDate)
}

ministers %>%
  filter(OrganizationLongEn == "Treasury Board") %>%
  arrange(id, StartDate)


roles %>%
  filter(NameEn == "Constituency Member") %>%
  mutate(EndDate = case_when(
    is.na(EndDate) ~ as.character(today()),
    TRUE ~ EndDate
  )) %>%
  mutate(role_id = row_number()) %>%
  gather(StartDate, EndDate, key = "period_bound", value = "date") %>%
  mutate(date = date(date)) %>%
  select(role_id, id, NameEn, OrganizationLongEn, date) %>%
  arrange(id, role_id, NameEn, OrganizationLongEn, date) %>%
  group_by(role_id) %>%
  complete(date = full_seq(date, 1), nesting(id, NameEn, OrganizationLongEn)) %>%
  ungroup()

senators <- roles %>%
  filter(NameEn == "Senator") %>%
  group_by(id) %>%
  summarize(
    date_appointed = min(StartDate),
    date_left = max(EndDate)
  ) %>%
  left_join(parliamentarians_backgrounds) %>%
  select(
    id,
    DisplayName,
    dob = DateOfBirth,
    date_appointed,
    date_left,
    dod = Death.DateOfDeath
  ) %>%
  mutate(
    dob = date(dob),
    dod = date(dod),
    age_at_appt = interval(dob, date_appointed) / years(1),
    age_at_leaving = interval(dob, date_left) / years(1),
    length_of_service = interval(date_appointed, date_left) / years(1),
    died_in_office = date_left == dod
  ) %>%
  arrange(age_at_appt)



# Interesting tables!

## Number of unique parliamentarians
count_unique_parliamentarians <- parliamentarians_fixed %>%
  select(PersonId) %>%
  unique() %>%
  summarize(count = n()) %>%
  pull()

## Number of parliamentarians by type (according to file), in proportion to total number of parliamentarians.
data_by_file %>%
  group_by(filename) %>%
  summarize(
    count = n(),
    proportion = count / count_unique_parliamentarians,
  ) %>%
  arrange(-count) %>%
  View()


## Clone the CSV dataset from the JSON.
## Missing: 
### date_of_parliamentary_entry,
### role_minister,
### role_critic,
### years_of_service,
### military_service,
lop_mps_clone <- parliamentarians_fixed %>% select(
  id = PersonId,
  `_id` = `_id`,
  name = DisplayName,
  birth_date = DateOfBirth,
  birth_city = CityOfBirthEn,
  birth_province_region = ProvinceOfBirthEn,
  birth_country = CountryOfBirthEn,
  deceased_date = Death.DateOfDeath,
  role_type_of_parliamentarian = TypeOfParliamentarianEn,
  seat_riding_senatorial_division = ConstituencyEn,
  seat_province_territory = ProvinceEn,
  gender = Gender,
  profession = ProfessionsEn,
  political_affiliation = PartyEn,
)



### trying to deal with the long table thing...
### BUT probably not a big deal, because most of this data is in `Roles`
parliamentarians_fixed %>% select(
  id,
  PrimeMinisterEn:AssistantCriticOfFr,
  PremiershipExperienceEn:MinistryDurationFr,
  EthnicityLongEn:EthnicityLongFr,
  IsPrimeMinister
) %>%
  group_by(id) %>%
  gather(-id, key = "key", value = "value") %>%
  group_by(id, key) %>%
  unique() %>%
  filter(value != "") %>%
  ungroup() %>%
  spread(key = key, value = value)

parliamentarians_fixed %>% select(
  id,
  PrimeMinisterEn:AssistantCriticOfFr,
  PremiershipExperienceEn:MinistryDurationFr,
  EthnicityLongEn:EthnicityLongFr,
  IsPrimeMinister
) %>%
  group_by(id) %>%
  gather(-id, key = "key", value = "value") %>%
  group_by(id, key) %>%
  unique() %>%
  ungroup() %>%
  group_by(id) %>%
  spread(key = key, value = value)

parliamentarians_fixed %>% select(
  id,
  PrimeMinisterEn:AssistantCriticOfFr,
  PremiershipExperienceEn:MinistryDurationFr,
  EthnicityLongEn:EthnicityLongFr,
  IsPrimeMinister
) %>%
  group_by(id) %>%
  pivot_longer(-id, names_to = "column", values_to = "value", values_drop_na = TRUE)



manifestos %>% mutate(lr = franzmann_kaiser(.)) %>% select(edate:partyabbrev, lr) %>% View()


abbottdc <- as_tibble(fromJSON("data/members/3761.json", flatten = TRUE))
abbottjjc <- as_tibble(fromJSON("data/members/8200.json", flatten = TRUE))

test <- abbottdc %>% bind_rows(abbottjjc)
## can't unnest directly (errors)
test %>% unnest()
## can unnest specific nested columns
test %>% select(Person.PersonId, Person.Education) %>% unnest() %>% View()


parliamentarians_backgrounds %>%
  select(id) %>%
  mutate(url = paste0("https://lop.parl.ca/ParlinfoWebApi/Person/GetPersonWebProfile/", id, "?callback=jQuery33107266187344061623_1569968990479&_=1569968990480")) %>%
  select(url) %>%
  write_csv("data/members/urls.csv")


vectorize_json = Vectorize(fromJSON)

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
