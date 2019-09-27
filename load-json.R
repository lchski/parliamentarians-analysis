library(tidyverse)
library(jsonlite)
library(lubridate)

# LOAD THE DATAZ

## Get list of data files
files_path <- "data/lop/"
files <- tibble(filename = dir(files_path, pattern = "*.json"))

## Pull rows from each file into a big table
## (this var used generally just for debug)
data_by_file <- files %>%
  mutate(
    contents = map(filename,
                   ~ as_tibble(fromJSON(file.path(files_path, .), flatten = TRUE))
                   )
  ) %>%
  unnest()

## Get rid of the filename column
parliamentarians <- data_by_file %>%
  select(-filename)

## Find columns that are "empty" (only one unique variable)
empty_columns <- parliamentarians %>%
  gather() %>%
  group_by(key) %>%
  unique() %>%
  summarize(count = n()) %>%
  filter(count == 1) %>%
  select(key) %>%
  pull()

## Version of the table _without_ list columns (makes it easier to identify distinct entries)
parliamentarians_fixed <- parliamentarians %>%
  select(-empty_columns) %>%
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
  unnest() %>%
  select(-Classes) %>%
  distinct()

## Table of professions
### FIXME: Doesn't work??
professions <- parliamentarians %>%
  select(id = PersonId, Professions) %>%
  unnest() %>%
  distinct()

## Table of years of service
### TODO: confirm assumption that multiple rows indicate a break in service (e.g. resigning as MP, then running a year later)
years_of_service <- parliamentarians %>%
  select(id = PersonId, YearsOfServiceSegments) %>%
  unnest() %>%
  distinct()




### count of each role
roles %>% select(NameEn) %>% group_by(NameEn) %>% summarize(count = n()) %>% arrange(-count) %>% View()


### figure out which columns vary, messing up distinct count (should be 5098)
parliamentarians_fixed %>%
  arrange(id) %>%
  select(-PrimeMinisterEn:-AssistantCriticOfFr) %>%
  distinct() %>%
  select(-PremiershipExperienceEn:-MinistryDurationFr) %>%
  distinct() %>%
  select(-EthnicityLongEn:-EthnicityLongFr) %>%
  distinct() %>%
  select(-IsPrimeMinister) %>%
  distinct() %>%
  View()

## Remove columns that differ across the data sources (because each contributes something different)
## (NB: Removing Age is a cop-out, because there's one weird exception in my dataset because Jack Iyerak Anawak's
##      birthday was the same as the day I downloaded the data, so the auto-calculated age column reflects that.
##      We can calculate it ourselves, instead.)
parliamentarians_fixed %>%
  arrange(id) %>%
  select(
    -PrimeMinisterEn:-AssistantCriticOfFr,
    -PremiershipExperienceEn:-MinistryDurationFr,
    -EthnicityLongEn:-EthnicityLongFr,
    -IsPrimeMinister
  ) %>%
  select(-Age) %>%
  distinct() %>%
  View()


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