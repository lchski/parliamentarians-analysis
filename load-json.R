library(tidyverse)
library(jsonlite)

lop_mps_json <- as_tibble(
  fromJSON("data/lop-mps.json", flatten = TRUE)
)

## Clone the CSV dataset from the JSON.
## Missing: 
### date_of_parliamentary_entry,
### role_minister,
### role_critic,
### years_of_service,
### military_service,
lop_mps_clone <- lop_mps_json %>% select(
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

## Table of roles
roles <- lop_mps_json %>% select(id = PersonId, Roles) %>% unnest()

## Table of professions
### FIXME: Doesn't work??
professions <- lop_mps_json %>% select(id = PersonId, Professions) %>% unnest()

## Table of years of service
### TODO: confirm assumption that multiple rows indicate a break in service (e.g. resigning as MP, then running a year later)
years_of_service <- lop_mps_json %>% select(id = PersonId, YearsOfServiceSegments) %>% unnest()
