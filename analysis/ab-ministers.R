# per: https://twitter.com/AlexUsherHESA/status/1230671719100272640

## AB born?
ministers %>%
  left_join(
    parliamentarians %>%
      select(Person.PersonId, Person.ProvinceOfBirthEn)
  ) %>%
  mutate(ab_born = Person.ProvinceOfBirthEn == "Alberta")

## AB constituency?
## TODO: combine this with ministers
members %>%
  filter(OrganizationProvinceEn == "Alberta")