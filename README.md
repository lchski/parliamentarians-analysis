# Parliamentarians: Who are they? (_at scale_!)

Playing with Library of Parliament data on parliamentarians in Canada.

## Interesting things to research

- Critic roles (which have poor dating, understandable)
  - How often do they change in a session?
  - Are they linked to (or do they generate) news coverage, legislation?
- Education
  - Which schools? (correlation to Senate/MP, Minister, etc?)
  - Trends over time: level of education, type of degrees

## API URLs

- Get all the peoples
  - Unfiltered: https://lop.parl.ca/ParlinfoWebAPI/Person/SearchAndRefine?callback=jQuery33105224920274316343_1569971548277&expression=&refiners=&_=1569971548282
  - Just a text search (`keyword`): https://lop.parl.ca/ParlinfoWebAPI/Person/SearchAndRefine?callback=jQuery33105224920274316343_1569971548277&expression=Abbott&refiners=&_=1569971548279
  - Text search and a refiner, prime minister (`refiners=5-1%2C`): https://lop.parl.ca/ParlinfoWebAPI/Person/SearchAndRefine?callback=jQuery33105224920274316343_1569971548277&expression=Abbott&refiners=5-1%2C&_=1569971548281
  - Just a refiner, prime minister (`refiners=5-1%2C`): https://lop.parl.ca/ParlinfoWebAPI/Person/SearchAndRefine?callback=jQuery33105370148350178735_1569971635373&expression=&refiners=5-1%2C&_=1569971635375
  - Refine for municipal experience (`refiners=28-3%2C`): https://lop.parl.ca/ParlinfoWebAPI/Person/SearchAndRefine?callback=jQuery33105370148350178735_1569971635373&expression=&refiners=28-3%2C&_=1569971635377
  - Refine for PM and municipal experience (`refiners=5-1%2C28-3%2C`): https://lop.parl.ca/ParlinfoWebAPI/Person/SearchAndRefine?callback=jQuery33106068412624955225_1569971757145&expression=&refiners=5-1%2C28-3%2C&_=1569971757147
- Get info from specific person ID: https://lop.parl.ca/ParlinfoWebApi/Person/GetPersonWebProfile/3761?callback=jQuery33107266187344061623_1569968990479&_=1569968990480
