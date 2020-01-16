
## To generate this file:
## 1. Navigate to https://lop.parl.ca/sites/ParlInfo/default/en_CA/People/parliamentarians
## 2. Open devtools
## 3. Put in a blank search
## 4. Copy the URL for the XHR request that goes out
## 5. Put that URL in browser, save the response as `data/lop/parliamentarians.json`
## 6. Edit `data/lop/parliamentarians.json` so it's proper JSON (see `load.R` for what we lop off), using HexFiend or similar
parliamentarians_to_download <- read_json("data/lop/parliamentarians.json", simplifyVector = TRUE, flatten = TRUE)

parliamentarian_ids <- parliamentarians_to_download$PersonId

parliamentarian_urls <- tibble(id = parliamentarian_ids) %>%
  mutate(url = paste0("https://lop.parl.ca/ParlinfoWebApi/Person/GetPersonWebProfile/", id, "?callback=jQuery33107266187344061623_1569968990479&_=1569968990480"))

## Write out to a file
## Remove the "url" heading row
parliamentarian_urls %>%
  select(url) %>%
  write_csv("data/parliamentarian-urls.csv")

## In terminal, navigate to `data/members`
## Run this curl: xargs -n 1 curl -O < ../parliamentarian-urls.csv
