# Members of Parliament: Who are they? (_at scale_!)
Diving into the data on Canadian Members of Parliament.

The data come from two sources:

1. Library of Parliament’s [Parlinfo](https://lop.parl.ca/sites/ParlInfo/default/en_CA/People/parliamentarians). [See my instructions for downloading](#appendix-extracting-the-data-from-parlinfo) if you want to replicate the dataset.
2. The Linked Parliamentary Data Project’s [supplementary data](https://www.lipad.ca/data/). _Note: this dataset currently sits unused. I checked it in by accident
   and didn’t bother restarting the repo. Maybe I’ll use this data someday? It seems useful/better structured._

## Appendix: Extracting the data from Parlinfo

Parlinfo offers [a handy search interface](https://lop.parl.ca/sites/ParlInfo/default/en_CA/People/parliamentarians). It’s one of those
awesome interfaces which allows you to hit “Search” and receive _all_ the records :grin:. It _also_ has a CSV export—woohoo! (Though a
direct download of the whole dataset would be even easier...)

Follow these steps to download the data from the search page:

1. Set your desired filters. For the dataset in this repo, I just selected “Member of Parliament” under “Federal Roles”.
2. Press the “Search” button. Wait a bit while the database chugs.
3. Activate the “show/hide columns” interface. Select all the columns you want. (I, being greedy, selected _all_ the columns—even Picture,
   which returns blank values.)
4. Press the “CSV” button. It’s downloaded, woo!

---

Columns to split across dates:

```
role_type_of_parliamentarian
seat_riding_senatorial_division
seat_province_territory [divides on lowercase letter immediately followed by uppercase letter without a space in between]
political_affiliation
role_minister
role_critic
```

Brainstorming here. Split across `role_type_of_parliamentarian` now... Maybe a fancy lookup join thing? We create exploded (but not by day, just `period_start` and `period_end`) tables for each of the above. Then we use the exploded `role_type_of_parliamentarian` as the canonical, and for each day look at each of the “exploded” tables for the above, pulling in the changed value for it (e.g. pulling in `seat_riding_senatorial_division` will usually be the same, but then it changes one day and you look to the next one). Something like that.


`role_type_of_parliamentarian`, `seat_riding_senatorial_division`, and `seat_province_territory` can all be linked. Split them, give them unique ID (`id` + `row_number()`), join, then split on the date range.

Separating on `seat_riding_senatorial_division`, some riding names have, e.g., “(City of)” in them (see parliamentarian 17, “Ottawa (City of)”):

```
  geography       period_start        
  <chr>           <chr>               
1 Ottawa          City of             
2 Sherbrooke      Town of             
3 Alberta         Provisional District
4 Saskatchewan    Provisional District
5 Ottawa          County of           
6 Bloor and Yonge Toronto             
```

So we need some funky regex:

* `(?<=[ 0-9])(\\))`: only select `)` if it has a number before it (i.e. not `of)`)
* ` (\\()(?=[0-9])`: only select ` (` if it has a number after it (i.e. not `(City)`)

Then some geographies have funky date ranges, for whatever reason. We get a few more geographies than we do roles (6194 vs 6188 as of writing). After merging, and without dropping the date ranges from the geographies, we can find them like so:

```
lop_mps_role_type_of_parliamentarian_by_role %>% filter(period_end.x != period_end.y)
```

Returns 10 as of writing. A few are off by one dates (assumed data entry), e.g., `9-2` (`Adams, Michael`). Others have odd overlaps, e.g., `1274-2`—there’s also a `1274-3` in the geographies, that’s not in the roles.

Just letting it slide, but noting here for future possible investigation.

## Interesting things to research

- Critic roles (which have poor dating, understandable)
  - How often do they change in a session?
  - Are they linked to (or do they generate) news coverage, legislation?
