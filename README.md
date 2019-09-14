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