# Parliamentarians: Who are they? (_at scale_!)

Playing with Library of Parliament data on parliamentarians in Canada.

To enable this repo, [download a copy of the data repo](https://github.com/lchski/lop-parliamentarians-data) (which includes instructions for downloading the data yourself, if you want an up-to-date copy). You’ll want to copy `data/source/lop/` from the data repo into the `data/` folder of this repo. Once you’re done, this repo should have a `data/source/lop/parliamentarians/` folder with over 5,000 files in it.

Then, you can run `load.R`. It may take a few minutes and throw up some errors / warnings—as long as it loads all the data, though, you’re all good! (If not, consider adding an issue so we can sort out what’s happening.) It’s a somewhat large dataset (probably over 1 GB once loaded into R).

## Interesting things to research

A scratch list for myself:

- Critic roles (which have poor dating, understandable)
  - How often do they change in a session?
  - Are they linked to (or do they generate) news coverage, legislation?
- Education
  - Which schools? (correlation to Senate/MP, Minister, etc?)
  - Trends over time: level of education, type of degrees

The `analysis/` folder contains various inquiries I’ve launched. It’s very messy.
