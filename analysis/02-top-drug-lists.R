### Author: JMZ
### Modified: 11/7/23
### Purpose: Compute top drugs by each prescriber (overall amount) and saves to file. Was used to help categorize prescribed drugs, as well as compute Table 3(see 04-plot-drug-groups.R)


library(tidyverse)

### Samples the medicare data to examine the drug categories
load('results/medicare-prescribing.Rda')

# Create a data frame of drugs listed
drug_list <- medicare_data$data |>
  bind_rows() |>
  mutate(Tot_Clms = as.numeric(Tot_Clms))

### Compute the number of unique drugs
drug_list |>
  pull(Gnrc_Name) |>
  unique() |>
  length()

# Group by the drugs to find the number prescribed by all prescribers:
all_top <- drug_list |>
  group_by(Gnrc_Name) |>
  summarize(tot = sum(Tot_Clms)) |>
  mutate(Prscrbr_Type = "All")

# Group by the drugs to find the number prescribed by each type of prescribers:
grouped_top <- drug_list |>
  group_by(Prscrbr_Type,Gnrc_Name) |>
  summarize(tot = sum(Tot_Clms))

# Determine which prescribed drugs are in the 90th percentile
top_drug_tot <- rbind(all_top,grouped_top) |>
  group_by(Prscrbr_Type) |>
  nest() |>
  mutate(pct_val = map(.x=data,.f=~quantile(.x$tot,.9)),
         data2 = map2(.x=data,.y=pct_val,.f=~filter(.x,tot>.y)),
         data2 = map(.x=data2,.f=~arrange(.x,desc(tot)))) |>
  select(Prscrbr_Type,data2) |>
  unnest(cols=c("data2"))

# Get the top 10 drugs for each of the different prescriber types
out_small <- top_drug_tot |>
  filter(Prscrbr_Type != "All") |>
  group_by(Prscrbr_Type) |>
  slice_max(tot,n=10)

# Get the generic name for each of the top drugs
unique_drugs <- tibble(names = top_drug_tot$Gnrc_Name |> unique())


# Save to a CSV file
write_csv(top_drug_tot,file='results/top_drugs.csv')
write_csv(unique_drugs,file='results/unique_drugs.csv')
write_csv(out_small ,file='results/unique_drugs_small.csv')
