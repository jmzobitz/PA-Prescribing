### Samples the medicare data to examine the drug categories

drug_list <- medicare_data$data %>%
  bind_rows() %>%
  filter(Prscrbr_Type != "Internal Medicine") %>%
  mutate(Tot_Clms = as.numeric(Tot_Clms))

### Number of unique drugs:
drug_list %>%
  pull(Gnrc_Name) %>%
  unique() %>%
  length()

all_top <- drug_list %>%
  group_by(Gnrc_Name) %>%
  summarize(tot = sum(Tot_Clms)) %>%
  mutate(Prscrbr_Type = "All")

grouped_top <- drug_list %>%
  group_by(Prscrbr_Type,Gnrc_Name) %>%
  summarize(tot = sum(Tot_Clms))

top_drug_tot <- rbind(all_top,grouped_top) %>%
  group_by(Prscrbr_Type) %>%
  nest() %>%
  mutate(pct_val = map(.x=data,.f=~quantile(.x$tot,.9)),
         data2 = map2(.x=data,.y=pct_val,.f=~filter(.x,tot>.y)),
         data2 = map(.x=data2,.f=~arrange(.x,desc(tot)))) %>%
  select(Prscrbr_Type,data2) %>%
  unnest(cols=c("data2"))


out_small <- top_drug_tot %>%
  filter(Prscrbr_Type != "All") %>%
  group_by(Prscrbr_Type) %>%
  slice_max(tot,n=10)

unique_drugs <- tibble(names = top_drug_tot$Gnrc_Name %>% unique())



write_csv(top_drug_tot,file='results/top_drugs.csv')
write_csv(unique_drugs,file='results/unique_drugs.csv')
write_csv(out_small ,file='results/unique_drugs_small.csv')
