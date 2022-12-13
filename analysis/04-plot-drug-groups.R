### Read in drug list
load('results/medicare-prescribing.Rda')
top_drugs <- readxl::read_xlsx('data/2022.08.17 - top drugs.xlsx',sheet='Drug Names')

### Yey!  Now we need to go into each of the medicare data lists and:
# filter out the drugs
# add in the categories

drug_list <- medicare_data %>%
  select(-json) %>%
  unnest(cols=c("data")) %>%
  filter(Prscrbr_Type != "Internal Medicine") %>%
  mutate(Tot_Clms = as.numeric(Tot_Clms))

drug_list_filtered <- drug_list %>%
  filter(Brnd_Name %in% top_drugs$names) %>%
  inner_join(top_drugs,by=c("Gnrc_Name"="names"))

drug_summary <- drug_list_filtered %>%
  group_by(Prscrbr_Type,category) %>%
  summarize(tot=sum(Tot_Clms)) %>%
  slice_max(order_by=tot,n=5)


drug_summary_10 <- drug_list_filtered %>%
  group_by(Prscrbr_Type,category) %>%
  summarize(tot=sum(Tot_Clms)) %>%
  slice_max(order_by=tot,n=10)

write_csv(drug_summary_10,file='results/top_drugs_categories.csv')

pa_drugs <- drug_summary %>% filter(Prscrbr_Type == "Physician Assistant") %>%
  pull(category) %>% unique()

np_drugs <- drug_summary %>% filter(Prscrbr_Type == "Nurse Practitioner") %>%
  pull(category) %>% unique()

fm_drugs <- drug_summary %>% filter(Prscrbr_Type == "Family") %>%
  pull(category) %>% unique()

gp_drugs <- drug_summary %>% filter(Prscrbr_Type == "General Practice") %>%
  pull(category) %>% unique()

# These are the pa_drugs that are NOT in the np_drugs
d1 <- pa_drugs[(pa_drugs %in% c(fm_drugs,np_drugs,gp_drugs))]
pa_drugs[!(pa_drugs %in% c(fm_drugs,np_drugs,gp_drugs))]

d2 <- np_drugs[(np_drugs %in% c(fm_drugs,pa_drugs,gp_drugs))]
np_drugs[!(np_drugs %in% c(fm_drugs,pa_drugs,gp_drugs))]

d3<- fm_drugs[(fm_drugs %in% c(np_drugs,pa_drugs,gp_drugs))]
fm_drugs[!(fm_drugs %in% c(np_drugs,pa_drugs,gp_drugs))]

d4 <- gp_drugs[(gp_drugs %in% c(np_drugs,pa_drugs,gp_drugs))]
gp_drugs[!(gp_drugs %in% c(np_drugs,pa_drugs,gp_drugs))]

### What's common?
# from the list that is common:
common_drugs <- d1 %>% intersect(d2) %>% intersect(d3) %>% intersect(d4)

text_vals_plot_claims <- tibble(category = common_drugs,
                         text_vals = c("e)","f)","d)"))


# Make a plot of trends over time:
p1_claims <- drug_list_filtered %>%
  filter(category %in% common_drugs) %>%
  group_by(year,Prscrbr_Type,category) %>%
  summarize(tot=sum(Tot_Clms)) %>%
  mutate(Prscrbr_Type = if_else(Prscrbr_Type=="Family","Family Medicine",Prscrbr_Type)) %>%
  ggplot(aes(x=year,y=tot,color=Prscrbr_Type,group=Prscrbr_Type)) + geom_point(size=4) +
  geom_smooth(method="lm",se=FALSE) +
  facet_grid(.~category,) +
  labs(x = "Year",
       y = "Number of Medicare Part D\nClaims (Millions)",
       color = "Drug category") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 22),
    legend.title = element_text(size=24),
    axis.title.x = element_text(size = 24),
    axis.text.x = element_text(size = 22), #angle = 45, vjust = 1, hjust=1),
    axis.text.y = element_text(size = 22),
    axis.title.y = element_text(size = 24),
    strip.text = element_text(size = 24)
  ) +
  scale_color_colorblind() +
  scale_y_continuous(labels = scales::label_number(suffix = "M", scale = 1e-6),limits=c(0,10e6)) +
  geom_text(data = text_vals_plot_claims,aes(label=text_vals),x=2013.5,y=9.0e6,inherit.aes=FALSE,size=10)



# keep

p2_tot <- drug_list_filtered %>%
  filter(category %in% common_drugs) %>%
  group_by(year,category,Prscrbr_Type) %>%
  summarize(tot = n_distinct(Prscrbr_NPI)) %>%
  mutate(Prscrbr_Type = if_else(Prscrbr_Type=="Family","Family Medicine",Prscrbr_Type)) %>%
  ggplot(aes(x=year,y=tot,color=Prscrbr_Type,group=Prscrbr_Type)) + geom_point(size=4) +
  geom_smooth(method="lm",se=FALSE) +
  facet_grid(.~category) +
  labs(x = "Year",
       y = "Number of Medicare Part D\nPrescribers (Thousands)") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 22),
    legend.title = element_text(size=24),
    axis.title.x = element_text(size = 24),
    axis.text.x = element_text(size = 22), #angle = 45, vjust = 1, hjust=1),
    axis.text.y = element_text(size = 22),
    axis.title.y = element_text(size = 24),
    strip.text = element_text(size = 24)
  ) +
  scale_color_colorblind() +
  scale_y_continuous(labels = scales::label_number(suffix = "K", scale = 1e-3),limits=c(0,15e3)) +
  guides(color="none") +
  geom_text(data = text_vals_plot,aes(label=text_vals),x=2013.5,y=14e3,inherit.aes=FALSE,size=10)



#ggsave(p1,filename = 'results/prescriber-types-drugs.png',width=18,height=7)


ptot_agg <- grid.arrange(
  p2_tot,
  p1_claims,
  nrow = 2)


ggsave(ptot_agg,filename = 'results/tot-drugs-agg.png',width=18,height=14)


drug_stats_fit <- drug_list_filtered %>%
  filter(category %in% common_drugs) %>%
  group_by(year,Prscrbr_Type,category) %>%
  summarize(tot=sum(Tot_Clms)) %>%
  mutate(Prscrbr_Type = if_else(Prscrbr_Type=="Family","Family Medicine",Prscrbr_Type)) %>%
  mutate(year_since = year - 2013) %>%
  group_by(Prscrbr_Type,category) %>%
  nest() %>%
  mutate(lm_fit = map(.x=data,.f=~lm(tot~year_since,data=.x))) %>%
  mutate(stats = map(.x=lm_fit,.f=broom::glance),
         summary = map(.x=lm_fit,.f=broom::tidy)) %>%
  select(-data,-lm_fit,-stats) %>%
  unnest(cols=c("summary")) %>%
  filter(term == "year_since") %>%
  mutate(signif_01 = p.value < 0.01,
         signif_05 = p.value < 0.05,
         neg = estimate < 0,
         type='claims')


drug_prescribers_stats_fit <- drug_list_filtered %>%
  filter(category %in% common_drugs) %>%
  group_by(year,Prscrbr_Type,category) %>%
  summarize(tot = n_distinct(Prscrbr_NPI)) %>%
  mutate(Prscrbr_Type = if_else(Prscrbr_Type=="Family","Family Medicine",Prscrbr_Type)) %>%
  mutate(year_since = year - 2013) %>%
  group_by(Prscrbr_Type,category) %>%
  nest() %>%
  mutate(lm_fit = map(.x=data,.f=~lm(tot~year_since,data=.x))) %>%
  mutate(stats = map(.x=lm_fit,.f=broom::glance),
         summary = map(.x=lm_fit,.f=broom::tidy)) %>%
  select(-data,-lm_fit,-stats) %>%
  unnest(cols=c("summary")) %>%
  filter(term == "year_since") %>%
  mutate(signif_01 = p.value < 0.01,
         signif_05 = p.value < 0.05,
         neg = estimate < 0,
         type='prescribers')

write_csv(rbind(drug_stats_fit,drug_prescribers_stats_fit),file='results/drugs_regression_stats.csv')


### Parity question


claims_freq <- drug_list_filtered %>%
  filter(category %in% common_drugs) %>%
  group_by(year,category,Prscrbr_Type) %>%
  summarize(tot=sum(Tot_Clms)) %>%
  mutate(freq = tot/sum(tot)) %>%
  mutate(Prscrbr_Type = if_else(Prscrbr_Type=="Family","Family Medicine",Prscrbr_Type),
         type = "claims")

prescriber_freq <- drug_list_filtered %>%
  filter(category %in% common_drugs) %>%
  group_by(year,category,Prscrbr_Type) %>%
  summarize(tot=n_distinct(Prscrbr_NPI)) %>%
  mutate(freq = tot/sum(tot)) %>%
  mutate(Prscrbr_Type = if_else(Prscrbr_Type=="Family","Family Medicine",Prscrbr_Type),
         type = "prescribers")

### Now do this for the entire dataset
save(prescriber_freq,claims_freq,file = 'results/drugs_rel_freq.Rda')

