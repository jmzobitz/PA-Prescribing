### Author: JMZ
### Modified: 11/7/23
### Purpose: Compute top drugs by each prescriber and saves to file. Create Figures 1-2, and also the relative frequency of each prescriber and drug


# Plot medicare totals by group of prescribers

library(tidyverse)
library(ggthemes)
library(gridExtra)
library(grid)
load('results/medicare-prescribing.Rda')

# Create some empty lists that we will be assigning data to
tot_numbers <- vector(mode = "list", length = nrow(medicare_data))
tot_numbers_state <- tot_numbers
tot_claims <- tot_numbers
tot_claims_prscrbr <- tot_numbers
tot_claims_state <- tot_numbers

# Loop through data sets of each year (2013-2021 --> 9 years)
for (i in 1:9) {

  # How many?
  tot_numbers[[i]] <- medicare_data$data[[i]] |>
    group_by(Prscrbr_Type) |>
    summarize(tot = n_distinct(Prscrbr_NPI))

  # How many per state?
  tot_numbers_state[[i]] <- medicare_data$data[[i]] |>
    group_by(Prscrbr_State_Abrvtn,Prscrbr_Type) |>
    summarize(tot = n_distinct(Prscrbr_NPI))

  # How many claims by prescriber?
  tot_claims_prscrbr[[i]] <- medicare_data$data[[i]] |>
    group_by(Prscrbr_Type) |>
    mutate(Tot_Clms = as.numeric(Tot_Clms)) |>
    summarize(tot = sum(Tot_Clms))

  tot_claims[[i]] <- medicare_data$data[[i]] |>
    mutate(Tot_Clms = as.numeric(Tot_Clms)) |>
    summarize(tot = sum(Tot_Clms))

  tot_claims_state[[i]] <- medicare_data$data[[i]] |>
    mutate(Tot_Clms = as.numeric(Tot_Clms)) |>
    group_by(Prscrbr_State_Abrvtn) |>
    summarize(tot = sum(Tot_Clms))

}

# Now combine them into one big list:
out_data <- tibble(year = medicare_data$year,
                   total = tot_numbers,
                   total_state = tot_numbers_state,
                   total_claims = tot_claims,
                   total_claims_prscrbr = tot_claims_prscrbr,
                   total_claims_state = tot_claims_state)

##### Create Figure 1
annual_ave_claims <- out_data |>
  unnest(cols=c("total_claims")) |>
  mutate(pct = c(0,diff(tot)/head(tot,-1))) |>
  slice_tail(n=-1) |>
  summarize(mean=mean(pct)) |>
  pull(mean)

annual_ave_pres <- out_data |>
  unnest(cols=c("total")) |>
  group_by(year) |>
  summarize(tot = sum(tot)) |>
  mutate(pct = c(0,diff(tot)/head(tot,-1))) |>
  slice_tail(n=-1) |>
  summarize(mean=mean(pct)) |>
  pull(mean)



p2a <- out_data |>
  unnest(cols=c("total")) |>
  group_by(year) |>
  summarize(tot=sum(tot)) |>
  #mutate(pct = c(0,diff(tot)/head(tot,-1))) |>
  #slice_tail(n=-1) |>  # Take the last row
  ggplot(aes(x=year,y=tot)) + geom_point(size=4) + geom_line(size=1.5) +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Number of Medicare Part D\nPrescribers (Thousands)") +
  scale_y_continuous(labels = scales::label_number(suffix = "K", scale = 1e-3),limits=c(0,70e3)) +
  annotate("text", x = 2013.5, y = 60000, label = "a)",size=10) +
  scale_x_continuous(breaks=seq(2013,2021,2)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 22),
    legend.title = element_text(size = 22),
    axis.title.x = element_text(size = 24),
    axis.text.x = element_text(size = 20), #,angle = 45, vjust = 1, hjust=1),
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 24),
  ) +
  scale_color_colorblind()



p2b <- out_data |>
  unnest(cols=c("total_claims")) |>
  #mutate(pct = c(0,diff(tot)/head(tot,-1))) |>
  #slice_tail(n=-1) |>  # Take the last row
  ggplot(aes(x=year,y=tot)) + geom_point(size=4) + geom_line(size=1.5) +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Number of Medicare Part D\nClaims (Millions)") +
  scale_x_continuous(breaks=seq(2013,2021,2)) +
  scale_y_continuous(labels = scales::label_number(suffix = "M", scale = 1e-6),limits=c(0,17.5e7)) +
  annotate("text", x = 2013.5, y = 15e7, label = "b)",size=10) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 22),
    legend.title = element_text(size = 22),
    axis.title.x = element_text(size = 24),
    axis.text.x = element_text(size = 20),#angle = 45, vjust = 1, hjust=1),
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 24),
  ) +
  scale_color_colorblind()




ptot <- grid.arrange(
  p2a,
  p2b,
  nrow = 1)

# This is figure 1:
ggsave(ptot,filename = 'results/tot-results.png',width=14)



######## Create Figure 2:
# Prescriber types by year
p1a <- out_data |>
  unnest(cols=c("total")) |>
  mutate(Prscrbr_Type = if_else(Prscrbr_Type=="Family","Family\nMedicine",Prscrbr_Type),
         Prscrbr_Type = if_else(Prscrbr_Type=="Nurse Practitioner","Nurse\nPractitioner",Prscrbr_Type),
         Prscrbr_Type = if_else(Prscrbr_Type=="Physician Assistant","Physician\nAssistant",Prscrbr_Type),
         Prscrbr_Type = if_else(Prscrbr_Type=="General Practice","General\nPractice",Prscrbr_Type),
         Prscrbr_Type = if_else(Prscrbr_Type=="Internal Medicine","Internal\nMedicine",Prscrbr_Type)) |>
  ggplot(aes(x=year,y=tot,color=Prscrbr_Type,shape=Prscrbr_Type)) + geom_point(size=10) + geom_line(size=4) + theme_minimal() +
  labs(#title = "Unique Medicare Prescribers",
       x = "Year",
       y = "Number of Medicare Part D\nPrescribers (Thousands)",
       color="Prescriber Type",
       shape = "Prescriber Type") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 22),
    legend.title = element_text(size = 22),
    axis.title.x = element_text(size = 28),
    axis.text.x = element_text(size = 24), #,angle = 45, vjust = 1, hjust=1),
    axis.text.y = element_text(size = 24),
    axis.title.y = element_text(size = 28),
  ) +
  scale_color_colorblind() +
  scale_shape_manual(values = 15:19) +
  scale_y_continuous(labels = scales::label_number(suffix = "K", scale = 1e-3),limits=c(0,25e3)) +
  scale_x_continuous(breaks=seq(2013,2021,2)) +
  guides(shape="none",color="none") +
  annotate("text",x=2013,y=22500,label="a)",size=12)


p1b <- out_data |>
  unnest(cols=c("total")) |>
  mutate(Prscrbr_Type = if_else(Prscrbr_Type=="Family","Family\nMedicine",Prscrbr_Type),
         Prscrbr_Type = if_else(Prscrbr_Type=="Nurse Practitioner","Nurse\nPractitioner",Prscrbr_Type),
         Prscrbr_Type = if_else(Prscrbr_Type=="Physician Assistant","Physician\nAssistant",Prscrbr_Type),
         Prscrbr_Type = if_else(Prscrbr_Type=="General Practice","General\nPractice",Prscrbr_Type),
         Prscrbr_Type = if_else(Prscrbr_Type=="Internal Medicine","Internal\nMedicine",Prscrbr_Type)) |>
  ggplot(aes(x=year,y=tot,fill=Prscrbr_Type)) +
  geom_col(position="fill") +
  scale_x_continuous(breaks=seq(2013,2021,2)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Year",
       y = "Percentage of Medicare Part D Prescribers",
       fill = "Prescriber type") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 22),
    legend.title = element_text(size = 22),
    axis.title.x = element_text(size = 28),
    axis.text.x = element_text(size = 24), #,angle = 45, vjust = 1, hjust=1),
    axis.text.y = element_text(size = 24),
    axis.title.y = element_text(size = 28),
  ) +
  scale_color_colorblind() +
  scale_fill_colorblind() +
  guides(fill="none") +
  annotate("text",x=2013,y=.90,label="b)",size=12,color='white')

### Claims by group
# Prescriber types by year
p1c <- out_data |>
  unnest(cols=c("total_claims_prscrbr")) |>
  mutate(Prscrbr_Type = if_else(Prscrbr_Type=="Family","Family\nMedicine",Prscrbr_Type),
         Prscrbr_Type = if_else(Prscrbr_Type=="Nurse Practitioner","Nurse\nPractitioner",Prscrbr_Type),
         Prscrbr_Type = if_else(Prscrbr_Type=="Physician Assistant","Physician\nAssistant",Prscrbr_Type),
         Prscrbr_Type = if_else(Prscrbr_Type=="General Practice","General\nPractice",Prscrbr_Type),
         Prscrbr_Type = if_else(Prscrbr_Type=="Internal Medicine","Internal\nMedicine",Prscrbr_Type)) |>
  ggplot(aes(x=year,y=tot,color=Prscrbr_Type,shape=Prscrbr_Type)) + geom_point(size=10) + geom_line(size=4) + theme_minimal() +
  labs(#title = "Unique Medicare Prescribers",
    x = "Year",
    y = "Number of Medicare Part D\nClaims (Millions)",
    color="Prescriber Type",
    shape = "Prescriber Type") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 22),
    legend.title = element_text(size = 22),
    axis.title.x = element_text(size = 28),
    axis.text.x = element_text(size = 24), #,angle = 45, vjust = 1, hjust=1),
    axis.text.y = element_text(size = 24),
    axis.title.y = element_text(size = 28),
  ) +
  scale_color_colorblind() +
  scale_x_continuous(breaks=seq(2013,2021,2)) +
  scale_shape_manual(values = 15:19) +
  scale_y_continuous(labels = scales::label_number(suffix = "M", scale = 1e-6),limits=c(.5e6,6e7)) +
  annotate("text",x=2013,y=55e6,label="c)",size=12)


p1d <- out_data |>
  unnest(cols=c("total_claims_prscrbr")) |>
  mutate(Prscrbr_Type = if_else(Prscrbr_Type=="Family","Family\nMedicine",Prscrbr_Type),
         Prscrbr_Type = if_else(Prscrbr_Type=="Nurse Practitioner","Nurse\nPractitioner",Prscrbr_Type),
         Prscrbr_Type = if_else(Prscrbr_Type=="Physician Assistant","Physician\nAssistant",Prscrbr_Type),
         Prscrbr_Type = if_else(Prscrbr_Type=="General Practice","General\nPractice",Prscrbr_Type),
  Prscrbr_Type = if_else(Prscrbr_Type=="Internal Medicine","Internal\nMedicine",Prscrbr_Type)) |>
  ggplot(aes(x=year,y=tot,fill=Prscrbr_Type)) +
  geom_col(position="fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks=seq(2013,2021,2)) +
  labs(x = "Year",
       y = "Percentage of Medicare Part D Claims",
       fill = "Prescriber type") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 22),
    legend.title = element_text(size = 22),
    axis.title.x = element_text(size = 28),
    axis.text.x = element_text(size = 24), #,angle = 45, vjust = 1, hjust=1),
    axis.text.y = element_text(size = 24),
    axis.title.y = element_text(size = 28),
  ) +
  scale_color_colorblind()  +
  scale_fill_colorblind() +
  annotate("text",x=2013,y=.90,label="d)",size=12,color='white')


yee <- out_data |>
  mutate(pct_tot = map(.x=tot_claims_prscrbr,.f=~(mutate(.x,pct=.x$tot/sum(.x$tot)) )) ) |>
           unnest(cols=c("total_claims_prscrbr"))




ptot_agg <- grid.arrange(
  p1a,
  p1b, p1c,p1d,
  nrow = 2,padding=0.75)

# This is figure 2
ggsave(ptot_agg,filename = 'results/tot-results-agg.png',width=30,height=20)



##### Determine the relative frequency of the overall groups (used in subsequent code)
prescriber_freq_all <-  out_data |>
  unnest(cols=c("total")) |>
  select(year,Prscrbr_Type,tot) |>
  group_by(year) |>
  mutate(freq=tot/sum(tot)) |>
  mutate(Prscrbr_Type = if_else(Prscrbr_Type=="Family","Family Medicine",Prscrbr_Type),
         type = "prescribers")

prescriber_freq_all |>
  filter(year %in% c(2013,2021),Prscrbr_Type %in% c("Nurse Practitioner","Physician Assistant"))

claims_freq_all <- out_data |>
  unnest(cols=c("total_claims_prscrbr")) |>
  select(year,Prscrbr_Type,tot) |>
  group_by(year) |>
  mutate(freq=tot/sum(tot)) |>
  mutate(Prscrbr_Type = if_else(Prscrbr_Type=="Family","Family Medicine",Prscrbr_Type),
         type = "claims")

claims_freq_all |>
  filter(year %in% c(2013,2021),Prscrbr_Type %in% c("Nurse Practitioner","Physician Assistant"))


  ### Save these for later
  save(prescriber_freq_all,claims_freq_all,file = 'results/rel_freq_all.Rda')

