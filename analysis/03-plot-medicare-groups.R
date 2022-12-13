# Plot medicare totals by group of prescribers

library(tidyverse)
library(ggthemes)
library(gridExtra)
library(grid)
load('results/medicare-prescribing.Rda')


tot_numbers <- vector(mode = "list", length = nrow(medicare_data))

tot_numbers_state <- tot_numbers

tot_claims <- tot_numbers

tot_claims_prscrbr <- tot_numbers

tot_claims_state <- tot_numbers

for (i in 1:8) {

  # How many?
  tot_numbers[[i]] <- medicare_data$data[[i]] %>%
    group_by(Prscrbr_Type) %>%
    filter(Prscrbr_Type != "Internal Medicine") %>%
    summarize(tot = n_distinct(Prscrbr_NPI))

  # How many per state?
  tot_numbers_state[[i]] <- medicare_data$data[[i]] %>%
    group_by(Prscrbr_State_Abrvtn,Prscrbr_Type) %>%
    filter(Prscrbr_Type != "Internal Medicine") %>%
    summarize(tot = n_distinct(Prscrbr_NPI))

  # How many claims by prescriber?

  tot_claims_prscrbr[[i]] <- medicare_data$data[[i]] %>%
    group_by(Prscrbr_Type) %>%
    filter(Prscrbr_Type != "Internal Medicine") %>%
    mutate(Tot_Clms = as.numeric(Tot_Clms)) %>%
    summarize(tot = sum(Tot_Clms))

  tot_claims[[i]] <- medicare_data$data[[i]] %>%
    filter(Prscrbr_Type != "Internal Medicine") %>%
    mutate(Tot_Clms = as.numeric(Tot_Clms)) %>%
    summarize(tot = sum(Tot_Clms))

  tot_claims_state[[i]] <- medicare_data$data[[i]] %>%
    filter(Prscrbr_Type != "Internal Medicine") %>%
    mutate(Tot_Clms = as.numeric(Tot_Clms)) %>%
    group_by(Prscrbr_State_Abrvtn) %>%
    summarize(tot = sum(Tot_Clms))

}


out_data <- tibble(year = medicare_data$year,
                   total = tot_numbers,
                   total_state = tot_numbers_state,
                   total_claims = tot_claims,
                   total_claims_prscrbr = tot_claims_prscrbr,
                   total_claims_state = tot_claims_state)



# Prescriber types by year
p1a <- out_data %>%
  unnest(cols=c("total")) %>%
  mutate(Prscrbr_Type = if_else(Prscrbr_Type=="Family","Family\nMedicine",Prscrbr_Type),
         Prscrbr_Type = if_else(Prscrbr_Type=="Nurse Practitioner","Nurse\nPractitioner",Prscrbr_Type),
         Prscrbr_Type = if_else(Prscrbr_Type=="Physician Assistant","Physician\nAssistant",Prscrbr_Type),
         Prscrbr_Type = if_else(Prscrbr_Type=="General Practice","General\nPractice",Prscrbr_Type)) %>%
  ggplot(aes(x=year,y=tot,color=Prscrbr_Type,shape=Prscrbr_Type)) + geom_point(size=6) + geom_line(size=2) + theme_minimal() +
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
    axis.text.x = element_text(size = 22), #,angle = 45, vjust = 1, hjust=1),
    axis.text.y = element_text(size = 22),
    axis.title.y = element_text(size = 28),
  ) +
  scale_color_colorblind() +
  scale_y_continuous(labels = scales::label_number(suffix = "K", scale = 1e-3),limits=c(0,20e3)) +
  guides(shape="none",color="none") +
  annotate("text",x=2013,y=17500,label="a)",size=12)


p1b <- out_data %>%
  unnest(cols=c("total")) %>%
  mutate(Prscrbr_Type = if_else(Prscrbr_Type=="Family","Family\nMedicine",Prscrbr_Type),
         Prscrbr_Type = if_else(Prscrbr_Type=="Nurse Practitioner","Nurse\nPractitioner",Prscrbr_Type),
         Prscrbr_Type = if_else(Prscrbr_Type=="Physician Assistant","Physician\nAssistant",Prscrbr_Type),
         Prscrbr_Type = if_else(Prscrbr_Type=="General Practice","General\nPractice",Prscrbr_Type)) %>%
  ggplot(aes(x=year,y=tot,fill=Prscrbr_Type)) +
  geom_col(position="fill") +
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
    axis.text.x = element_text(size = 22), #,angle = 45, vjust = 1, hjust=1),
    axis.text.y = element_text(size = 22),
    axis.title.y = element_text(size = 28),
  ) +
  scale_color_colorblind() +
  guides(fill="none") +
  annotate("text",x=2013,y=.90,label="b)",size=12)

### Claims by group
# Prescriber types by year
p1c <- out_data %>%
  unnest(cols=c("total_claims_prscrbr")) %>%
  mutate(Prscrbr_Type = if_else(Prscrbr_Type=="Family","Family\nMedicine",Prscrbr_Type),
         Prscrbr_Type = if_else(Prscrbr_Type=="Nurse Practitioner","Nurse\nPractitioner",Prscrbr_Type),
         Prscrbr_Type = if_else(Prscrbr_Type=="Physician Assistant","Physician\nAssistant",Prscrbr_Type),
         Prscrbr_Type = if_else(Prscrbr_Type=="General Practice","General\nPractice",Prscrbr_Type)) %>%
  ggplot(aes(x=year,y=tot,color=Prscrbr_Type,shape=Prscrbr_Type)) + geom_point(size=6) + geom_line(size=2) + theme_minimal() +
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
    axis.text.x = element_text(size = 22), #,angle = 45, vjust = 1, hjust=1),
    axis.text.y = element_text(size = 22),
    axis.title.y = element_text(size = 28),
  ) +
  scale_color_colorblind() +
  scale_y_continuous(labels = scales::label_number(suffix = "M", scale = 1e-6),limits=c(.5e6,6e7)) +
  annotate("text",x=2013,y=52e6,label="c)",size=12)


p1d <- out_data %>%
  unnest(cols=c("total_claims_prscrbr")) %>%
  mutate(Prscrbr_Type = if_else(Prscrbr_Type=="Family","Family\nMedicine",Prscrbr_Type),
         Prscrbr_Type = if_else(Prscrbr_Type=="Nurse Practitioner","Nurse\nPractitioner",Prscrbr_Type),
         Prscrbr_Type = if_else(Prscrbr_Type=="Physician Assistant","Physician\nAssistant",Prscrbr_Type),
         Prscrbr_Type = if_else(Prscrbr_Type=="General Practice","General\nPractice",Prscrbr_Type)) %>%
  ggplot(aes(x=year,y=tot,fill=Prscrbr_Type)) +
  geom_col(position="fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Year",
       y = "Percentage of Medicare Part D Claims",
       fill = "Prescriber type") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 22),
    legend.title = element_text(size = 22),
    axis.title.x = element_text(size = 28),
    axis.text.x = element_text(size = 22), #,angle = 45, vjust = 1, hjust=1),
    axis.text.y = element_text(size = 22),
    axis.title.y = element_text(size = 28),
  ) +
  scale_color_colorblind()  +
  annotate("text",x=2013,y=.90,label="d)",size=12)


yee <- out_data %>%
  mutate(pct_tot = map(.x=tot_claims_prscrbr,.f=~(mutate(.x,pct=.x$tot/sum(.x$tot)) )) ) %>%
           unnest(cols=c("total_claims_prscrbr"))


#ggsave(p1,filename = 'results/prescriber-types.png',width=6)

ptot_agg <- grid.arrange(
  p1a,
  p1b, p1c,p1d,
  nrow = 2,padding=0.75)


ggsave(ptot_agg,filename = 'results/tot-results-agg.png',width=22,height=17)



annual_ave_claims <- out_data %>%
  unnest(cols=c("total_claims")) %>%
  mutate(pct = c(0,diff(tot)/head(tot,-1))) %>%
  slice_tail(n=-1) %>%
  summarize(mean=mean(pct)) %>%
  pull(mean)

annual_ave_pres <- out_data %>%
  unnest(cols=c("total")) %>%
  group_by(year) %>%
  summarize(tot = sum(tot)) %>%
  mutate(pct = c(0,diff(tot)/head(tot,-1))) %>%
  slice_tail(n=-1) %>%
  summarize(mean=mean(pct)) %>%
  pull(mean)



p2a <- out_data %>%
  unnest(cols=c("total")) %>%
  group_by(year) %>%
  summarize(tot=sum(tot)) %>%
  #mutate(pct = c(0,diff(tot)/head(tot,-1))) %>%
  #slice_tail(n=-1) %>%  # Take the last row
  ggplot(aes(x=year,y=tot)) + geom_point(size=4) + geom_line(size=1.5) +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Number of Medicare Part D\nPrescribers (Thousands)") +
  scale_y_continuous(labels = scales::label_number(suffix = "K", scale = 1e-3),limits=c(0,50e3)) +
  annotate("text", x = 2013.5, y = 45000, label = "a)",size=10) +
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



p2b <- out_data %>%
  unnest(cols=c("total_claims")) %>%
  #mutate(pct = c(0,diff(tot)/head(tot,-1))) %>%
  #slice_tail(n=-1) %>%  # Take the last row
  ggplot(aes(x=year,y=tot)) + geom_point(size=4) + geom_line(size=1.5) +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Number of Medicare Part D\nClaims (Millions)") +
  scale_y_continuous(labels = scales::label_number(suffix = "M", scale = 1e-6),limits=c(0,100e6)) +
  annotate("text", x = 2013.5, y = 90e6, label = "b)",size=10) +
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

ggsave(ptot,filename = 'results/tot-results.png',width=14)

### Determine the relative frequency of the overall groups
prescriber_freq_all <-  out_data %>%
  unnest(cols=c("total")) %>%
  select(year,Prscrbr_Type,tot) %>%
  group_by(year) %>%
  mutate(freq=tot/sum(tot)) %>%
  mutate(Prscrbr_Type = if_else(Prscrbr_Type=="Family","Family Medicine",Prscrbr_Type),
         type = "prescribers")

claims_freq_all <- out_data %>%
  unnest(cols=c("total_claims_prscrbr")) %>%
  select(year,Prscrbr_Type,tot) %>%
  group_by(year) %>%
  mutate(freq=tot/sum(tot)) %>%
  mutate(Prscrbr_Type = if_else(Prscrbr_Type=="Family","Family Medicine",Prscrbr_Type),
         type = "claims")


  ### Save these for later
  save(prescriber_freq_all,claims_freq_all,file = 'results/rel_freq_all.Rda')

######### OLD CODE

  p2_pct <- out_data %>%
    unnest(cols=c("total_claims")) %>%
    mutate(pct = c(0,diff(tot)/head(tot,-1))) %>%
    slice_tail(n=-1) %>%  # Take the last row
    ggplot(aes(x=year,y=pct)) + geom_point(size=2) + geom_line() +
    geom_hline(yintercept = annual_ave,linetype='dashed',color='red') +
    theme_minimal() +
    labs(
      caption = "Dashed line is the average percentage change from 2014-2020.",
      x = "Year",
      y = "Interannual claim percentage change") +
    scale_y_continuous(labels=scales::percent,limits = c(-0.05,0.05)) +
    annotate("text", x = 2015, y = .0325, label = "b)",size=10) +
    theme_bw() +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 10),
      axis.title.x = element_text(size = 14),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      axis.title.y = element_text(size = 14)
    ) +
    scale_color_colorblind()



  out_data %>%
  unnest(cols=c("total_state")) %>%
  filter(Prscrbr_Type == "Nurse Practitioner",year %in% c(2013,2020)) %>%
  select(year,Prscrbr_State_Abrvtn,tot) %>%
  pivot_wider(names_from = "year",values_from = "tot",names_prefix = "y") %>%
  na.omit() %>%
ggplot(
       aes(y = reorder(Prscrbr_State_Abrvtn, y2013),
           x = y2013,
           xend = y2020)) +
  ggalt::geom_dumbbell(size = 1.2,
                size_x = 3,
                size_xend = 3,
                colour = "grey",
                colour_x = "blue",
                colour_xend = "red") +
  theme_minimal() +
  labs(title = "Change in NPs prescribing in medicare",
       subtitle = "2013 to 2020",
       x = "Number of NPs",
       y = "State abbreviation")


  out_data %>%
    unnest(cols=c("total_state")) %>%
    filter(Prscrbr_Type == "Physician Assistant",year %in% c(2013,2019)) %>%
    select(year,Prscrbr_State_Abrvtn,tot) %>%
    pivot_wider(names_from = "year",values_from = "tot",names_prefix = "y") %>%
    na.omit() %>%
    ggplot(
      aes(y = reorder(Prscrbr_State_Abrvtn, y2013),
          x = y2013,
          xend = y2019)) +
    ggalt::geom_dumbbell(size = 1.2,
                         size_x = 3,
                         size_xend = 3,
                         colour = "grey",
                         colour_x = "blue",
                         colour_xend = "red") +
    theme_minimal() +
    labs(title = "Change in PAs prescribing in medicare",
         subtitle = "2013 to 2019",
         x = "Number of PAs",
         y = "State abbreviation")


### Compute the aggregate number of claims over time
  p3 <- gridExtra::grid.arrange(p1,p2,
                          ncol = 2, nrow = 1)

  ggsave(p3,filename = 'results/trends-combined.png',width=15,height=6)
