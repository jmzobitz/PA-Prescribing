### Author: JMZ
### Modified: 11/7/23
### Purpose: Download medicare data from CMS from their API database

### Query the API for medicare data and save the results
library(tidyverse)
library(httr)
library(jsonlite)
library(curl)



base_url <- tibble(year = c(2013:2021),
                   json = c("https://data.cms.gov/data-api/v1/dataset/92d814bd-e2fb-48c2-95e7-a4b388a2c4be/data",
                            "https://data.cms.gov/data-api/v1/dataset/2af61f9c-327c-4a23-8b7f-15e38b56e25a/data",
                            "https://data.cms.gov/data-api/v1/dataset/5da1b683-99ea-4734-8216-66ffdcd5e443/data",
                            "https://data.cms.gov/data-api/v1/dataset/0015c60c-af38-4d06-98bd-f058c0abb778/data",
                            "https://data.cms.gov/data-api/v1/dataset/04b93a42-c533-4e5c-8df9-a8f254886cde/data",
                            "https://data.cms.gov/data-api/v1/dataset/4861ecfc-a656-4dcd-accb-b9c3b840dfcb/data",
                            "https://data.cms.gov/data-api/v1/dataset/5a27f7a8-c7af-434f-a26c-54db03e22cd1/data",
                            "https://data.cms.gov/data-api/v1/dataset/016d9d07-83eb-434d-91cb-0e7183d89492/data",
                            "https://data.cms.gov/data-api/v1/dataset/9552739e-3d05-4c1b-8eff-ecabf391e2e5/data") )



data_list <- vector(mode = "list", length = nrow(base_url))

# Define a function that will take a medicare json url and acquire the data

acquire_medicare_data <- function(json_url) {

  # Define how we are offsetting the data (by inspection the max number of records is 5000)

  offsets <- seq(0,3000000,by=5000)

  medicare_list <- vector(mode = "list", length = length(offsets))

  for(i in 1:length(offsets)) {
    if (i %% 50 == 0) {print(i)}
      # Where we are currently
    offset_type <- paste0("offset=",offsets[[i]])
    curr_command <- paste0(json_url,
                           "?",  # Need a question mark to examine date
                           offset_type,
                           "&size=5000",
                           "&column=Prscrbr_NPI,Prscrbr_State_Abrvtn,Prscrbr_Type,Brnd_Name,Gnrc_Name,Tot_Clms"
    )

    req <- curl::curl_fetch_memory(curr_command)
    out_json <- jsonlite::prettify(rawToChar(req$content))

    medicare_list[[i]] <- jsonlite::fromJSON(out_json) %>%
      filter(Prscrbr_Type %in% c("Nurse Practitioner","Family Medicine","Physician Assistant","Family Practice","General Practice","Internal Medicine"))


  }

  # Bind these up and return
  medicare_out <- bind_rows(medicare_list) %>%
    mutate(Prscrbr_Type = if_else(str_detect(Prscrbr_Type,"Family"),"Family",Prscrbr_Type))

  return(medicare_out)


}

for (i in 1:9) {
  print(i)
  data_list[[i]] <- acquire_medicare_data(base_url$json[i])
}

medicare_data <- tibble(base_url,data = data_list)

save(medicare_data,file = 'results/medicare-prescribing.Rda' )
