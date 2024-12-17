#*******************************************************************************
#*******************************************************************************
#Qld threatened species information database and prioritisation
#Code to create main prioritisation table
#2024

#Tracy Rout

#17-12-2024
#*******************************************************************************
#*******************************************************************************


#Initialising ####
#Clean up work station
rm(list = ls())

#Version date - for outputs
ver_date = "171224"

#Install necessary packages
#install.packages("tidyverse")
#install.packages("DataCombine")
#install.packages("skimr")

#Load packages
library(tidyverse)
library(readxl) #installed in tidyverse but needs to be loaded specifically
library(dplyr)
library(stringr)
library(DataCombine)
library(skimr)
library(lubridate)
select <- dplyr::select
options(scipen = 999) # to change the notation


#Set working directory ####
#setwd("C:/Your path/TS Database in R")


#Import data (Excel sheets) ####
#Database management information
species_ref <- read_excel("Input files/2024_reference_list_update_210624.xlsx", sheet = "2024 ref list") %>% #updated
  rename(QSPid = QSPid_june2024)

species_replacements <- read_excel("Input files/Species_replacements_TRout_141024.xlsx", sheet = "All Replacements 2024") #updated

species_name_replacements <- species_replacements %>%
  select(original_name, replacement_name) %>%
  filter(!is.na(original_name)) %>%
  rename(
    from = original_name,
    to = replacement_name)

taxon_ID_replacements <- species_replacements %>%
  select(original_taxon_ID, replacement_taxon_ID) %>%
  filter(!is.na(original_taxon_ID)) %>%
  rename(
    from = original_taxon_ID,
    to = replacement_taxon_ID)


#Listing information - all updated 2024
wildnet_allsp <- read.csv("Input files/Wildnet_downloaded_210624.csv")
epbc_additional <- read.csv("Input files/EPBC_additional_status_modified_TRout_270524.csv")
iucn <- read.csv("Input files/IUCN_assessments_downloaded_080724.csv")

#DETSI data and priorities - not updated
vbmf_values <- read_excel("Input files/VBMF_values_DES_modified_TRout_040823.xlsx") 
endemic <- read_excel("Input files/Endemic_Nearendemic_2019_modified_TRout_210823.xlsx") 
action_species <- read.csv("Input files/Federal_action_plan_species_TRout_020623.csv") 

data_deficient_fauna <- read_excel("Input files/Data_deficient_fauna_DES_received_310723.xlsx") %>% 
  select(`Scientific name`, `Data deficient (n<10)`) %>%
  rename("Species name" = "Scientific name", "Data deficient DETSI species" = "Data deficient (n<10)") %>%
  filter(!is.na(`Data deficient DETSI species`))
data_deficient_flora <- read_excel("Input files/Data_deficient_flora_DES_received_310723.xlsx") %>% 
  select(`Species`, `<10 records`) %>%
  rename("Species name" = "Species", "Data deficient DETSI species" = "<10 records") %>%
  filter(!is.na(`Data deficient DETSI species`))
data_deficient_allsp <- bind_rows(data_deficient_fauna, data_deficient_flora) %>%
  mutate(
    `Data deficient DETSI species` = "Yes"
  )

bot1 <- read_excel("Input files/BOTI_DES_260719.xlsx")
bot2 <- read_excel("Input files/BOTII_DES_modified_TRout_170921.xlsx")
DETSI_list <- read_excel("Input files/DES_list_DES_received_060821.xlsx", sheet = "species shortlist", range = "A1:AD571")
hab_loss_climate_sens_fauna <- read_excel("Input files/Habitat_loss_climate_sensitivity_DES_MLaidlaw_received_060622.xlsx", sheet = "Fauna", range = "A1:G135")
hab_loss_climate_sens_flora <- read_excel("Input files/Habitat_loss_climate_sensitivity_DES_MLaidlaw_received_060622.xlsx", sheet = "Flora", range = "A1:G244")
hab_loss_climate_sens_allsp <- bind_rows(hab_loss_climate_sens_fauna, hab_loss_climate_sens_flora)
tso_species <- read_excel("Input files/TSO_species_DES_modified_TRout_110422.xlsx", sheet = "TSO species")


#Other threatened species research data - not updated
red_hot_plants <- read_excel("Input files/Red_hot_plants_Qld_JSilcock_received_201219.xlsx", sheet = "QLD_CR_EN_ALL")
red_hot_animals <- read_excel("Input files/Red_hot_animals_TRout_060622.xlsx")

#CSIRO outputs
spatial_analyses <- read_excel("Input files/QSP_spatial_analyses_CSIRO_281124.xlsx") #updated 2024
overlap_species_external <- read_excel("Input files/QSP_number_of_species_overlap_30pct_external_CSIRO_171224.xlsx") #External, updated 2024
overlap_species_threat_external <- read_excel("Input files/QSP_number_of_species_threat_overlap_30pct_external_CSIRO_171224.xlsx") #External, updated 2024
overlap_species_internal <- read_excel("Input files/QSP_number_of_species_overlap_30pct_internal_CSIRO_171224.xlsx") #Internal, updated 2024
overlap_species_threat_internal <- read_excel("Input files/QSP_number_of_species_threat_overlap_30pct_internal_CSIRO_171224.xlsx") #Internal, updated 2024
survey <- read_excel("Input files/QSP_species_survey_benefit_goal_feasibility_CSIRO_271124.xlsx", range = "A1:U105") #updated Nov 2023
feasibility_actions <- read_excel("Input files/QSPid_feasibility_based_on_actions_CSIRO_121123.xlsx") #updated Nov 2023
costs <- read.csv("Output files/main_table_costs_281124.csv") #Output of ts_database_2024_cost_models.R - ensure date is latest

#Prepare columns and column names ####
#Create a separate and single row for each species in VBMF values data frame
vbmf_values <- separate_rows(vbmf_values,`Species name`, sep = ", ") #Separate to one species per row
vbmf_values <- vbmf_values[complete.cases(vbmf_values[ , c('Species name')]), ] #Remove rows where species name is NA
vbmf_values <- group_by(vbmf_values, `Species name`) %>% #group by species
  summarise(Parks = toString(unique(Park)),
            Key_Values = toString(unique(`Key Value`)),
            VBMF_Values = toString(unique(`VBMF value`)),
            Notes = toString(unique(`Notes including aggegrations`)))

#Rename a couple of problematic column names
colnames(bot2)[8] <- 'Criterion 1a Conservation status'
colnames(bot2)[14] <- 'Criterion 3b Feasibility of conservation'

#Copy federal cassowary information to apply to both northern and southern populations - these are listed separately in the database
epbc_additional$Scientific.Name <- gsub(epbc_additional$Scientific.Name, pattern = "Casuarius casuarius johnsonii", replacement = "Casuarius casuarius johnsonii northern population;Casuarius casuarius johnsonii southern population") 
epbc_additional <- separate_rows(epbc_additional, Scientific.Name, sep = ";")


#Create a consistent species name column (species_name_dbase) that can be used as a key to join data frames ####
#Create a column in each dataframe called species_name_dbase, start as a copy of existing species names
action_species <- mutate(action_species, species_name_dbase = action_species$`species_name`)
bot1 <- mutate(bot1, species_name_dbase = bot1$`Species Name`)
bot2 <- mutate(bot2, species_name_dbase = bot2$`Scientific name`)
data_deficient_allsp <- mutate(data_deficient_allsp, species_name_dbase = data_deficient_allsp$`Species name`)
DETSI_list <- mutate(DETSI_list, species_name_dbase = DETSI_list$`Scientific name`)
endemic <- mutate(endemic, species_name_dbase = endemic$Taxon)
epbc_additional <- mutate(epbc_additional, species_name_dbase = epbc_additional$`Scientific.Name`)
iucn <- mutate(iucn, species_name_dbase = iucn$`scientificName`)
red_hot_animals <- mutate(red_hot_animals, species_name_dbase = red_hot_animals$`Species name`)
red_hot_plants <- mutate(red_hot_plants, species_name_dbase = red_hot_plants$`Species`)
species_ref <- mutate(species_ref, species_name_dbase = species_ref$species_name_june2024)
tso_species <- mutate(tso_species, species_name_dbase = tso_species$`Species name`)
vbmf_values <- mutate(vbmf_values, species_name_dbase = vbmf_values$`Species name`)
wildnet_allsp <- mutate(wildnet_allsp, species_name_dbase = wildnet_allsp$Scientific_name)
#Note that we will be using taxon ID to join habitat loss climate sensitivity, and QSPid to data frames that were CSIRO outputs


#Transform species names so they are easy to match consistently - remove all non-alphanumeric characters, spaces, and convert to lower case
action_species$species_name_dbase <- gsub("×", "x", action_species$species_name_dbase) #replace this special character with a normal x
action_species$species_name_dbase <- gsub("[^[:alnum:]]", "", action_species$species_name_dbase) #remove all other non-alphanumeric characters (including spaces)
action_species$species_name_dbase <- tolower(action_species$species_name_dbase) #convert to lower case

bot1$species_name_dbase <- gsub("×", "x", bot1$species_name_dbase) #replace this special character with a normal x
bot1$species_name_dbase <- gsub("[^[:alnum:]]", "", bot1$species_name_dbase) #remove all other non-alphanumeric characters (including spaces)
bot1$species_name_dbase <- tolower(bot1$species_name_dbase) #convert to lower case

bot2$species_name_dbase <- gsub("×", "x", bot2$species_name_dbase) #replace this special character with a normal x
bot2$species_name_dbase <- gsub("[^[:alnum:]]", "", bot2$species_name_dbase) #remove all other non-alphanumeric characters (including spaces)
bot2$species_name_dbase <- tolower(bot2$species_name_dbase) #convert to lower case

data_deficient_allsp$species_name_dbase <- gsub("×", "x", data_deficient_allsp$species_name_dbase) #replace this special character with a normal x
data_deficient_allsp$species_name_dbase <- gsub("[^[:alnum:]]", "", data_deficient_allsp$species_name_dbase) #remove all other non-alphanumeric characters (including spaces)
data_deficient_allsp$species_name_dbase <- tolower(data_deficient_allsp$species_name_dbase) #convert to lower case

DETSI_list$species_name_dbase <- gsub("×", "x", DETSI_list$species_name_dbase) #replace this special character with a normal x
DETSI_list$species_name_dbase <- gsub("[^[:alnum:]]", "", DETSI_list$species_name_dbase) #remove all other non-alphanumeric characters (including spaces)
DETSI_list$species_name_dbase <- tolower(DETSI_list$species_name_dbase) #convert to lower case

endemic$species_name_dbase <- gsub("×", "x", endemic$species_name_dbase) #replace this special character with a normal x
endemic$species_name_dbase <- gsub("[^[:alnum:]]", "", endemic$species_name_dbase) #remove all other non-alphanumeric characters (including spaces)
endemic$species_name_dbase <- tolower(endemic$species_name_dbase) #convert to lower case

epbc_additional$species_name_dbase <- gsub("×", "x", epbc_additional$species_name_dbase) #replace this special character with a normal x
epbc_additional$species_name_dbase <- gsub("[^[:alnum:]]", "", epbc_additional$species_name_dbase) #remove all other non-alphanumeric characters (including spaces)
epbc_additional$species_name_dbase <- tolower(epbc_additional$species_name_dbase) #convert to lower case

iucn$species_name_dbase <- gsub("×", "x", iucn$species_name_dbase) #replace this special character with a normal x
iucn$species_name_dbase <- gsub("[^[:alnum:]]", "", iucn$species_name_dbase) #remove all other non-alphanumeric characters (including spaces)
iucn$species_name_dbase <- tolower(iucn$species_name_dbase) #convert to lower case

red_hot_animals$species_name_dbase <- gsub("×", "x", red_hot_animals$species_name_dbase) #replace this special character with a normal x
red_hot_animals$species_name_dbase <- gsub("[^[:alnum:]]", "", red_hot_animals$species_name_dbase) #remove all other non-alphanumeric characters (including spaces)
red_hot_animals$species_name_dbase <- tolower(red_hot_animals$species_name_dbase) #convert to lower case

red_hot_plants$species_name_dbase <- gsub("×", "x", red_hot_plants$species_name_dbase) #replace this special character with a normal x
red_hot_plants$species_name_dbase <- gsub("[^[:alnum:]]", "", red_hot_plants$species_name_dbase) #remove all other non-alphanumeric characters (including spaces)
red_hot_plants$species_name_dbase <- tolower(red_hot_plants$species_name_dbase) #convert to lower case

species_ref$species_name_dbase <- gsub("×", "x", species_ref$species_name_dbase) #replace this special character with a normal x
species_ref$species_name_dbase <- gsub("[^[:alnum:]]", "", species_ref$species_name_dbase) #remove all other non-alphanumeric characters (including spaces)
species_ref$species_name_dbase <- tolower(species_ref$species_name_dbase) #convert to lower case

tso_species$species_name_dbase <- gsub("×", "x", tso_species$species_name_dbase) #replace this special character with a normal x
tso_species$species_name_dbase <- gsub("[^[:alnum:]]", "", tso_species$species_name_dbase) #remove all other non-alphanumeric characters (including spaces)
tso_species$species_name_dbase <- tolower(tso_species$species_name_dbase) #convert to lower case

vbmf_values$species_name_dbase <- gsub("×", "x", vbmf_values$species_name_dbase) #replace this special character with a normal x
vbmf_values$species_name_dbase <- gsub("[^[:alnum:]]", "", vbmf_values$species_name_dbase) #remove all other non-alphanumeric characters (including spaces)
vbmf_values$species_name_dbase <- tolower(vbmf_values$species_name_dbase) #convert to lower case

wildnet_allsp$species_name_dbase <- gsub("×", "x", wildnet_allsp$species_name_dbase) #replace this special character with a normal x
wildnet_allsp$species_name_dbase <- gsub("[^[:alnum:]]", "", wildnet_allsp$species_name_dbase) #remove all other non-alphanumeric characters (including spaces)
wildnet_allsp$species_name_dbase <- tolower(wildnet_allsp$species_name_dbase) #convert to lower case


#Do the same for the species name replacements
species_name_replacements$from <- gsub("×", "x", species_name_replacements$from) #replace this special character with a normal x
species_name_replacements$from <- gsub("[^[:alnum:]]", "", species_name_replacements$from) #remove all other non-alphanumeric characters (including spaces)
species_name_replacements$from <- tolower(species_name_replacements$from) #convert to lower case

species_name_replacements$to <- gsub("×", "x", species_name_replacements$to) #replace this special character with a normal x
species_name_replacements$to <- gsub("[^[:alnum:]]", "", species_name_replacements$to) #remove all other non-alphanumeric characters (including spaces)
species_name_replacements$to <- tolower(species_name_replacements$to) #convert to lower case


#Clean up EPBC dataset ####
#Use information for listed populations of koala and grey nurse shark as this applies in Qld
epbc_additional <- epbc_additional %>%
  filter(
    species_name_dbase != "phascolarctoscinereus",
    species_name_dbase != "carchariastaurus")

epbc_additional$species_name_dbase <- gsub(epbc_additional$species_name_dbase, pattern = "phascolarctoscinereuscombinedpopulationsofqldnswandtheact", replacement = "phascolarctoscinereus") 
epbc_additional$species_name_dbase <- gsub(epbc_additional$species_name_dbase, pattern = "carchariastauruseastcoastpopulation", replacement = "carchariastaurus") 

all_epbc <- epbc_additional %>%
  mutate(
    Conservation.Advice = dmy(Conservation.Advice),
    Recovery.Plan.1 = dmy(Recovery.Plan.1),
    Recovery.Plan.2 = dmy(Recovery.Plan.2),
    Recovery.Plan = case_when( #Take only most recent recovery plan date
      Recovery.Plan.1 > Recovery.Plan.2 ~ Recovery.Plan.1,
      Recovery.Plan.2 > Recovery.Plan.1 ~ Recovery.Plan.2,
      !is.na(Recovery.Plan.1) ~ Recovery.Plan.1),
    "EPBC common name" = na_if(Common.Name, " ")) %>%
  select(`EPBC common name`, EPBC.Threatened.Species.Date.Effective, Kingdom:Family, Conservation.Advice, Recovery.Plan, EPBC.Act.Migratory, Bonn, CAMBA, JAMBA, ROKAMBA, Marine.Status, Cetacean, species_name_dbase) %>%
  distinct()
  
  
#Get rid of blanks by setting to NA
all_epbc[all_epbc == ""] <- NA


#Replace inconsistent species names in species_name_dbase columns to match the names in species_ref ####
#Note - not doing this for EPBC additional dataset as it seems to list old and new species names
for (i in 1:nrow(species_name_replacements)) 
{
  
  for (j in 1:nrow(action_species))
  {
    if (action_species$species_name_dbase[j] == species_name_replacements$from[i])
    {action_species$species_name_dbase[j] <- species_name_replacements$to[i]}
  }
  
  for (j in 1:nrow(bot1))
  {
    if (bot1$species_name_dbase[j] == species_name_replacements$from[i])
    {bot1$species_name_dbase[j] <- species_name_replacements$to[i]}
  }
  
  for (j in 1:nrow(bot2))
  {
    if (bot2$species_name_dbase[j] == species_name_replacements$from[i])
    {bot2$species_name_dbase[j] <- species_name_replacements$to[i]}
  }
  
  for (j in 1:nrow(data_deficient_allsp))
  {
    if (data_deficient_allsp$species_name_dbase[j] == species_name_replacements$from[i])
    {data_deficient_allsp$species_name_dbase[j] <- species_name_replacements$to[i]}
  }
  
  for (j in 1:nrow(DETSI_list))
  {
    if (DETSI_list$species_name_dbase[j] == species_name_replacements$from[i])
    {DETSI_list$species_name_dbase[j] <- species_name_replacements$to[i]}
  }

  for (j in 1:nrow(endemic))
  {
    if (endemic$species_name_dbase[j] == species_name_replacements$from[i])
    {endemic$species_name_dbase[j] <- species_name_replacements$to[i]}
  }
  
  for (j in 1:nrow(iucn))
  {
    if (iucn$species_name_dbase[j] == species_name_replacements$from[i])
    {iucn$species_name_dbase[j] <- species_name_replacements$to[i]}
  }
  
  for (j in 1:nrow(red_hot_animals))
  {
    if (red_hot_animals$species_name_dbase[j] == species_name_replacements$from[i])
    {red_hot_animals$species_name_dbase[j] <- species_name_replacements$to[i]}
  }
  
  for (j in 1:nrow(red_hot_plants))
  {
    if (red_hot_plants$species_name_dbase[j] == species_name_replacements$from[i])
    {red_hot_plants$species_name_dbase[j] <- species_name_replacements$to[i]}
  }
  
  for (j in 1:nrow(tso_species))
  {
    if (tso_species$species_name_dbase[j] == species_name_replacements$from[i])
    {tso_species$species_name_dbase[j] <- species_name_replacements$to[i]}
  }
  
  for (j in 1:nrow(vbmf_values))
  {
    if (vbmf_values$species_name_dbase[j] == species_name_replacements$from[i])
    {vbmf_values$species_name_dbase[j] <- species_name_replacements$to[i]}
  }
}
#Now we have a key column (species_name_dbase) that we can use to match species consistently across these dataframes

#Replace any WildNet taxon IDs belonging to subsceded species ####
for (i in 1:nrow(taxon_ID_replacements)) 
{
  for (j in 1:nrow(hab_loss_climate_sens_allsp))
  {
    if (hab_loss_climate_sens_allsp$`Taxon Id`[j] == taxon_ID_replacements$from[i])
    {hab_loss_climate_sens_allsp$`Taxon Id`[j] <- taxon_ID_replacements$to[i]}
  }
}

#Tidy up each data frame before joining - Species reference list (updated) ####
species_ref_join <- species_ref %>%
  mutate(
    "Species name source" = "WildNet",
    "WildNet download date" = "2024") %>%
  rename("Species name" = species_name_june2024, 
         "WildNet Taxon Id" = wildnet_taxon_id_june2024) %>%
  select(QSPid, `Species name`, `Species name source`, `WildNet Taxon Id`, `WildNet download date`, `Species 'active' for prioritisation`, species_name_dbase)


#WildNet data (updated) ####
wildnet_join <- wildnet_allsp %>%
  select(species_name_dbase, Kingdom:Family, Common_name, NCA_status, EPBC_status, Confidential, Endemicity) %>%
  mutate(
    "Confidential and Endemic source" = "Wildnet",
    "NCA status" = case_when( #codes from here https://www.data.qld.gov.au/dataset/conservation-status-of-queensland-wildlife/resource/d20c485d-b5dc-4c07-8304-df6a21783d4b
      NCA_status == "C" ~ "Least concern",
      NCA_status == "CR" ~ "Critically Endangered",
      NCA_status == "E" ~ "Endangered",
      NCA_status == "EX" ~ "Extinct",  
      NCA_status == "I" ~ "International",
      NCA_status == "NT" ~ "Near threatened",
      NCA_status == "P" ~ "Prohibited",
      NCA_status == "PE" ~ "Extinct in the Wild",
      NCA_status == "SL" ~ "Special least concern",
      NCA_status == "V" ~ "Vulnerable",
      TRUE ~ as.character(NA)),
    "NCA status source" = case_when(
      !is.na(`NCA status`) ~ "Wildnet",
      TRUE ~ as.character(NA)),
    "NCA status source date" = case_when(
      !is.na(`NCA status`) ~ "2024",
      TRUE ~ as.character(NA)),
    "EPBCA status" = case_when( #codes from here https://www.data.qld.gov.au/dataset/conservation-status-of-queensland-wildlife/resource/d20c485d-b5dc-4c07-8304-df6a21783d4b
      EPBC_status == "CD" ~ "Conservation Dependent",
      EPBC_status == "CE" ~ "Critically Endangered",
      EPBC_status == "E" ~ "Endangered",
      EPBC_status == "EX" ~ "Extinct",  
      EPBC_status == "XW" ~ "Extinct in the Wild",
      EPBC_status == "V" ~ "Vulnerable",
      TRUE ~ as.character(NA)),
    "EPBCA status source" = case_when(
      !is.na(`EPBCA status`) ~ "Wildnet",
      TRUE ~ as.character(NA)),
    "EPBCA status source date" = case_when(
      !is.na(`EPBCA status`) ~ "2024",
      TRUE ~ as.character(NA)),
    "Confidential species" = case_when(
      Confidential == "Y" ~ "Yes",
      Confidential == "N" ~ "No",
      TRUE ~ as.character(NA)),
    "Endemic to Queensland" = case_when(
      Endemicity == "IA" ~ "Introduced (Intranational)",
      Endemicity == "II" ~ "Introduced (International)",
      Endemicity == "IU" ~ "Introduced (Unknown)",
      Endemicity == "Q" ~ "Queensland Endemic",
      Endemicity == "QA" ~ "Intranational",
      Endemicity == "QAI" ~ "Not Endemic to Australia",
      Endemicity == "QI" ~ "Regional Endemic",
      Endemicity == "U" ~ "Unknown",
      Endemicity == "VA" ~ "Vagrant (Intranational)",
      Endemicity == "VI" ~ "Vagrant (International)",
      Endemicity == "VU" ~ "Vagrant (Unknown)",
      Endemicity == "XA" ~ "Exotic (Intranational)",
      Endemicity == "XI" ~ "Exotic (International)",
      Endemicity == "XU" ~ "Exotic (Unknown)",
      TRUE ~ as.character(NA)),
    "WildNet common name" = case_when(
      Common_name == "" ~ as.character(NA),
      !is.na(Common_name) ~ `Common_name`,
      TRUE ~ as.character(NA))) %>%
  select(
    species_name_dbase:Family,
    `WildNet common name`,
    `NCA status`, 
    `NCA status source`, 
    `NCA status source date`,
    `EPBCA status`, 
    `EPBCA status source`, 
    `EPBCA status source date`,
    `Confidential species`, 
    `Endemic to Queensland`,
    `Confidential and Endemic source`) %>%
  rename(
    "WildNet kingdom" = "Kingdom",
    "WildNet class" = "Class",
    "WildNet family" = "Family")

#EPBC data (updated)####
all_epbc_join <- all_epbc %>%
  mutate(
    "Conservation advice and recovery plan source" = case_when(
      !is.na(`Conservation.Advice`) ~ "Commonwealth Species Profiles and Threats database",
      !is.na(`Recovery.Plan`) ~ "Commonwealth Species Profiles and Threats database",
      TRUE ~ as.character(NA)),
    "Conservation advice and recovery plan source date" = case_when(
      !is.na(`Conservation.Advice`) ~ "2024",
      !is.na(`Recovery.Plan`) ~ "2024",
      TRUE ~ as.character(NA)),
    "Migratory, marine and cetacean species source" = "Commonwealth Species Profiles and Threats database",
    "Migratory, marine and cetacean species source date" = "2024",
    "On EPBCA migratory species list" = case_when(
        !is.na(EPBC.Act.Migratory) ~ "Yes",
        TRUE ~ as.character(NA)),
    "Included in Bonn Convention" = case_when(
        !is.na(Bonn) ~ "Yes",
        TRUE ~ as.character(NA)),
    "Included in CAMBA" = case_when(
        !is.na(CAMBA) ~ "Yes",
        TRUE ~ as.character(NA)),
    "Included in JAMBA" = case_when(
      !is.na(JAMBA) ~ "Yes",
      TRUE ~ as.character(NA)),
    "Included in ROKAMBA" = case_when(
      !is.na(ROKAMBA) ~ "Yes",
      TRUE ~ as.character(NA)),
    "On EPBCA marine species list" = case_when(
      !is.na(Marine.Status) ~ "Yes",
      TRUE ~ as.character(NA)),
    "Cetacean (special protections in the Australian Whale Sanctuary)" = case_when(
      !is.na(Cetacean) ~ "Yes",
      TRUE ~ as.character(NA)),
    "Conservation advice date" = as.character(year(ymd(`Conservation.Advice`))),
    "Recovery plan date" = as.character(year(ymd(`Recovery.Plan`)))) %>%
  select(
    species_name_dbase, 
    EPBC.Threatened.Species.Date.Effective,
    `EPBC common name`,
    Kingdom:Family,
    `Conservation advice date`, 
    `Recovery plan date`, 
    `Conservation advice and recovery plan source`,
    `Conservation advice and recovery plan source date`,
    `On EPBCA migratory species list`:`Cetacean (special protections in the Australian Whale Sanctuary)`,
    `Migratory, marine and cetacean species source`,
    `Migratory, marine and cetacean species source date`) %>%
  rename(
    "Listing Date" = "EPBC.Threatened.Species.Date.Effective",
    "EPBC kingdom" = "Kingdom",
    "EPBC class" = "Class",
    "EPBC family" = "Family")

#Back on Track data (not updated) ####
bot1_join <- bot1 %>%
  mutate(
    "BOT I conservation status" = case_when(
      `Criteria 1a` == 4 ~ "Endangered",
      `Criteria 1a` == 3 ~ "Vulnerable",
      `Criteria 1a` == 2 ~ "Near Threatened",
      `Criteria 1a` == 1 ~ "Least Concern",
      TRUE ~ as.character(NA))) %>%
  select(
    species_name_dbase, 
    Kingdom:`Class Name`,
    `Common Name`,
    `BOT I conservation status`,
    `State Rank`,
    `Criteria 3b`) %>%
  rename(
    "BOT I kingdom" = "Kingdom",
    "BOT I family" = "Family",
    "BOT I class" = "Class Name",
    "BOT I common name" = "Common Name",
    "BOT I priority status" = "State Rank",
    "BOT I feasibility" = "Criteria 3b")

bot_test <- bot1 %>%
  left_join(species_ref_join)

bot2_join <- bot2 %>%
  mutate(
    "BOT II conservation status" = case_when(
      `Criterion 1a Conservation status` == 4 ~ "Endangered",
      `Criterion 1a Conservation status` == 3 ~ "Vulnerable",
      `Criterion 1a Conservation status` == 2 ~ "Near Threatened",
      `Criterion 1a Conservation status` == 1 ~ "Least Concern",
      TRUE ~ as.character(NA)),
    "Evolutionary value" = case_when(
      `Criterion 2d\r\nEvolutionary value` == 4 ~ "Species is the only representative of its family",
      `Criterion 2d\r\nEvolutionary value` == 3 ~ "Species is the only representative of its genus",
      `Criterion 2d\r\nEvolutionary value` == 2 ~ "Species in a genus with 2, 3 or 4 species",
      `Criterion 2d\r\nEvolutionary value` == 1 ~ "Species in a genus with more than 4 species",
      TRUE ~ as.character(NA)),
    "Evolutionary value source" = case_when(
      !is.na(`Evolutionary value`) ~ "Back on Track II",
      TRUE ~ as.character(NA)),
    "Evolutionary value source date" = case_when(
      !is.na(`Evolutionary value`) ~ "2010",
      TRUE ~ as.character(NA)),
    "Ecological value" = case_when(
      `Criterion 2a\r\nEcological value` == 4 ~ "Keystone species or structuring species, top predator, significant dispersal or pollination agent",
      `Criterion 2a\r\nEcological value` == 3 ~ "Species of significance in ecosystem/s processes but shares this role with one or two other species in the same functional group in the ecosystems in which it lives",
      `Criterion 2a\r\nEcological value` == 2 ~ "Role in ecosystem processes is shared by 3, 4 or 5 other species in the same functional group in the ecosystem/s in which it lives",
      `Criterion 2a\r\nEcological value` == 1 ~ "Role in ecosystem processes is shared by numerous other species in the same functional group in the ecosystem/s in which it lives",
      TRUE ~ as.character(NA)),
    "Ecological value source" = case_when(
      !is.na(`Ecological value`) ~ "Back on Track II",
      TRUE ~ as.character(NA)),
    "Ecological value source date" = case_when(
      !is.na(`Ecological value`) ~ "2010",
      TRUE ~ as.character(NA))) %>%
  select(
    species_name_dbase,
    Family,
    `Common name`,
    `BOT II conservation status`,
    `Back on Track status`,
    `Evolutionary value`,
    `Evolutionary value source`,
    `Evolutionary value source date`,
    `Ecological value`,
    `Ecological value source`,
    `Ecological value source date`,
    `Criterion 3b Feasibility of conservation`) %>%
  rename(
    "BOT II family" = "Family",
    "BOT II common name" = "Common name",
    "BOT II priority status" = "Back on Track status",
    "BOT II feasibility" = "Criterion 3b Feasibility of conservation")

#IUCN data - updated 2024 ####
iucn_join <- iucn %>%
  mutate(
    "IUCN status source" = "IUCN",
    "IUCN status source date" = "2024") %>%
  select(
    species_name_dbase,
    redlistCategory,
    `IUCN status source`,
    `IUCN status source date`) %>%
  rename(
    "IUCN status" = redlistCategory)

  
#Endemic (not updated) ####  
endemic_join <- endemic %>%
  mutate(
    "Endemic/near endemic QPWS analysis" = "Yes",
    "Endemic/near endemic QPWS analysis date" = "2019") %>%
  select(
    species_name_dbase,
    `Endemic/near endemic QPWS analysis`,
    `Endemic/near endemic QPWS analysis date`)

endemic_test <- endemic %>%
  left_join(species_ref_join)


#TSO species (not updated) ####
tso_species_join <- tso_species %>%
  mutate(
    "DETSI TSO species" = "Yes",
    "DETSI TSO species date" = "2023-2024") %>%
  select(
    species_name_dbase,
    `DETSI TSO species`,
    `DETSI TSO species date`)

tso_species_test <- tso_species_join %>%
  left_join(species_ref_join)

#Commonwealth action plan priority sp (not updated) ####
action_species_join <- action_species %>%
  mutate(
    "Commonwealth threatened species action plan priority" = "Yes",
    "Commonwealth threatened species action plan priority date" = "2022") %>%
  select(
    species_name_dbase,
    `Commonwealth threatened species action plan priority`,
    `Commonwealth threatened species action plan priority date`)


#VBMF values (not updated) ####
vbmf_values_join <- vbmf_values %>%
  mutate(
    "VBMF key value - species and habitat date" = "2023") %>%
  select(
    species_name_dbase,
    VBMF_Values,
    `VBMF key value - species and habitat date`) %>%
  rename(
    "VBMF key value - species and habitat" = "VBMF_Values")


vbmf_values_test <- vbmf_values %>%
  left_join(species_ref_join)


#Red hot lists (not updated) ####
red_hot_animals_join <- red_hot_animals %>%
  mutate(
    "On red hot red list" = "Yes",
    "Red hot red list source" = case_when(
      `Red hot list` == "Butterflies" ~ "Geyle et al. 2021. Butterflies on the brink: identifying the Australian butterflies (Lepidoptera) most at risk of extinction. Austral Entomology 60 98-110.",
      `Red hot list` == "Birds" ~ "Geyle et al. 2018. Quantifying extinction risk and forecasting the number of impending Australian bird and mammal extinctions. Pacific Conservation Biology 24(2) 157-167.",
      `Red hot list` == "Fish" ~ "Lintermans et al. 2020. Big trouble for little fish: identifying Australian freshwater fishes in imminent risk of extinction. Pacific Conservation Biology 26 365-377.",
      `Red hot list` == "Mammals" ~ "Geyle et al. 2018. Quantifying extinction risk and forecasting the number of impending Australian bird and mammal extinctions. Pacific Conservation Biology 24(2) 157-167.",
      `Red hot list` == "Snakes and lizards" ~ "Geyle et al. 2021. Reptiles on the brink: identifying the Australian terrestrial snake and lizad species most at risk of extinction. Pacific Conservation Biology 27(1) 3-12.",
      TRUE ~ as.character(NA)),
    "Red hot red list source date" = case_when(
      `Red hot list` == "Butterflies" ~ "2021",
      `Red hot list` == "Birds" ~ "2018",
      `Red hot list` == "Fish" ~ "2020",
      `Red hot list` == "Mammals" ~ "2018",
      `Red hot list` == "Snakes and lizards" ~ "2021",
      TRUE ~ as.character(NA))) %>%
  select(
    species_name_dbase,
    `On red hot red list`,
    `Red hot red list source`,
    `Red hot red list source date`)

red_hot_plants_join <- red_hot_plants %>%
  mutate(
    "On red hot red list" = case_when(
      `RED HOT?` == "RED HOT" ~ "Yes",
      TRUE ~ as.character(NA)),
    "Red hot red list source" = "Silcock & Fensham 2018. Using evidence of decline and extinction risk to identify priority regions, habitats and threats for plant conservation in Australia. Australian Journal of Botany 66 541-555.",
    "Red hot red list source date" = "2018") %>%
  filter(
    !is.na(`On red hot red list`)) %>%
  select(
    species_name_dbase,
    `On red hot red list`,
    `Red hot red list source`,
    `Red hot red list source date`)

#Join animals and plants together, add probability of extinction from animals
red_hot_join <- red_hot_animals_join %>%
  bind_rows(red_hot_plants_join) %>%
  left_join(red_hot_animals, by = "species_name_dbase") %>%
  mutate("Red hot list - Inferred conservation status" = case_when(
    `Probability of extinction`<= 0.0207 ~ "Least Concern",
    `Probability of extinction`<= 0.2 ~ "Vulnerable",
    `Probability of extinction`<= 0.75 ~ "Endangered",
    `Probability of extinction`<= 1 ~ "Critically Endangered")) %>%
  select(
    species_name_dbase,
    `On red hot red list`,
    `Red hot red list source`,
    `Red hot red list source date`,
    `Probability of extinction`,
    `Red hot list - Inferred conservation status`) %>%
  rename(
    "Red hot list - Probability of extinction" = "Probability of extinction")


#Habitat loss and climate sensitivity (not updated) ####
hab_loss_climate_sens_allsp_join <- hab_loss_climate_sens_allsp %>%
  mutate(
    "Habitat climate sensitivity % date" = "2019") %>%
  select(
    `Taxon Id`,
    `% model permutation importance due to climate variables`,
    `Habitat climate sensitivity % date`) %>%
  rename(
    "Habitat climate sensitivity %" = "% model permutation importance due to climate variables")

#Data deficient (not updated) ####
data_deficient_allsp_join <- data_deficient_allsp %>%
  mutate(
    "Data deficient DETSI species date" = "2019") %>%
  select(
    species_name_dbase,
    `Data deficient DETSI species`,
    `Data deficient DETSI species date`)

data_deficient_test <- data_deficient_allsp %>%
  left_join(species_ref_join)


#CSIRO data ####

spatial_analyses <- spatial_analyses %>%
  select(QSPid, `Dominant IBRA IMCRA`:`Dominant vegetation type`)

overlap_species_external <- overlap_species_external %>%
  select(-Species) %>%
  rename("Number species 30% spatial overlap" = Number_of_spatial_overlap_above_Thr)

overlap_species_threat_external <- overlap_species_threat_external %>%
  select(-Species) %>%
  mutate(
    "Co-benefits - number species 30% spatial threat overlap state rank" = dense_rank(desc(Number_of_spatial_threat_overlap_above_30pct))) %>%
  rename("Co-benefits - number species 30% spatial threat overlap" = Number_of_spatial_threat_overlap_above_30pct)

overlap_species_internal <- overlap_species_internal %>%
  select(-Species) %>%
  rename("Number species 30% spatial overlap" = Number_of_spatial_overlap_above_Thr)

overlap_species_threat_internal <- overlap_species_threat_internal %>%
  select(-Species) %>%
  mutate(
    "Co-benefits - number species 30% spatial threat overlap state rank" = dense_rank(desc(Number_of_spatial_threat_overlap_above_30pct))) %>%
  rename("Co-benefits - number species 30% spatial threat overlap" = Number_of_spatial_threat_overlap_above_30pct)

survey <- survey %>%
  select(-`Species name`) %>%
  rename(
    "QSPid" = "QSPID",
    "Survey - Management goal" = "Management goal",
    "Survey - Management goal source" = "Management goal source",
    "Survey - Pr extinction 20 years without additional management" = "Pr extinction 20 years without additional management",
    "Survey - Pr extinction 20 years ideal management" = "Pr extinction 20 years ideal management",
    "Survey - Potential benefit 20 years if managed" = "Potential benefit 20 years if managed",
    "Survey - Potential benefit 20 years if managed source" = "Potential benefit 20 years if managed source",
    "Survey - Potential benefit 20 years if managed source date" = "Potential benefit 20 years if managed source date")

feasibility_actions <- feasibility_actions %>%
  mutate(
    `Feasibility of actions - action level estimates - best guess` = `Feasibility of actions - action level estimates - best guess`*100,
    `Feasibility of actions - action level estimates - lower` = `Feasibility of actions - action level estimates - lower`*100,
    `Feasibility of actions - action level estimates - upper` = `Feasibility of actions - action level estimates - upper`*100) %>%
  select(-`Species name`)

costs <- costs %>%
  rename(
    "Estimated management cost" = Total.cost.adjusted,
    "Estimated management cost window $/yr" = Cost.window,
    "Source of management actions costed" = Costed.action.source)

#Create all_species_info data frame combining all data frames ####
all_species_info <- species_ref_join %>%
  left_join(wildnet_join, by = "species_name_dbase") %>%
  left_join(all_epbc_join, by = "species_name_dbase") %>%
  left_join(bot1_join, by = "species_name_dbase") %>%
  left_join(bot2_join, by = "species_name_dbase") %>%
  left_join(iucn_join, by = "species_name_dbase") %>%
  left_join(endemic_join, by = "species_name_dbase") %>%
  left_join(tso_species_join, by = "species_name_dbase") %>%
  left_join(action_species_join, by = "species_name_dbase") %>%
  left_join(vbmf_values_join, by = "species_name_dbase") %>%
  left_join(red_hot_join, by = "species_name_dbase") %>%
  left_join(hab_loss_climate_sens_allsp_join, by = c("WildNet Taxon Id" = "Taxon Id")) %>%
  left_join(data_deficient_allsp_join, by = "species_name_dbase") %>%
  left_join(spatial_analyses, by = "QSPid") %>%
  left_join(overlap_species_external, by = "QSPid") %>%
  left_join(overlap_species_threat_external, by = "QSPid") %>%
  left_join(survey, by = "QSPid") %>%
  left_join(feasibility_actions, by = "QSPid") %>%
  left_join(costs, by = "QSPid")


#Write all_species_info to csv file ####
write.csv(all_species_info, file = paste("Output files/all_species_info_", ver_date,".csv", sep  = ""), row.names=FALSE)


#Create new columns to consolidate taxonomic information ####
#Hierarchy is: Wildnet,BOTI, BOTII, EPBC
all_species_info <- all_species_info %>%
                          mutate(
                          "Common name" = case_when(
                            !is.na(`WildNet common name`) ~ `WildNet common name`,
                            !is.na(`BOT I common name`) ~ `BOT I common name`,
                            !is.na(`BOT II common name`) ~ `BOT II common name`,
                            !is.na(`EPBC common name`) ~ `EPBC common name`,
                            TRUE ~ as.character(NA)),
                          "Common name" = str_to_sentence(`Common name`),
                          "Common name source" = case_when(
                            !is.na(`WildNet common name`) ~ "WildNet",
                            !is.na(`BOT I common name`) ~ "Back on Track I",
                            !is.na(`BOT II common name`) ~ "Back on Track II",
                            !is.na(`EPBC common name`) ~ "Commonwealth Species Profiles and Threats database",
                            TRUE ~ as.character(NA)),
                          
                          "Kingdom" = case_when(
                            !is.na(`WildNet kingdom`) ~ `WildNet kingdom`,
                            !is.na(`BOT I kingdom`) ~ `BOT I kingdom`, 
                            !is.na(`EPBC kingdom`) ~ `EPBC kingdom`,
                            TRUE ~ as.character(NA)),
                          "Kingdom source" = case_when(
                            !is.na(`WildNet kingdom`) ~ "WildNet",
                            !is.na(`BOT I kingdom`) ~ "Back on Track I", 
                            !is.na(`EPBC kingdom`) ~ "Commonwealth Species Profiles and Threats database",
                            TRUE ~ as.character(NA)),
                          
                          "Class" = case_when(
                            !is.na(`WildNet class`) ~ `WildNet class`,
                            !is.na(`BOT I class`) ~ `BOT I class`,
                            `EPBC class`== "Magnoliopsida" ~ "land plants",
                            `EPBC class`== "Polypodiatae" ~ "land plants",
                            `EPBC class`== "Liliopsida" ~ "land plants",
                            !is.na(`EPBC class`) ~ `EPBC class`,
                            QSPid == 843 ~ "land plants", #This species doesn't have a class source
                            TRUE ~ as.character(NA)),
                          "Class source" = case_when(
                            !is.na(`WildNet class`) ~ "WildNet",
                            !is.na(`BOT I class`) ~ "Back on Track I", 
                            `EPBC class`== "Magnoliopsida" ~ "WildNet species profile search",
                            `EPBC class`== "Polypodiatae" ~ "WildNet species profile search",
                            `EPBC class`== "Liliopsida" ~ "WildNet species profile search",
                            !is.na(`EPBC class`) ~ "Commonwealth Species Profiles and Threats database",
                            QSPid == 843 ~ "WildNet species profile search", 
                            TRUE ~ as.character(NA)),
                          
                          "Family" = case_when(
                            !is.na(`WildNet family`) ~ `WildNet family`,
                            !is.na(`BOT I family`) ~ `BOT I family`,
                            !is.na(`BOT II family`) ~ `BOT II family`,
                            !is.na(`EPBC family`) ~ `EPBC family`,
                            TRUE ~ as.character(NA)),
                          "Family source" = case_when(
                            !is.na(`WildNet family`) ~ "WildNet",
                            !is.na(`BOT I family`) ~ "Back on Track I",
                            !is.na(`BOT II family`) ~ "Back on Track I",
                            !is.na(`EPBC family`) ~ "Commonwealth Species Profiles and Threats database",
                            TRUE ~ as.character(NA))
                          )


#Code to visually check that the consolidating worked correctly
commonname_check <- select(all_species_info, QSPid, 'Common name', 'Common name source', 'WildNet common name', 'BOT I common name', 'BOT II common name', 'EPBC common name')
kingdom_check <- select(all_species_info, QSPid, 'Kingdom', 'Kingdom source', 'WildNet kingdom', 'BOT I kingdom', 'EPBC kingdom')
class_check <- select(all_species_info, QSPid, 'Class', 'Class source', 'WildNet class', 'BOT I class', 'EPBC class')
family_check <- select(all_species_info, QSPid, 'Family', 'Family source', 'WildNet family', 'BOT I family', 'BOT II family', 'EPBC family')
#check what's going on with species that don't have any kingdom, class, family info, or on WildNet


#Now clean up Kingdom and Class
all_species_info$Kingdom <- tolower(all_species_info$Kingdom) #convert to lower case
all_species_info$Kingdom <- gsub("plantae", "plants", all_species_info$Kingdom) #replace plantae with plants
all_species_info$Kingdom <- gsub("animalia", "animals", all_species_info$Kingdom) #replace animalia with animals
all_species_info$Class <- gsub("Aves", "birds", all_species_info$Class) #replace Aves with birds


#Consolidate feasibility from BOT I and BOT II ####
all_species_info <- all_species_info %>%
  mutate(
    "Feasibility of species recovery BOT I and II categorical" = case_when(
      `BOT II feasibility` == 0 ~ as.numeric(NA),
      `BOT I feasibility` == 0 ~ as.numeric(NA),
      !is.na(`BOT II feasibility`) ~ `BOT II feasibility`,
      !is.na(`BOT I feasibility`) ~ `BOT I feasibility`,
      TRUE ~ as.numeric(NA)),
    "Feasibility of species recovery BOT I and II %" = case_when(
      `Feasibility of species recovery BOT I and II categorical`== 4 ~ 98.00,
      `Feasibility of species recovery BOT I and II categorical`== 3 ~ 85.00,
      `Feasibility of species recovery BOT I and II categorical`== 2 ~ 63.00,
      `Feasibility of species recovery BOT I and II categorical`== 1 ~ 25.00,
      TRUE ~ as.numeric(NA)),
    "Feasibility of species recovery BOT I and II source" = case_when(
      `BOT II feasibility` == 0 ~ as.character(NA),
      `BOT I feasibility` == 0 ~ as.character(NA),
      !is.na(`BOT II feasibility`) ~ "Back on Track II",
      !is.na(`BOT I feasibility`) ~ "Back on Track I",
      TRUE ~ as.character(NA)))


#Best available analysis - Integrate best available information ####
all_species_info_best <- all_species_info %>%
  mutate(
    "Species estimated habitat extent pre-clearing km2" = case_when(
      `Species potential habitat spatial data source` == "Not available" ~ -1,
      TRUE ~ `Species estimated habitat extent pre-clearing km2`),
    "Species estimated habitat extent remaining km2" = case_when(
      `Species potential habitat spatial data source` == "Not available" ~ -1,
      TRUE ~ `Species estimated habitat extent remaining km2`),
    "Species estimated habitat remaining %" = case_when(
      `Species potential habitat spatial data source` == "Not available" ~ -1,
      TRUE ~ `Species estimated habitat remaining %`),
    "Combined conservation status" = case_when(
      #First if any source says it's extinct, then NA
      `EPBCA status` == "Extinct"| `NCA status` == "Extinct"|`NCA status` == "Extinct in the Wild"| `IUCN status` == "Extinct" ~ as.character(NA),
      #Then hierarchy is EPBCA, NCA, IUCN, inferred from Red hot list probability, BOTI, BOTII
      `EPBCA status` == "Critically Endangered"|`EPBCA status` == "Endangered"|`EPBCA status` == "Vulnerable" ~ `EPBCA status`,
      `NCA status` == "Critically Endangered"|`NCA status` == "Endangered"|`NCA status` == "Vulnerable" ~ `NCA status`,
      `IUCN status` == "Critically Endangered"|`IUCN status` == "Endangered"|`IUCN status` == "Vulnerable" ~ `IUCN status`,
      `Red hot list - Inferred conservation status` == "Critically Endangered"|`Red hot list - Inferred conservation status` == "Endangered"|`Red hot list - Inferred conservation status` == "Vulnerable" ~ `Red hot list - Inferred conservation status`,
      `BOT II conservation status` == "Endangered"|`BOT II conservation status` == "Vulnerable" ~ `BOT II conservation status`,
      `BOT I conservation status` == "Endangered"|`BOT I conservation status` == "Vulnerable" ~ `BOT I conservation status`,
      TRUE ~ as.character(NA)),
    "Combined conservation status source" = case_when(
      `EPBCA status` == "Extinct"| `NCA status` == "Extinct"|`NCA status` == "Extinct in the Wild"| `IUCN status` == "Extinct" ~ as.character(NA),
      `EPBCA status` == "Critically Endangered"|`EPBCA status` == "Endangered"|`EPBCA status` == "Vulnerable" ~ "EPBCA",
      `NCA status` == "Critically Endangered"|`NCA status` == "Endangered"|`NCA status` == "Vulnerable" ~ "NCA",
      `IUCN status` == "Critically Endangered"|`IUCN status` == "Endangered"|`IUCN status` == "Vulnerable" ~ "IUCN",
      `Red hot list - Inferred conservation status` == "Critically Endangered"|`Red hot list - Inferred conservation status` == "Endangered"|`Red hot list - Inferred conservation status` == "Vulnerable" ~ "Inferred from red hot list probability of extinction",
      `BOT II conservation status` == "Endangered"|`BOT II conservation status` == "Vulnerable" ~ "Back on Track II",
      `BOT I conservation status` == "Endangered"|`BOT I conservation status` == "Vulnerable" ~ "Back on Track I",
      TRUE ~ as.character(NA)),
    "Management goal" = case_when(
      !is.na(`Survey - Management goal`) ~ `Survey - Management goal`,
      !is.na(`Combined conservation status`) ~ "Downlist by one category",
      TRUE ~ as.character(NA)),
    "Management goal source" = case_when(
      !is.na(`Survey - Management goal`) ~ `Survey - Management goal source`,
      !is.na(`Combined conservation status`) ~ "Assumed goal: downlist",
      TRUE ~ as.character(NA)),
    "Status in 20 years if goal achieved" = case_when(
      `Combined conservation status` == "Vulnerable" ~ "Least Concern",
      `Combined conservation status` == "Endangered" ~ "Vulnerable",
      `Combined conservation status` == "Critically Endangered" ~ "Endangered",
      TRUE ~ as.character(NA)),
    "Pr extinction 20 years without additional management" = case_when(
      !is.na(`Survey - Pr extinction 20 years without additional management`) ~ `Survey - Pr extinction 20 years without additional management`,
      !is.na(`Red hot list - Probability of extinction`) ~ as.numeric(`Red hot list - Probability of extinction`),
      `Combined conservation status` == "Vulnerable" ~ 0.02,
      `Combined conservation status` == "Endangered" ~ 0.2,
      `Combined conservation status` == "Critically Endangered" ~ 0.75,
      TRUE ~ as.numeric(NA)),
    "Pr extinction 20 years without additional management source" = case_when(
      !is.na(`Survey - Pr extinction 20 years without additional management`) ~ `Survey - Potential benefit 20 years if managed source`,
      !is.na(`Red hot list - Probability of extinction`) ~ "Red hot red list",
      !is.na(`Pr extinction 20 years without additional management`) ~ "Conservation status",
      TRUE ~ as.character(NA)),
    "Pr extinction 20 years ideal management" = case_when(
      !is.na(`Survey - Pr extinction 20 years ideal management`) ~ `Survey - Pr extinction 20 years ideal management`,
      `Status in 20 years if goal achieved` == "Least Concern" ~ 0,
      `Status in 20 years if goal achieved` == "Vulnerable" ~ 0.02,
      `Status in 20 years if goal achieved` == "Endangered" ~ 0.2,
      TRUE ~ as.numeric(NA)),
    "Potential benefit 20 years if managed" = case_when(
      !is.na(`Survey - Potential benefit 20 years if managed`) ~ `Survey - Potential benefit 20 years if managed`,
      `Status in 20 years if goal achieved` == "Least Concern" ~ 0.02,
      `Status in 20 years if goal achieved` == "Vulnerable" ~ 0.18,
      `Status in 20 years if goal achieved` == "Endangered" ~ 0.55,
      TRUE ~ as.numeric(NA)),
    "Potential benefit 20 years if managed source" = case_when(
      !is.na(`Survey - Potential benefit 20 years if managed`) ~ `Survey - Potential benefit 20 years if managed source`,
      !is.na(`Potential benefit 20 years if managed`) ~ "Conservation status and assumed goal",
      TRUE ~ as.character(NA)),
    "Potential benefit 20 years if managed source date" = case_when(
      !is.na(`Survey - Potential benefit 20 years if managed`) ~ as.character(`Survey - Potential benefit 20 years if managed source date`),
      !is.na(`Potential benefit 20 years if managed`) ~ "2023",
      TRUE ~ as.character(NA)),
    "State-wide feasibility of recovery (%) - best available" = case_when(
      !is.na(`Feasibility of actions - species level estimate - best guess`) ~ `Feasibility of actions - species level estimate - best guess`,
      !is.na(`Feasibility of actions - action level estimates - best guess`) ~ `Feasibility of actions - action level estimates - best guess`,
      TRUE ~ as.numeric(NA)),
    "State-wide feasibility of recovery (%) - best available source" = case_when(
      !is.na(`Feasibility of actions - species level estimate - best guess`) ~ `Feasibility of actions - species level estimate source`,
      !is.na(`Feasibility of actions - action level estimates - best guess`) ~ `Feasibility of actions - action level estimate - source`,
      TRUE ~ as.character(NA)),
    "Management cost estimate date" = case_when(
      is.na(`Estimated management cost`) ~ "Not available",
      TRUE ~ "2022"),
    "Cost-effectiveness - persistence improvement per $ million" = 
      `Potential benefit 20 years if managed`/(`Estimated management cost`*20/1000000), #cost-effectiveness calculated with 20 year cost
    "Cost-effectiveness - state rank" = dense_rank(desc(`Cost-effectiveness - persistence improvement per $ million`)))

all_species_info_checkprio <- all_species_info_best %>%
  select(
    QSPid,
    `Species name`,
    `Species name source`,
    `Species 'active' for prioritisation`,
    `NCA status`,
    `NCA status source`,
    `NCA status source date`,
    `EPBCA status`, 
    `EPBCA status source`, 
    `EPBCA status source date`, 
    `IUCN status`,
    `IUCN status source`,
    `IUCN status source date`,
    `BOT I conservation status`,
    `BOT II conservation status`,
    `Combined conservation status`,
    `Combined conservation status source`,
    `Realm`,
    `Dominant vegetation type`,
    `Species estimated habitat extent pre-clearing km2`,
    `Species estimated habitat extent remaining km2`,
    `Species estimated habitat remaining %`,
    `Pre-clear and remnant habitat source date`,
    `Species potential habitat spatial data source`,
    `Species potential habitat spatial data source date`,
    `Threatened species recovery information survey attempted`,
    `Threatened species recovery information survey number of responses`,
    `Threatened species recovery information survey most recent updates`,
    `Number species 30% spatial overlap`,
    `Co-benefits - number species 30% spatial threat overlap`,
    `Co-benefits - number species 30% spatial threat overlap state rank`,
    `Management goal`,
    `Management goal source`,
    `Status in 20 years if goal achieved`,
    `Pr extinction 20 years without additional management`,
    `Pr extinction 20 years without additional management source`,
    `Pr extinction 20 years ideal management`,
    `Potential benefit 20 years if managed`,
    `Potential benefit 20 years if managed source`,
    `Potential benefit 20 years if managed source date`,
    `State-wide feasibility of recovery (%) - best available`,
    `State-wide feasibility of recovery (%) - best available source`,
    `Estimated management cost`,
    `Estimated management cost window $/yr`,
    `Source of management actions costed`,
    `Management cost estimate date`,
    `Cost-effectiveness - persistence improvement per $ million`,
    `Cost-effectiveness - state rank`)

#Check prioritisation info - Write to csv file ####
write.csv(all_species_info_checkprio, file = paste("Output files/check_prio_info_", ver_date,".csv", sep  = ""), row.names=FALSE)


#Create external main prioritisation table ####
main_prioritisation_table_best <- all_species_info_best %>%
  select(
    QSPid,
    `Species name`,
    `Species name source`,
    `Species 'active' for prioritisation`,
    `WildNet Taxon Id`,
    `WildNet download date`,
    `Common name`,
    `Common name source`,
    `Kingdom`,
    `Kingdom source`,
    `Class`,
    `Class source`,
    `Family`,
    `Family source`,
    `Evolutionary value`,
    `Evolutionary value source`,
    `Evolutionary value source date`,
    `Ecological value`,
    `Ecological value source`,
    `Ecological value source date`,
    `Confidential species`,
    `Endemic to Queensland`,
    `Confidential and Endemic source`,
    `Conservation advice date`,
    `Recovery plan date`,
    `Conservation advice and recovery plan source`,
    `Conservation advice and recovery plan source date`,
    `NCA status`,
    `NCA status source`,
    `NCA status source date`,
    `EPBCA status`, 
    `EPBCA status source`, 
    `EPBCA status source date`, 
    `IUCN status`,
    `IUCN status source`,
    `IUCN status source date`,
    `BOT I conservation status`,
    `BOT II conservation status`,
    `Combined conservation status`,
    `Combined conservation status source`,
    `On EPBCA migratory species list`,
    `Included in Bonn Convention`,
    `Included in CAMBA`,
    `Included in JAMBA`,
    `Included in ROKAMBA`,
    `On EPBCA marine species list`,
    `Cetacean (special protections in the Australian Whale Sanctuary)`,
    `Migratory, marine and cetacean species source`,
    `Migratory, marine and cetacean species source date`,
    `DETSI TSO species`,
    `DETSI TSO species date`,
    `Commonwealth threatened species action plan priority`, 
    `Commonwealth threatened species action plan priority date`,
    `BOT I priority status`,
    `BOT II priority status`,
    `VBMF key value - species and habitat`,
    `VBMF key value - species and habitat date`,
    `Endemic/near endemic QPWS analysis`,
    `Endemic/near endemic QPWS analysis date`, 
    `On red hot red list`,
    `Red hot red list source`,
    `Red hot red list source date`,
    `Realm`,
    `Dominant vegetation type`,
    `Species estimated habitat extent pre-clearing km2`,
    `Species estimated habitat extent remaining km2`,
    `Species estimated habitat remaining %`,
    `Pre-clear and remnant habitat source date`,
    `Habitat climate sensitivity %`,
    `Habitat climate sensitivity % date`,
    `Species potential habitat spatial data source`,
    `Species potential habitat spatial data source date`,
    `Confirmed sightings - number of records`,
    `Date of earliest record included`,
    `Date of most recent record included`,
    `Species records source`,
    `Data deficient DETSI species`,
    `Data deficient DETSI species date`,
    `Threat confidence`,
    `Threat confidence source`,
    `Action confidence`,
    `Action confidence source`,
    `Threatened species recovery information survey attempted`,
    `Threatened species recovery information survey number of responses`,
    `Threatened species recovery information survey most recent updates`,
    `Number species 30% spatial overlap`,
    `Co-benefits - number species 30% spatial threat overlap`,
    `Co-benefits - number species 30% spatial threat overlap state rank`,
    `Management goal`,
    `Management goal source`,
    `Status in 20 years if goal achieved`,
    `Pr extinction 20 years without additional management`,
    `Pr extinction 20 years without additional management source`,
    `Pr extinction 20 years ideal management`,
    `Potential benefit 20 years if managed`,
    `Potential benefit 20 years if managed source`,
    `Potential benefit 20 years if managed source date`,
    `Feasibility of species recovery BOT I and II categorical`,
    `Feasibility of species recovery BOT I and II %`,
    `Feasibility of species recovery BOT I and II source`,
    `Feasibility of actions - species level estimate - best guess`,
    `Feasibility of actions - species level estimate - lower`,
    `Feasibility of actions - species level estimate - upper`,
    `Feasibility of actions - species level estimate source`,
    `Feasibility of actions - species level estimate source date`,
    `Feasibility of actions - action level estimates - best guess`,
    `Feasibility of actions - action level estimates - lower`,
    `Feasibility of actions - action level estimates - upper`,
    `Feasibility of actions - action level estimate - source`,
    `Feasibility of actions - action level estimate - source date`,
    `State-wide feasibility of recovery (%) - best available`,
    `State-wide feasibility of recovery (%) - best available source`,
    `Estimated management cost window $/yr`,
    `Source of management actions costed`,
    `Management cost estimate date`,
    `Cost-effectiveness - persistence improvement per $ million`,
    `Cost-effectiveness - state rank`)

#External main prioritisation table - Replace NAs ####
#Replace with "No" for binary columns, -1 for numeric columns, and "Not available" for others
main_prioritisation_table_best <- main_prioritisation_table_best %>%
  mutate(
    `Common name`= replace_na(`Common name`, "Not available"),
    `Common name source`= replace_na(`Common name source`, "Not available"),
    `Kingdom`= replace_na(`Kingdom`, "Not available"),
    `Kingdom source`= replace_na(`Kingdom source`, "Not available"),
    `Class`= replace_na(`Class`, "Not available"),
    `Class source`= replace_na(`Class source`, "Not available"),
    `Family`= replace_na(`Family`, "Not available"),
    `Family source`= replace_na(`Family source`, "Not available"),
    `Evolutionary value`= replace_na(`Evolutionary value`, "Not available"),
    `Evolutionary value source`= replace_na(`Evolutionary value source`, "Not available"),
    `Evolutionary value source date`= replace_na(`Evolutionary value source date`, "Not available"),
    `Ecological value`= replace_na(`Ecological value`, "Not available"),
    `Ecological value source`= replace_na(`Ecological value source`, "Not available"),
    `Ecological value source date`= replace_na(`Ecological value source date`, "Not available"),
    `Confidential species`= replace_na(`Confidential species`, "Not available"),
    `Endemic to Queensland`= replace_na(`Endemic to Queensland`, "Not available"),
    `Confidential and Endemic source`= replace_na(`Confidential and Endemic source`, "Not available"),
    `Conservation advice date`= replace_na(`Conservation advice date`, "Not available"),
    `Recovery plan date`= replace_na(`Recovery plan date`, "Not available"),
    `Conservation advice and recovery plan source`= replace_na(`Conservation advice and recovery plan source`, "Not available"),
    `Conservation advice and recovery plan source date`= replace_na(`Conservation advice and recovery plan source date`, "Not available"),
    `NCA status`= replace_na(`NCA status`, "Not available"),
    `NCA status source`= replace_na(`NCA status source`, "Not available"),
    `NCA status source date`= replace_na(`NCA status source date`, "Not available"),
    `EPBCA status`= replace_na(`EPBCA status`, "Not available"), 
    `EPBCA status source`= replace_na(`EPBCA status source`, "Not available"), 
    `EPBCA status source date`= replace_na(`EPBCA status source date`, "Not available"), 
    `IUCN status`= replace_na(`IUCN status`, "Not available"),
    `IUCN status source`= replace_na(`IUCN status source`, "Not available"),
    `IUCN status source date`= replace_na(`IUCN status source date`, "Not available"),
    `BOT I conservation status`= replace_na(`BOT I conservation status`, "Not available"),
    `BOT II conservation status`= replace_na(`BOT II conservation status`, "Not available"),
    `Combined conservation status`= replace_na(`Combined conservation status`, "Not available"),
    `Combined conservation status source`= replace_na(`Combined conservation status source`, "Not available"),
    `On EPBCA migratory species list`= replace_na(`On EPBCA migratory species list`,"No"),
    `Included in Bonn Convention`= replace_na(`Included in Bonn Convention`, "No"),
    `Included in CAMBA`= replace_na(`Included in CAMBA`, "No"),
    `Included in JAMBA` = replace_na(`Included in JAMBA`, "No"),
    `Included in ROKAMBA`= replace_na(`Included in ROKAMBA`, "No"),
    `On EPBCA marine species list` = replace_na(`On EPBCA marine species list`, "No"),
    `Cetacean (special protections in the Australian Whale Sanctuary)` = replace_na(`Cetacean (special protections in the Australian Whale Sanctuary)`, "No"),
    `Migratory, marine and cetacean species source` = replace_na(`Migratory, marine and cetacean species source`, "Not available"),
    `Migratory, marine and cetacean species source date` = replace_na(`Migratory, marine and cetacean species source date`, "Not available"),
    `DETSI TSO species` = replace_na(`DETSI TSO species`, "No"),
    `DETSI TSO species date` = replace_na(`DETSI TSO species date`, "Not available"),
    `Commonwealth threatened species action plan priority` = replace_na(`Commonwealth threatened species action plan priority`, "No"), 
    `Commonwealth threatened species action plan priority date`= replace_na(`Commonwealth threatened species action plan priority date`, "Not available"),
    `BOT I priority status`= replace_na(`BOT I priority status`, "Not available"),
    `BOT II priority status`= replace_na(`BOT II priority status`, "Not available"),
    `VBMF key value - species and habitat` = replace_na(`VBMF key value - species and habitat`, "No"),
    `VBMF key value - species and habitat date`= replace_na(`VBMF key value - species and habitat date`, "Not available"),
    `Endemic/near endemic QPWS analysis` = replace_na(`Endemic/near endemic QPWS analysis`, "No"),
    `Endemic/near endemic QPWS analysis date`= replace_na(`Endemic/near endemic QPWS analysis date`, "Not available"), 
    `On red hot red list` = replace_na(`On red hot red list`, "No"),
    `Red hot red list source`= replace_na(`Red hot red list source`, "Not available"),
    `Red hot red list source date`= replace_na(`Red hot red list source date`, "Not available"),
    `Realm`= replace_na(`Realm`, "Not available"),
    `Dominant vegetation type`= replace_na(`Dominant vegetation type`, "Not available"),
    `Dominant vegetation type`= str_replace(`Dominant vegetation type`, "0", "Not available"),
    `Species estimated habitat extent pre-clearing km2`= replace_na(`Species estimated habitat extent pre-clearing km2`, -1),
    `Species estimated habitat extent remaining km2`= replace_na(`Species estimated habitat extent remaining km2`, -1),
    `Species estimated habitat remaining %`= replace_na(`Species estimated habitat remaining %`, -1),
    `Pre-clear and remnant habitat source date`= replace_na(`Pre-clear and remnant habitat source date`, "Not available"),
    `Habitat climate sensitivity %` = replace_na(`Habitat climate sensitivity %`, -1),
    `Habitat climate sensitivity % date`= replace_na(`Habitat climate sensitivity % date`, "Not available"),
    `Species potential habitat spatial data source`= replace_na(`Species potential habitat spatial data source`, "Not available"),
    `Species potential habitat spatial data source date`= replace_na(`Species potential habitat spatial data source date`, "Not available"),
    `Confirmed sightings - number of records`= replace_na(`Confirmed sightings - number of records`, -1),
    `Date of earliest record included`= replace_na(`Date of earliest record included`, "Not available"),
    `Date of most recent record included`= replace_na(`Date of most recent record included`, "Not available"),
    `Species records source` = replace_na(`Species records source`, "Not available"),
    `Data deficient DETSI species` = replace_na(`Data deficient DETSI species`, "No"),
    `Data deficient DETSI species date` = replace_na(`Data deficient DETSI species date`, "Not available"),
    `Threat confidence` = replace_na(`Threat confidence`, "Not available"),
    `Threat confidence source` = replace_na(`Threat confidence source`, "Not available"),
    `Action confidence` = replace_na(`Action confidence`, "Not available"),
    `Action confidence source` = replace_na(`Action confidence source`, "Not available"),
    `Threatened species recovery information survey attempted`= replace_na(`Threatened species recovery information survey attempted`, "No"),
    `Threatened species recovery information survey number of responses` = replace_na(`Threatened species recovery information survey number of responses`, -1),
    `Threatened species recovery information survey most recent updates`= replace_na(as.character(`Threatened species recovery information survey most recent updates`), "Not available"),
    `Number species 30% spatial overlap`= replace_na(`Number species 30% spatial overlap`, -1),
    `Co-benefits - number species 30% spatial threat overlap`= replace_na(`Co-benefits - number species 30% spatial threat overlap`, -1),
    `Co-benefits - number species 30% spatial threat overlap state rank`= replace_na(`Co-benefits - number species 30% spatial threat overlap state rank`, -1),
    `Management goal`= replace_na(`Management goal`, "Not available"),
    `Management goal source`= replace_na(`Management goal source`, "Not available"),
    `Status in 20 years if goal achieved`= replace_na(`Status in 20 years if goal achieved`, "Not available"),
    `Pr extinction 20 years without additional management` = replace_na(`Pr extinction 20 years without additional management`, -1),
    `Pr extinction 20 years without additional management source`= replace_na(`Pr extinction 20 years without additional management source`, "Not available"),
    `Pr extinction 20 years ideal management` = replace_na(`Pr extinction 20 years ideal management`, -1),
    `Potential benefit 20 years if managed` = replace_na(`Potential benefit 20 years if managed`, -1),
    `Potential benefit 20 years if managed source`= replace_na(`Potential benefit 20 years if managed source`, "Not available"),
    `Potential benefit 20 years if managed source date`= replace_na(`Potential benefit 20 years if managed source date`, "Not available"),
    `Feasibility of species recovery BOT I and II categorical` = replace_na(`Feasibility of species recovery BOT I and II categorical`, -1),
    `Feasibility of species recovery BOT I and II %` = replace_na(`Feasibility of species recovery BOT I and II %`, -1),
    `Feasibility of species recovery BOT I and II source`= replace_na(`Feasibility of species recovery BOT I and II source`, "Not available"),
    `Feasibility of actions - species level estimate - best guess`= replace_na(`Feasibility of actions - species level estimate - best guess`, -1),
    `Feasibility of actions - species level estimate - lower`= replace_na(`Feasibility of actions - species level estimate - lower`, -1),
    `Feasibility of actions - species level estimate - upper`= replace_na(`Feasibility of actions - species level estimate - upper`, -1),
    `Feasibility of actions - species level estimate source`= replace_na(`Feasibility of actions - species level estimate source`, "Not available"),
    `Feasibility of actions - species level estimate source date`= replace_na(as.character(`Feasibility of actions - species level estimate source date`), "Not available"),
    `Feasibility of actions - action level estimates - best guess`= replace_na(`Feasibility of actions - action level estimates - best guess`, -1),
    `Feasibility of actions - action level estimates - lower`= replace_na(`Feasibility of actions - action level estimates - lower`, -1),
    `Feasibility of actions - action level estimates - upper`= replace_na(`Feasibility of actions - action level estimates - upper`, -1),
    `Feasibility of actions - action level estimate - source`= replace_na(`Feasibility of actions - action level estimate - source`, "Not available"),
    `Feasibility of actions - action level estimate - source date`= replace_na(as.character(`Feasibility of actions - action level estimate - source date`), "Not available"),
    `State-wide feasibility of recovery (%) - best available` = replace_na(`State-wide feasibility of recovery (%) - best available`, -1),
    `State-wide feasibility of recovery (%) - best available source` = replace_na(`State-wide feasibility of recovery (%) - best available source`, "Not available"),
    `Estimated management cost window $/yr`= replace_na(`Estimated management cost window $/yr`, "Not available"),
    `Source of management actions costed`= replace_na(`Source of management actions costed`, "Not available"),
    `Management cost estimate date`= replace_na(as.character(`Management cost estimate date`), "Not available"),
    `Cost-effectiveness - persistence improvement per $ million`= replace_na(`Cost-effectiveness - persistence improvement per $ million`, -1),
    `Cost-effectiveness - state rank`= replace_na(`Cost-effectiveness - state rank`, -1))

#External main prioritisation table - Write to csv file ####
write.csv(main_prioritisation_table_best, file = paste("Output files/main_prioritisation_table_external_", ver_date,".csv", sep  = ""), row.names=FALSE)

#Internal main prioritisation table - using internal co-benefit overlap analyses####
all_info_internal <- main_prioritisation_table_best %>%
  select(-`Number species 30% spatial overlap`) %>% #take external analysis out
  select(-`Co-benefits - number species 30% spatial threat overlap`) %>%
  select(-`Co-benefits - number species 30% spatial threat overlap state rank`) %>%
  left_join(overlap_species_internal, by = "QSPid") %>% #put internal analysis in
  left_join(overlap_species_threat_internal, by = "QSPid")

main_prioritisation_table_internal <- all_info_internal %>%
  select(
    QSPid,
    `Species name`,
    `Species name source`,
    `Species 'active' for prioritisation`,
    `WildNet Taxon Id`,
    `WildNet download date`,
    `Common name`,
    `Common name source`,
    `Kingdom`,
    `Kingdom source`,
    `Class`,
    `Class source`,
    `Family`,
    `Family source`,
    `Evolutionary value`,
    `Evolutionary value source`,
    `Evolutionary value source date`,
    `Ecological value`,
    `Ecological value source`,
    `Ecological value source date`,
    `Confidential species`,
    `Endemic to Queensland`,
    `Confidential and Endemic source`,
    `Conservation advice date`,
    `Recovery plan date`,
    `Conservation advice and recovery plan source`,
    `Conservation advice and recovery plan source date`,
    `NCA status`,
    `NCA status source`,
    `NCA status source date`,
    `EPBCA status`, 
    `EPBCA status source`, 
    `EPBCA status source date`, 
    `IUCN status`,
    `IUCN status source`,
    `IUCN status source date`,
    `BOT I conservation status`,
    `BOT II conservation status`,
    `Combined conservation status`,
    `Combined conservation status source`,
    `On EPBCA migratory species list`,
    `Included in Bonn Convention`,
    `Included in CAMBA`,
    `Included in JAMBA`,
    `Included in ROKAMBA`,
    `On EPBCA marine species list`,
    `Cetacean (special protections in the Australian Whale Sanctuary)`,
    `Migratory, marine and cetacean species source`,
    `Migratory, marine and cetacean species source date`,
    `DETSI TSO species`,
    `DETSI TSO species date`,
    `Commonwealth threatened species action plan priority`, 
    `Commonwealth threatened species action plan priority date`,
    `BOT I priority status`,
    `BOT II priority status`,
    `VBMF key value - species and habitat`,
    `VBMF key value - species and habitat date`,
    `Endemic/near endemic QPWS analysis`,
    `Endemic/near endemic QPWS analysis date`, 
    `On red hot red list`,
    `Red hot red list source`,
    `Red hot red list source date`,
    `Realm`,
    `Dominant vegetation type`,
    `Species estimated habitat extent pre-clearing km2`,
    `Species estimated habitat extent remaining km2`,
    `Species estimated habitat remaining %`,
    `Pre-clear and remnant habitat source date`,
    `Habitat climate sensitivity %`,
    `Habitat climate sensitivity % date`,
    `Species potential habitat spatial data source`,
    `Species potential habitat spatial data source date`,
    `Confirmed sightings - number of records`,
    `Date of earliest record included`,
    `Date of most recent record included`,
    `Species records source`,
    `Data deficient DETSI species`,
    `Data deficient DETSI species date`,
    `Threat confidence`,
    `Threat confidence source`,
    `Action confidence`,
    `Action confidence source`,
    `Threatened species recovery information survey attempted`,
    `Threatened species recovery information survey number of responses`,
    `Threatened species recovery information survey most recent updates`,
    `Number species 30% spatial overlap`,
    `Co-benefits - number species 30% spatial threat overlap`,
    `Co-benefits - number species 30% spatial threat overlap state rank`,
    `Management goal`,
    `Management goal source`,
    `Status in 20 years if goal achieved`,
    `Pr extinction 20 years without additional management`,
    `Pr extinction 20 years without additional management source`,
    `Pr extinction 20 years ideal management`,
    `Potential benefit 20 years if managed`,
    `Potential benefit 20 years if managed source`,
    `Potential benefit 20 years if managed source date`,
    `Feasibility of species recovery BOT I and II categorical`,
    `Feasibility of species recovery BOT I and II %`,
    `Feasibility of species recovery BOT I and II source`,
    `Feasibility of actions - species level estimate - best guess`,
    `Feasibility of actions - species level estimate - lower`,
    `Feasibility of actions - species level estimate - upper`,
    `Feasibility of actions - species level estimate source`,
    `Feasibility of actions - species level estimate source date`,
    `Feasibility of actions - action level estimates - best guess`,
    `Feasibility of actions - action level estimates - lower`,
    `Feasibility of actions - action level estimates - upper`,
    `Feasibility of actions - action level estimate - source`,
    `Feasibility of actions - action level estimate - source date`,
    `State-wide feasibility of recovery (%) - best available`,
    `State-wide feasibility of recovery (%) - best available source`,
    `Estimated management cost window $/yr`,
    `Source of management actions costed`,
    `Management cost estimate date`,
    `Cost-effectiveness - persistence improvement per $ million`,
    `Cost-effectiveness - state rank`)

#Internal main prioritisation table - Replace NAs ####
#Replace with "No" for binary columns, -1 for numeric columns, and "Not available" for others
main_prioritisation_table_internal <- main_prioritisation_table_internal %>%
  mutate(
    `Number species 30% spatial overlap`= replace_na(`Number species 30% spatial overlap`, -1),
    `Co-benefits - number species 30% spatial threat overlap`= replace_na(`Co-benefits - number species 30% spatial threat overlap`, -1),
    `Co-benefits - number species 30% spatial threat overlap state rank`= replace_na(`Co-benefits - number species 30% spatial threat overlap state rank`, -1))

#Internal main prioritisation table - Write to csv file ####
write.csv(main_prioritisation_table_internal, file = paste("Output files/main_prioritisation_table_internal_", ver_date,".csv", sep  = ""), row.names=FALSE)
