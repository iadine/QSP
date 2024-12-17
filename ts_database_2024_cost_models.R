#*******************************************************************************
#*******************************************************************************
#Qld threatened species information database and prioritisation
#Code to generate management cost estimates
#2024

#Tracy Rout

#28-11-2024
#*******************************************************************************
#*******************************************************************************


#Initialising ####
#Clean up work station
rm(list = ls())

#Version date - for outputs
ver_date = "281124"

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


#Info from spatial analyses
spatial_analyses <- read_excel("Input files/QSP_spatial_analyses_CSIRO_281124.xlsx")

#Datasets to get conservation status and taxonomic information
bot1 <- read_excel("Input files/BOTI_DES_260719.xlsx")
bot2 <- read_excel("Input files/BOTII_DES_modified_TRout_170921.xlsx")
epbc_additional <- read.csv("Input files/EPBC_additional_status_modified_TRout_270524.csv")
iucn <- read.csv("Input files/IUCN_assessments_downloaded_080724.csv")
red_hot_plants <- read_excel("Input files/Red_hot_plants_Qld_JSilcock_received_201219.xlsx", sheet = "QLD_CR_EN_ALL")
red_hot_animals <- read_excel("Input files/Red_hot_animals_TRout_060622.xlsx")
wildnet_allsp <- read.csv("Input files/Wildnet_downloaded_210624.csv")

#Rename a couple of problematic column names
colnames(bot2)[8] <- 'Criterion 1a Conservation status'
colnames(bot2)[14] <- 'Criterion 3b Feasibility of conservation'

#Copy federal cassowary information to apply to both northern and southern populations - these are listed separately in the database
epbc_additional$Scientific.Name <- gsub(epbc_additional$Scientific.Name, pattern = "Casuarius casuarius johnsonii", replacement = "Casuarius casuarius johnsonii northern population;Casuarius casuarius johnsonii southern population") 
epbc_additional <- separate_rows(epbc_additional, Scientific.Name, sep = ";")

#Actions and cost model information
action_matrix <- read.csv("Output files/species_action_matrix_internal_271124.csv") #This is output from ts_database_2024_species_threat_action_matrices.R - ensure date is latest and use internal version
cost_models <- read_excel("Input files/Cost_models_extracted_251124.xlsx")


#Create a consistent species name column (species_name_dbase) that can be used as a key to join data frames ####
bot1 <- mutate(bot1, species_name_dbase = bot1$`Species Name`)
bot2 <- mutate(bot2, species_name_dbase = bot2$`Scientific name`)
epbc_additional <- mutate(epbc_additional, species_name_dbase = epbc_additional$`Scientific.Name`)
iucn <- mutate(iucn, species_name_dbase = iucn$`scientificName`)
red_hot_animals <- mutate(red_hot_animals, species_name_dbase = red_hot_animals$`Species name`)
red_hot_plants <- mutate(red_hot_plants, species_name_dbase = red_hot_plants$`Species`)
species_ref <- mutate(species_ref, species_name_dbase = species_ref$species_name_june2024)
wildnet_allsp <- mutate(wildnet_allsp, species_name_dbase = wildnet_allsp$Scientific_name)

#Note that we will be using QSPid to join data frames that were CSIRO outputs

#Transform species names so they are easy to match consistently - remove all non-alphanumeric characters, spaces, and convert to lower case
bot1$species_name_dbase <- gsub("×", "x", bot1$species_name_dbase) #replace this special character with a normal x
bot1$species_name_dbase <- gsub("[^[:alnum:]]", "", bot1$species_name_dbase) #remove all other non-alphanumeric characters (including spaces)
bot1$species_name_dbase <- tolower(bot1$species_name_dbase) #convert to lower case

bot2$species_name_dbase <- gsub("×", "x", bot2$species_name_dbase) #replace this special character with a normal x
bot2$species_name_dbase <- gsub("[^[:alnum:]]", "", bot2$species_name_dbase) #remove all other non-alphanumeric characters (including spaces)
bot2$species_name_dbase <- tolower(bot2$species_name_dbase) #convert to lower case

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
  select(Common.Name, Kingdom:Family, species_name_dbase) %>%
  distinct()
#Get rid of blanks by setting to NA
all_epbc[all_epbc == ""] <- NA


#Replace inconsistent species names in species_name_dbase columns to match the names in species_ref ####
#Note - not doing this for EPBC additional dataset as it seems to list old and new species names
for (i in 1:nrow(species_name_replacements)) 
{
  
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
}
#Now we have a key column (species_name_dbase) that we can use to match species consistently across these dataframes


#Tidy up each data frame before joining - Species reference list (updated) ####
species_ref_join <- species_ref %>%
  rename("Species name" = species_name_june2024, 
         "WildNet Taxon Id" = wildnet_taxon_id_june2024) %>%
  select(QSPid, "Species name", "WildNet Taxon Id", "Species 'active' for prioritisation", species_name_dbase)

#WildNet data (updated) ####
wildnet_join <- wildnet_allsp %>%
  mutate(
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
    "EPBCA status" = case_when( #codes from here https://www.data.qld.gov.au/dataset/conservation-status-of-queensland-wildlife/resource/d20c485d-b5dc-4c07-8304-df6a21783d4b
      EPBC_status == "CD" ~ "Conservation Dependent",
      EPBC_status == "CE" ~ "Critically Endangered",
      EPBC_status == "E" ~ "Endangered",
      EPBC_status == "EX" ~ "Extinct",  
      EPBC_status == "XW" ~ "Extinct in the Wild",
      EPBC_status == "V" ~ "Vulnerable",
      TRUE ~ as.character(NA)),
    "WildNet common name" = case_when(
      Common_name == "" ~ as.character(NA),
      !is.na(Common_name) ~ `Common_name`,
      TRUE ~ as.character(NA))) %>%
  select(
    species_name_dbase,
    Kingdom:Family,
    `WildNet common name`,
    `NCA status`, 
    `EPBCA status`) %>%
  rename(
    "WildNet kingdom" = "Kingdom",
    "WildNet class" = "Class",
    "WildNet family" = "Family")

#EPBC data (updated)####
all_epbc_join <- all_epbc %>%
  select(
    species_name_dbase, 
    Common.Name,
    Kingdom:Family) %>%
  rename(
    "EPBC kingdom" = "Kingdom",
    "EPBC class" = "Class",
    "EPBC family" = "Family",
    "EPBC common name" = "Common.Name")

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
    `BOT I conservation status`) %>%
  rename(
    "BOT I kingdom" = "Kingdom",
    "BOT I family" = "Family",
    "BOT I class" = "Class Name",
    "BOT I common name" = "Common Name")

bot2_join <- bot2 %>%
  mutate(
    "BOT II conservation status" = case_when(
      `Criterion 1a Conservation status` == 4 ~ "Endangered",
      `Criterion 1a Conservation status` == 3 ~ "Vulnerable",
      `Criterion 1a Conservation status` == 2 ~ "Near Threatened",
      `Criterion 1a Conservation status` == 1 ~ "Least Concern",
      TRUE ~ as.character(NA))) %>%
  select(
    species_name_dbase,
    Family,
    `Common name`,
    `BOT II conservation status`) %>%
  rename(
    "BOT II family" = "Family",
    "BOT II common name" = "Common name")

#IUCN data - updated 2024 ####
iucn_join <- iucn %>%
  select(
    species_name_dbase,
    redlistCategory) %>%
  rename(
    "IUCN status" = redlistCategory)

#Red hot lists (not updated) ####
red_hot_animals_join <- red_hot_animals %>%
  select(species_name_dbase)

red_hot_plants_join <- red_hot_plants %>%
  mutate(
    "On red hot red list" = case_when(
      `RED HOT?` == "RED HOT" ~ "Yes",
      TRUE ~ as.character(NA))) %>%
  filter(
    !is.na(`On red hot red list`)) %>%
  select(species_name_dbase)

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
    `Probability of extinction`,
    `Red hot list - Inferred conservation status`) %>%
  rename(
    "Red hot list - Probability of extinction" = "Probability of extinction")


#Spatial data ####
spatial_analyses_join <- spatial_analyses %>%
  select(QSPid, `Dominant IBRA IMCRA`:`Dominant vegetation type`)


#Create all_species_info data frame combining all data frames ####
all_species_info <- species_ref_join %>%
  left_join(wildnet_join, by = "species_name_dbase") %>%
  left_join(all_epbc_join, by = "species_name_dbase") %>%
  left_join(bot1_join, by = "species_name_dbase") %>%
  left_join(bot2_join, by = "species_name_dbase") %>%
  left_join(iucn_join, by = "species_name_dbase") %>%
  left_join(red_hot_join, by = "species_name_dbase") %>%
  left_join(spatial_analyses_join, by = "QSPid")


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

#Add combined conservation status and status in 20 years ####
all_species_info2 <- all_species_info %>%
  mutate(
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

    "Status in 20 years if goal achieved" = case_when(
      `Combined conservation status` == "Vulnerable" ~ "Least Concern",
      `Combined conservation status` == "Endangered" ~ "Vulnerable",
      `Combined conservation status` == "Critically Endangered" ~ "Endangered",
      TRUE ~ as.character(NA)),
    
    "Species type" = case_when(
      `Class` == "land plants" ~ "Plant",
      `Class` == "arachnids"|`Class` == "bivalves"|`Class` == "insects"|`Class` == "malacostracans"|`Class` == "maxillopods"|`Class` == "snails" ~ "Invertebrate",
      TRUE ~ "Vertebrate"))

write.csv(all_species_info2, file = paste("Output files/species_info_check_", ver_date,".csv", sep  = ""), row.names=FALSE)



#Extract species information needed for costs models ####
species_info_costs <- all_species_info2 %>%
  select(
    QSPid, 
    `Species name`, 
    `Species 'active' for prioritisation`,
    `Common name`, 
    `Species type`, 
    `Realm`, 
    `Dominant Bioregion`,
    `Dominant vegetation type`, 
    `Species estimated habitat extent pre-clearing km2`,
    `Species estimated habitat extent remaining km2`,
    `Species potential habitat spatial data source`,
    `Combined conservation status`,
    `Status in 20 years if goal achieved`) %>%
  
  mutate(
    "Remoteness region" = case_when(
      `Dominant Bioregion`== "Cape York Peninsula" ~ "West and Cape",
      `Dominant Bioregion`== "Channel Country" ~ "West and Cape",
      `Dominant Bioregion`== "Desert Uplands" ~ "West and Cape",
      `Dominant Bioregion`== "Einasleigh Uplands" ~ "West and Cape",
      `Dominant Bioregion`== "Gulf Plains" ~ "West and Cape",
      `Dominant Bioregion`== "Mitchell Grass Downs" ~ "West and Cape",
      `Dominant Bioregion`== "Mount Isa Inlier" ~ "West and Cape",
      `Dominant Bioregion`== "Mulga Lands" ~ "West and Cape",
      `Dominant Bioregion`== "Northeast Shelf Province" ~ "West and Cape",
      `Dominant Bioregion`== "Northeast Shelf Transition" ~ "West and Cape",
      `Dominant Bioregion`== "Northern Shelf Province" ~ "West and Cape",
      `Dominant Bioregion`== "Simpson Strzelecki Dunefields" ~ "West and Cape",
      TRUE ~ "Eastern"),
    
    "Target Cons Status" = case_when(
      `Status in 20 years if goal achieved`=="Least Concern" ~ 3000,
      `Status in 20 years if goal achieved`=="Vulnerable" ~ 2000,
      `Status in 20 years if goal achieved`=="Endangered" ~ 500,
      TRUE ~ 0),
    
    "Potential habitat model available" = case_when(
      `Species potential habitat spatial data source` == "Potential habitat models - 2024 - Queensland series" ~ "Yes",
      `Species potential habitat spatial data source` == "Potential habitat models - 2022 - Queensland series" ~ "Yes",
      TRUE ~ "No"),
    
    "Species target" = case_when(
      `Potential habitat model available` == "No" ~ `Target Cons Status`,
      `Species estimated habitat extent pre-clearing km2` < `Target Cons Status` ~ `Species estimated habitat extent pre-clearing km2`,
      TRUE ~ `Target Cons Status`),
    
    "Species restoration target" = case_when(
      `Species estimated habitat extent remaining km2` > `Species target` ~ 0,
      `Potential habitat model available` == "Yes" ~ `Species target` - `Species estimated habitat extent remaining km2`,
      TRUE ~ `Species target`*0.11)
      )

write.csv(species_info_costs, file = paste("Output files/species_info_costs_", ver_date,".csv", sep  = ""), row.names=FALSE)


#Join together action and species information ####
action_matrix_join <- action_matrix %>%
  select(QSPid:Action.level.2, Number.of.units, Action.source, Action.source.date) %>%
  rename(
    "Action level 1" = `Action.level.1`,
    "Action level 2" = `Action.level.2`) %>%
  distinct()

species_info_costs_join <- species_info_costs %>%
  select(
    QSPid,
    `Species type`,
    Realm,
    `Dominant vegetation type`,
    `Remoteness region`,
    `Species target`,
    `Species restoration target`)

action_species_info_costs <- action_matrix_join %>%
  left_join(species_info_costs_join, by = 'QSPid')

cost_per_action <- action_species_info_costs %>%
  mutate(
    "Assumed number units" = as.character(NA), #text
    "Cost per unit" = as.numeric(NA)) #placeholders, fill these columns in next
    


for (i in 1:nrow(cost_models)) 
{
  for (j in 1:nrow(cost_per_action))
  {
    if (cost_per_action$`Action level 1`[j] == cost_models$`Action level 1`[i] && cost_per_action$`Action level 2`[j] == cost_models$`Action level 2`[i])
        {
        if (cost_models$`Variation type`[i] == "None")
        {cost_per_action$`Assumed number units`[j] <- cost_models$`Target units assumptions where not specified`[i]
        cost_per_action$`Cost per unit`[j] <- cost_models$`Total cost per unit`[i]}

        else if (cost_models$`Variation type`[i] == "Remoteness region" && cost_per_action$`Remoteness region`[j] == cost_models$`Variations`[i])
        {cost_per_action$`Assumed number units`[j] <- cost_models$`Target units assumptions where not specified`[i]
          cost_per_action$`Cost per unit`[j] <- cost_models$`Total cost per unit`[i]}

        else if (cost_models$`Variation type`[i] == "Species type" && cost_per_action$`Species type`[j] == cost_models$`Variations`[i])
        {cost_per_action$`Assumed number units`[j] <- cost_models$`Target units assumptions where not specified`[i]
          cost_per_action$`Cost per unit`[j] <- cost_models$`Total cost per unit`[i]}

        else if (cost_models$`Variation type`[i] == "Vegetation type" && cost_per_action$`Dominant vegetation type`[j] == cost_models$`Variations`[i])
          {cost_per_action$`Assumed number units`[j] <- cost_models$`Target units assumptions where not specified`[i]
          cost_per_action$`Cost per unit`[j] <- cost_models$`Total cost per unit`[i]}

        }

    }
}

cost_per_action2 <- cost_per_action %>%
  mutate(
    "Number of units" = case_when(
      `Assumed number units` == "1" ~ 1,
      Number.of.units > 0 ~ Number.of.units,
      `Assumed number units` == "SpTarget" ~ `Species target`,
      `Assumed number units` == "SpRestoreTarg" ~ `Species restoration target`,
      TRUE ~ as.numeric(`Assumed number units`)),
      
    "Cost per unit" = case_when(
      `Remoteness region` == "West and Cape" ~ `Cost per unit`*1.3,
      TRUE ~ `Cost per unit`),
    "Cost" = `Number of units` * `Cost per unit`)



write.csv(cost_per_action2, file = paste("Output files/cost_per_action_", ver_date,".csv", sep  = ""), row.names=FALSE)


cost_per_action_sum <- cost_per_action2 %>%
  #mutate(Cost = replace_na(Cost, 0)) %>%
  group_by(QSPid) %>%
  summarise(
    Total.cost = sum(Cost, na.rm = TRUE))

#Find species with only one action where cost of action is NA
cost_action_join <- cost_per_action2 %>%
  select(QSPid, Cost)

num_actions <- cost_per_action2 %>%
  group_by(QSPid) %>%
  summarise(count = n()) %>%
  filter(count == 1) %>%
  left_join(cost_action_join, by = join_by(QSPid)) %>%
  filter(is.na(Cost)) %>%
  select(QSPid, count) %>%
  rename("one_action_NA" = `count`)

cost_per_action_sum2 <- cost_per_action_sum %>%
  left_join(num_actions, by = join_by(QSPid)) %>%
  mutate(
    Total.cost.adjusted = case_when(
      one_action_NA == 1 ~ NA,
      TRUE ~ Total.cost)
  )


species_action_source <- action_matrix %>%
  select(QSPid, Action.source, Action.source.date) %>%
  distinct()

species_cost_input <- cost_per_action_sum2 %>%
  left_join(species_action_source, by = join_by(QSPid)) %>%
  mutate(
    Costed.action.source = case_when(
      is.na(Total.cost.adjusted) ~ "Not available",
      TRUE ~ Action.source),
    
    Costed.action.source.date = case_when(
      is.na(Total.cost.adjusted) ~ "Not available",
      TRUE ~ Action.source.date),
    
    Cost.window = case_when(
      is.na(Total.cost.adjusted) ~ "Not available",
      Total.cost.adjusted <    500000 ~ "<0.5 million",
      Total.cost.adjusted <=  1000000 ~ "0.5-1 million",
      Total.cost.adjusted <=  5000000 ~ "1-5 million",
      Total.cost.adjusted <= 10000000 ~ "5-10 million",
      Total.cost.adjusted <= 50000000 ~ "10-50 million",
      Total.cost.adjusted >  50000000 ~ ">50 million")
  )


species_cost_input2 <- species_cost_input %>%
  select(QSPid, Total.cost.adjusted, Cost.window, Costed.action.source, Costed.action.source.date)

write.csv(species_cost_input2, file = paste("Output files/main_table_costs_", ver_date,".csv", sep  = ""), row.names=FALSE)







