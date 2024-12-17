#*******************************************************************************
#*******************************************************************************
#Qld threatened species information database and prioritisation
#Code to create species threat and action matrices
#2024


#Tracy Rout 

#27-11-24
#*******************************************************************************
#*******************************************************************************


#Initialising ####
#Clean up work station
rm(list = ls())

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
select <- dplyr::select
options(scipen = 999) # to change the notation

ver_date = "271124"

#Set working directory ####
#setwd("C:/Your path/TS Database in R")


#Import data (Excel sheets) ####
#Database management information
species_ref <- read_excel("Input files/2024_reference_list_update_210624.xlsx", sheet = "2024 ref list") %>%
  rename(
    species_name = species_name_june2024,
    wildnet_taxon_id = wildnet_taxon_id_june2024,
    wildnet_taxon_id_source = wildnet_taxon_id_source_june2024,
    QSPid = QSPid_june2024)

species_replacements <- read_excel("Input files/Species_replacements_TRout_141024.xlsx", sheet = "All Replacements 2024")

species_name_replacements <- species_replacements %>%
  select(original_name, replacement_name) %>%
  filter(!is.na(original_name)) %>%
  rename(
    from = original_name,
    to = replacement_name)

#Read in threat data from different sources
threats_bot1 <- read_excel("Input files/BOTI_threat_transfer_TRout_220623.xlsx", sheet = "Sp with threats formatted")
threats_bot2 <- read_excel("Input files/BOTII_threat_transfer_TRout_220623.xlsx", sheet = "Sp with threats formatted")
threats_ward <- read_excel("Input files/EPBC_threat_transfer_TRout_080724.xlsx", sheet = "Sp with threats formatted") #updated with cassowary threats - extrapolated to both populations

threat_typology <- read_excel("Input files/Threat_typology_numbers_JCarwardine_received_190623.xlsx") %>%
  select(Number:Definition) %>%
  rename('Typology number' = Number, 'Threat level 1'= "Typology level 1", 'Threat level 2'= "Typology level 2", 'Threat level 3'= "Typology level 3")

#These datasets have data linked to QSPid
threats_desdocs_22 <- read.csv("Input files/Threats_DESdocuments_modified_TRout_GYoung_080823.csv")
threats_desdocs_23 <- read_excel("Input files/Threats_DESdocuments_2023species_modified_TRout_140823.xlsx", sheet = "For database")
threats_desdocs_24 <- read_excel("Input files/Threats_Actions_DESdocuments_modified_TRout_JCarwardine_111124.xlsx", sheet = "Threats")

threats_surveys_22 <- read_excel("Input files/Threats_expert_surveys_modified_TRout_160924.xlsx", sheet = "2022 survey threat matrix clean", range = 'A1:I1243')
threats_surveys_23 <- read_excel("Input files/Threats_expert_surveys_modified_TRout_160924.xlsx", sheet = "2023 survey threat matrix clean")


#The first three sets of threat data need to be joined to QSPid ####
threats_bot1 <- threats_bot1 %>%
  mutate(
    "Threat source" = "Back on Track I",
    "Threat source date" = "2010" 
  )
threats_bot2 <- threats_bot2 %>%
  mutate(
    "Threat source" = "Back on Track II",
    "Threat source date" = "2014"
  )

threats_ward <- threats_ward %>%
  mutate(
    "Threat source" = "Ward et al. 2021. A national-scale dataset for threats impacting Australia's imperiled flora and fauna. Ecology and Evolution 11(17): 11749-11761.",
    "Threat source date" = "2021"
  )

#Join these three datasets together
threats_bot_ward <- bind_rows(threats_bot1, threats_bot2, threats_ward) #join transferred threats into one data frame

#Now create a key column in species_ref and all_threats_indes so we can join them together
species_ref <- mutate(species_ref, species_name_dbase = species_ref$species_name)
threats_bot_ward <- mutate(threats_bot_ward, species_name_dbase = threats_bot_ward$`Species name`)

#This involves removing all spaces and non alphanumeric characters in key column and converting to lower case
species_ref$species_name_dbase <- gsub("×", "x", species_ref$species_name_dbase) #replace this special character with a normal x
species_ref$species_name_dbase <- gsub("[^[:alnum:]]", "", species_ref$species_name_dbase) #remove all other non-alphanumeric characters (including spaces)
species_ref$species_name_dbase <- tolower(species_ref$species_name_dbase) #convert to lower case

threats_bot_ward$species_name_dbase <- gsub("×", "x", threats_bot_ward$species_name_dbase) #replace this special character with a normal x
threats_bot_ward$species_name_dbase <- gsub("[^[:alnum:]]", "", threats_bot_ward$species_name_dbase) #remove all other non-alphanumeric characters (including spaces)
threats_bot_ward$species_name_dbase <- tolower(threats_bot_ward$species_name_dbase) #convert to lower case

#do the same for the species name replacements
species_name_replacements$from <- gsub("×", "x", species_name_replacements$from) #replace this special character with a normal x
species_name_replacements$from <- gsub("[^[:alnum:]]", "", species_name_replacements$from) #remove all other non-alphanumeric characters (including spaces)
species_name_replacements$from <- tolower(species_name_replacements$from) #convert to lower case

species_name_replacements$to <- gsub("×", "x", species_name_replacements$to) #replace this special character with a normal x
species_name_replacements$to <- gsub("[^[:alnum:]]", "", species_name_replacements$to) #remove all other non-alphanumeric characters (including spaces)
species_name_replacements$to <- tolower(species_name_replacements$to) #convert to lower case


#Now we replace any superceded species names
for (i in 1:nrow(species_name_replacements)) 
{
  for (j in 1:nrow(threats_bot_ward))
  {
    if (threats_bot_ward$species_name_dbase[j] == species_name_replacements$from[i])
    {threats_bot_ward$species_name_dbase[j] <- species_name_replacements$to[i]}
  }
}

#We remove duplicates from threats_bot_ward and select just the columns we will use for the species-threat matrix
threats_bot_ward <- distinct(threats_bot_ward, .keep_all = TRUE) %>%
  select(species_name_dbase,`Finest level`, `Threat details`, `Threat source`, `Threat source date`)

#Now we join threats_bot_ward to species_ref to get QSPid for species
threats_bot_ward_formatted <- threats_bot_ward %>% 
  left_join(species_ref, by = 'species_name_dbase') %>%
  filter(!is.na(QSPid)) %>%
  select(QSPid, `Finest level`:`Threat source date`) %>%
  rename('Typology number' = `Finest level`)
  

#Clean up formatting of threat data from DES documents ####
threats_desdocs_22_formatted <- threats_desdocs_22 %>%
  select(QSPid, Typology, Threat.details, Source, Year) %>% #We want to get the new typology names so just take number
  rename('Typology number' = `Typology`, 'Threat details' = Threat.details, 'Threat source' = Source, 'Threat source date' = Year) 
threats_desdocs_22_formatted$`Threat details` <- iconv(threats_desdocs_22_formatted$`Threat details`, "UTF-8", "ASCII", sub = " ")
threats_desdocs_22_formatted$`Threat source` <- iconv(threats_desdocs_22_formatted$`Threat source`, "UTF-8", "ASCII", sub = " ") #Fixing a problem with the text introducing characters
threats_desdocs_22_formatted$`Threat source date` <- as.character(threats_desdocs_22_formatted$`Threat source date`)

threats_desdocs_23_formatted <- threats_desdocs_23 %>%
  select(QSPid, Typology, "Threat details", Source, Year) %>% #We want to get the new typology names so just take number
  rename('Typology number' = `Typology`, 'Threat source' = Source, 'Threat source date' = Year) 
threats_desdocs_23_formatted$`Threat details` <- iconv(threats_desdocs_23_formatted$`Threat details`, "UTF-8", "ASCII", sub = " ")
threats_desdocs_23_formatted$`Threat source` <- iconv(threats_desdocs_23_formatted$`Threat source`, "UTF-8", "ASCII", sub = " ") #Fixing a problem with the text introducing characters
threats_desdocs_23_formatted$`Threat source date` <- as.character(threats_desdocs_23_formatted$`Threat source date`)

threats_desdocs_24_formatted <- threats_desdocs_24 %>%
  select(QSPid, Typology, "Threat details", Reference, `Threat source date`) %>% #We want to get the new typology names so just take number
  rename('Typology number' = `Typology`, 'Threat source' = Reference)
threats_desdocs_24_formatted$`Threat details` <- iconv(threats_desdocs_24_formatted$`Threat details`, "UTF-8", "ASCII", sub = " ")
threats_desdocs_24_formatted$`Threat source` <- iconv(threats_desdocs_24_formatted$`Threat source`, "UTF-8", "ASCII", sub = " ") #Fixing a problem with the text introducing characters
threats_desdocs_24_formatted$`Threat source date` <- as.character(threats_desdocs_24_formatted$`Threat source date`)

#Join the three together
threats_desdocs_formatted <- threats_desdocs_22_formatted %>%
  bind_rows(threats_desdocs_23_formatted) %>%
  bind_rows(threats_desdocs_24_formatted)

#Join BOT, Ward, DES threats together and add species names, wildnet id and threat typology level names####
threats_bot_ward_desdocs <- threats_bot_ward_formatted %>%
  bind_rows(threats_desdocs_formatted) %>%
  left_join(threat_typology, by = 'Typology number') %>%
  left_join(species_ref, by = 'QSPid') %>%
  select(species_name, wildnet_taxon_id, QSPid, 'Typology number', 'Threat level 1':'Threat level 3', 'Threat details', 'Threat source', 'Threat source date')

#Format some threats
threats_bot_ward_desdocs_formatted <- threats_bot_ward_desdocs %>%
  mutate(
    'Threat level 1' = case_when(
      `Typology number` == "None documented" ~ "Not available",
      `Typology number` == "?" ~ "Not available",
      is.na(`Typology number`) ~ "Not available",
      `Typology number` == "EX" ~ "Not available",
      TRUE ~ `Threat level 1`),
    'Threat level 2' = case_when(
      `Typology number` == "None documented" ~ "Not available",
      `Typology number` == "EX" ~ "Not available",
      `Threat level 2` == "Details if known" ~ "See threat details, if available",
      `Threat level 2` == "Named species (where known)" ~ "See threat details, if available",
      `Threat level 2` == "Details" ~ "See threat details, if available",
      `Threat level 2` == "Specify details where known (e.g. pathogen, named pathogen, other)" ~ "See threat details, if available",
      `Threat level 2` == "Specify change if known" ~ "See threat details, if available",
      `Threat level 2` == "" ~ "See threat details, if available",
      is.na(`Threat level 2`) ~ "See threat details, if available",
      TRUE ~ `Threat level 2`),
    'Threat level 3' = case_when(
      `Typology number` == "None documented" ~ "Not available",
      `Typology number` == "EX" ~ "Not available",
      `Threat level 3` == "Details if known" ~ "See threat details, if available",
      `Threat level 3` == "Named species (where known)" ~ "See threat details, if available",
      `Threat level 3` == "Details" ~ "See threat details, if available",
      `Threat level 3` == "Specify details where known (e.g. pathogen, named pathogen, other)" ~ "See threat details, if available",
      `Threat level 3` == "Specify change if known" ~ "See threat details, if available",
      `Threat level 3` == "" ~ "See threat details, if available",
      is.na(`Threat level 3`) ~ "See threat details, if available",
      TRUE ~ `Threat level 3`),
    'Threat details' = case_when(
      `Threat details` == " " ~ "None provided",
      `Threat details` == "" ~ "None provided",
      is.na(`Threat details`) ~ "None provided",
      TRUE ~ `Threat details`),
    'Threat source' = case_when(
      `Threat source` == "" ~ "Not available",
      `Threat source` == "NA" ~ "Not available",
      is.na(`Threat source`) ~ "Not available",
      TRUE ~ `Threat source`),
    'Threat source date' = case_when(
      `Threat source date` == "" ~ "Not available",
      `Threat source date` == "NA" ~ "Not available",
      is.na(`Threat source date`) ~ "Not available",
      TRUE ~ `Threat source date`)) %>%
  select(-`Typology number`) %>%
  rename('Species name' = species_name, 'Wildnet Taxon Id'= wildnet_taxon_id) 


#Format threat data from surveys ####
threats_surveys <- threats_surveys_22 %>%
  bind_rows(threats_surveys_23)

#Join with species_ref to get Wildnet Taxon Id, reformat
threats_surveys_formatted <- threats_surveys %>%
  left_join(species_ref, by = 'QSPid') %>%
  select(species_name, wildnet_taxon_id, QSPid, 'Threat level 1':'Threat source date' ) %>%
  mutate(
    'Threat level 3' = case_when(
      is.na(`Threat level 3`) ~ "See threat details, if available",
      TRUE ~ `Threat level 3`),
    'Threat details' = case_when(
      `Threat details` == " " ~ "None provided",
      `Threat details` == "" ~ "None provided",
      is.na(`Threat details`) ~ "None provided",
      TRUE ~ `Threat details`),
    ) %>%
  rename('Species name' = species_name, 'Wildnet Taxon Id'= wildnet_taxon_id) 
threats_surveys_formatted$`Threat details` <- iconv(threats_surveys_formatted$`Threat details`, "UTF-8", "ASCII", sub = " ")
threats_surveys_formatted$`Threat source` <- iconv(threats_surveys_formatted$`Threat source`, "UTF-8", "ASCII", sub = " ")
threats_surveys_formatted$`Threat source` <- na_if(threats_surveys_formatted$`Threat source`,"")
threats_surveys_formatted$`Threat source date` <- as.character(threats_surveys_formatted$`Threat source date`)
threats_surveys_formatted$`Threat source date` <- na_if(threats_surveys_formatted$`Threat source date`,"")


#Join all threat data together ####
all_threats_formatted <- threats_bot_ward_desdocs_formatted %>%
  bind_rows(threats_surveys_formatted) %>%
  distinct() #removes a couple of duplicate

#Table of QSPid with number of threats
num_threats <- all_threats_formatted %>%
  group_by(QSPid) %>%
  summarise(QSPid_count = n())

#Join number of threats back to all threats
all_threats_formatted <- all_threats_formatted %>%
  left_join(num_threats, by = "QSPid") 

excess_threats <- all_threats_formatted %>%
  filter(`Threat level 1` == "Not available" & QSPid_count > 1)

#Remove excess Not available threats
all_threats_formatted_clean <- all_threats_formatted %>%
  filter((`Threat level 1` != "Not available") |(`Threat level 1` == "Not available" & QSPid_count == 1)) %>%
  select(QSPid:`Threat source date`)
#Keep if threat level 1 doesn't equal Not available, or if it does and it's the only threat for that species

#Check which species have threats
all_threats_formatted_clean_sp <- all_threats_formatted_clean %>%
  select(QSPid) %>%
  distinct() %>%
  mutate(
    "Threats" = "Yes")

check_sp_threats <- species_ref %>%
  left_join(all_threats_formatted_clean_sp, by = join_by(QSPid))


#Write threat matrix as a file ####
write.csv(all_threats_formatted_clean, file = paste("Output files/species_threat_matrix_internal_", ver_date,".csv", sep  = ""), row.names=FALSE)

threat_matrix_external <- all_threats_formatted_clean %>%
  filter(
    `Threat source` != "Queensland Department of Environment, Science and Innovation 2024"
  )

write.csv(threat_matrix_external, file = paste("Output files/species_threat_matrix_external_", ver_date,".csv", sep  = ""), row.names=FALSE)


#End of threat matrix




#Read in action typology ####
action_typology <- read_excel("Input files/Action_typology_numbers_DESI_received_020924.xlsx") %>%
  select(`Action level 1`:`Management units`)

#Threat to action translation - test with threat typology ####
threat_action_translate1_test <- threat_typology %>% #Add action level 1
  mutate(
    "Action level 1" = case_when(
      `Threat level 1` == "Adverse fire regimes" ~ "Fire management", #1
      `Threat level 1` == "Changed surface and groundwater regimes" ~ "Freshwater restoration", #2
      `Threat level 1` == "Climate change & severe weather" ~ "Intensive in-situ management; Habitat protection", #3
      `Threat level 1` == "Restricted populations / low population viability" ~ "Ex-situ management", #4
      `Threat level 1` == "Habitat loss and fragmentation" ~ "Habitat protection", #5
      `Threat level 1` == "Adverse grazing practices" ~ "Grazing management", #6
      `Threat level 1` == "Native forest harvesting" ~ "Forestry management; Site management", #7
      `Threat level 1` == "Invasive and problematic animals" & `Threat level 2` == "Over-abundant macropods" ~ "Native herbivore management", #8.11
      `Threat level 1` == "Invasive and problematic animals" & `Threat level 2` == "Native species interactions" ~ "Other problematic species management", #8.12
      `Threat level 1` == "Invasive and problematic animals" & `Threat level 2` == "Non-native birds" ~ "Invasive/problematic bird management", #8.2
      `Threat level 1` == "Invasive and problematic animals" & `Threat level 2` == "Non-native fish" ~ "Invasive fish management", #8.3 
      `Threat level 1` == "Invasive and problematic animals" & `Threat level 2` == "Invasive predatory mammals" ~ "Invasive predator management", #8.7
      `Threat level 1` == "Invasive and problematic animals" & `Threat level 2` == "Small invasive herbivores" ~ "Rabbit management", #8.8
      `Threat level 1` == "Invasive and problematic animals" & `Threat level 2` == "Large invasive herbivores" ~ "Invasive large herbivore management", #8.9
      `Threat level 1` == "Invasive and problematic animals" & `Threat level 2` == "Large invasive omnivores" ~ "Invasive pig management", #8.10
      `Threat level 1` == "Invasive and problematic animals" ~ "Small invasive animal control", #all other 8 
      `Threat level 1` == "Invasive and problematic plants" ~ "Weed management", #9
      `Threat level 1` == "Problematic disease and pathogens" ~ "Disease management", #10
      `Threat level 1` == "Harm from human activities" & `Threat level 2` == "Human intrusion" ~ "Site management", #11.2
      `Threat level 1` == "Harm from human activities" & `Threat level 2` == "Entanglement and/or ingestion" ~ "Site management", #11.4
      `Threat level 1` == "Harm from human activities" ~ "Fishing, hunting, harvesting management", #all other 11
      `Threat level 1` == "Pollution and solid waste" ~ "Site management", #12
      TRUE ~ "Not available")) %>%
  separate_rows(`Action level 1`, sep = "; ")
    
    
threat_action_translate2_test <- threat_action_translate1_test %>% #Add action level 2
  mutate(    
    "Action level 2" = case_when(
      `Action level 1` == "Disease management" & `Threat level 3` == "Phytophthora dieback (Phytophthora cinnamomi)" ~ "Biosecurity; Phosphite application - aerial; Phosphite injection - ground", #10 with phytophthera
      `Action level 1` == "Disease management" ~ "Biosecurity", #all other 10
      `Action level 1` == "Ex-situ management" ~ "Other/unspecified", #4
      `Action level 1` == "Fishing, hunting, harvesting management" & `Threat level 2` == "Intentional harvest" ~ "Co-development of species management plan", #11.1
      `Action level 1` == "Fishing, hunting, harvesting management" & `Threat level 2` == "Targeted killing e.g., poisoning or trapping" ~ "Increase compliance", #11.3
      `Action level 1` == "Fishing, hunting, harvesting management" & `Threat level 2` == "Unintentional poisoning" ~ "Increase compliance", #11.5
      `Action level 1` == "Fishing, hunting, harvesting management" & `Threat level 2` == "Bycatch" ~ "Monitoring of commercial fishing to increase compliance; Improve techniques of commercial fishing", #11.6 (2 actions)
      `Action level 1` == "Forestry management" ~ "Manage forestry practices (liaison, industry change)", #7
      `Action level 1` == "Freshwater restoration" & `Threat level 2` == "Alteration to surface water and infiltration" ~ "Maintenance/restoration of necessary flow regimes; Bank stabilisation", #2.2 (2 actions)
      `Action level 1` == "Freshwater restoration" & `Threat level 2` == "Dams and other structural barriers (e.g., weirs and barrages)" ~ "Maintenance/restoration of necessary flow regimes; Instream structure installation e.g., fish ways; Instream structure removal", #2.3 (3 actions)
      `Action level 1` == "Freshwater restoration" ~ "Maintenance/restoration of necessary flow regimes", #all other 2
      `Action level 1` == "Grazing management" ~ "Fencing waterways; Sustainable co-management of land", #6 (2 actions)
      `Action level 1` == "Habitat protection" ~ "Retain habitat / protect from loss and development", #3 and 5
      `Action level 1` == "Site management" & `Threat level 1` == "Native forest harvesting" ~ "Education signage, barriers to restrict access", #7
      `Action level 1` == "Site management" & `Threat level 1` == "Pollution and solid waste" ~ "Pollution and waste management", #12
      `Action level 1` == "Site management" & `Threat level 2` == "Human intrusion" ~ "Education signage, barriers to restrict access", #11.2
      `Action level 1` == "Site management" & `Threat level 2` == "Entanglement and/or ingestion" ~ "Pollution and waste management", #11.4
      `Action level 1` == "Not available" ~ "Not available",
      TRUE ~ "Other/unspecified"
    )) %>%
  separate_rows(`Action level 2`, sep = "; ")

write.csv(threat_action_translate2_test, file = paste("Output files/threat_action_translate_test_", ver_date,".csv", sep  = ""), row.names=FALSE)

#Threat to action translation - from threat matrix ####
threat_action_translate1 <- all_threats_formatted_clean %>% #Add action level 1
  mutate(
    "Action level 1" = case_when(
      `Threat level 1` == "Adverse fire regimes" ~ "Fire management", #1
      `Threat level 1` == "Changed surface and groundwater regimes" ~ "Freshwater restoration", #2
      `Threat level 1` == "Climate change & severe weather" ~ "Intensive in-situ management; Habitat protection", #3
      `Threat level 1` == "Restricted populations / low population viability" ~ "Ex-situ management", #4
      `Threat level 1` == "Habitat loss and fragmentation" ~ "Habitat protection; Terrestrial habitat restoration", #5
      `Threat level 1` == "Adverse grazing practices" ~ "Grazing management", #6
      `Threat level 1` == "Native forest harvesting" ~ "Forestry management; Site management", #7
      `Threat level 1` == "Invasive and problematic animals" & `Threat level 2` == "Over-abundant macropods" ~ "Native herbivore management", #8.11
      `Threat level 1` == "Invasive and problematic animals" & `Threat level 2` == "Native species interactions" ~ "Other problematic species management", #8.12
      `Threat level 1` == "Invasive and problematic animals" & `Threat level 2` == "Non-native birds" ~ "Invasive/problematic bird management", #8.2
      `Threat level 1` == "Invasive and problematic animals" & `Threat level 2` == "Non-native fish" ~ "Invasive fish management", #8.3 
      `Threat level 1` == "Invasive and problematic animals" & `Threat level 2` == "Invasive predatory mammals" ~ "Invasive predator management", #8.7
      `Threat level 1` == "Invasive and problematic animals" & `Threat level 2` == "Small invasive herbivores" ~ "Rabbit management", #8.8
      `Threat level 1` == "Invasive and problematic animals" & `Threat level 2` == "Large invasive herbivores" ~ "Invasive large herbivore management", #8.9
      `Threat level 1` == "Invasive and problematic animals" & `Threat level 2` == "Large invasive omnivores" ~ "Invasive pig management", #8.10
      `Threat level 1` == "Invasive and problematic animals" ~ "Small invasive animal control", #all other 8 
      `Threat level 1` == "Invasive and problematic plants" ~ "Weed management", #9
      `Threat level 1` == "Problematic disease and pathogens" ~ "Disease management", #10
      `Threat level 1` == "Harm from human activities" & `Threat level 2` == "Human intrusion" ~ "Site management", #11.2
      `Threat level 1` == "Harm from human activities" & `Threat level 2` == "Entanglement and/or ingestion" ~ "Site management", #11.4
      `Threat level 1` == "Harm from human activities" ~ "Fishing, hunting, harvesting management", #all other 11
      `Threat level 1` == "Pollution and solid waste" ~ "Site management", #12
      `Threat level 1` == "Other threats not covered" ~ "Other",
      TRUE ~ "Not available")) %>%
  separate_rows(`Action level 1`, sep = "; ")


threat_action_translate2 <- threat_action_translate1 %>% #Add action level 2
  mutate(    
    "Action level 2" = case_when(
      `Action level 1` == "Disease management" & `Threat level 3` == "Phytophthora dieback (Phytophthora cinnamomi)" ~ "Biosecurity; Phosphite application - aerial; Phosphite injection - ground", #10 with phytophthera
      `Action level 1` == "Disease management" ~ "Biosecurity", #all other 10
      `Action level 1` == "Ex-situ management" ~ "Other/unspecified", #4
      `Action level 1` == "Fishing, hunting, harvesting management" & `Threat level 2` == "Intentional harvest" ~ "Co-development of species management plan", #11.1
      `Action level 1` == "Fishing, hunting, harvesting management" & `Threat level 2` == "Targeted killing e.g., poisoning or trapping" ~ "Increase compliance", #11.3
      `Action level 1` == "Fishing, hunting, harvesting management" & `Threat level 2` == "Unintentional poisoning" ~ "Increase compliance", #11.5
      `Action level 1` == "Fishing, hunting, harvesting management" & `Threat level 2` == "Bycatch" ~ "Monitoring of commercial fishing to increase compliance; Improve techniques of commercial fishing", #11.6 (2 actions)
      `Action level 1` == "Forestry management" ~ "Manage forestry practices (liaison, industry change)", #7
      `Action level 1` == "Freshwater restoration" & `Threat level 2` == "Alteration to surface water and infiltration" ~ "Maintenance/restoration of necessary flow regimes; Bank stabilisation", #2.2 (2 actions)
      `Action level 1` == "Freshwater restoration" & `Threat level 2` == "Dams and other structural barriers (e.g., weirs and barrages)" ~ "Maintenance/restoration of necessary flow regimes; Instream structure installation e.g., fish ways; Instream structure removal", #2.3 (3 actions)
      `Action level 1` == "Freshwater restoration" ~ "Maintenance/restoration of necessary flow regimes", #all other 2
      `Action level 1` == "Grazing management" ~ "Fencing waterways; Sustainable co-management of land", #6 (2 actions)
      `Action level 1` == "Habitat protection" ~ "Retain habitat / protect from loss and development", #3 and 5
      `Action level 1` == "Site management" & `Threat level 1` == "Native forest harvesting" ~ "Education signage, barriers to restrict access", #7
      `Action level 1` == "Site management" & `Threat level 1` == "Pollution and solid waste" ~ "Pollution and waste management", #12
      `Action level 1` == "Site management" & `Threat level 2` == "Human intrusion" ~ "Education signage, barriers to restrict access", #11.2
      `Action level 1` == "Site management" & `Threat level 2` == "Entanglement and/or ingestion" ~ "Pollution and waste management", #11.4
      `Action level 1` == "Terrestrial habitat restoration" ~ "Assisted natural regeneration",
      `Action level 1` == "Not available" ~ "Not available",
      TRUE ~ "Other/unspecified"),
    "Action source" = "Actions assumed based on threats",
    "Action source date" = "2024") %>%
  separate_rows(`Action level 2`, sep = "; ")

write.csv(threat_action_translate2, file = paste("Output files/threat_action_translate_matrix_", ver_date,".csv", sep  = ""), row.names=FALSE)

#check the list of species in this matrix
sp_threat_action_list <- threat_action_translate2 %>%
  select(QSPid) %>%
  distinct()

#Actions matrix ####
action_matrix_translate <- threat_action_translate2 %>%
  mutate(
    "Action details" = "",
    "Unit" = "",
    "Number of units" = as.double(""))

for (i in 1:nrow(action_typology)) 
{
  for (j in 1:nrow(action_matrix_translate))
  {
    if (action_matrix_translate$`Action level 1`[j] == action_typology$`Action level 1`[i] && action_matrix_translate$`Action level 2`[j] == action_typology$`Action level 2`[i])
    {action_matrix_translate$Unit[j] <- action_typology$`Management units`[i]}
  }
}

action_matrix_translate_formatted <- action_matrix_translate %>%
  select(QSPid, `Action level 1`, `Action level 2`, `Action details`, `Unit`, `Number of units`, `Action source`, `Action source date`) %>%
  distinct() #get rid of any duplicates


#Import real action data ####
#Actions from survey
actions_surveys <- read_excel("Input files/Actions_expert_surveys_CSIRO_061124.xlsx", sheet = "surveyactions2022-23 for databa")

#Actions from DES docs
actions_desdocs24 <- read_excel("Input files/Threats_Actions_DESdocuments_modified_TRout_JCarwardine_111124.xlsx", sheet = "Management Actions")

actions_surveys_join <- actions_surveys %>%
  mutate(
    "Action details" = "",
    "Action source" = case_when(
      `Year` == "2022" ~ "Threatened species recovery information survey 2022",
      `Year` == "2023" ~ "Threatened species recovery information survey 2023")) %>%
  select(QSPid, `Action level 1`, `Action level 2`, `Action details`, `Units`, `Average of Quantity`, `Action source`, `Year`) %>%
  rename("Unit" = `Units`, "Number of units" = `Average of Quantity`, "Action source date" = `Year`) 
actions_surveys_join$`Action source date` <- as.character(actions_surveys_join$`Action source date`)

actions_desdocs24_join <- actions_desdocs24 %>%
  select(QSPid, `Management Action Typology Broad Level`, `Management Action second category`, `Comments`, `Unit`, `Number of units`, `Reference`, `Action source date`) %>%
  rename("Action level 1" = `Management Action Typology Broad Level`,
         "Action level 2" = `Management Action second category`,
         "Action details" = `Comments`,
         "Action source" = `Reference`)
actions_desdocs24_join$`Action source date` <- as.character(actions_desdocs24_join$`Action source date`)

#Hierarchy of actions = survey then DES docs then assumed ####

#Species with actions from the survey
species_actions_survey <- actions_surveys_join %>%
  select(QSPid) %>%
  distinct()

#First remove DES docs actions for species that have survey actions
desdocs_species_toremove <- actions_desdocs24_join

for (i in 1:nrow(species_actions_survey)) 
{
  for (j in 1:nrow(desdocs_species_toremove))
  {
    if (desdocs_species_toremove$QSPid[j] == species_actions_survey$QSPid[i])
    {desdocs_species_toremove$QSPid[j] <- 0}
  }
}

desdocs_species_removed <- desdocs_species_toremove %>%
  filter(QSPid != 0)

#Group actions from surveys and DESI docs
real_actions <- actions_surveys_join %>%
  bind_rows(desdocs_species_removed)

#species QSPid that have real actions from surveys and DES docs
species_w_actions <- real_actions %>%
  select(QSPid) %>%
  distinct()


#Now override assumed actions with real actions ####
#First remove assumed actions for species with real actions
assumed_species_toremove <- action_matrix_translate_formatted

for (i in 1:nrow(species_w_actions)) 
{
  for (j in 1:nrow(assumed_species_toremove))
  {
    if (assumed_species_toremove$QSPid[j] == species_w_actions$QSPid[i])
    {assumed_species_toremove$QSPid[j] <- 0}
  }
}

assumed_species_removed <- assumed_species_toremove %>%
  filter(QSPid != 0)

#check
species_removed_list <- assumed_species_removed %>%
  select(QSPid) %>%
  distinct()


#Now add in real actions
action_matrix <- assumed_species_removed %>%
  bind_rows(real_actions) %>%
  distinct() #get rid of any duplicates in survey or desdocs

action_matrix_formatted <- action_matrix %>%
  mutate(
    'Action level 1' = case_when(
      is.na(`Action level 1`) ~ "Not available",
      TRUE ~ `Action level 1`),
    'Action level 2' = case_when(
      `Action level 1` == "Not available" ~ "Not available",
      `Action level 2` == "Other" ~ "Other/unspecified",
      `Action level 2` == "Unspecified" ~ "Other/unspecified",
      is.na(`Action level 2`) ~ "Other/unspecified",
      TRUE ~ `Action level 2`),
    'Action details' = case_when(
      is.na(`Action details`) ~ "Not available",
      `Action details` == "" ~ "Not available",
      TRUE ~ `Action details`),
    'Unit' = case_when(
      is.na(`Unit`) ~ "Not available",
      is.na(`Number of units`) ~ "Not available",
      `Action level 1` == "Other" ~ "Not available",
      `Unit` == "" ~ "Not available",
      TRUE ~ `Unit`),
    'Number of units' = case_when(
      is.na(`Number of units`) ~ -1,
      `Action level 1` == "Other" ~ -1,
      TRUE ~ `Number of units`),
    'Action source' = case_when(
      `Action level 1` == "Not available" ~ "Not available",
      `Action source` == "" ~ "Not available",
      `Action source` == "NA" ~ "Not available",
      is.na(`Action source`) ~ "Not available",
      TRUE ~ `Action source`),
    'Action source date' = case_when(
      `Action level 1` == "Not available" ~ "Not available",
      `Action source date` == "" ~ "Not available",
      `Action source date` == "NA" ~ "Not available",
      is.na(`Action source date`) ~ "Not available",
      TRUE ~ `Action source date`))


write.csv(action_matrix_formatted, file = paste("Output files/species_action_matrix_internal_", ver_date,".csv", sep  = ""), row.names=FALSE)

action_matrix_external <- action_matrix_formatted %>%
  mutate(
    'Action level 1' = case_when(
      `Action source` == "Queensland Department of Environment, Science and Innovation 2024" ~ "Not available",
      TRUE ~ `Action level 1`),
    'Action level 2' = case_when(
      `Action source` == "Queensland Department of Environment, Science and Innovation 2024" ~ "Not available",
      TRUE ~ `Action level 2`),
    'Action details' = case_when(
      `Action source` == "Queensland Department of Environment, Science and Innovation 2024" ~ "Not available",
      TRUE ~ `Action details`),
    'Unit' = case_when(
      `Action source` == "Queensland Department of Environment, Science and Innovation 2024" ~ "Not available",
      TRUE ~ `Unit`),
    'Number of units' = case_when(
      `Action source` == "Queensland Department of Environment, Science and Innovation 2024" ~ -1,
      TRUE ~ `Number of units`),
    'Action source date' = case_when(
      `Action source` == "Queensland Department of Environment, Science and Innovation 2024" ~ "Not available",
      TRUE ~ `Action source date`),
    'Action source' = case_when(
      `Action source` == "Queensland Department of Environment, Science and Innovation 2024" ~ "Not available",
      TRUE ~ `Action source`)) %>%
  distinct()

write.csv(action_matrix_external, file = paste("Output files/species_action_matrix_external_", ver_date,".csv", sep  = ""), row.names=FALSE)

