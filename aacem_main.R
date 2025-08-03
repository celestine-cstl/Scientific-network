
# ATALLAH Joanne
# AUBEL Xavier 
# COUSTILLAC Cêlestine  
# EZIN Philia 
# MADOR Fanis

# PREMIERE PARTIE : Traitement des données ----

## Packages ----
library(dplyr)
library(stringr)
library(tidyr)

## Fonctions ----
source("aacem_src_utilities.R")




## QUESTION 1 ----
### Chargement des données brutes ----

#### Firm-Brevet ----
base_brevet_firm <- read.table("DATA/202202_EPO_App_reg_small.txt", 
                               sep = ",", 
                               quote = '"',
                               header = TRUE, 
                               fill = TRUE,
                               na.strings = c("", "NA", "NULL")) |>
  filter(ctry_code == "FR") |> 
  mutate(app_name = harmonize_names(app_name))

base_brevet_firm <- base_brevet_firm |> 
  mutate(app_name = if_else(grepl("CNRS CNRS", app_name), "CNRS", app_name)) |> 
  mutate(app_name = if_else(grepl("CNRS ETABLISSEMENT PUBLIQUE NATIONAL A CARACTERE SCIENTIFIQUE ET TECHNOLOGIQUE", app_name), "CNRS", app_name)) |> 
  mutate(app_name = if_else(grepl("LE CNRS CNRS", app_name), "CNRS", app_name)) |> 
  mutate(app_name = if_else(grepl("THE CNRS CNRS", app_name), "CNRS", app_name)) |> 
  mutate(app_name = if_else(grepl("CNRS CNRS EPST", app_name), "CNRS", app_name)) |> 
  mutate(app_name = if_else(grepl("CNRS ETABLISSEMENT PUBLIC A CARACTERE SCIENTIFIQUE CULTUREL ET PROFESSIONNEL", app_name), "CNRS", app_name)) |> 
  mutate(app_name = if_else(grepl("CNRS CNRS ETABLISSEMENT PUBLIC SCIENTIFIQUE ET TECHNOLOGIQUE EPST", app_name), "CNRS", app_name)) |> 
  mutate(app_name = if_else(grepl("CNRS DIRECTION DE LA POLITIQUE INDUSTRIELLE", app_name), "CNRS", app_name)) |> 
  mutate(app_name = if_else(grepl("CNRS DIRECTION DE LINNOVATION ET DES RELATIONS AVEC LES ENTREPRISES DIRE", app_name), "CNRS", app_name)) |> 
  mutate(app_name = if_else(grepl("CNRS MISSION DES RELATIONS AVEC LES ENTREPRISES", app_name), "CNRS", app_name)) |> 
  mutate(app_name = if_else(grepl("CNRS CENTRE NATIONAL DE RECHERCHE SCIENTIFIQUE", app_name), "CNRS", app_name)) |> 
  mutate(app_name = if_else(grepl("CNRS CNRS ETABLISSEMENT PUBLIC A CARACTERE SCIENTIFIQUE ET TECHNOLOGIQUE", app_name), "CNRS", app_name)) |> 
  mutate(app_name = if_else(grepl("CNRS ETABLISSEMENT PUBLIC", app_name), "CNRS", app_name)) |> 
  mutate(app_name = if_else(grepl("CENTRE NATIONAL DE LA RECHERCHE SCIENTIFIQUE", app_name), "CNRS", app_name)) |> 
  mutate(app_name = if_else(grepl("CEA ET AUX ENERGIES ALTERNATIVES CEA", app_name), "CEA", app_name)) |> 
  mutate(app_name = if_else(grepl("CEA ET AUX ENERGIES ALTERNATIVES", app_name), "CEA", app_name)) |> 
  mutate(app_name = if_else(grepl("CEA CEA", app_name), "CEA", app_name)) |> 
  mutate(app_name = if_else(grepl("CEA CEA", app_name), "CEA", app_name)) |> 
  mutate(app_name = if_else(grepl("COMMISSARIAT A LENERGIE ATOMIQUE", app_name), "CEA", app_name)) |> 
  mutate(app_name = if_else(grepl("THE COMMISSARIAT A LENERGIE ATOMIQUE ET AUX ALTERNATIVES CEA", app_name), "CEA", app_name)) |> 
  mutate(app_name = if_else(grepl("AIR LIQUIDE ELECTRONICS SYSTEMS", app_name), "AIR LIQUIDE", app_name)) |> 
  mutate(app_name = if_else(grepl("AIR LIQUIDE EUROPEAN HOMECARE OPERATIONS SERVICES", app_name), "AIR LIQUIDE", app_name)) |>  
  mutate(app_name = if_else(grepl("AIR LIQUIDE FRANCE INDUSTRIE", app_name), "AIR LIQUIDE", app_name)) |> 
  mutate(app_name = if_else(grepl("AIR LIQUIDE MEDICAL SYSTEMS", app_name), "AIR LIQUIDE", app_name)) |> 
  mutate(app_name = if_else(grepl("AIR LIQUIDE SANTE FRANCE", app_name), "AIR LIQUIDE", app_name)) |> 
  mutate(app_name = if_else(grepl("AIR LIQUIDE SANTE INTERNATIONAL", app_name), "AIR LIQUIDE", app_name)) |> 
  mutate(app_name = if_else(grepl("AIR LIQUIDE SERVICES", app_name), "AIR LIQUIDE", app_name)) |> 
  mutate(app_name = if_else(grepl("AIR LIQUIDE WELDING FRANCE", app_name), "AIR LIQUIDE", app_name)) |>  
  mutate(app_name = if_else(grepl("CRYOSPACE LAIR LIQUIDE AEROSPATIALE", app_name), "AIR LIQUIDE", app_name)) |>  
  mutate(app_name = if_else(grepl("LAIR LIQUIDE POUR LETUDE ET LEXPLOITATION DES PROCEDES GEORGES CLAUDE", app_name), "AIR LIQUIDE", app_name)) |> 
  mutate(app_name = if_else(grepl("LAIR LIQUIDE ANONYME POUR LETUDE ET LEXPLOITATION DES PROCEDES GEORGES CLAUDE", app_name), "AIR LIQUIDE", app_name)) |> 
  mutate(app_name = if_else(grepl("LAIR LIQUIDE SOCIETE ANOMYME POUR LETUDE ET LEXPLOITATION DES PROCEDES GEORGES CLAUDE", app_name), "AIR LIQUIDE", app_name)) |> 
  mutate(app_name = if_else(grepl("LAIR LIQUIDE SOCIETE ANONYME A DIRECTOIRE ET CONSEIL DE SURVEILLANCE POUR LETUDE ET L EXPLOITATION DES PROCEDES GEORGES C", app_name), "AIR LIQUIDE", app_name)) |> 
  mutate(app_name = if_else(grepl("LAIR LIQUIDE SOCIETE ANONYME POUR LETUDE ET LEXPLOITATION DES PROCEDES GEORGES CLAUDE", app_name), "AIR LIQUIDE", app_name)) |> 
  mutate(app_name = if_else(grepl("SAFRANS", app_name), "SAFRAN", app_name)) |>  
  mutate(app_name = if_else(grepl("SAFRAN TRANSMISSION SYSTEMS", app_name), "SAFRAN", app_name)) |>  
  mutate(app_name = if_else(grepl("SAFRAN SEATS", app_name), "SAFRAN", app_name)) |>  
  mutate(app_name = if_else(grepl("SAFRAN POWER UNITS", app_name), "SAFRAN", app_name)) |>  
  mutate(app_name = if_else(grepl("SAFRAN NACELLES", app_name), "SAFRAN", app_name)) |>  
  mutate(app_name = if_else(grepl("SAFRAN IDENTITY SECURITY", app_name), "SAFRAN", app_name)) |>  
  mutate(app_name = if_else(grepl("SAFRAN HELICOPTER ENGINES", app_name), "SAFRAN", app_name)) |>  
  mutate(app_name = if_else(grepl("SAFRAN ELECTRONICS DEFENSE", app_name), "SAFRAN", app_name)) |>  
  mutate(app_name = if_else(grepl("SAFRAN ELECTRICAL POWER", app_name), "SAFRAN", app_name)) |>  
  mutate(app_name = if_else(grepl("SAFRAN CERAMICS", app_name), "SAFRAN", app_name)) |>  
  mutate(app_name = if_else(grepl("SAFRAN AIRCRAFT ENGINES", app_name), "SAFRAN", app_name)) |>  
  mutate(app_name = if_else(grepl("SAFRAN AEROSYSTEMS", app_name), "SAFRAN", app_name)) |>  
  mutate(app_name = if_else(grepl("SAFRAN AEROTECHNICS", app_name), "SAFRAN", app_name)) |>  
  mutate(app_name = if_else(grepl("SAFRAN DATA SYSTEMS", app_name), "SAFRAN", app_name)) |>  
  mutate(app_name = if_else(grepl("SAFRAN FILTRATION SYSTEMS", app_name), "SAFRAN", app_name)) |>  
  mutate(app_name = if_else(grepl("SAFRAN VENTILATION SYSTEMS", app_name), "SAFRAN", app_name)) |>  
  mutate(app_name = if_else(grepl("SAFRAN ELECTRONICS DEFENSE COCKPIT SOLUTIONS", app_name), "SAFRAN", app_name)) |>  
  mutate(app_name = if_else(grepl("SAFRAN ENGINEERING SERVICES", app_name), "SAFRAN", app_name)) |>  
  mutate(app_name = if_else(grepl("SAFRAN INDENTITY AND SECURITY", app_name), "SAFRAN", app_name)) |>  
  mutate(app_name = if_else(grepl("SAFRAN LANDING SYSTEM", app_name), "SAFRAN", app_name)) 





#### IPC-Brevet ----
base_ipc_brevet <- read.table("DATA/202202_EPO_IPC_small.txt", 
                              sep = ",", 
                              header = TRUE, 
                              fill = TRUE,
                              na.strings = c("", "NA", "NULL")) |>
  filter(prio_year >= 2010 & prio_year <= 2020) |>
  mutate(IPC_4 = substr(IPC, 1, 4))



#### IPC-DEF ----

ipc <- list.files("DATA", 
                  pattern = "EN_ipc_section_[A-H]_title_list_20120101.txt", 
                  full.names = TRUE)


base_def_ipc <- bind_rows(lapply(ipc, extract_ipc_definitions)) |> 
  filter(nchar(IPC_code) == 4) |> 
  rename(IPC_4 = IPC_code) |> 
  mutate(Definition = str_to_sentence(Definition),
         Definition = str_trim(str_remove(Definition, "\\(.*$")))








#### base_complete qui fusionne les données ----
base_complete <- merge(base_brevet_firm, base_ipc_brevet, by = "appln_id")
base_complete <- merge(base_complete, base_def_ipc, by.x = "IPC_4", by.y = "IPC_4")



### Nombre de brevets ----
n_patents <- base_complete |>
  group_by(app_name) |>
  summarize(n_patents = n_distinct(appln_id))



### Ville et département principaux ----
city_dept_main <- base_complete |>
  group_by(app_name, city, postal_code) |>
  summarize(frequency = n(), .groups = "drop") |>
  arrange(app_name, desc(frequency)) |>
  group_by(app_name) |>
  slice_head(n = 1) |>
  mutate(addr_dept_main = ifelse(postal_code %in% c("971", "972", "973", "974", "975", "976", "977", "978", "986", "987", "988"),
                                 postal_code,
                                 substr(as.character(postal_code), 1, 2))) |>
  select(app_name, addr_city_main = city, addr_dept_main)





### IPC principaux et secondaires ----
ipc_counts <- base_complete |>
  group_by(app_name, IPC_4) |>
  summarize(frequency = n(), .groups = "drop") |>
  arrange(app_name, desc(frequency))

ipc_main_secondary <- ipc_counts |>
  group_by(app_name) |>
  slice_head(n = 2) |>
  mutate(ipc_code_type = ifelse(row_number() == 1, "main_IPC", "secondary_IPC")) |>
  pivot_wider(names_from = ipc_code_type, values_from = IPC_4) |>
  # S'assurer que main_IPC n'est pas NA si secondary_IPC est défini
  mutate(main_IPC = if_else(is.na(main_IPC) & !is.na(secondary_IPC), secondary_IPC, main_IPC),
         secondary_IPC = if_else(main_IPC == secondary_IPC, NA_character_, secondary_IPC))

ipc_main_secondary <- ipc_main_secondary |>
  left_join(base_def_ipc, by = c("main_IPC" = "IPC_4")) |>
  rename(main_IPC_desc = Definition) |>
  left_join(base_def_ipc, by = c("secondary_IPC" = "IPC_4")) |>
  rename(secondary_IPC_desc = Definition)



### RESULTAT : Base_brevets ----
base_brevets <- merge(n_patents, city_dept_main, by = "app_name", all.x = TRUE)
base_brevets <- merge(base_brevets, ipc_main_secondary, by = "app_name", all.x = TRUE)

base_brevets <- base_brevets |>
  rename(firm_name = app_name,
         ipc_main_code = main_IPC,
         ipc_main_desc = main_IPC_desc,
         ipc_second_code = secondary_IPC,
         ipc_second_desc = secondary_IPC_desc) |>
  select(firm_name, n_patents, ipc_main_code, ipc_main_desc, ipc_second_code, 
         ipc_second_desc, addr_city_main, addr_dept_main)


base_brevets <- base_brevets |>       # Supprimer les doublons
  distinct(firm_name, .keep_all = TRUE)























## QUESTION 2 ----

### Chargement données bruts ----
base_emp_fmt <- read.csv("DATA/emp_offers_fmt.tsv",  
                         sep = ",",  
                         header = TRUE, 
                         na.strings = c("", "NA", "NULL")) |> 
  mutate(entreprise = harmonize_names(entreprise)) |>
  harmonize_skills_column(col_name = "competences_requises") |> 
  mutate(entreprise = if_else(grepl("TOTALENERGIES DIGITAL FACTORY", intitule_poste), 
                              "TOTAL ENERGIE", 
                              entreprise))



### Nombre d'offres d'emploie ----
n_offres <-base_emp_fmt |> 
  group_by(entreprise) |> 
  summarise(n_offres = n()) 


### Secteur principal ----
sector <- base_emp_fmt |>
  mutate(sector_list = str_split(secteur, ",\\s*")) |>
  unnest(sector_list) |>
  group_by(entreprise, sector_list) |>
  summarise(n = n(), .groups = "drop") |>
  arrange(entreprise, desc(n)) |>
  group_by(entreprise) |>
  filter(n == max(n)) |>
  summarise(sector_main = paste(sector_list, collapse = ", "), 
            .groups = "drop") |>
  left_join(base_emp_fmt |> select(entreprise, secteur), by = "entreprise") |>
  mutate(sector_main = if_else(is.na(sector_main) & !all(is.na(secteur)), 
                               paste(secteur, collapse = ", "), 
                               sector_main)) |>
  select(entreprise, sector_main) |>
  distinct() |> 
  mutate(sector_main = na_if(sector_main, "NA")) 


### Experience requise moyenne ----
avg_req_exp <- base_emp_fmt |> 
  group_by(entreprise) |> 
  summarise(avg_req_exp = mean(experience_requise, na.rm = TRUE)) |> 
  mutate(avg_req_exp = ifelse(is.nan(avg_req_exp), NA, avg_req_exp))



### Compétence la plus récurrente ----
top_skill_req <- base_emp_fmt |> 
  mutate(skills_split = strsplit(competences_requises, ",\\s*")) |> 
  unnest(skills_split) |> 
  mutate(skills_split = str_to_title(skills_split)) |> 
  group_by(entreprise, skills_split) |> 
  tally(sort = TRUE) |> 
  top_n(1, n) |> 
  summarise(top_skill_req = paste(unique(skills_split), collapse = ", "))



### Salaire annuel moyen ----
salaire_par_entreprise <- base_emp_fmt |> 
  mutate(salaire_annuel = sapply(salaire, function(x) tryCatch(convert_salary_to_annual(x), error = function(e) NA))) |> 
  group_by(entreprise) |> 
  summarize(salaire_moyen = mean(salaire_annuel, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(salaire_moyen = ifelse(is.nan(salaire_moyen), NA, salaire_moyen))


### Département principal ----
addr_dept_main <- base_emp_fmt |> 
  group_by(entreprise, departement) |>      
  summarise(n = n(), .groups = "drop") |> 
  arrange(entreprise, desc(n)) |>        
  group_by(entreprise) |> 
  slice_head(n = 1) |>  
  select(entreprise, addr_dept_main = departement)




### RESULTAT : base_emp ----
base_emp <- addr_dept_main |> 
  left_join(salaire_par_entreprise, by = "entreprise") |> 
  left_join(top_skill_req, by = "entreprise") |>
  left_join(avg_req_exp, by = "entreprise") |> 
  left_join(sector, by = "entreprise") |> 
  left_join(n_offres, by = "entreprise") |> 
  select(firm_name = entreprise, 
         n_offres,
         sector_main,  
         avg_req_exp,  
         top_skill_req,  
         avg_wage = salaire_moyen,  
         addr_dept_main) |> 
  mutate(avg_wage = as.numeric(avg_wage))|> 
  filter(!is.na(firm_name)) 





## QUESTION 3 ----
base_emp_inno <- merge(base_brevets, base_emp, by = "firm_name", all = TRUE)

base_emp_inno <- base_emp_inno |>
  mutate(addr_dept_main.x = as.character(addr_dept_main.x),
         addr_dept_main.y = as.character(addr_dept_main.y),
         addr_dept_main = coalesce(addr_dept_main.x, addr_dept_main.y),
         avg_wage = as.numeric(avg_wage),
         n_patents = ifelse(is.na(n_patents), 0, n_patents),
         n_offres = ifelse(is.na(n_offres), 0, n_offres)) |>
  select(-addr_dept_main.x, -addr_dept_main.y)
