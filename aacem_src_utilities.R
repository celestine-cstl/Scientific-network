
# ATALLAH Joanne
# AUBEL Xavier 
# COUSTILLAC Cêlestine  
# EZIN Philia 
# MADOR Fanis


## Packages ----
library(dplyr)
library(stringr)
library(tidyr)


## Fonctions ----

### Fonction d'harmonisation des noms d'entreprises

harmonize_names <- function(name) {
  name |>
    str_to_upper() |>
    str_replace_all("À|Á|Â|Ä|Ã|Å", "A") |>
    str_replace_all("Æ", "AE") |>
    str_replace_all("È|É|Ê|Ë|È", "E") |>
    str_replace_all("Ì|Í|Î|Ï|Ì", "I") |>
    str_replace_all("Ò|Ó|Ô|Ö|Õ|Ø", "O") |>
    str_replace_all("Œ", "OE") |>
    str_replace_all("Ù|Ú|Û|Ü|Ù", "U") |>
    str_replace_all("Ç", "C") |>
    str_replace_all("[^A-Z0-9\\s]", "") |>
    str_replace_all("\\b(SA|INC|LTD|SARL|SOCIETE|GROUP|SAS|SCS|S A|SASU|SLC|SNC|SE|SOCIETE ANONYME|SOCIETE CIVILE|PAR ACTIONS SIMPLIFIEE|LLC)\\b", "") |>
    
    str_replace_all("CENTRE HOSPITALIER REGIONAL ET UNIVERSITAIRE|CENTRE HOSPITALIER REGIONAL  UNIVERSITAIRE", "CHRU") |>
    str_replace_all("CENTRE HOSPITALIER UNIVERSITAIRE|CENTRE HOSPITALIER ET UNIVERSITAIRE", "CHU") |>
    str_replace_all("CENTRE HOSPITALIER REGIONAL", "CHR") |>
    
    str_replace_all("COMPAGNIE GERVAIS DANONE", "DANONE") |>
    str_replace_all("COMPAGNIE INDUSTRIELLE ET FINANCIERE DINGENIERIE INGENICO|INGENICO GROUP", "INGENICO") |>
    str_replace_all("COMPAGNIE PLASTIC OMNIUM|PLASTIC OMNIUM SYSTEMES URBAINS|OMNIUM NATIONAL INDUSTRIEL DES PEINTURES|OMNIUM DE REVALORISATION INDUSTRIELLE ODRI", "OMNIUM") |>
    str_replace_all("CTEC CONSTELLIUM TECHNOLOGY CENTER|CONSTELLIUM FRANCE", "CONSTELLIUM") |>
    str_replace_all("UNIVERSITE AMIENS PICARDIE JULES VERNE", "UNIVERSITE DE PICARDIE JULES VERNE") |>
    str_replace_all("INRIA INRIA|INRIA INSTITUT NATIONAL DE LA RECHERCHE EN INFORMATIQUE ET EN AUTOMATIQUE|INRIA INSTITUT NATIONAL DE RECHERCHE EN INFORMATIQUE ET EN AUTOMATIQUE|INSTITUT NATIONAL DE LA RECHERCHE EN INFORMATIQUE ET EN AUTOMATIQUE INRIA|INSTITUT NATIONAL DE RECHERCHE EN INFORMATIQUE ET EN AUTOMATIQUE|INSTITUT NATIONAL DE RECHERCHE EN INFORMATIQUE ET EN AUTOMATIQUE INRIA", "INRIA") |>
    str_replace_all("INSTITUT NATIONAL DE RECHERCHE POUR LAGRICULTURE LALIMENTATION ET LENVIRONNEMENT|INSTITUT NATIONAL DE RECHERCHE EN SCIENCES ET TECHNOLOGIES POUR LENVIRONNEMENT ET LAGRICULTURE IRSTEA", "IRSTEA") |>
    str_replace_all("SOCIETE D EXPLOITATION DE PRODUITS POUR LES INDUSTRIES CHIMIQUES SEPPIC|SOCIETE DEXPLOITATION DE PRODUITS POUR LES INDUSTRIES CHIMIQUES SEPPIC", "SEPPIC") |>
    str_replace_all("SNCF MOBILITES", "SNCF") |>
    str_replace_all("SNCF RESEAU", "SNCF") |>
    str_replace_all("SNCF VOYAGEURS", "SNCF") |>
    str_replace_all("SNCF CONNECT TECH", "SNCF") |>
    str_replace_all("SOCIETE NATIONALE DES CHEMINS DE FER FRANCAIS SNCF IDSJ", "SNCF") |>
    str_replace_all("SOCIETE NATIONALE SNCF", "SNCF") |>
    str_replace_all("SOCIETE NATIONALE DES CHEMINS DE FER FRANAIS SNCF", "SNCF") |>
    str_replace_all("ZODIAC ACTUATION SYSTEMS|ZODIAC AERO DUCT SYSTEMS|ZODIAC AEROSAFETY SYSTEMS|ZODIAC AEROSPACE|ZODIAC AUTOMOTIVE DIVISION|ZODIAC CABIN INTERIORS EUROPE|ZODIAC COATING|ZODIAC DATA SYSTEMS|ZODIAC HYDRAULICS|ZODIAC MILPRO INTERNATIONAL", "ZODIAC") |>
    str_replace_all("INSTITUT NATIONAL DE LA RECHERCHE AGRONOMIQUE", "INSTITUT NATIONAL DE LA RECHERCHE AGRONOMIQUE") |>
    str_replace_all("INRA INSTITUT NATIONAL DE LA RECHERCHE AGRONOMIQUE", "INSTITUT NATIONAL DE LA RECHERCHE AGRONOMIQUE") |>
    str_replace_all("INSTITUT NATIONAL DE LA RECHERCHE AGRONOMIQUE INRA", "INSTITUT NATIONAL DE LA RECHERCHE AGRONOMIQUE") |>
    str_replace_all("LINSTITUT NATIONAL DE LA RECHERCHE AGRONOMIQUE", "INSTITUT NATIONAL DE LA RECHERCHE AGRONOMIQUE") |>
    str_replace_all("INSTITUT NATIONAL DE RECHERCHE EN AGRONOMIE INRA", "INSTITUT NATIONAL DE LA RECHERCHE AGRONOMIQUE") |>    
    str_replace_all("AIRBUS SOCIETE", "AIRBUS") |> 
    str_replace_all("AIRBUS OPERATIONS", "AIRBUS") |> 
    str_replace_all("AIRBUS HELICOPTERS", "AIRBUS") |> 
    str_replace_all("AIRBUS DEFENCE AND SPACE", "AIRBUS") |> 
    str_replace_all("AIRBUS DS", "AIRBUS") |> 
    str_replace_all("AIRBUS GROUP", "AIRBUS") |> 
    str_replace_all("AIRBUS OPERATIONS SOCIETE", "AIRBUS") |> 
    str_replace_all("AIRBUS DS SLC", "AIRBUS") |> 
    str_replace_all("AIRBUS INTERIORS SERVICES", "AIRBUS") |> 
    str_replace_all("AIRBUS ONEWEB SATELLITES", "AIRBUS") |> 
    str_replace_all("AIRBUS CYBERSECURITY", "AIRBUS") |> 
    str_replace_all("AIRBUS DS GEO", "AIRBUS") |> 
    str_replace_all("EUROPEAN AERONAUTIC DEFENCE AND SPACE COMPANY EADS FRANCE AIRBUS GROUP", "AIRBUS") |> 
    str_replace_all("AIRBUS DS ELECTRONICS AND BORDER SECURITY", "AIRBUS") |> 
    str_replace_all("ALSTOM", "ALSTOM") |> 
    str_replace_all("ALSTOM HYDRO FRANCE", "ALSTOM") |> 
    str_replace_all("ALSTOM APTIS", "ALSTOM") |> 
    str_replace_all("ALSTOM TECHNOLOGIES", "ALSTOM") |> 
    str_replace_all("ALSTOM RENEWABLE TECHNOLOGIES", "ALSTOM") |> 
    str_replace_all("ALSTOM TRANSPORT", "ALSTOM") |> 
    str_replace_all("ALSTOM TRANSPORT TECHNOLOGIES", "ALSTOM") |> 
    str_replace_all("APHP ASSISTANCE PUBLIQUE HOPITAUX DE PARIS", "APHP") |> 
    str_replace_all("ASSISTANCE PUBLIQUEHOPITAUX DE PARIS APHP", "APHP") |> 
    str_replace_all("ASSISTANCE PUBLIQUEHOPITAUX DE PARIS", "APHP") |> 
    str_replace_all("ASSISTANCE PUBLIQUE HOPITAUX DE PARIS", "APHP") |> 
    str_replace_all("APHP ASSISTANCE PUBLIQUE HOPITAUX DE PARIS", "APHP") |> 
    str_replace_all("AREVA CREUSOT FORGE|AREVA H2GEN|AREVA MINES|AREVA NC|AREVA NP|AREVA RENOUVELABLES|AREVA STOCKAGE DENERGIE", "AREVA") |>
    str_replace_all("BUBENDORFF SOCIETE ANONYME", "BUBENDORFF") |>
    str_replace_all("KUHNHUARD|KUHN HUARD|KUHN SOCIETE ANONYME|KUHNAUDUREAU", "KUHN") |>
    str_replace_all("LEGRAND FRANCE|LEGRAND SNC", "LEGRAND") |>
    str_replace_all("LOUIS VUITTON MALLETIER", "LVMH") |>
    str_replace_all("ONERA ONERA", "ONERA") |>
    str_replace_all("PIERRE FABRE MEDICAMENT|PIERRE FABRE DERMOCOSMETIQUE", "PIERRE FABRE") |>
    str_replace_all("SOMFY PROTECT BY MYFOX|SOMFY ACTIVITES|LSOMFY ACTIVITES", "SOMFY") |>
    str_replace_all("A RAYMOND ET CIESCS", "A RAYMOND ET CIE") |>
    str_replace_all("INSERM INSERM", "INSERM") |>
    str_replace_all("INSERM INSTITUT NATIONAL DE LA SANTE ET DE LA RECHERCHE SCIENTIFIQUE", "INSERM") |>
    str_replace_all("COMPAGNIE GENERALE DES ETABLISSEMENTS MICHELIN|MICHELIN TRAVEL PARTNER|COMPAGNIE GENERALE DES ETABLISSEMENT MICHELINMICHELIN CIE", "MICHELIN") |>
    str_replace_all("VALEOTHS", "VALEO") |>
    str_replace_all("VALEO ETUDES ELECTRONIQUES", "VALEO") |>
    str_replace_all("VALEO TRANSMISSIONS MATERIAUX DE FRICTION", "VALEO") |>
    str_replace_all("VALEO BELGIQUE", "VALEO") |>
    str_replace_all("VALEO NORTH AMERICA", "VALEO") |>
    str_replace_all("VALEOL", "VALEO") |>
    str_replace_all("YTHALES INVEST|THALES DIS DESIGN SERVICES|THALES LAS FRANCE|THALES SESO|THALES ALENIA SPACE FRANCE", "THALES") |>
    str_replace_all("ETABLISSEMENTS GEORGES RENAULT|CREATIONS ANDRE RENAULT|RENAULT DEFENSE|RENAULT TECH", "RENAULT") |>
    str_replace_all("ORANGE BUSINESS|ORANGE VALLEE", "ORANGE") |>
    str_replace_all("PEUGEOT MOTOCYCLES", "PEUGEOT CITROEN AUTOMOBILES") |>
    str_replace_all("PEUGEOT SAVEURS", "PEUGEOT CITROEN AUTOMOBILES") |>
    str_replace_all("PEUGEOT SAVEURS SNC", "PEUGEOT CITROEN AUTOMOBILES") |>
    str_replace_all("CNES CNES", "CNES") |>
    str_replace_all("CNES C N E S", "CNES") |>
    str_replace_all("ALBEA LE TREPORT|ALBEA SERVICES", "ALBEA") |>
    str_replace_all("ALCATEL INTERNATIONAL|ALCATEL SHANGHAI BELL CO|ALCATEL LUCENT|ALCATEL SUBMARINE NETWORKS", "ALCATEL") |>
    str_replace_all("APTAR FRANCE|APTARGROUP|APTAR STELMI", "APTAR") |>
    str_replace_all("CENTRE NATIONAL DETUDES SPATIALES", "CNES") |>
    str_replace_all("CENTRE NATIONAL DETUDES SPATIALES CNES", "CNES") |>
    str_replace_all("CGG SERVICES|CGGVERITAS SERVICES", "CGG") |>
    str_replace_all("COMPAGNIE PLASTIC OMNIUM|COMPAGNIE PLASTIC OMNIUM SE", "COMPAGNIE PLASTIC OMNIUM") |>
    str_replace_all("CONSTELLIUM ISSOIRE|CONSTELLIUM NEUF BRISACH|CONSTELLIUM NEUFBRISACH", "CONSTELLIUM") |>
    str_replace_all("DASSAULT AVIATION|DASSAULT SYSTEMES", "DASSAULT") |>
    str_replace_all("ESSILOR INTERNATIONAL", "ESSILOR") |>
    str_replace_all("ESSILOR INTERNATIONAL COMPAGNIE GENERALE DOPTIQUE", "ESSILOR") |>
    str_replace_all("FAURECIA AUTOMOTIVE INDUSTRIE|FAURECIA BLOC AVANT|FAURECIA INTERIEUR INDUSTRIE|FAURECIA SIEGES DAUTOMOBILE|FAURECIA SYSTEMES DECHAPPEMENT", "FAURECIA") |>
    str_replace_all("IDEMIA FRANCE", "IDEMIA") |>
    str_replace_all("IDEMIA IDENTITY SECURITY FRANCE", "IDEMIA") |>
    str_replace_all("INSERM INSERM", "INSERM") |>
    str_replace_all("INSERM INSTITUT NATIONAL DE LA SANTE ET DE LA RECHERCHE MEDICALE", "INSERM") |>
    str_replace_all("INSTITUT DE RECHERCHE POUR LE DEVELOPPEMENT|INSTITUT DE RECHERCHE POUR LE DEVELOPPEMENT IRD", "IRD") |>
    str_replace_all("L V M H RECHERCHE|LVMH RECHERCHE", "LVMH") |>
    str_replace_all("OBERTHUR CASH PROTECTION|OBERTHUR FIDUCIAIRE|OBERTHUR TECHNOLOGIES", "OBERTHUR") |>
    str_replace_all("OFFICE NATIONAL DETUDES ET DE RECHERCHES AEROSPATIALES", "ONERA") |>
    str_replace_all("OFFICE NATIONAL DETUDES ET DE RECHERCHES AEROSPATIALES ONERA", "ONERA") |>
    str_replace_all("ONERA OFFICE NATIONAL DETUDES ET DE RECHERCHES AEROSPATIALES", "ONERA") |>
    str_replace_all("PARROT DRONES|PARROT", "PARROT") |>
    str_replace_all("PEUGEOT CITROEN AUTOMOBILES SOCIETE ANONYME|PEUGEOT CITROEN AUTOMOBILES", "PSAS") |>
    str_replace_all("PSA AUTOMOBILES|PSA PEUGEOT CITROEN AUTOMOBILES", "PSA") |>
    str_replace_all("RENAULT TRUCKS|RENAULT", "RENAULT") |>
    str_replace_all("SAGEM DEFENSE SECURITE", "SAGEMCOM") |>
    str_replace_all("SAGEMCOM BROADBAND", "SAGEMCOM") |>
    str_replace_all("SAGEMCOM DOCUMENTS", "SAGEMCOM") |>
    str_replace_all("SAGEMCOM ENERGY TELECOM", "SAGEMCOM") |>
    str_replace_all("SAINTGOBAIN ABRASIFS|SAINTGOBAIN ADFORS|SAINTGOBAIN CENTRE DE RECHERCHES ET DETUDES EUROPEEN|SAINTGOBAIN CRISTAUX ET DETECTEURS|SAINTGOBAIN GLASS FRANCE|SAINTGOBAIN ISOVER|SAINTGOBAIN PAM|SAINTGOBAIN PLACO|SAINTGOBAIN WEBER", "SAINTGOBAIN") |>
    str_replace_all("SANOFI BIOTECHNOLOGY|SANOFI PASTEUR|SANOFI", "SANOFI") |>
    str_replace_all("SCHNEIDER TOSHIBA INVERTER EUROPE|SCHNEIDER ELECTRIC INDUSTRIES", "SCHNEIDER") |>
    str_replace_all("SERVICE PETROLIERS SCHLUMBERGER|SERVICES PETROLIERS SCHLUMBERGER|SERVICES PETROLIERS SCHLUMBERGER SPS|SCHLUMBERGER SPS", "SCHLUMBERGER") |>
    str_replace_all("SIDEL ENGINEERING CONVEYING SOLUTIONS|SIDEL END OF LINE SOLUTIONS FRANCE|SIDEL PACKING SOLUTIONS|SIDEL PARTICIPATIONS", "SIDEL") |>
    str_replace_all("STMICROELECTRONICS ALPS|STMICROELECTRONICS CROLLES 2|STMICROELECTRONICS GRENOBLE 2|STMICROELECTRONICS ROUSSET|STMICROELECTRONICS TOURS", "STMICROELECTRONICS") |>
    str_replace_all("TECHNIPLAST|TECHNIP NPOWER|TECHNIP FRANCE", "TECHNIP") |>
    str_replace_all("THALES DIS FRANCE|THALES", "THALES") |>
    str_replace_all("THOMSON LICENSING DTV|THOMSON LICENSING", "THOMSON") |>
    str_replace_all("TOTAL MARKETING SERVICES|TOTAL RAFFINAGE CHIMIE|TOTAL RAFFINAGE FRANCE|TOTAL RAFFINAGE MARKETING|TOTAL SE|TOTAL", "TOTAL") |>
    str_replace_all("VALEO COMFORT AND DRIVING ASSISTANCE|VALEO EMBRAYAGES|VALEO EQUIPEMENTS ELECTRIQUES MOTEUR|VALEO MATERIAUX DE FRICTION|VALEO SECURITE HABITACLE|VALEO SIEMENS EAUTOMOTIVE FRANCE|VALEO SYSTEMES DE CONTROLE MOTEUR|VALEO SYSTEMES DESSUYAGE|VALEO SYSTEMES THERMIQUES|VALEO SYSTEMES THERMIQUESTHS|VALEO VISION", "VALEO") |>
    str_replace_all("ZODIAC AERO ELECTRIC|ZODIAC AEROTECHNICS|ZODIAC POOL CARE EUROPE|ZODIAC SEATS FRANCE", "ZODIAC") |>
    
    str_replace_all("CDISCOUNT STAGE", "CDISCOUNT") |>
    str_replace_all("SII ILE DE FRANCE|SII NORD|SII OUEST|SII TECHNOLOGIES GMBH HRSERVICE|SII ATLANTIQUE|SII EST", "SII") |>
    str_replace_all("KAINO CONSULTING", "KAINO") |>
    str_replace_all("INSITOO FREELANCES", "INSITOO") |>
    str_replace_all("HAVAS MEDIA|HAVAS GROUP", "HAVAS") |>
    str_replace_all("ONEPOINT GROUP", "ONEPOINT") |>
    str_replace_all("OCTOPUS IT|OCTOPUS ENERGY GROUP", "OCTOPUS ENERGY") |>
    str_replace_all("VINCI TERRASSEMENT|VINCI TECHNOLOGIES|VINCI PROJETS 8|VINCI GRANDS PROJETS|VINCI FRANCE|VINCI ENVIRONNEMENT|VINCI ENERGIES|VINCI AUTOROUTES|VINCI CONSTRUCTION", "VINCI") |>
    str_replace_all("VEEPEEFRANCE", "VEEPEE") |>
    str_replace_all("TALAN CONSULTING", "TALAN") |>
    str_replace_all("TEAMIS", "TEAM IS") |>
    str_replace_all("SOMFY GROUP", "SOMFY") |>
    str_replace_all("SOCIETE GENERALE SECURITIES SERVICES", "SOCIETE GENERALE") |>
    str_replace_all("ALTELIOS TECHNOLOGY GROUP", "ALTELIOS TECHNOLOGY") |>
    str_replace_all("AXA STENMAN FRANCE|AXA FUNDS MANAGEMENT|AXA PARTNERS|AXA EN FRANCE|AXA ASSURANCES", "AXA") |>
    str_replace_all("BANQUE POPULAIRE RIVES DE PARIS|BANQUE POPULAIRE MEDITERRANEE", "BANQUE POPULAIRE") |>
    str_replace_all("BOUYGUES IMMOBILIER|BOUYGUES CONSTRUCTION IT|BOUYGUES TELECOM|BOUYGUES TRAVAUX PUBLICS|BOUYGUES ENERGIES ET SERVICES|BOUYGUES CONSTRUCTION MATERIEL|BOUYGUES CONSTRUCTION", "BOUYGUES") |>
    str_replace_all("BPCE SOLUTIONS INFORMATIQUES", "BPCE") |>
    str_replace_all("CAISSE DEPARGNE AQUITAINE POITOU CHARENTES|CAISSE DEPARGNE GRAND EST EUROPE|CAISSE DEPARGNE RHONE ALPES", "CAISSE DEPARGNE") |>
    str_replace_all("CAISSE DES DEPOTS ET CONSIGNATIONS", "CAISSE DES DEPOTS") |>
    str_replace_all("CAPGEMINI INVENT", "CAPGEMINI") |>
    str_replace_all("CGI FINANCE", "CGI") |>
    
    str_replace_all("CLS COLLECTE LOCALISATION SATELLITES", "CLS") |>
    str_replace_all("CREDIT AGRICOLE ASSURANCES|CREDIT AGRICOLE CENTRE LOIRE|CREDIT AGRICOLE CHAMPAGNE BOURGOGNE|CREDIT AGRICOLE CIB|CREDIT AGRICOLE CONSUMER FINANCE|CREDIT AGRICOLE DES REGIONS DU CENTRE|CREDIT AGRICOLE DU NORD EST|CREDIT AGRICOLE GROUP INFRASTRUCTURE PLATEFORM", "CREDIT AGRICOLE") |>
    str_replace_all("DAVIDSON CONSULTING", "DAVIDSON") |>
    str_replace_all("EUROINFORMATION DEVELOPPEMENTS", "EURO INFORMATION") |>
    str_replace_all("EVOTEO CONSULTING", "EVOTEO") |>
    str_replace_all("ENGIE SOLUTIONS|ENGIE GREEN FRANCE|ENGIE HOME SERVICES", "ENGIE") |>
    str_replace_all("FREE MOBILE|FREE RESEAU|FREE INFRASTRUCTURE|FREEBOX", "FREE") |>
    str_replace_all("GENERALI VIE|GENERALI FRANCE", "GENERALI") |>
    str_replace_all("GRANT THORNTON FRANCE", "GRANT THORNTON") |>
    str_replace_all("GROUPAMA ASSET MANAGEMENT|GROUPAMA ASSURANCES MUTUELLES|GROUPAMA CENTRE MANCHE|GROUPAMA GAN VIE|GROUPAMA RHONE ALPES AUVERGNE|GROUPAMA SUPPORTS ET SERVICES", "GROUPAMA") |>
    str_replace_all("ICM INSTITUT DU CERVEAU ET DA LA MOELLE EPINIERE", "INSTITUT DU CERVEAU ET DE LA MOELLE EPINIERE") |>
    str_replace_all("INSTITUT DU CERVEAU", "INSTITUT DU CERVEAU ET DE LA MOELLE EPINIERE") |>
    str_replace_all("ICM INSTITUT DU CERVEAU ET DE LA MOELLE EPINIERE", "INSTITUT DU CERVEAU ET DE LA MOELLE EPINIERE") |>
    str_replace_all("INSTITUT DU CERVEAU ET DE LA MOELLE EPINIERE ICM", "INSTITUT DU CERVEAU ET DE LA MOELLE EPINIERE") |>
    str_replace_all("INSTITUT DU CERVEAU ET DE LA MOELLE EPINIEREICM", "INSTITUT DU CERVEAU ET DE LA MOELLE EPINIERE") |>
    str_replace_all("LINSTITUT DU CERVEAU ET DE LA MOELLE EPINIERE", "INSTITUT DU CERVEAU ET DE LA MOELLE EPINIERE") |>
    str_replace_all("ICM INSTITUT DU CERVEAU", "INSTITUT DU CERVEAU ET DE LA MOELLE EPINIERE") |>
    str_replace_all("LE GROUPE LA POSTE|LA POSTE GROUPE", "LA POSTE") |>
    str_replace_all("BPCE SOLUTIONS INFORMATIQUES|GROUPE BPCE", "BPCE") |>
    str_replace_all("GROUPE DEMETER|DEMETER CENTRE", "DEMETER") |>
    str_replace_all("GROUPE KERING", "KERING") |>
    str_replace_all("LIMAGRAIN EUROPE|GROUPE LIMAGRAIN", "LIMAGRAIN") |>
    str_replace_all("GROUPE OGF", "OGF") |>
    str_replace_all("ORTEC ENGINEERING|ORTEC EXPANSION|GROUPE ORTEC", "ORTEC") |>
    str_replace_all("COMPAGNIE FINANCIERE ET DE PARTICIPATIONS ROULLIER|GROUPE ROULLIER", "ROULLIER") |>
    str_replace_all("GROUPE SEB", "SEB") |>
    str_replace_all("RENAULT DEFENSE|RENAULT GROUP", "RENAULT") |>
    str_replace_all("SCHNEIDER PROTECTION CONTROLE|SCHNEIDER ELECTRIC ENERGY FRANCE|SCHNEIDER ELECTRIC|SCHNEIDER ELECTRIC PROTECTION CONTROLE", "SCHNEIDER") |>
    str_replace_all("PSA RETAIL FRANCE|GROUPE PSA", "PSA") |>
    str_replace_all("TOTAL ENERGIES NOUVELLES ACTIVITES USA|TOTAL DIRECT ENERGIE|TOTAL ENERGIES NOUVELLES ACTIVITES USA|TOTAL IMMERSION|TOTAL PETROCHEMICALS FRANCE|TOTAL RAFFINAGE MARKETING|TOTAL SOLAR|TOTAL SOLAR INTERNATIONAL","TOTAL ENERGIE") |> 
    
    str_replace_all("\\(.*?\\)", "") |>
    str_replace_all("[\\.,\\-\\'\\&]", " ") |>
    str_squish() |>
    str_trim()
}


### Fonction pour harmoniser les compétences
harmonize_skills_column <- function(df, col_name) {
  df |> 
    mutate(
      !!col_name := str_replace_all(.data[[col_name]], 
                                    c("Base De Donnée" = "Bases de données",
                                      "Bases de donnée" = "Bases de données",
                                      "Database" = "Bases de données",
                                      "Deep Learning" = "Apprentissage profond",
                                      "French" = "Français",
                                      "English" = "Anglais",
                                      "Creativity" = "Créativité",
                                      "Critical Thinking" = "Esprit Critique",
                                      "Adaptability" = "Adaptabilité",
                                      "Attention to Detail" = "Rigueur",
                                      "Minutieux"  = "Rigueur",
                                      "Attention aux Détails" = "Rigueur",
                                      "Database" = "Base de donnée",
                                      "Programming" = "Programmation",
                                      "Collaboration" = "Equipe et Collaboration",
                                      "équipe"= "Equipe et Collaboration",
                                      "Team Player"  = "Equipe et Collaboration",
                                      "Team Collaboration" = "Equipe et Collaboration",
                                      "Team Player" = "Collaboration",
                                      "Optimization" = "Optimisation",
                                      "Data Visualization" = "Visualisation de donnée",
                                      "Dataviz" = "Visualisation de donnée",
                                      "Deep Learning" = "Apprentissage profond",
                                      "Artificial intelligence" = "Intelligence artificielle",
                                      "Autonomous" = "Autonome",
                                      "Autonôme" = "Autonome",
                                      "data collection" = "collecte de données",
                                      "Communication Skills" = "Communication",
                                      "Presentation Skills" = "Compétences en Présentation",
                                      "Curious" = "Curiosité",
                                      "Data Cleaning" = "Nettoyage de donnée",
                                      "Project Management" = "Gestion de Projets",
                                      "Time Management" = "Gestion du Temps",
                                      "NLP" = "Natural Language Processing",
                                      "Forecasting" = "Prévision",
                                      "Proactive" = "Proactif",
                                      "Reporting" = "Rapport",
                                      "Statistics" = "Statistique")))
}



### Fonction pour IPC
extract_ipc_definitions <- function(file_path) {
  lines <- readLines(file_path)
  data.frame(IPC_code = str_extract(lines, "^[A-Z0-9]+"),  
             Definition = str_trim(str_replace(lines, "^[A-Z0-9]+", "")))
}



### Fonction salaire
convert_salary_to_annual <- function(salaire) {
  if (is.na(salaire) || salaire == "" || grepl("non spécifié", salaire, ignore.case = TRUE)|| grepl("par jour", salaire, ignore.case = TRUE)) {
    return(NA)
  }
  
  salaire <- as.character(salaire)
  unit <- 1
  if (grepl("K", salaire, ignore.case = TRUE)) {
    unit <- 1000
  } else if (grepl("semaine", salaire, ignore.case = TRUE)) {
    unit <- 52
  } else if (grepl("mois", salaire, ignore.case = TRUE)) {
    unit <- 12
  } else if (grepl("par an", salaire, ignore.case = TRUE)) {
    unit <- 1
  }
  
  if (grepl("à", salaire)) {
    salary_range <- strsplit(salaire, "à")[[1]]
  } else if (grepl("-", salaire)) {
    salary_range <- strsplit(salaire, "-")[[1]] 
  } else {
    salaire <- gsub("[^0-9.,]", "", salaire)
    salaire <- as.numeric(gsub(",", ".", salaire))  
    return(salaire * unit)  
  }
  salary_min <- as.numeric(gsub("[^0-9.,]", "", salary_range[1]))
  salary_max <- as.numeric(gsub("[^0-9.,]", "", salary_range[2])) 
  salary_min <- as.numeric(gsub(",", ".", salary_min))
  salary_max <- as.numeric(gsub(",", ".", salary_max))
  average_salary <- mean(c(salary_min, salary_max), na.rm = TRUE)
  return(average_salary * unit)
}

