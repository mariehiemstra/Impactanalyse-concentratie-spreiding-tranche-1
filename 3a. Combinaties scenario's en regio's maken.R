#-------------------------------------------------------------------------------------------------------------------------------------------------------------
# Project: Impactanalyse concentratie en spreiding
# Auteur:  SiRM
# Datum:   januari 2025
# Doel:    Combinaties scenario's en regio's maken (3a)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------

# Clear all variables in R's memory
rm(list=ls())
options(scipen=100, digits=5)

# Libraries
library(readxl)
library(data.table)
library(writexl)
library(tidyverse)
library(sf)
library(plyr)

#----------------------------------- Mappen -----------------------------------#

gebruikersnaam      = #Vul hier naam in
  
sogiz               = #path naar map
shp_bron            = #path naar map
analyse             = #path naar map
input               = #path naar map
input_model         = #path naar map
output              = #path naar map
bron                = #path naar map
admin               = #path naar map
bron_NZa            = #path naar map

#-------------------------------- Tabellen ------------------------------------#

# Koppeltabellen
koppel_regio         = read_excel(paste0(analyse, "# Databestand scenario's C&S"), sheet = "Type regio's")
namen_interventies   = read_excel(paste0(analyse, '# Koppeltabellen'), sheet = "Namen interventies")
namen_kort           = read_excel(paste0(input_model, '# Koppeltabellen'), sheet = "Instellingsnamen kort")
koppel_hoofdscenario = read_excel(paste0(analyse, "# Databestand koppeltabel hoofdscenarios.xlsx"))
proxy_rectum         = read_excel(paste0(analyse, "# Databestand overige gegevens"), sheet = "Proxy's rectum")
namen_interventies$Interventie[namen_interventies$Aandoening == 'Rectum'] = unique(proxy_rectum$Interventie[grepl('rectum', proxy_rectum$Interventie)])
rm(proxy_rectum) # Was alleen nodig omdat rectum anders niet goed koppelt

# Scenario's en nieuwe aantallen per ziekenhuis
scenarios = read_excel(paste0(analyse, '# Databestand scenarios en aantallen.xlsx'))
scenarios = subset(scenarios, select = -c(Herverdeling, Type_verdeling))
setnames(scenarios, 'Hoofdregio', 'Regio impactanalyse')
# Per subregio koppelen of het een hoofdregio is, subregio of vaatnetwerk
scenarios = merge(scenarios, koppel_regio, by = 'Subregio', all.x = T)

# Hier eventueel specifieke scenario's uit regio's eruit halen. 

# Hernoemen
setnames(scenarios, c('aantal_2022', 'aantal_stap2', 'aantal_extra'), 
         c('Aantal_huidig', 'Aantal_nieuw', 'Aantal_verschil'))

#------------------------------------------------------------------------------#
#------------------------------ Combinaties maken -----------------------------#
#------------------------------------------------------------------------------#

# Omdat we een integrale impactanalyse maken en kijken naar de totale impact op een ziekenhuis
# moeten we scenario's van subregio's onco / hoofdregio's onco / vaatregio's combineren
# Aanpak is door vanuit elk ziekenhuis te kijken in welke regio's / netwerken die zit
# En dan alle mogelijke combinaties van de betreffende scenario's in die regio's te maken
# We maken dus een lijst per ziekenhuis welke combinaties van scenario's relevant zijn

# Leeg dataframe maken om alle combinaties van scenario's in te zetten
combi_scenarios        = data.frame(matrix(data = NA, nrow = 0, ncol = 7))
names(combi_scenarios) = c('Instelling','Hoofdregio', 'Subregio', 'Vaatnetwerk', 
                           'Hoofdscenario', 'Subscenario', 'Vaatscenario')
combi_scenarios        = combi_scenarios %>% mutate_at(c('Instelling','Hoofdregio', 'Subregio', 
                                                         'Vaatnetwerk', 'Hoofdscenario', 'Subscenario', 
                                                         'Vaatscenario'), 'as.character')

instellingen = unique(scenarios$Instelling)

# Per instelling checken we welke scenario's relevant zijn
# Door te kijken welke scenario's voor de hoofdregio/subregio/vaatnetwerk er voor die instelling zijn

for(i in 1:length(instellingen)){
  scenarios0    = scenarios[scenarios$Instelling == instellingen[i],]
  scenarios0    = unique(scenarios0[,c('Subregio', 'Scenario', 'Type regio')])
  Hoofdscenario = scenarios0[!is.na(scenarios0$`Type regio`) & scenarios0$`Type regio` == 'Hoofdregio',]$Scenario
  Subscenario   = scenarios0[!is.na(scenarios0$`Type regio`) &scenarios0$`Type regio` == 'Subregio',]$Scenario
  Vaatscenario  = scenarios0[!is.na(scenarios0$`Type regio`) &scenarios0$`Type regio` == 'Vaatnetwerk',]$Scenario
  # Sommige ziekenhuizen zitten in meerdere hoofdregio's
  # Dan aparte combinaties maken waar een hoofdregio in voorkomt
  check         = unique(scenarios0$Subregio[scenarios0$`Type regio` == 'Hoofdregio'])
  # Combinatie hoofd/sub/vaat:
  if(length(check) > 0 & length(Subscenario) > 0 & length(Vaatscenario) > 0){
    Hoofdscenario = scenarios0[scenarios0$`Type regio` == 'Hoofdregio' & scenarios0$Subregio == check[1],]$Scenario
    combi_scenarios0 = crossing(Hoofdscenario, Subscenario, Vaatscenario)
    combi_scenarios0$Hoofdregio = check[1]
    combi_scenarios0$Subregio = unique(scenarios0$Subregio[scenarios0$`Type regio` == 'Subregio'])
    combi_scenarios0$Vaatnetwerk = unique(scenarios0$Subregio[scenarios0$`Type regio` == 'Vaatnetwerk'])
    combi_scenarios0$Instelling = instellingen[i]
    combi_scenarios = bind_rows(combi_scenarios, combi_scenarios0)
  } else {}
  # Combinatie hoofd/sub/vaat - ziekenhuizen met 2e hoofdregio:
  if(length(check) > 0 & length(Subscenario) > 0 & length(Vaatscenario) > 0 & length(check) == 2){
    Hoofdscenario = scenarios0[scenarios0$`Type regio` == 'Hoofdregio' & scenarios0$Subregio == check[2],]$Scenario
    combi_scenarios0 = crossing(Hoofdscenario, Subscenario, Vaatscenario)
    combi_scenarios0$Hoofdregio = check[2]
    combi_scenarios0$Subregio = unique(scenarios0$Subregio[scenarios0$`Type regio` == 'Subregio'])
    combi_scenarios0$Vaatnetwerk = unique(scenarios0$Subregio[scenarios0$`Type regio` == 'Vaatnetwerk'])
    combi_scenarios0$Instelling = instellingen[i]
    combi_scenarios = bind_rows(combi_scenarios, combi_scenarios0)
  } else {}
  # Combinatie hoofd/sub/geen vaat
  if(length(check) > 0 & length(Subscenario) > 0){
    Hoofdscenario = scenarios0[scenarios0$`Type regio` == 'Hoofdregio' & scenarios0$Subregio == check[1],]$Scenario
    combi_scenarios0 = crossing(Hoofdscenario, Subscenario)
    combi_scenarios0$Hoofdregio = check[1]
    combi_scenarios0$Subregio = unique(scenarios0$Subregio[scenarios0$`Type regio` == 'Subregio'])
    combi_scenarios0$Instelling = instellingen[i]
    combi_scenarios = bind_rows(combi_scenarios, combi_scenarios0)
  } else{}
  # Combinatie hoofd/sub/geen vaat - ziekenhuizen met 2e hoofdregio
  if(length(check) > 0 & length(Subscenario) > 0 & length(check) == 2){
    Hoofdscenario = scenarios0[scenarios0$`Type regio` == 'Hoofdregio' & scenarios0$Subregio == check[2],]$Scenario
    combi_scenarios0 = crossing(Hoofdscenario, Subscenario)
    combi_scenarios0$Hoofdregio = check[2]
    combi_scenarios0$Subregio = unique(scenarios0$Subregio[scenarios0$`Type regio` == 'Subregio'])
    combi_scenarios0$Instelling = instellingen[i]
    combi_scenarios = bind_rows(combi_scenarios, combi_scenarios0)
  } else{}
  # Combinatie hoofd/geen sub/geen vaat
  if(length(check) > 0){
    Hoofdscenario = scenarios0[scenarios0$`Type regio` == 'Hoofdregio' & scenarios0$Subregio == check[1],]$Scenario
    combi_scenarios0 = crossing(Hoofdscenario)
    combi_scenarios0$Hoofdregio = check[1]
    combi_scenarios0$Instelling = instellingen[i]
    combi_scenarios = bind_rows(combi_scenarios, combi_scenarios0)
  } else{}
  # Combinatie hoofd/geen sub/geen vaat - ziekenhuizen met 2e hoofdregio
  if(length(check) > 0 & length(check) == 2){
    Hoofdscenario = scenarios0[scenarios0$`Type regio` == 'Hoofdregio' & scenarios0$Subregio == check[2],]$Scenario
    combi_scenarios0 = crossing(Hoofdscenario)
    combi_scenarios0$Hoofdregio = check[2]
    combi_scenarios0$Instelling = instellingen[i]
    combi_scenarios = bind_rows(combi_scenarios, combi_scenarios0)
  } else{}
  # Combinatie geen hoofd/sub/geen vaat
  if(length(Subscenario) > 0){
                combi_scenarios0 = crossing(Subscenario)
                combi_scenarios0$Subregio = unique(scenarios0$Subregio[scenarios0$`Type regio` == 'Subregio'])
                combi_scenarios0$Instelling = instellingen[i]
                combi_scenarios = bind_rows(combi_scenarios, combi_scenarios0)
  } else{}
  # Combinatie geen hoofd/geen sub/vaat
  if(length(Vaatscenario) > 0){
    combi_scenarios0 = crossing(Vaatscenario)
    combi_scenarios0$Vaatnetwerk = unique(scenarios0$Subregio[!is.na(scenarios0$`Type regio`) & scenarios0$`Type regio` == 'Vaatnetwerk'])
    combi_scenarios0$Instelling = instellingen[i]
    combi_scenarios = bind_rows(combi_scenarios, combi_scenarios0)
  } else{}
  # Combinatie hoofd/geen sub/vaat
  if(length(check) > 0 & length(Vaatscenario) > 0){
    Hoofdscenario = scenarios0[scenarios0$`Type regio` == 'Hoofdregio' & scenarios0$Subregio == check[1],]$Scenario
    combi_scenarios0 = crossing(Hoofdscenario, Vaatscenario)
    combi_scenarios0$Hoofdregio = check[1]
    combi_scenarios0$Vaatnetwerk = unique(scenarios0$Subregio[!is.na(scenarios0$`Type regio`) &scenarios0$`Type regio` == 'Vaatnetwerk'])
    combi_scenarios0$Instelling = instellingen[i]
    combi_scenarios = bind_rows(combi_scenarios, combi_scenarios0)
  } else {}
  # Combinatie hoofd/geen sub/vaat - ziekenhuizen met 2e hoofdregio
  if(length(check) > 0 & length(Vaatscenario) > 0 & length(check) == 2){
    Hoofdscenario = scenarios0[scenarios0$`Type regio` == 'Hoofdregio' & scenarios0$Subregio == check[2],]$Scenario
    combi_scenarios0 = crossing(Hoofdscenario, Vaatscenario)
    combi_scenarios0$Hoofdregio = check[2]
    combi_scenarios0$Vaatnetwerk = unique(scenarios0$Subregio[!is.na(scenarios0$`Type regio`) &scenarios0$`Type regio` == 'Vaatnetwerk'])
    combi_scenarios0$Instelling = instellingen[i]
    combi_scenarios = bind_rows(combi_scenarios, combi_scenarios0)
  } else {}
}

rm(combi_scenarios0, scenarios0, Hoofdscenario, Subscenario, Vaatscenario)

# Aantallen / herverdeling koppelen:
combi_scenarios1 = merge(combi_scenarios, scenarios, by.x = c('Hoofdregio', 'Hoofdscenario', 'Instelling'), by.y = c('Subregio', 'Scenario', 'Instelling'), allow.cartesian = T)
combi_scenarios2 = merge(combi_scenarios, scenarios, by.x = c('Subregio', 'Subscenario', "Instelling"), by.y = c('Subregio', 'Scenario', "Instelling"), allow.cartesian = T)
combi_scenarios3 = merge(combi_scenarios, scenarios, by.x = c('Vaatnetwerk', 'Vaatscenario', "Instelling"), by.y = c('Subregio', 'Scenario', "Instelling"), allow.cartesian = T)
combi_scenarios = bind_rows(combi_scenarios1, combi_scenarios2, combi_scenarios3)
rm(combi_scenarios1, combi_scenarios2, combi_scenarios3)

# Kolom toevoegen die combinatie is van de 3 type scenario's:
combi_scenarios$Combinatie_hoofd_sub_vaat_scenario = apply(combi_scenarios[,c('Hoofdscenario', 'Subscenario', 'Vaatscenario')], 1, 
                                function(x) paste(x[!is.na(x)], collapse = " | "))
# Kolom toevoegen die combinatie is van de 3 regio's:
combi_scenarios$Combinatie_regios = apply(combi_scenarios[,c('Hoofdregio', 'Subregio', 'Vaatnetwerk')], 1, 
                                                           function(x) paste(x[!is.na(x)], collapse = " | "))

# NA vervangen
combi_scenarios$Hoofdregio[is.na(combi_scenarios$Hoofdregio)] = 'Niet selecteren/niet van toepassing'
combi_scenarios$Subregio[is.na(combi_scenarios$Subregio)] = 'Niet selecteren/niet van toepassing'
combi_scenarios$Vaatnetwerk[is.na(combi_scenarios$Vaatnetwerk)] = 'Niet selecteren/niet van toepassing'
combi_scenarios$Hoofdscenario[is.na(combi_scenarios$Hoofdscenario)] = 'Niet selecteren/niet van toepassing'
combi_scenarios$Subscenario[is.na(combi_scenarios$Subscenario)] = 'Niet selecteren/niet van toepassing'
combi_scenarios$Vaatscenario[is.na(combi_scenarios$Vaatscenario)] = 'Niet selecteren/niet van toepassing'

combi_scenarios = subset(combi_scenarios, select = -c(Soort, `Type regio`))

# Voor regio's met meerdere subregio's de combinaties maken:
# CCNNO
CCNNO = combi_scenarios[combi_scenarios$Combinatie_regios %in% c('CCN NO | Oncologisch Netwerk Friesland',
                                                                 'CCN NO | Oncologienetwerk Groningen-Drenthe',
                                                                 'CCN NO | Oncologienetwerk Veluwe-IJssel',
                                                                 'CCN NO | ONZ netwerk'),]
CCNNO$Subregio = "Alle subregio's"
CCNNO$Combinatie_regios = "CCN NO | Alle subregio's"
# Hierna voegen we toelichting hoofdscenario toe, dus deze alvast toevoegen aan koppeltabel
CCNNO2 = koppel_hoofdscenario[koppel_hoofdscenario$Subregio %in% c('Oncologisch Netwerk Friesland',
                                                                   'Oncologienetwerk Groningen-Drenthe',
                                                                   'Oncologienetwerk Veluwe-IJssel',
                                                                   'ONZ netwerk'),]
CCNNO2$Subregio = "Alle subregio's"
koppel_hoofdscenario = bind_rows(koppel_hoofdscenario, CCNNO2)
combi_scenarios = bind_rows(combi_scenarios, CCNNO)
# Nu ook met vaat
CCNNO = combi_scenarios[combi_scenarios$Combinatie_regios %in% c('CCN NO | Oncologisch Netwerk Friesland | Vaatnetwerk Noord-Nederland',
                                                                 'CCN NO | Oncologienetwerk Groningen-Drenthe | Vaatnetwerk Noord-Nederland',
                                                                 'CCN NO | Oncologienetwerk Veluwe-IJssel',
                                                                 'CCN NO | ONZ netwerk'),]
CCNNO$Subregio = "Alle subregio's"
CCNNO$Combinatie_regios = "CCN NO | Alle subregio's | Vaatnetwerk Noord-Nederland"
combi_scenarios = bind_rows(combi_scenarios, CCNNO)
# CCN ZW
CCNZW = combi_scenarios[combi_scenarios$Combinatie_regios %in% c('CCN Zuid-West | EMBRAZE',
                                                                 'CCN Zuid-West | Concord'),]
CCNZW$Subregio = "Concord en EMBRAZE"
# Hierna voegen we toelichting hoofdscenario toe, dus deze alvast toevoegen aan koppeltabel
CCNZW2 = koppel_hoofdscenario[koppel_hoofdscenario$Subregio %in% c('EMBRAZE', 'Concord'),]
CCNZW2$Subregio = "Concord en EMBRAZE"
koppel_hoofdscenario = bind_rows(koppel_hoofdscenario, CCNZW2)
CCNZW$Combinatie_regios = "CCN Zuid-West | Concord en EMBRAZE"
combi_scenarios = bind_rows(combi_scenarios, CCNZW)
# Nu ook met vaat
CCNZW = combi_scenarios[combi_scenarios$Combinatie_regios %in% c('CCN Zuid-West | EMBRAZE | Vaatnetwerk Groot-Rijnmond en Zeeland',
                                                                 'CCN Zuid-West | Concord | Vaatnetwerk Groot-Rijnmond en Zeeland'),]
CCNZW$Subregio = "Concord en EMBRAZE"
CCNZW$Combinatie_regios = "CCN Zuid-West | Concord en EMBRAZE | Vaatnetwerk Groot-Rijnmond en Zeeland"
combi_scenarios = bind_rows(combi_scenarios, CCNZW)
# OncoNoVo+ en twee vaatnetwerken:
# Noord-Holland Flevoland
NHF = combi_scenarios[combi_scenarios$Combinatie_regios %in% c('OncoNoVo+ | Vaatnetwerk Amsterdam',
                                                               'OncoNoVo+ | Vaatnetwerk A9'),]
NHF$Vaatnetwerk = "Alle vaatnetwerken"
# Hierna voegen we toelichting hoofdscenario toe, dus deze alvast toevoegen aan koppeltabel
NHF2 = koppel_hoofdscenario[koppel_hoofdscenario$Subregio %in% c('Vaatnetwerk Amsterdam', 'Vaatnetwerk A9'),]
NHF2$Subregio = "Alle vaatnetwerken"
koppel_hoofdscenario = bind_rows(koppel_hoofdscenario, NHF2)
NHF$Combinatie_regios = "OncoNoVo+ | Alle vaatnetwerken"
combi_scenarios = bind_rows(combi_scenarios, NHF)
# Eventueel voor meer combinaties toevoegen
rm(CCNNO, CCNNO2, CCNZW, CCNZW2, NHF, NHF2)

# Combinaties eruit halen die niet relevant zijn / die we niet gaan bekijken:
unique(combi_scenarios$Combinatie_regios)
combi_scenarios = combi_scenarios[!combi_scenarios$Combinatie_regios %in% c("CCN NO",
                                                                            "CCN NO | Vaatnetwerk Noord-Nederland",
                                                                            "CCN Zuid-West",
                                                                            "CCN Zuid-West | EMBRAZE | VascuZON",
                                                                            "CCN Zuid-West | VascuZON",
                                                                            "Onco Oost | EMBRAZE",
                                                                            "Onco Oost | EMBRAZE | VascuZON",
                                                                            "OncoMid | Vaatnetwerk Groot-Rijnmond en Zeeland",
                                                                            "OncoWest | Vaatchirurgie West-Nederland" ,
                                                                            "Vaatchirurgie West-Nederland",
                                                                            "OncoNoVo+ | Vaatnetwerk Midden-Nederland"),]

# Extra combinaties toevoegen voor ziekenhuizen die niet in het vaatnetwerk zitten
vaat_en_onco <- combi_scenarios[combi_scenarios$Hoofdregio != "Niet selecteren/niet van toepassing" & combi_scenarios$Vaatnetwerk != "Niet selecteren/niet van toepassing",]
vaat_en_onco <- vaat_en_onco[(vaat_en_onco$Hoofdregio != "CCN NO" & vaat_en_onco$Hoofdregio != "CCN Zuid-West") | (vaat_en_onco$Subregio == "Alle subregio's" | vaat_en_onco$Subregio == "Concord en EMBRAZE"),]
missende_ziekenhuizen <- setdiff(distinct(combi_scenarios[combi_scenarios!= "OncoWest",c("Instelling", "Regio impactanalyse")]), 
                                 distinct(vaat_en_onco[,c("Instelling", "Regio impactanalyse")]))

# Voor ZuidOost VascuZON toevoegen bij zkh vaatnetwerk Limburg en Anna bij OncoZON
toevoegen <- combi_scenarios[combi_scenarios$Instelling %in% c("Zuyderland", "VieCuri Venlo", "Maastricht UMC+", "Laurentius Ziekenhuis"),]
toevoegen$Vaatnetwerk <- "VascuZON"
toevoegen$Combinatie_regios = paste0(toevoegen$Combinatie_regios, " | VascuZON")
scenarios_erbij <- unique(combi_scenarios[combi_scenarios$Vaatnetwerk == "VascuZON",]$Vaatscenario)

for (scenario in scenarios_erbij){
  toevoegen_scenario <- toevoegen
  toevoegen_scenario$Vaatscenario <- scenario
  toevoegen_scenario$Combinatie_hoofd_sub_vaat_scenario <- paste0(toevoegen_scenario$Combinatie_hoofd_sub_vaat_scenario, " | ",scenario)
  combi_scenarios <- rbind(combi_scenarios, toevoegen_scenario)
}

toevoegen <- combi_scenarios[combi_scenarios$Instelling %in% c("Anna Ziekenhuis"),]
toevoegen$Hoofdregio <- "OncoZON"
toevoegen$Combinatie_regios <- paste0("OncoZON | ", toevoegen$Combinatie_regios)
scenarios_erbij <- unique(combi_scenarios[combi_scenarios$Hoofdregio == "OncoZON",]$Hoofdscenario)

for (scenario in scenarios_erbij){
  toevoegen_scenario <- toevoegen
  toevoegen_scenario$Hoofdscenario = scenario
  toevoegen_scenario$Combinatie_hoofd_sub_vaat_scenario <- paste0(scenario, " | ",toevoegen_scenario$Combinatie_hoofd_sub_vaat_scenario)
  combi_scenarios <- rbind(combi_scenarios, toevoegen_scenario)
}

combi_scenarios$`Regio impactanalyse` <- ifelse(combi_scenarios$Hoofdregio == "Onco Oost", "Oost-Nederland", combi_scenarios$`Regio impactanalyse`)

# Tabel maken voor PowerBI met toelichting bij scenario's
tabel = unique(combi_scenarios[,c('Hoofdregio', 'Hoofdscenario', 'Subregio', 'Subscenario', 'Vaatnetwerk', 'Vaatscenario')])
tabel = melt(setDT(tabel), id.vars = c('Hoofdregio', 'Subregio', 'Vaatnetwerk'), variable.name = 'Soort', value.name = 'Scenario')
tabel = tabel[!tabel$Scenario == 'Niet selecteren/niet van toepassing',]
tabel$Soort = NULL
tabel = unique(tabel)
koppel_hoofdscenario$Hoofdregio = NULL

# Scenario's tabel compleet maken
scenarios1 = scenarios[scenarios$Subregio %in% c('Oncologisch Netwerk Friesland',
                                                 'Oncologienetwerk Groningen-Drenthe',
                                                 'Oncologienetwerk Veluwe-IJssel',
                                                 'ONZ netwerk'),]
scenarios1$Subregio = "Alle subregio's"
scenarios2 = scenarios[scenarios$Subregio %in% c('EMBRAZE', 'Concord'),]
scenarios2$Subregio = "Concord en EMBRAZE"
scenarios3 = scenarios[scenarios$Subregio %in% c('Vaatnetwerk Amsterdam', 'Vaatnetwerk A9'),]
scenarios3$Subregio = "Alle vaatnetwerken"
scenarios = bind_rows(scenarios, scenarios1, scenarios2, scenarios3)

koppel_aantallen = unique(scenarios[,c('Subregio', 'Scenario', 'Interventie', 'Instelling', 
                                       'Aantal_huidig', 'Aantal_nieuw', 'Aantal_verschil')])
koppel_hoofdscenario = merge(koppel_hoofdscenario, koppel_aantallen, by = c('Subregio', "Scenario"), all.x = T, allow.cartesian = T)
tabel1 = copy(koppel_hoofdscenario)
setnames(tabel1, c('Subregio', 'Hoofdscenario', 'Aantal_huidig', 'Aantal_nieuw', 'Aantal_verschil', 'Instelling', 'Interventie'), c('Hoofdregio', 'Hoofdscenario1', 'Aantal_huidig1', 'Aantal_nieuw1', 'Aantal_verschil1', 'Instelling1', 'Interventie1'))
tabel = merge(tabel, tabel1, by = c('Hoofdregio', 'Scenario'), all.x = T, allow.cartesian = T)
setnames(tabel1, c('Hoofdregio', 'Hoofdscenario1', 'Aantal_huidig1', 'Aantal_nieuw1', 'Aantal_verschil1', 'Instelling1', 'Interventie1'), c('Subregio', 'Hoofdscenario2',  'Aantal_huidig2', 'Aantal_nieuw2', 'Aantal_verschil2', 'Instelling2', 'Interventie2'))
tabel = merge(tabel, tabel1, by = c('Subregio', 'Scenario'), all.x = T, allow.cartesian = T)
setnames(tabel1, c('Subregio', 'Hoofdscenario2',  'Aantal_huidig2', 'Aantal_nieuw2', 'Aantal_verschil2', 'Instelling2', 'Interventie2'), c('Vaatnetwerk', 'Hoofdscenario3',  'Aantal_huidig3', 'Aantal_nieuw3', 'Aantal_verschil3', 'Instelling3', 'Interventie3'))
tabel = merge(tabel, tabel1, by = c('Vaatnetwerk', 'Scenario'), all.x = T, allow.cartesian = T)
tabel$Toelichting = ifelse(is.na(tabel$Hoofdscenario1) == F, tabel$Hoofdscenario1,
                         ifelse(is.na(tabel$Hoofdscenario2) == F, tabel$Hoofdscenario2,
                                ifelse(is.na(tabel$Hoofdscenario3) == F, tabel$Hoofdscenario3, NA)))
tabel$Aantal_huidig = ifelse(is.na(tabel$Aantal_huidig1) == F, tabel$Aantal_huidig1,
                           ifelse(is.na(tabel$Aantal_huidig2) == F, tabel$Aantal_huidig2,
                                  ifelse(is.na(tabel$Aantal_huidig3) == F, tabel$Aantal_huidig3, NA)))
tabel$Aantal_nieuw = ifelse(is.na(tabel$Aantal_nieuw1) == F, tabel$Aantal_nieuw1,
                             ifelse(is.na(tabel$Aantal_nieuw2) == F, tabel$Aantal_nieuw2,
                                    ifelse(is.na(tabel$Aantal_nieuw3) == F, tabel$Aantal_nieuw3, NA)))
tabel$Aantal_verschil = ifelse(is.na(tabel$Aantal_verschil1) == F, tabel$Aantal_verschil1,
                             ifelse(is.na(tabel$Aantal_verschil2) == F, tabel$Aantal_verschil2,
                                    ifelse(is.na(tabel$Aantal_verschil3) == F, tabel$Aantal_verschil3, NA)))
tabel$Instelling = ifelse(is.na(tabel$Instelling1) == F, tabel$Instelling1,
                               ifelse(is.na(tabel$Instelling2) == F, tabel$Instelling2,
                                      ifelse(is.na(tabel$Instelling3) == F, tabel$Instelling3, NA)))
tabel$Interventie = ifelse(is.na(tabel$Interventie1) == F, tabel$Interventie1,
                               ifelse(is.na(tabel$Interventie2) == F, tabel$Interventie2,
                                      ifelse(is.na(tabel$Interventie3) == F, tabel$Interventie3, NA)))

tabel = tabel[,c('Hoofdregio', 'Subregio', 'Vaatnetwerk', 'Toelichting', 'Scenario', 'Interventie', 'Instelling',
                 'Aantal_huidig', 'Aantal_nieuw', 'Aantal_verschil')]
#Namen interventies aanpassen (korter)
tabel = merge(tabel, namen_interventies, by = c('Interventie'), all.x = T)
tabel = tabel[!is.na(tabel$Interventie),]
tabel$Aandoening = NULL
tabel$Naam_interventie_nieuw[tabel$Interventie == 'Totaal'] = 'Totaal'
tabel$Interventie = tabel$Naam_interventie_nieuw
tabel$Naam_interventie_nieuw = NULL
#Labels voor ziekenhuisnamen korter maken
tabel = merge(tabel, namen_kort, by = 'Instelling', all.x = T)
tabel$Instelling = NULL
setnames(tabel, c('Ziekenhuis_naam_kort'), c('Ziekenhuis'))

unique(tabel$Interventie)

# Rijen verwijderen als aantal huidig en aantal nieuw beide 0 is
# combi_scenarios = combi_scenarios[!(combi_scenarios$Aantal_huidig == 0 & combi_scenarios$Aantal_nieuw == 0),]
# tabel = tabel[!(tabel$Aantal_huidig == 0 & tabel$Aantal_nieuw == 0),]

write_xlsx(tabel, paste0(analyse, '# Databestand tabel toelichting scenarios'))
rm(tabel1, scenarios1, scenarios2, scenarios3)

write_xlsx(combi_scenarios, paste0(analyse, '# Databestand combi scenarios'))
