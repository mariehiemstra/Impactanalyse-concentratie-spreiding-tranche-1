#-------------------------------------------------------------------------------------------------------------------------------------------------------------
#Project: Impactanalyse concentratie en spreiding
#Auteur:  SiRM
#Datum:   januari 2025
#Doel:    Berekening verschil in aantal interventies o.b.v. reistijd (3c)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------

# Clear all variables in R's memory
rm(list=ls())
options(scipen=100, digits=2)

# Libraries
library(readxl)
library(data.table)
library(writexl)
library(tidyverse)
library(sf)

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

combis_scenarios = data.table(read_excel(paste0(input_model, "# Databestand combi scenarios")))
reistijdentabel_2 = data.table(read_excel(paste0(input_model, "# Databestand input code voor reistijden")))

# Aantal inwoners per pc4-gebied (2022)
inwoners = read.csv(paste0(sogiz, 'bevolking (CBS).csv'), sep = ';')
inwoners = inwoners[inwoners$jaar == 2022,]
inwoners = inwoners %>% dplyr::group_by(pc4) %>% dplyr::summarise(inwoners = sum(inwoners, na.rm = T))

#---------------------------- Reistijdentabel uitdraaien -------------------------------#

# Benigne long uit tabel halen, want omdat ze maar in een regio voorkomen hele hoge reistijden vanuit heel NL
reistijdentabel_2 <- reistijdentabel_2[!reistijdentabel_2$Interventie %in% c("Long - benigne longchirurgie")]
reistijdentabel_2 <- reistijdentabel_2[!(reistijdentabel_2$Interventie == "Nier - lokale behandelingen" & reistijdentabel_2$Subregio =="Oncologienetwerk Veluwe-IJssel"),]

reistijdentabel_2$verhouding_aantal <- reistijdentabel_2$aantal_na_stap_2 / reistijdentabel_2$aantal_pc4
reistijdentabel_2 <- left_join(reistijdentabel_2, inwoners, by = c("pc4"))
reistijdentabel_2$aantal_inwoners <- reistijdentabel_2$verhouding_aantal * reistijdentabel_2$inwoners
reistijdentabel_2$inwoners = NULL
reistijdentabel_2$extra_reistijd <- reistijdentabel_2$reistijd_na_concentratie_en_spreiding - reistijdentabel_2$reistijd_voor_concentratie

gemiddelde_reistijden <- reistijdentabel_2[!reistijdentabel_2$extra_reistijd == 0,.(gem_reistijd_voor_concentratie = sum(reistijd_voor_concentratie * aantal_na_stap_2)/sum(aantal_na_stap_2),
                                              gem_extra_reistijd = sum(extra_reistijd * aantal_na_stap_2)/sum(aantal_na_stap_2),
                                              wegingsfactor = sum(aantal_na_stap_2)), #wegingsfactor voor als je gemiddelde over interventies wil nemen
                                           by = c("Subregio","Aandoening", "Interventie", "Scenario")]

combis <- distinct(combis_scenarios[,-c("Instelling", "Aantal_huidig","Aantal_nieuw", "Aantal_verschil")])
combis <- combis[!combis$Interventie == "Long - benigne longchirurgie",]

combi_gem_reistijd <- setnames(left_join(combis, gemiddelde_reistijden, by = c("Hoofdregio"="Subregio","Aandoening", "Interventie", "Hoofdscenario"= "Scenario")), 
                          old = c("gem_reistijd_voor_concentratie", "gem_extra_reistijd", "wegingsfactor"), 
                          new=c("gem_reistijd_voor_concentratie_hoofd", "gem_extra_reistijd_hoofd", "wegingsfactor_hoofd"))
combi_gem_reistijd <- setnames(left_join(combi_gem_reistijd, gemiddelde_reistijden, by = c("Subregio"="Subregio","Aandoening", "Interventie", "Subscenario"= "Scenario")), 
                               old = c("gem_reistijd_voor_concentratie", "gem_extra_reistijd", "wegingsfactor"), 
                               new=c("gem_reistijd_voor_concentratie_sub", "gem_extra_reistijd_sub", "wegingsfactor_sub"))
combi_gem_reistijd <- setnames(left_join(combi_gem_reistijd, gemiddelde_reistijden, by = c("Vaatnetwerk"="Subregio","Aandoening", "Interventie", "Vaatscenario"= "Scenario")), 
                               old = c("gem_reistijd_voor_concentratie", "gem_extra_reistijd", "wegingsfactor"), 
                               new=c("gem_reistijd_voor_concentratie_vaat", "gem_extra_reistijd_vaat",  "wegingsfactor_vaat" ))

# Regio per interventie dus geen overlap tussen de scenarios
check = combi_gem_reistijd[is.na(gem_reistijd_voor_concentratie_hoofd) & is.na(gem_reistijd_voor_concentratie_sub) & is.na(gem_reistijd_voor_concentratie_vaat)]

combi_gem_reistijd$scenario_relevant <- ifelse(!is.na(combi_gem_reistijd$gem_reistijd_voor_concentratie_hoofd),
                                               combi_gem_reistijd$Hoofdscenario,
                                               ifelse(!is.na(combi_gem_reistijd$gem_reistijd_voor_concentratie_sub),
                                                      combi_gem_reistijd$Subscenario,
                                                      ifelse(!is.na(combi_gem_reistijd$gem_reistijd_voor_concentratie_vaat),
                                                             combi_gem_reistijd$Vaatscenario,0)))

combi_gem_reistijd$gemiddelde_reistijd_voor_concentratie <- ifelse(!is.na(combi_gem_reistijd$gem_reistijd_voor_concentratie_hoofd),
                                                                   combi_gem_reistijd$gem_reistijd_voor_concentratie_hoofd,
                                                                   ifelse(!is.na(combi_gem_reistijd$gem_reistijd_voor_concentratie_sub),
                                                                          combi_gem_reistijd$gem_reistijd_voor_concentratie_sub,
                                                                   ifelse(!is.na(combi_gem_reistijd$gem_reistijd_voor_concentratie_vaat),
                                                                          combi_gem_reistijd$gem_reistijd_voor_concentratie_vaat,0)))
combi_gem_reistijd$gemiddelde_extra_reistijd <- ifelse(!is.na(combi_gem_reistijd$gem_extra_reistijd_hoofd),combi_gem_reistijd$gem_extra_reistijd_hoofd,
                                                                   ifelse(!is.na(combi_gem_reistijd$gem_extra_reistijd_sub),
                                                                          combi_gem_reistijd$gem_extra_reistijd_sub,
                                                                          ifelse(!is.na(combi_gem_reistijd$gem_extra_reistijd_vaat),
                                                                                 combi_gem_reistijd$gem_extra_reistijd_vaat,0)))
combi_gem_reistijd$wegingsfactor <- ifelse(!is.na(combi_gem_reistijd$wegingsfactor_hoofd),combi_gem_reistijd$wegingsfactor_hoofd,
                                                       ifelse(!is.na(combi_gem_reistijd$wegingsfactor_sub),combi_gem_reistijd$wegingsfactor_sub,
                                                              ifelse(!is.na(combi_gem_reistijd$wegingsfactor_vaat),combi_gem_reistijd$wegingsfactor_vaat,0)))

reistijden_output <- combi_gem_reistijd[,-c("gem_reistijd_voor_concentratie_hoofd", "gem_extra_reistijd_hoofd","wegingsfactor_hoofd","gem_reistijd_voor_concentratie_sub",
                                            "gem_extra_reistijd_sub", "wegingsfactor_sub", "gem_reistijd_voor_concentratie_vaat", "gem_extra_reistijd_vaat", "wegingsfactor_vaat")]

# Categorie reistijd toevoegen
reistijdentabel_2$cat_extra_reistijd <- ifelse(reistijdentabel_2$extra_reistijd == 0, "aantal_inw_geen_extra_reistijd", 
                                               ifelse(reistijdentabel_2$extra_reistijd < 5, "aantal_inw_minder_dan_5_min", 
                                                      ifelse(reistijdentabel_2$extra_reistijd < 10, "aantal_inw_5_tot_10_min", 
                                                             ifelse(reistijdentabel_2$extra_reistijd < 15, "aantal_inw_10_tot_15_min",
                                                                    ifelse(reistijdentabel_2$extra_reistijd < 20, "aantal_inw_15_tot_20_min",
                                                                           ifelse(reistijdentabel_2$extra_reistijd < 25, "aantal_inw_20_tot_25_min",
                                                                                  ifelse(reistijdentabel_2$extra_reistijd <30, "aantal_inw_25_tot_30_min",
                                                                                         ifelse(reistijdentabel_2$extra_reistijd <45, "aantal_inw_30_tot_45_min", "aantal_inw_meer_dan_45_min"))))))))

reistijden_cat <- reistijdentabel_2[,.(aantal_inwoners = sum(aantal_na_stap_2)), 
                                    by = c("Subregio","Aandoening", "Interventie", "Scenario", "cat_extra_reistijd")]

combi_cat_reistijd <- setnames(left_join(combis, reistijden_cat, by = c("Hoofdregio"="Subregio","Aandoening", "Interventie", "Hoofdscenario"= "Scenario"), relationship = 'many-to-many'),
                               old=c("cat_extra_reistijd","aantal_inwoners"), new=c("cat_extra_reistijd_hoofd","aantal_inwoners_hoofd"))
combi_cat_reistijd <- setnames(left_join(combi_cat_reistijd, reistijden_cat, by = c("Subregio"="Subregio","Aandoening", "Interventie", "Subscenario"= "Scenario"), relationship = 'many-to-many'),
                                old=c("cat_extra_reistijd","aantal_inwoners"), new=c("cat_extra_reistijd_sub","aantal_inwoners_sub"))
combi_cat_reistijd <- setnames(left_join(combi_cat_reistijd, reistijden_cat, by = c("Vaatnetwerk"="Subregio","Aandoening", "Interventie", "Vaatscenario"= "Scenario"), relationship = 'many-to-many'), 
                               old=c("cat_extra_reistijd","aantal_inwoners"), new=c("cat_extra_reistijd_vaat","aantal_inwoners_vaat"))

combi_cat_reistijd$`Reistijd categorie` <- ifelse(!is.na(combi_cat_reistijd$cat_extra_reistijd_hoofd),combi_cat_reistijd$cat_extra_reistijd_hoofd,
                                                            ifelse(!is.na(combi_cat_reistijd$cat_extra_reistijd_sub),combi_cat_reistijd$cat_extra_reistijd_sub,
                                                                   ifelse(!is.na(combi_cat_reistijd$cat_extra_reistijd_vaat),combi_cat_reistijd$cat_extra_reistijd_vaat,"nvt")))

combi_cat_reistijd$`Aantal inwoners` <- ifelse(!is.na(combi_cat_reistijd$aantal_inwoners_hoofd),combi_cat_reistijd$aantal_inwoners_hoofd,
                                                ifelse(!is.na(combi_cat_reistijd$aantal_inwoners_sub),combi_cat_reistijd$aantal_inwoners_sub,
                                                       ifelse(!is.na(combi_cat_reistijd$aantal_inwoners_vaat),combi_cat_reistijd$aantal_inwoners_vaat,NA)))

combi_cat_reistijd <- combi_cat_reistijd[combi_cat_reistijd$`Reistijd categorie`!="nvt", -c("cat_extra_reistijd_hoofd", "cat_extra_reistijd_sub", "cat_extra_reistijd_vaat",
                                                                                           "aantal_inwoners_hoofd", "aantal_inwoners_sub", "aantal_inwoners_vaat")]

reistijden_output_2 <- left_join(reistijden_output, combi_cat_reistijd, by = c("Hoofdregio","Hoofdscenario" ,"Subregio", "Vaatnetwerk" ,                      
                                                                             "Subscenario", "Vaatscenario","Aandoening" ,"Interventie",                       
                                                                             "Regio impactanalyse" ,"Combinatie_hoofd_sub_vaat_scenario", "Combinatie_regios"))

reistijden_output_2$filter_voor_gemiddeldes <- ifelse(reistijden_output_2$`Reistijd categorie` == "aantal_inw_geen_extra_reistijd", 1, 0)

reistijden_output_gefilterd <- data.table(reistijden_output_2)
reistijden_output_gefilterd$Combinatie_hoofd_sub_vaat_scenario <- reistijden_output_gefilterd$scenario_relevant
reistijden_output_gefilterd$scenario_relevant = NULL
reistijden_output_gefilterd <- reistijden_output_gefilterd[!duplicated(reistijden_output_gefilterd[,-c("Hoofdscenario", "Subscenario", "Vaatscenario")]),]

# --------------------------Berekenen maximale reistijden----------------------# 
max_reistijden <- reistijdentabel_2[,.(max_reistijd = max(reistijd_na_concentratie_en_spreiding),
                             max_extra = max(extra_reistijd)), by =c ("Scenario", "Interventie", "Subregio")]

#---------------------------Bestanden wegschrijven-----------------------------#
write_xlsx(max_reistijden, paste0(analyse, '# Databestand max reistijden'))
write_xlsx(reistijden_output_gefilterd, paste0(analyse, '# Databestand reistijdenanalyse voor PowerBI'))

