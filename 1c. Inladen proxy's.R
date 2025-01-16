#-------------------------------------------------------------------------------------------------------------------------------------------------------------
#Project: Impactanalyse concentratie en spreiding
#Auteur:  SiRM
#Datum:   januari 2025
#Doel:    Inladen proxy's
#-------------------------------------------------------------------------------------------------------------------------------------------------------------

#Clear all variables in R's memory
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

#----------------------------- Koppeltabellen ---------------------------------#

# Inladen namen excelbestanden waarin instellingen data hebben aangeleverd
koppel_excel = read_excel(paste0(bron, '# Bronbestand koppeling organisatienamen en excelbestanden'))
koppel_excel = koppel_excel[!is.na(koppel_excel$`Naam excelbestand kengetallen`),]
# Koppelbestand om proxy's handige naam te geven
koppel_proxy = read_excel(paste0(input_model, "#Bronbestand koppeltabellen"), sheet = "Proxy's")

# Volumenormen
iv_proxy = read_excel(paste0(analyse, "# Bronbestand scenario's C&S"), sheet = 'Interventie_proxy')

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#----------------------------------- Proxy's ----------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# Leeg dataframe maken om proxy's instellingen in te zetten
proxy_tot = data.frame(matrix(data = NA, nrow = 0, ncol = 6))
names(proxy_tot) = c('Instelling', 'Onderwerp', 'Variabele', 'Interventie', 'Waarde_landelijk', 'Waarde_instelling')

proxy_tot = proxy_tot %>% mutate_at(c('Instelling', 'Onderwerp', 'Variabele', 'Interventie'), 'as.character')
proxy_tot = proxy_tot %>% mutate_at(c('Waarde_landelijk', 'Waarde_instelling'), 'as.numeric')

# Data instellingen laden - oncologie + vaatchirurgie:
koppel_excel_OV = koppel_excel[koppel_excel$`Type data-uitvraag` == 'Oncologie + vaatchirurgie',]
koppel_proxy_OV = koppel_proxy[,c('Rij_nummer - onco + vaat', 'Interventie2', 'Variabele2')]
setnames(koppel_proxy_OV, 'Rij_nummer - onco + vaat', 'Rij_nummer')

for(i in 1:nrow(koppel_excel_OV)){
  
  instelling = koppel_excel_OV[[i, "Instelling"]]
  naam_ken   = koppel_excel_OV[[i, "Naam excelbestand kengetallen"]]
  naam_pr    = koppel_excel_OV[[i, "Naam excelbestand proxy's"]]
  
  proxy = read_excel(paste0(bron, "# naam map/",naam_pr,".xlsx"), sheet = 2)
  # ERCP polikliniekbezoeken hebben in uitvraag zelfde naam als chirurgische, dus aanpassen
  proxy[12, 2] = 'Aantal polikliniekbezoeken voor interventie per interventie - Radiotherapie'
  proxy[13, 2] = 'Aantal polikliniekbezoeken na interventie per interventie - Radiotherapie'
  proxy[14, 2] = 'Aantal polikliniekbezoeken voor interventie per interventie - ERCP'
  proxy[15, 2] = 'Aantal polikliniekbezoeken na interventie per interventie - ERCP'
  proxy[28, 2] = 'Duur polikliniekbezoeken voor interventie per interventie - Radiotherapie'
  proxy[29, 2] = 'Duur polikliniekbezoeken na interventie per interventie - Radiotherapie'
  proxy[30, 2] = 'Duur polikliniekbezoeken voor interventie per interventie - ERCP'
  proxy[31, 2] = 'Duur polikliniekbezoeken na interventie per interventie - ERCP'
  proxy = proxy[!is.na(proxy$...2),]
  proxy = proxy[!is.na(proxy$...4),]
  proxy = proxy[!is.na(proxy$`Proxy's productieparameters`),]
  proxy = proxy[!proxy$...3 == 'Landelijke proxy',]
  nrow(proxy) #handmatig geteld - er moeten 193 rijen overblijven, dus dat klopt
  
  proxy$Waarde_landelijk = as.numeric(proxy$...3)
  # Als een instelling een proxywaarde wil aanpassen, deze in aparte kolom zetten met aangepaste proxy's
  proxy$Waarde_instelling = as.numeric(ifelse(proxy$...4 == 'Ja' & is.na(proxy$...5) == F, proxy$...5, proxy$...3))
  proxy = proxy[,c(2, 7, 8)]
  setnames(proxy, '...2', 'Interventie')

  # In kengetallen zitten ook nog waarden op interventieniveau, die toevoegen:
  proxy_OK = read_excel(paste0(bron, "Gegevens aangeleverd door ziekenhuizen/Kengetallen/",naam_ken,".xlsx"), sheet = 2, range="A229:B236")
  names(proxy_OK) = c('Interventie', 'Waarde_instelling')
  proxy_OK$Waarde_landelijk = proxy_OK$Waarde_instelling # We hebben geen landelijke waardes, dus gelijk zetten aan waarde instelling
  proxy_RT = read_excel(paste0(bron, "Gegevens aangeleverd door ziekenhuizen/Kengetallen/",naam_ken,".xlsx"), sheet = 2, range="A240:B244")
  names(proxy_RT) = c('Interventie', 'Waarde_instelling')
  proxy_RT$Waarde_landelijk = proxy_RT$Waarde_instelling # We hebben geen landelijke waardes, dus gelijk zetten aan waarde instelling
  proxy = bind_rows(proxy, proxy_OK, proxy_RT)
  
  proxy$Rij_nummer = seq(1:nrow(proxy))
  
  proxy$Variabele = c(proxy$Interventie[1:22],
                      rep("Percentage klinische opname", 22), rep("Gemiddelde opnameduur patiënten na IC", 13),
                      rep("Gemiddelde opnameduur patiënten zonder IC", 22), rep("Percentage opname IC", 18),
                      rep("Gemiddelde IC-opnameduur", 13), rep("Gemiddeld aantal SEH-bezoeken", 22),
                      proxy$Interventie[133:155], 
                      rep("OK-tijd en scopietijd", 10), rep("Tijd per fractie radiotherapie", 4),
                      rep("Systeemtherapie", 24), rep('OK-tijd en scopietijd', 7),
                      rep('Aantal fracties radiotherapie',4))
  proxy$Onderwerp = c(rep("Polikliniek", 22), rep("Verpleegafdeling", 57), 
                      rep("IC", 31), rep("SEH", 22), rep("Personeelsinzet", 19),
                      rep("Capaciteit", 4), rep("OK en scopiekamer", 10),
                      rep("Radiotherapie", 4), 
                      rep("Systeemtherapie", 24),
                      rep("OK en scopiekamer", 7),
                      rep("Radiotherapie", 4))
  # Met een koppeltabel geven we per rij in de data-uitvraag aan 
  # - voor welke interventie(s) de proxy van toepassing is
  # - een handige variabelenaam voor de proxy
  proxy = merge(proxy, koppel_proxy_OV, by = c('Rij_nummer'), all.x=T)
  
  proxy$Interventie = proxy$Interventie2
  proxy$Variabele   = proxy$Variabele2
  proxy             = subset(proxy, select = -c(Interventie2, Variabele2, Rij_nummer))
  proxy$Instelling  = instelling
  proxy_tot = bind_rows(proxy_tot, proxy)
}

# Data instellingen laden - oncologie:

koppel_excel_O = koppel_excel[koppel_excel$`Type data-uitvraag` == 'Oncologie',]
koppel_proxy_O = koppel_proxy[,c('Rij_nummer onco', 'Interventie2', 'Variabele2')]
setnames(koppel_proxy_O, 'Rij_nummer onco', 'Rij_nummer')

for(i in 1:nrow(koppel_excel_O)){
  
  instelling = koppel_excel_O[[i, "Instelling"]]
  naam_ken   = koppel_excel_O[[i, "Naam excelbestand kengetallen"]]
  naam_pr    = koppel_excel_O[[i, "Naam excelbestand proxy's"]]
  
  proxy = read_excel(paste0(bron, "# naam map/",naam_pr,".xlsx"), sheet = 2)
  proxy[12, 2] = 'Aantal polikliniekbezoeken voor interventie per interventie - Radiotherapie'
  proxy[13, 2] = 'Aantal polikliniekbezoeken na interventie per interventie - Radiotherapie'
  proxy[14, 2] = 'Aantal polikliniekbezoeken voor interventie per interventie - ERCP'
  proxy[15, 2] = 'Aantal polikliniekbezoeken na interventie per interventie - ERCP'
  proxy[28, 2] = 'Duur polikliniekbezoeken voor interventie per interventie - Radiotherapie'
  proxy[29, 2] = 'Duur polikliniekbezoeken na interventie per interventie - Radiotherapie'
  proxy[30, 2] = 'Duur polikliniekbezoeken voor interventie per interventie - ERCP'
  proxy[31, 2] = 'Duur polikliniekbezoeken na interventie per interventie - ERCP'
  proxy = proxy[!is.na(proxy$...2),]
  proxy = proxy[!is.na(proxy$...4),]
  proxy = proxy[!is.na(proxy$`Proxy's productieparameters`),]
  proxy = proxy[!proxy$...3 == 'Landelijke proxy',]

  proxy$Waarde_landelijk = as.numeric(proxy$...3)
  #Als een instelling een proxywaarde wil aanpassen, deze in aparte kolom zetten met aangepaste proxy's
  proxy$Waarde_instelling = as.numeric(ifelse(proxy$...4 == 'Ja' & is.na(proxy$...5) == F, proxy$...5, proxy$...3))
  proxy = proxy[,c(2, 7, 8)]
  setnames(proxy, '...2', 'Interventie')

  # In kengetallen zitten ook nog waarden op interventieniveau, die toevoegen:
  proxy_OK = read_excel(paste0(bron, "Gegevens aangeleverd door ziekenhuizen/Kengetallen/",naam_ken,".xlsx"), sheet = 2, range="A226:B233")
  names(proxy_OK) = c('Interventie', 'Waarde_instelling')
  proxy_OK$Waarde_landelijk = proxy_OK$Waarde_instelling # We hebben geen landelijke waardes, dus gelijk zetten aan waarde instelling
  proxy_RT = read_excel(paste0(bron, "Gegevens aangeleverd door ziekenhuizen/Kengetallen/",naam_ken,".xlsx"), sheet = 2, range="A237:B241")
  names(proxy_RT) = c('Interventie', 'Waarde_instelling')
  proxy_RT$Waarde_landelijk = proxy_RT$Waarde_instelling # We hebben geen landelijke waardes, dus gelijk zetten aan waarde instelling
  proxy = bind_rows(proxy, proxy_OK, proxy_RT)
  
  proxy$Rij_nummer = seq(1:nrow(proxy))
  
  proxy$Variabele = c(proxy$Interventie[1:22],
                      rep("Percentage klinische opname", 18), rep("Gemiddelde opnameduur patiënten na IC", 9),
                      rep("Gemiddelde opnameduur patiënten zonder IC", 18), rep("Percentage opname IC", 14),
                      rep("Gemiddelde IC-opnameduur", 9), rep("Gemiddeld aantal SEH-bezoeken", 18),
                      proxy$Interventie[109:131], 
                      rep("OK-tijd en scopietijd", 4), rep("Tijd per fractie radiotherapie", 4),
                      rep("Systeemtherapie", 24), rep('OK-tijd en scopietijd', 7),
                      rep('Aantal fracties radiotherapie',4))
  proxy$Onderwerp = c(rep("Polikliniek", 22), rep("Verpleegafdeling", 45), 
                      rep("IC", 23), rep("SEH", 18), rep("Personeelsinzet", 19),
                      rep("Capaciteit", 4), rep("OK en scopiekamer", 4),
                      rep("Radiotherapie", 4), 
                      rep("Systeemtherapie", 24),
                      rep("OK en scopiekamer", 7),
                      rep("Radiotherapie", 4))
  #Met een koppeltabel geven we per rij in de data-uitvraag aan 
  #- voor welke interventie(s) de proxy van toepassing is
  #- een handige variabelenaam voor de proxy
  proxy = merge(proxy, koppel_proxy_O, by = c('Rij_nummer'), all.x=T)
  
  proxy$Interventie = proxy$Interventie2
  proxy$Variabele   = proxy$Variabele2
  proxy             = subset(proxy, select = -c(Interventie2, Variabele2, Rij_nummer))
  proxy$Instelling  = instelling
  proxy_tot = bind_rows(proxy_tot, proxy)
}

# Data instellingen laden - radiotherapie:

koppel_excel_R = koppel_excel[koppel_excel$`Type data-uitvraag` == 'Radiotherapie',]
koppel_proxy_R = koppel_proxy[,c('Rij_nummer RT', 'Interventie2', 'Variabele2')]
setnames(koppel_proxy_R, 'Rij_nummer RT', 'Rij_nummer')

for(i in 1:nrow(koppel_excel_R)){
  
  instelling = koppel_excel_R[[i, "Instelling"]]
  naam_ken   = koppel_excel_R[[i, "Naam excelbestand kengetallen"]]
  naam_pr    = koppel_excel_R[[i, "Naam excelbestand proxy's"]]
  
  proxy = read_excel(paste0(bron, "# naam map/",naam_pr,".xlsx"), sheet = 2)
  proxy = proxy[!is.na(proxy$...2),]
  proxy = proxy[!is.na(proxy$...4),]
  proxy = proxy[!is.na(proxy$`Proxy's productieparameters`),]
  proxy = proxy[!proxy$...3 == 'Landelijke proxy',]
  
  proxy$Waarde_landelijk = as.numeric(proxy$...3)
  #Als een instelling een proxywaarde wil aanpassen, deze in aparte kolom zetten met aangepaste proxy's
  proxy$Waarde_instelling = as.numeric(ifelse(proxy$...4 == 'Ja' & is.na(proxy$...5) == F, proxy$...5, proxy$...3))
  proxy = proxy[,c(2, 7, 8)]
  setnames(proxy, '...2', 'Interventie')
  
  # In kengetallen zitten ook nog waarden op interventieniveau, die toevoegen:
  proxy_RT = read_excel(paste0(bron, "Gegevens aangeleverd door ziekenhuizen/Kengetallen/",naam_ken,".xlsx"), sheet = 2, range="A58:B62")
  names(proxy_RT) = c('Interventie', 'Waarde_instelling')
  proxy_RT$Waarde_landelijk = proxy_RT$Waarde_instelling # We hebben geen landelijke waardes, dus gelijk zetten aan waarde instelling
  proxy = bind_rows(proxy, proxy_RT)
  
  proxy$Rij_nummer = seq(1:nrow(proxy))
  
  proxy$Variabele = c(proxy$Interventie[1:8], 
                      rep("Tijd per fractie radiotherapie", 4),
                      rep('Aantal fracties radiotherapie',4))
  proxy$Onderwerp = c(rep("Polikliniek", 4), 
                      rep("Personeelsinzet", 3),
                      rep("Capaciteit", 1), 
                      rep("Radiotherapie", 8))
  # Met een koppeltabel geven we per rij in de data-uitvraag aan 
  # - voor welke interventie(s) de proxy van toepassing is
  # - een handige variabelenaam voor de proxy
  proxy = merge(proxy, koppel_proxy_R, by = c('Rij_nummer'), all.x=T)
  
  proxy$Interventie = proxy$Interventie2
  proxy$Variabele   = proxy$Variabele2
  proxy             = subset(proxy, select = -c(Interventie2, Variabele2, Rij_nummer))
  proxy$Instelling  = instelling
  proxy_tot = bind_rows(proxy_tot, proxy)
}

rm(koppel_excel, koppel_excel_O, koppel_excel_OV, koppel_excel_R, koppel_proxy, koppel_proxy_O,
   koppel_proxy_OV, koppel_proxy_R, proxy_OK, proxy_RT, proxy)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#----------------------- Missende OK-tijden berekenen -------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# Sommige ziekenhuizen hebben voor bepaalde interventies geen OK-tijd aangeleverd
# Terwijl ze die interventie wel doen
# In dat geval pakken we het landelijk gemiddelde
# Daarbij maken we onderscheid tussen algemene ziekenhuizen en UMC's

OK_tijd = proxy_tot[proxy_tot$Variabele == 'OK-tijd',]
OK_tijd$Type_instelling = ifelse(OK_tijd$Instelling %in% c('Amsterdam UMC', 
                                                           'Erasmus MC', 
                                                           'LUMC', 
                                                           'Maastricht UMC+', 
                                                           'UMCG Groningen', 
                                                           'UMC Utrecht', 
                                                           'Radboudumc'), 
                                 'Academisch ziekenhuis', 
                                 'Algemeen ziekenhuis')
OK_tijd_bekend = OK_tijd[is.na(OK_tijd$Waarde_instelling) == F & OK_tijd$Waarde_instelling > 0,]
OK_tijd_bekend = OK_tijd_bekend[!is.na(OK_tijd_bekend$Instelling),]

# Koppelen aan aantallen
# Soms hebben ziekenhuizen OK-tijd aangeleverd voor iets dat ze niet doen
NZa = read_excel(paste0(input_model, '# Databestand Volumes instellingen'))
NZa$voert_uit = 1
NZa = subset(NZa, select = -c(Norm_optellen, W, aantal, Aandoening))
# Maag/slokdarm aantallen zijn hier samengevoegd maar in data-uitvraag opgesplitst
# Dus hier ook opsplitsen zodat we kunnen koppelen
NZa_maag = NZa[grepl("maagresecties", NZa$Interventie),]
NZa_maag$Interventie = 'Maag - oncologische, chirurgische maagresecties'
NZa_maag2 = NZa[grepl("endoscopisch", NZa$Interventie),]
NZa_maag2$Interventie = 'Maag - oncologische, endoscopische resecties (vroeg)carcinoom'
NZa_slokdarm = NZa[grepl("maagresecties", NZa$Interventie),]
NZa_slokdarm$Interventie = 'Slokdarm - oncologische, chirurgische slokdarmresecties'
NZa_slokdarm2 = NZa[grepl("endoscopisch", NZa$Interventie),]
NZa_slokdarm2$Interventie = 'Slokdarm - oncologische, endoscopische resecties (vroeg)carcinoom'
NZa = NZa[!grepl('maagresecties', NZa$Interventie),]
NZa = NZa[!grepl('endoscopisch', NZa$Interventie),]
NZa = bind_rows(NZa, NZa_maag, NZa_maag2, NZa_slokdarm, NZa_slokdarm2)
rm(NZa_maag, NZa_maag2, NZa_slokdarm, NZa_slokdarm2)

# Koppelen zodat we OK-tijden kunnen verwijderen als ziekenhuizen het niet uitvoeren
# Anders worden die wel meegenomen in het gemiddelde
OK_tijd_bekend = merge(OK_tijd_bekend, NZa, by = c('Instelling', 'Interventie'), all.x = T, all.y = F)
OK_tijd_bekend = OK_tijd_bekend[!is.na(OK_tijd_bekend$voert_uit),]

# Gemiddeldes berekenen:
OK_tijd_bekend = OK_tijd_bekend %>% dplyr::group_by(Interventie, Type_instelling) %>%
  dplyr::summarise(gem_waarde_instelling = mean(Waarde_instelling))

# Nu tabel maken met OK-tijden die niet aangeleverd zijn terwijl ze de interventie wel uitvoeren:
OK_tijd_onbekend = OK_tijd[(is.na(OK_tijd$Waarde_instelling) | OK_tijd$Waarde_instelling == 0),]
OK_tijd_onbekend = merge(OK_tijd_onbekend, NZa, by = c('Instelling', 'Interventie'), all.x = T)
OK_tijd_onbekend = OK_tijd_onbekend[!is.na(OK_tijd_onbekend$voert_uit),]
# Gemiddeldes koppelen en lege waarde vervangen door gemiddelde:
OK_tijd_onbekend = merge(OK_tijd_onbekend, OK_tijd_bekend, by = c('Interventie', 'Type_instelling'), all.x = T)
OK_tijd_onbekend$Waarde_instelling = OK_tijd_onbekend$gem_waarde_instelling
OK_tijd_onbekend$Waarde_landelijk = OK_tijd_onbekend$gem_waarde_instelling
OK_tijd_onbekend = subset(OK_tijd_onbekend, select=-c(voert_uit, gem_waarde_instelling, Type_instelling))

# Koppelen aan tabel met proxy's
proxy_tot = bind_rows(proxy_tot, OK_tijd_onbekend)
# Gemiddelde OK-tijden opslaan:
OK_tijd_bekend = setDT(OK_tijd_bekend) %>% dcast(Interventie ~ `Type_instelling`, fun.aggregate = sum)
write_xlsx(OK_tijd_bekend, paste0(analyse, 'Databestand Gemiddelde OK-tijden'))

rm(OK_tijd_bekend, OK_tijd_onbekend, OK_tijd)

# Nu kunnen we alle lege waarden en 0 verwijderen:
proxy_tot = proxy_tot[!(is.na(proxy_tot$Waarde_landelijk) & is.na(proxy_tot$Waarde_instelling)),]
proxy_tot = proxy_tot[!(proxy_tot$Waarde_landelijk == 0 & is.na(proxy_tot$Waarde_instelling)),]
proxy_tot = proxy_tot[!(is.na(proxy_tot$Waarde_landelijk) & proxy_tot$Waarde_instelling == 0),]
proxy_tot = proxy_tot[!(proxy_tot$Waarde_landelijk == 0 & proxy_tot$Waarde_instelling == 0),]

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#---------------------------- Financiële proxy's ------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# De proxy's voor omzet/kosten per interventie zaten niet in de data-uitvraag
# Omdat ziekenhuizen deze niet konden aanpassen
# Daarom voegen we deze apart toe

proxy_fin = read_excel(paste0(admin, '# Datauitvraag Financiële proxywaarden'), sheet = 2)
names(proxy_fin) = proxy_fin[4,]
proxy_fin = proxy_fin[5:nrow(proxy_fin),c(4:6)]

# Alleen de relevante selecteren die we meenemen in scenario's:
proxy_fin = proxy_fin[proxy_fin$Interventie %in% iv_proxy$Interventie,]

# Deze financiële proxy's moeten we herhalen voor elk ziekenhuis die de
# betreffende interventie uitvoert
proxy_fin2 = unique(proxy_tot[,c('Instelling', 'Interventie')])
proxy_fin2 = proxy_fin2[!proxy_fin2$Interventie == 'Totaal',]
proxy_fin2 = merge(proxy_fin2, proxy_fin, by = c('Interventie'), all.x = T)
proxy_fin2 = melt(proxy_fin2, id.vars = c('Interventie', 'Instelling'), 
                  variable.name = 'Variabele', 
                  value.name = 'Waarde_landelijk')
proxy_fin2$Waarde_landelijk = as.numeric(proxy_fin2$Waarde_landelijk)
proxy_fin2$Waarde_instelling = proxy_fin2$Waarde_landelijk
proxy_fin2$Onderwerp = 'Financieel'

# Koppelen aan overige proxy's
proxy_tot = bind_rows(proxy_tot, proxy_fin2)
rm(proxy_fin, proxy_fin2, iv_proxy)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#---------------------------- Tabel opslaan -----------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

write_xlsx(proxy_tot, paste0(analyse, "# Databestand Samengevoegde proxy's"))
