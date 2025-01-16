#-------------------------------------------------------------------------------------------------------------------------------------------------------------
#Project:  Impactanalyse concentratie en spreiding
#Auteur:  SiRM
#Datum:   januari 2025
#Doel:    Inladen kengetallen (1b)
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

#Koppeltabel voor specialismen:
koppel_specialisme = read_excel(paste0('# Koppeltabel'), sheet = 'Specialisme')

# Inladen namen excelbestanden waarin instellingen data hebben aangeleverd
koppel_excel = read_excel(paste0('# Databestand Koppeltabel namen en bestanden'))
koppel_excel = koppel_excel[!is.na(koppel_excel$`Naam excelbestand kengetallen`),] #Niet aangeleverde instellingen verwijderen

# Leeg dataframe maken om kengetallen instellingen in te zetten
data_tot = data.frame(matrix(data = NA, nrow = 0, ncol = 7))
names(data_tot) = c('Instelling', 'Locatie', 'Locatienaam', 'Locatieplaats', 'Variabele', 'Specialisme', 'Waarde')
data_tot = data_tot %>% mutate_at(c('Instelling', 'Locatie', 'Locatienaam', 'Locatieplaats', 'Variabele', 'Specialisme'), 'as.character')
data_tot = data_tot %>% mutate_at(c('Waarde'), 'as.numeric')

#We hebben met drie verschillende formulieren de kengetallen uitgevraagd:
#- Kengetallen mbt oncologie en vaatchirurgie
#- Kengetallen alleen oncologie
#- Kengetallen alleen over radiotherapie
#In koppel_excel geven we per ziekenhuis aan welke uitvraag ze hebben gekregen
#Hieronder laden we met een for loop alle data in, apart voor de drie uitvragen

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------- Oncologie + vaatchirurgie --------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

koppel_excel_OV = koppel_excel[koppel_excel$`Type data-uitvraag` == 'Oncologie + vaatchirurgie',]

for(i in 1:nrow(koppel_excel_OV)){
  
  instelling      = koppel_excel_OV[[i, "Instelling"]]
  naam            = koppel_excel_OV[[i, "Naam excelbestand kengetallen"]]
  
  #Namen locaties
  
  # Overzicht locaties maken
  locaties2 = read_excel(paste0(bron, "# naam map/",naam,".xlsx"), sheet = 2, range = "A13:D15")
  setnames(locaties2, '...1', 'Variabele')
  locaties2$Organisatie = c(instelling, 'nvt')
  locaties = data.frame(t(locaties2[,-1]))
  locaties = locaties %>% rownames_to_column(var="Locatie")
  names(locaties) = c('Locatie', 'Locatienaam', 'Locatieplaats')
  locaties$Locatie = c('Locatie 1', 'Locatie 2', 'Locatie 3', 'Organisatie')
    
  # Faciliteiten
  data1 = read_excel(paste0(bron, "# naam map/",naam,".xlsx"), sheet = 2, range = "A41:F50")
  data1 = data1[c(3:nrow(data1)),]
  data1$Toelichting = NULL
  names(data1) = c('Variabele', 'Organisatie', 'Locatie 1', 'Locatie 2', 'Locatie 3')
  data1$Specialisme = 'Totaal'
  # IC
  data2 = read_excel(paste0(bron, "# naam map/",naam,".xlsx"), sheet = 2, range = "A53:F56")
  data2 = data2[c(3:nrow(data2)),]
  data2$Toelichting = NULL
  names(data2) = c('Variabele', 'Organisatie', 'Locatie 1', 'Locatie 2', 'Locatie 3')
  data2$Specialisme = 'Totaal'
  #Polikliniekbezoeken
  data3 = read_excel(paste0(bron, "# naam map/",naam,".xlsx"), sheet = 2, range = "A60:E88")
  data3 = data3[c(3:nrow(data3)),]
  names(data3) = c('Specialisme', 'Organisatie', 'Locatie 1', 'Locatie 2', 'Locatie 3')
  data3$Variabele = 'Polikliniekbezoeken'
  data3$Specialisme[data3$Specialisme == 'Totaal aantal polikliniekbezoeken in 2022'] = 'Totaal'
  #OK-tijd
  data4 = read_excel(paste0(bron, "# naam map/",naam,".xlsx"), sheet = 2, range = "A92:E120")
  data4 = data4[c(3:nrow(data4)),]
  names(data4) = c('Specialisme', 'Organisatie', 'Locatie 1', 'Locatie 2', 'Locatie 3')
  data4$Variabele = 'OK-tijd'
  data4$Specialisme[data4$Specialisme == 'Totale OK-tijd in uren in 2022'] = 'Totaal'
  #Dagbehandeling
  data5 = read_excel(paste0(bron, "# naam map/",naam,".xlsx"), sheet = 2, range = "A124:E152")
  data5 = data5[c(3:nrow(data5)),]
  names(data5) = c('Specialisme', 'Organisatie', 'Locatie 1', 'Locatie 2', 'Locatie 3')
  data5$Variabele = 'Dagbehandelingen'
  data5$Specialisme[data5$Specialisme == 'Totaal aantal dagbehandelingen in 2022'] = 'Totaal'
  #Verpleegdagen
  data6 = read_excel(paste0(bron, "# naam map/",naam,".xlsx"), sheet = 2, range = "A156:E184")
  data6 = data6[c(3:nrow(data6)),]
  names(data6) = c('Specialisme', 'Organisatie', 'Locatie 1', 'Locatie 2', 'Locatie 3')
  data6$Variabele = 'Verpleegdagen'
  data6$Specialisme[data6$Specialisme == 'Totaal aantal verpleegdagen in 2022'] = 'Totaal'
  #Radiotherapie
  data7 = read_excel(paste0(bron, "# naam map/",naam,".xlsx"), sheet = 2, range = "A188:E192")
  data7 = data7[c(3:nrow(data7)),]
  names(data7) = c('Variabele', 'Organisatie', 'Locatie 1', 'Locatie 2', 'Locatie 3')
  data7$Specialisme = 'Totaal'
  data7$Variabele[data7$Variabele == 'Totaal aantal interventies radiotherapie'] = 'Interventies radiotherapie'
  data7$Variabele[data7$Variabele == 'Totaal aantal fracties radiotherapie'] = 'Fracties radiotherapie'
  #Financieel
  data8 = read_excel(paste0(bron, "# naam map/",naam,".xlsx"), sheet = 2, range = "A196:B222")
  names(data8) = c('Specialisme', 'Organisatie')
  data8$Variabele = 'Omzet'
  data8$Specialisme[data8$Specialisme == "Totale omzet uit DBC's en OZP's geopend in 2022"] = 'Totaal'
  #Radiotherapie maag/slokdarm aantallen
  data9 = read_excel(paste0(bron, "# naam map/",naam,".xlsx"), sheet = 2, range = "A248:B249")
  names(data9) = c('Variabele', 'Organisatie')
  data9$Variabele = 'Aantallen radiotherapie maag/slokdarm'
  data9$Specialisme = 'Totaal'
  #Samenvoegen
  data = bind_rows(data1, data2, data3, data4, data5, data6, data7, data8, data9)
  data = setDT(data) %>% melt(id.vars=c('Variabele', 'Specialisme'), variable.name = 'Locatie', value.name = 'Waarde')
  data$Instelling = instelling
  data = merge(data, locaties, by = 'Locatie', all.x = T)
  data$Waarde = as.numeric(data$Waarde)
  data_tot = bind_rows(data_tot, data)
}

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#--------------------------------- Oncologie ----------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

koppel_excel_O  = koppel_excel[koppel_excel$`Type data-uitvraag` == 'Oncologie',]

for(i in 1:nrow(koppel_excel_O)){
  
  instelling      = koppel_excel_O[[i, "Instelling"]]
  naam            = koppel_excel_O[[i, "Naam excelbestand kengetallen"]]
  
  #Namen locaties
  
  # Overzicht locaties maken
  locaties2 = read_excel(paste0(bron, "# naam map/",naam,".xlsx"), sheet = 2, range = "A14:D16")
  setnames(locaties2, '...1', 'Variabele')
  locaties2$Organisatie = c(instelling, 'nvt')
  locaties = data.frame(t(locaties2[,-1]))
  locaties = locaties %>% rownames_to_column(var="Locatie")
  names(locaties) = c('Locatie', 'Locatienaam', 'Locatieplaats')
  locaties$Locatie = c('Locatie 1', 'Locatie 2', 'Locatie 3', 'Organisatie')
  
  # Faciliteiten
  data1 = read_excel(paste0(bron, "# naam map/",naam,".xlsx"), sheet = 2, range = "A38:F47")
  data1 = data1[c(3:nrow(data1)),]
  data1$Toelichting = NULL
  names(data1) = c('Variabele', 'Organisatie', 'Locatie 1', 'Locatie 2', 'Locatie 3')
  data1$Specialisme = 'Totaal'
  # IC
  data2 = read_excel(paste0(bron, "# naam map/",naam,".xlsx"), sheet = 2, range = "A50:F53")
  data2 = data2[c(3:nrow(data2)),]
  data2$Toelichting = NULL
  names(data2) = c('Variabele', 'Organisatie', 'Locatie 1', 'Locatie 2', 'Locatie 3')
  data2$Specialisme = 'Totaal'
  #Polikliniekbezoeken
  data3 = read_excel(paste0(bron, "# naam map/",naam,".xlsx"), sheet = 2, range = "A57:E85")
  data3 = data3[c(3:nrow(data3)),]
  names(data3) = c('Specialisme', 'Organisatie', 'Locatie 1', 'Locatie 2', 'Locatie 3')
  data3$Variabele = 'Polikliniekbezoeken'
  data3$Specialisme[data3$Specialisme == 'Totaal aantal polikliniekbezoeken in 2022'] = 'Totaal'
  #OK-tijd
  data4 = read_excel(paste0(bron, "# naam map/",naam,".xlsx"), sheet = 2, range = "A89:E117")
  data4 = data4[c(3:nrow(data4)),]
  names(data4) = c('Specialisme', 'Organisatie', 'Locatie 1', 'Locatie 2', 'Locatie 3')
  data4$Variabele = 'OK-tijd'
  data4$Specialisme[data4$Specialisme == 'Totale OK-tijd in uren in 2022'] = 'Totaal'
  #Dagbehandeling
  data5 = read_excel(paste0(bron, "# naam map/",naam,".xlsx"), sheet = 2, range = "A121:E149")
  data5 = data5[c(3:nrow(data5)),]
  names(data5) = c('Specialisme', 'Organisatie', 'Locatie 1', 'Locatie 2', 'Locatie 3')
  data5$Variabele = 'Dagbehandelingen'
  data5$Specialisme[data5$Specialisme == 'Totaal aantal dagbehandelingen in 2022'] = 'Totaal'
  #Verpleegdagen
  data6 = read_excel(paste0(bron, "# naam map/",naam,".xlsx"), sheet = 2, range = "A153:E181")
  data6 = data6[c(3:nrow(data6)),]
  names(data6) = c('Specialisme', 'Organisatie', 'Locatie 1', 'Locatie 2', 'Locatie 3')
  data6$Variabele = 'Verpleegdagen'
  data6$Specialisme[data6$Specialisme == 'Totaal aantal verpleegdagen in 2022'] = 'Totaal'
  #Radiotherapie
  data7 = read_excel(paste0(bron, "# naam map/",naam,".xlsx"), sheet = 2, range = "A185:E189")
  data7 = data7[c(3:nrow(data7)),]
  names(data7) = c('Variabele', 'Organisatie', 'Locatie 1', 'Locatie 2', 'Locatie 3')
  data7$Specialisme = 'Totaal'
  data7$Variabele[data7$Variabele == 'Totaal aantal interventies radiotherapie'] = 'Interventies radiotherapie'
  data7$Variabele[data7$Variabele == 'Totaal aantal fracties radiotherapie'] = 'Fracties radiotherapie'
  #Financieel
  data8 = read_excel(paste0(bron, "# naam map/",naam,".xlsx"), sheet = 2, range = "A193:B219")
  names(data8) = c('Specialisme', 'Organisatie')
  data8$Variabele = 'Omzet'
  data8$Organisatie = as.numeric(data8$Organisatie)
  data8$Specialisme[data8$Specialisme == "Totale omzet uit DBC's en OZP's geopend in 2022"] = 'Totaal'
  #Radiotherapie maag/slokdarm aantallen
  data9 = read_excel(paste0(bron, "# naam map/",naam,".xlsx"), sheet = 2, range = "A245:B246")
  names(data9) = c('Variabele', 'Organisatie')
  data9$Variabele = 'Aantallen radiotherapie maag/slokdarm'
  data9$Specialisme = 'Totaal'
  #Samenvoegen
  data = bind_rows(data1, data2, data3, data4, data5, data6, data7, data8, data9)
  data = setDT(data) %>% melt(id.vars=c('Variabele', 'Specialisme'), variable.name = 'Locatie', value.name = 'Waarde')
  data$Instelling = instelling
  data = merge(data, locaties, by = 'Locatie', all.x = T)
  data$Waarde = as.numeric(data$Waarde)
  data_tot = bind_rows(data_tot, data)
}

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------ Radiotherapie ---------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

koppel_excel_R  = koppel_excel[koppel_excel$`Type data-uitvraag` == 'Radiotherapie',]

for(i in 1:nrow(koppel_excel_R)){
  
  instelling      = koppel_excel_R[[i, "Instelling"]]
  naam            = koppel_excel_R[[i, "Naam excelbestand kengetallen"]]
  
  #Namen locaties
  
  # Overzicht locaties maken
  locaties2 = read_excel(paste0(bron, "# naam map/",naam,".xlsx"), sheet = 2, range = "A10:E12")
  setnames(locaties2, '...1', 'Variabele')
  locaties2$Organisatie = c(instelling, 'nvt')
  locaties = data.frame(t(locaties2[,-1]))
  locaties = locaties %>% rownames_to_column(var="Locatie")
  names(locaties) = c('Locatie', 'Locatienaam', 'Locatieplaats')
  locaties$Locatie = c('Locatie 1', 'Locatie 2', 'Locatie 3', 'Locatie 4', 'Organisatie')
  
  # Faciliteiten
  data1 = read_excel(paste0(bron, "# naam map/",naam,".xlsx"), sheet = 2, range = "A23:E26")
  data1 = data1[c(3:nrow(data1)),]
  names(data1) = c('Variabele', 'Locatie 1', 'Locatie 2', 'Locatie 3', 'Locatie 4')
  data1$Specialisme = 'Totaal'
  #Polikliniekbezoeken
  data3 = read_excel(paste0(bron, "# naam map/",naam,".xlsx"), sheet = 2, range = "A30:E35")
  data3 = data3[c(3:nrow(data3)),]
  names(data3) = c('Specialisme', 'Locatie 1', 'Locatie 2', 'Locatie 3', 'Locatie 4')
  data3$Variabele = 'Polikliniekbezoeken'
  data3$Specialisme[data3$Specialisme == 'Totaal aantal polikliniekbezoeken in 2022'] = 'Totaal'
  #Radiotherapie
  data7 = read_excel(paste0(bron, "# naam map/",naam,".xlsx"), sheet = 2, range = "A39:E43")
  data7 = data7[c(3:nrow(data7)),]
  names(data7) = c('Variabele', 'Locatie 1', 'Locatie 2', 'Locatie 3', 'Locatie 4')
  data7$Specialisme = 'Totaal'
  data7$Variabele[data7$Variabele == 'Totaal aantal interventies radiotherapie'] = 'Interventies radiotherapie'
  data7$Variabele[data7$Variabele == 'Totaal aantal fracties radiotherapie'] = 'Fracties radiotherapie'
  #Financieel
  data8 = read_excel(paste0(bron, "# naam map/",naam,".xlsx"), sheet = 2, range = "A47:B50")
  names(data8) = c('Specialisme', 'Organisatie')
  data8$Variabele = 'Omzet'
  data8$Specialisme[data8$Specialisme == "Totale omzet uit DBC's en OZP's geopend in 2022"] = 'Totaal'
  #Radiotherapie maag/slokdarm aantallen
  data9 = read_excel(paste0(bron, "# naam map/",naam,".xlsx"), sheet = 2, range = "A66:B67")
  names(data9) = c('Variabele', 'Organisatie')
  data9$Variabele = 'Aantallen radiotherapie maag/slokdarm'
  data9$Specialisme = 'Totaal'
  #Samenvoegen
  data = bind_rows(data1, data3, data7, data8, data9)
  data = setDT(data) %>% melt(id.vars=c('Variabele', 'Specialisme'), variable.name = 'Locatie', value.name = 'Waarde')
  
  data$Waarde = as.numeric(data$Waarde)
  
  #Totalen toevoegen
  R_tot = data[!data$Locatie == 'Organisatie',]
  R_tot = R_tot %>% dplyr::group_by(Variabele, Specialisme) %>% dplyr::summarise(Waarde = sum(Waarde, na.rm=T))
  R_tot$Locatie = 'Organisatie'
  data = bind_rows(data, R_tot) 
  
  data$Instelling = instelling
  data = merge(data, locaties, by = 'Locatie', all.x = T)
  data_tot = bind_rows(data_tot, data)
}

# Rijen met lege waarde wissen
data_tot = data_tot[!is.na(data_tot$Waarde),]
data_tot = data_tot[!data_tot$Waarde == 0,]

# In de oorspronkelijke uitvraag stond een andere specialismecode bij radiologie (360 i.p.v. 362)
# Voor Oost-NL hebben we later data uitgevraagd en de specialismecode aangepast naar 362
# Om alles goed te laten koppelen passen we Oost-NL terug naar 360 aan:
data_tot$Specialisme[data_tot$Specialisme == '362 - Radiologie'] = '360 - Radiologie'

# Dataframes wissen
rm(koppel_excel, data, data1, data2, data3, data4, data5, data6, data7,
   data8, data9, koppel_excel_O, koppel_excel_OV, koppel_excel_R, locaties, locaties2,
   R_tot)

# De NZa had geen goede cijfers over de aantallen van radiotherapie maag/slokdarm
# Daarom hebben we die uitgevraagd in de kengetallen
# We slaan deze apart op:
RT_MS = data_tot[data_tot$Variabele == 'Aantallen radiotherapie maag/slokdarm',]

# Aparte dataset maken voor het overzicht van de faciliteiten per instelling
# En de kengetallen
# Dus data_tot = kengetallen
data_tot$Variabele[data_tot$Variabele == 'Aantal IC-dagen (exclusief neonatale en kinder-IC)'] = 'IC-dagen'
faciliteiten = data_tot[!data_tot$Variabele %in% c('Polikliniekbezoeken', 'OK-tijd', 'IC-dagen', 'Dagbehandelingen',
                                                   'Verpleegdagen', 'Interventies radiotherapie',
                                                   'Fracties radiotherapie', 'Omzet'),]
data_tot = data_tot[data_tot$Variabele %in% c('Polikliniekbezoeken', 'OK-tijd', 'IC-dagen', 'Dagbehandelingen',
                                              'Verpleegdagen', 'Interventies radiotherapie',
                                              'Fracties radiotherapie', 'Omzet'),]

# In de impactanalyse zetten we voor de relatieve impact de absolute impact af tegen:
# - De totalen per organisatie
# - De totalen per vakgroep/specialisme
# Daarom maken we hier twee aparte datasets voor de totalen (organisatie/vakgroep)
# Dataset voor kengetallen specialismen:
data_spl = data_tot[data_tot$Specialisme != 'Totaal',]
data_spl = data_spl[data_spl$Locatie == 'Organisatie',]
# Dataset voor totalen (organisatie/locatie)
data_tot = data_tot[data_tot$Specialisme == 'Totaal',]

# Radiotherapie toewijzen aan specialisme radiotherapie
# Interventies niet nodig - we zetten het alleen af tegen aantal fracties
data_spl = data_spl[!data_spl$Variabele == 'Interventies radiotherapie',]
data_spl$Specialisme[data_spl$Variabele == 'Fracties radiotherapie'] = "361 - Radiotherapie"

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------ SEH-bezoeken ----------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# Het totaal aantal SEH-bezoeken per organisatie/locatie is aangeleverd door
# de proxytafel. Deze moeten we dus nog toevoegen aan de kengetallen.

# Kolommen voor locatienaam en locatieplaats netjes zetten
# Zodat we kunnen koppelen aan de tabel met SEH-bezoeken
# Als locatienaam niet is ingevuld dan geen data, dus verwijderen
data_tot = data_tot[!is.na(data_tot$Locatienaam),]
data_tot$Locatienaam = ifelse(data_tot$Locatie == 'Organisatie', 'Totaal ziekenhuis', data_tot$Locatienaam)
data_tot$Locatieplaats = ifelse(data_tot$Locatie == 'Organisatie', 'Totaal ziekenhuis', data_tot$Locatieplaats)
data_tot$Locatie = NULL

# SiRM locatienamen koppelen
koppel_locaties = read_excel(paste0(analyse, '# Koppeltabel'), sheet = 'Locaties ziekenhuizen')
# Bij sommige ziekenhuizen geen data uitgevraagd (doen geen tranche 1)
# Of soms geen data per locatie aangeleverd
# Die locaties/ziekenhuizen verwijderen uit koppeltabel
koppel_locaties = koppel_locaties[!is.na(koppel_locaties$Locatienaam),]
koppel_locaties = unique(subset(koppel_locaties, select = -c(pc4, Locatienaam_proxy_SEH, Locatieplaats_proxy_SEH)))
# Koppelen - elke naam van een ziekenhuislocatie wordt op dezelfde manier weergegeven
data_tot = merge(data_tot, koppel_locaties, by = c('Instelling','Locatienaam', 'Locatieplaats'), all.x = T)

# Check of alles goed gekoppeld is
x = data_tot[is.na(data_tot$Locatienaam_SiRM),]
x = unique(x[,c('Instelling', 'Locatienaam', 'Locatieplaats')]) #Alleen 'totaal ziekenhuis'
# Dus locatienamen goed gekoppeld
# Als het kengetallen van de hele organisatie zijn en niet per locatie
# Dan in kolommen voor locatie 'totaal ziekenhuis' invullen
data_tot$Locatienaam_SiRM[is.na(data_tot$Locatienaam_SiRM)] = 'Totaal ziekenhuis'
data_tot$Locatieplaats_SiRM[is.na(data_tot$Locatieplaats_SiRM)] = 'Totaal ziekenhuis'
data_tot = subset(data_tot, select = -c(Locatienaam, Locatieplaats))
setnames(data_tot, c('Locatienaam_SiRM', 'Locatieplaats_SiRM'), c('Locatienaam', 'Locatieplaats'))

# Nu SEH-bezoeken koppelen
proxy_SEH = read_excel(paste0('# Databestand SEH bezoeken'))
names(proxy_SEH) = c('Locatienaam_proxy_SEH', 'Waarde')
proxy_SEH = proxy_SEH[c(3:83),]
# Ziekenhuisnamen omzetten
koppel_locaties = read_excel(paste0(analyse, '# Koppeltabel'), sheet = 'Locaties ziekenhuizen')
koppel_locaties = koppel_locaties[,c('Instelling', 'Locatienaam_SiRM', 'Locatieplaats_SiRM', 'Locatienaam_proxy_SEH')]
# Rij verwijderen als kolom 'Locatienaam_proxy_SEH' NA is --> locatie heeft geen SEH
koppel_locaties = koppel_locaties[!is.na(koppel_locaties$Locatienaam_proxy_SEH),]
proxy_SEH = merge(proxy_SEH, koppel_locaties, by = 'Locatienaam_proxy_SEH', all.x = T)
proxy_SEH$Locatienaam_proxy_SEH = NULL
setnames(proxy_SEH, c('Locatienaam_SiRM', 'Locatieplaats_SiRM'), c('Locatienaam', 'Locatieplaats'))

# Locaties optellen als ze maar 1 locatie hebben aangeleverd (Amsterdam UMC)
proxy_SEH = proxy_SEH %>% dplyr::group_by(Instelling, Locatienaam, Locatieplaats) %>%
  dplyr::summarise(Waarde = sum(as.numeric(Waarde), na.rm=T))

# Ook het totaal aantal SEH-bezoeken per organisatie bepalen i.p.v. per locatie
# (voor als ziekenhuis meerdere locaties met SEH heeft)
proxy_SEH_tot = proxy_SEH %>% dplyr::group_by(Instelling) %>%
  dplyr::summarise(Waarde = sum(as.numeric(Waarde), na.rm=T))
proxy_SEH_tot$Locatienaam = 'Totaal ziekenhuis'
proxy_SEH_tot$Locatieplaats = 'Totaal ziekenhuis'
proxy_SEH$Waarde = as.numeric(proxy_SEH$Waarde)
proxy_SEH = bind_rows(proxy_SEH, proxy_SEH_tot)

# Overige kolommen toevoegen zodat we kunnen samenvoegen met data_tot
proxy_SEH$Specialisme = 'Totaal'
proxy_SEH$Variabele = 'SEH-bezoeken'

# Voor drie ziekenhuizen heeft de proxytafel geen SEH-bezoeken aangeleverd
# Deze hebben we zelf aangevuld via online bronnen
SEH_ontbrekend = read_excel(paste0(analyse, '# Data overige gegevens'), sheet = 'Ontbrekende SEH-bezoeken')
SEH_ontbrekend$Bron = NULL
proxy_SEH = proxy_SEH[!proxy_SEH$Waarde == 0,]
proxy_SEH = bind_rows(proxy_SEH, SEH_ontbrekend)

# SEH-bezoeken toevoegen aan kengetallen
data_tot = bind_rows(data_tot, proxy_SEH)

# AUMC handmatig aanpassen, die heeft alles op totale organisatie
data_tot$Locatienaam[data_tot$Instelling == 'Amsterdam UMC'] = 'Totaal ziekenhuis'
data_tot$Locatieplaats[data_tot$Instelling == 'Amsterdam UMC'] = 'Totaal ziekenhuis'
data_tot = unique(data_tot)

# Locatienaam goed invullen voor PowerBI
data_tot$Locatieplaats2 = sub(".*, ", "", data_tot$Locatienaam)
data_tot$Locatieplaats2 = sub(".*locatie ", "", data_tot$Locatieplaats2)
data_tot$Specialisme = ifelse(data_tot$Locatienaam == 'Totaal ziekenhuis', data_tot$Locatienaam,paste0(data_tot$Instelling, " locatie ", data_tot$Locatieplaats2))
data_tot$Locatieplaats2 = NULL

rm(x, koppel_locaties, proxy_SEH, proxy_SEH_tot, SEH_ontbrekend)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------ Schatting totale scopietijd -------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# Om voor de scopietijd ook de relatieve impact te kunnen berekenen
# Moeten we de totale scopietijd per ziekenhuis schatten

scopiekamers = faciliteiten[faciliteiten$Variabele == 'Aantal scopiekamers in zorginstelling',]

# Berekening capaciteit scopiekamers (uitleg proxytafel):
# - Een scopiekamer is gemiddeld tussen 08.00-16.00 uur beschikbaar. Dat zijn acht uur. De berekening gaat uit van vijf werkdagen in een week.
# - De gemiddeld beschikbare capaciteit per scopiekamer per dag is gelijk aan de tijd dat de kamer beschikbaar is op een dag.
# - Het betreft de beschikbaarheid volgens het vaste programma voor de scopiekamers dat wordt ingepland.
# - Tijd voor uitloop, wisseltijd, spoed en weekenddiensten worden niet meegenomen bij het berekenen van de capaciteit. 
# - Gemiddeld aantal werkweken per jaar - proxy van proxytafel = 46

scopiekamers = scopiekamers[scopiekamers$Locatie == 'Organisatie',]
scopiekamers = subset(scopiekamers, select = -c(Locatie, Locatienaam, Locatieplaats))
scopiekamers$Waarde = scopiekamers$Waarde * 8 * 5 * 46
length(unique(scopiekamers$Instelling))

# Toevoegen aan kengetallen:
scopiekamers$Variabele = 'Scopietijd'
scopiekamers$Specialisme = 'Totaal ziekenhuis'
scopiekamers$Locatienaam = 'Totaal ziekenhuis'
scopiekamers$Locatieplaats = 'Totaal ziekenhuis'
data_tot = bind_rows(data_tot, scopiekamers)

rm(scopiekamers)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------- Tabel met totalen opslaan --------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# Omdat voor sommige interventies uit tranche 1 de zorgactiviteiten onder meerdere
# specialismes kunnen vallen, zetten we de absolute impact van een verschuiving
# van die interventies af van de som van de relevantie specialismes
# Bijvoorbeeld: oncologische longresecties heeft betrekking tot chirurgie 
# en cardiothoracale chirurgie
# Hiervoor hebben we een koppeltabel gemaakt:
koppel_specialisme = unique(koppel_specialisme[, c('Specialisme', 'Specialisme_optellen')])
data_spl = merge(data_spl, koppel_specialisme, by = 'Specialisme', allow.cartesian = T)
rm(koppel_specialisme)
data_spl = data_spl %>% dplyr::group_by(Instelling, Locatienaam, Locatieplaats, Specialisme_optellen, 
                                          Variabele) %>%
  dplyr::summarise(Waarde = sum(Waarde, na.rm = T))
setnames(data_spl, 'Specialisme_optellen', 'Specialisme')

data_tot$Variabele[data_tot$Variabele == 'Omzet'] = 'Bruto schadelast'
data_spl$Variabele[data_spl$Variabele == 'Omzet'] = 'Bruto schadelast'

# Omzet per specialisme verwijderen uit kengetallen - gebruiken we uiteindelijk niet vanwege vertrouwelijkheid
data_spl = data_spl[!data_spl$Variabele == 'Bruto schadelast',]

# Tabellen met kengetallen opslaan - dit is weer input voor volgende R-code
write_xlsx(data_tot, paste0(analyse, '# Kengetallen instellingen'))
write_xlsx(data_spl, paste0(analyse, '# Kengetallen instellingen - specialisme'))

