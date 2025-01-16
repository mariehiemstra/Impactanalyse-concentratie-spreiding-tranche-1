#-------------------------------------------------------------------------------------------------------------------------------------------------------------
#Project: Impactanalyse concentratie en spreiding
#Auteur:  SiRM
#Datum:   januari 2025
#Doel:    Koppelen bestanden voor PowerBI (3b)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------

#Clear all variables in R's memory
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

# Kengetallen
data_ZA            = read_excel(paste0(analyse, '# Databestand gebruik faciliteiten'))
# Proxy's
data_tot           = read_excel(paste0(analyse, '# Databestand kengetallen instellingen'))
data_spl           = read_excel(paste0(analyse, '# Databestand kengetallen instellingen - Specialisme'))
data_tijd          = read_excel(paste0(analyse, '# Databestand tijd zorgprofessionals'))
# Koppeltabellen
loc_interventies   = read_excel(paste0(analyse, '# Koppeltabellen'), sheet = "Interventies - locaties")
namen_interventies = read_excel(paste0(analyse, '# Koppeltabellen'), sheet = "Namen interventies kort")
koppel_zp          = read_excel(paste0(input_model, '# Koppeltabellen'), sheet = 'Zorgprofessionals')
namen_kort         = read_excel(paste0(input_model, '# Koppeltabellen'), sheet = 'Instellingsnamen kort')

combi_scenarios    = read_excel(paste0(analyse, '# Databestand combi scenarios'))

van_naar           = read_excel(paste0(input_model, '# Databestand herverdeling van en naar'))
nier               = read_excel(paste0(input_model, '# Databestand behandelingen nier per ziekenhuis'))

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#-------------------- Proxy's nier/AAA complex omzetten -----------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# Normaal gesproken rekenen we voor de impact op het ontvangende ziekenhuis
# met de proxy's van het ontvangende ziekenhuis
# Maar bij nier - lokale behandelingen en AAA complex is dat niet logisch
# Bijv. als het ziekenhuis op dit moment alleen focale therapie doet
# Maar partiële nefrectomieën ontvangt
# Dus voor deze behandelingen moeten we rekenen met de proxy's van het 
# zendende ziekenhuis

# Check of t ook goed is als proxy 0 is en hij niet in de data zit
# van_naar: omschrijft per scenario/interventie van welk ziekenhuis
# naar welk ziekenhuis de interventies verschuiven en hoeveel
van_naar = van_naar[!van_naar$aantal_verplaatsing == 0,]

# Soms ontvangt een ziekenhuis van meerdere ziekenhuizen interventie
# Daarvoor: per ontvangend ziekenhuis/interventie/scenario berekenen
# welk aandeel van welk ziekenhuis komt
# Dit aandeel gebruiken we om later een gewogen gemiddelde te nemen
# van de proxy's van de zendende ziekenhuizen
van_naar_tot = van_naar %>% dplyr::group_by(Instelling_na_stap_2, Interventie, Scenario) %>%
  dplyr::summarise(aantal_tot = sum(aantal_verplaatsing, na.rm = T))
van_naar = merge(van_naar, van_naar_tot, by = c('Instelling_na_stap_2', 'Interventie', 'Scenario'), all.x = T)
van_naar$W = van_naar$aantal_verplaatsing / van_naar$aantal_tot
van_naar = subset(van_naar, select = -c(aantal_verplaatsing, aantal_tot))

# Eerst de proxy's voor de impact op de zorgaanbieder omzetten:
van_naar_ZA = merge(van_naar, data_ZA, by = c('Interventie', 'Instelling'), all.x = T)
# Alleen voor lokale behandelingen nier, algemene aorta interventies en 
# maag/slokdarm (operaties/endoscopische resecties)
# rekenen we met de proxy's van het zendende ziekenhuis i.p.v. ontvangend
van_naar_ZA = van_naar_ZA[van_naar_ZA$Interventie %in% c('Nier - lokale behandelingen', 'AAA - aorta interventies (complex)'),]
van_naar_ZA$Waarde_instelling = van_naar_ZA$Waarde_instelling * van_naar_ZA$W
van_naar_ZA$Waarde_landelijk = van_naar_ZA$Waarde_landelijk * van_naar_ZA$W
van_naar_ZA = van_naar_ZA %>% dplyr::group_by(Instelling_na_stap_2, Interventie, Scenario, Variabele, Specialisme) %>%
  dplyr::summarise(Waarde_landelijk = sum(Waarde_landelijk, na.rm = T),
                   Waarde_instelling = sum(Waarde_instelling, na.rm = T))
setnames(van_naar_ZA, 'Instelling_na_stap_2', 'Instelling')
van_naar_ZA = setDT(van_naar_ZA) %>% melt(id.vars = c('Instelling', 'Interventie', 'Scenario', 'Variabele', 'Specialisme'), 
                     variable.name = 'Type_proxy', value.name = 'Impact_per_interventie_c')

# Nu de proxy's voor de impact op de uren van zorgprofessionals omzetten:
van_naar_tijden = merge(van_naar, data_tijd, by = c('Interventie', 'Instelling'), all.x = T)
van_naar_tijden = van_naar_tijden[van_naar_tijden$Interventie %in% c('Nier - lokale behandelingen', 'AAA - aorta interventies (complex)'),]
van_naar_tijden$Waarde_instelling = van_naar_tijden$Waarde_instelling * van_naar_tijden$W
van_naar_tijden$Waarde_landelijk = van_naar_tijden$Waarde_landelijk * van_naar_tijden$W
van_naar_tijden = van_naar_tijden %>% dplyr::group_by(Instelling_na_stap_2, Interventie, Scenario, Zorgprofessional) %>%
  dplyr::summarise(Waarde_landelijk = sum(Waarde_landelijk, na.rm = T),
                   Waarde_instelling = sum(Waarde_instelling, na.rm = T))
setnames(van_naar_tijden, 'Instelling_na_stap_2', 'Instelling')

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------- Tabel faciliteiten maken ---------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# Vervolgens koppelen we aan alle scenario's de impact voor de verschuiving van 1 interventie
# Dus er worden per scenario / Interventie / Ziekenhuis rijen toegevoegd voor impact op OK-tijd, IC-dagen, etc.

# Long format
data_ZA = melt(setDT(data_ZA), id.vars = c('Instelling', 'Variabele', 'Interventie', 'Specialisme'),
                variable.name = 'Type_proxy', value.name = 'Impact_per_interventie')
# Scenario's koppelen aan tabel met impact op zorgaanbieder per interventie
ZA = merge(combi_scenarios, data_ZA, by = c('Instelling', 'Interventie'), allow.cartesian = T, all.x = T)

# Bij AAA complex, maag/slokdarm (operaties/endoscopische resecties) 
# en nier lokale behandelingen vervangen door proxy's zendend ziekenhuis
setnames(van_naar_ZA, 'Scenario', 'Subscenario')
ZA = merge(ZA, van_naar_ZA, by = c('Instelling', 'Interventie', 'Variabele', 'Specialisme', 'Subscenario', 'Type_proxy'), all.x = T)
ZA$Impact_per_interventie = ifelse(is.na(ZA$Impact_per_interventie_c), ZA$Impact_per_interventie, ZA$Impact_per_interventie_c)
ZA$Impact_per_interventie_c = NULL
setnames(van_naar_ZA, 'Subscenario', 'Hoofdscenario')
ZA = merge(ZA, van_naar_ZA, by = c('Instelling', 'Interventie', 'Variabele', 'Specialisme', 'Hoofdscenario', 'Type_proxy'), all.x = T)
ZA$Impact_per_interventie = ifelse(is.na(ZA$Impact_per_interventie_c), ZA$Impact_per_interventie, ZA$Impact_per_interventie_c)
ZA$Impact_per_interventie_c = NULL
setnames(van_naar_ZA, 'Hoofdscenario', 'Subscenario')
ZA = merge(ZA, van_naar_ZA, by = c('Instelling', 'Interventie', 'Variabele', 'Specialisme', 'Subscenario', 'Type_proxy'), all.x = T)
ZA$Impact_per_interventie = ifelse(is.na(ZA$Impact_per_interventie_c), ZA$Impact_per_interventie, ZA$Impact_per_interventie_c)
ZA$Impact_per_interventie_c = NULL
setnames(van_naar_ZA, 'Subscenario', 'Vaatscenario')
ZA = merge(ZA, van_naar_ZA, by = c('Instelling', 'Interventie', 'Variabele', 'Specialisme', 'Vaatscenario', 'Type_proxy'), all.x = T)
ZA$Impact_per_interventie = ifelse(is.na(ZA$Impact_per_interventie_c), ZA$Impact_per_interventie, ZA$Impact_per_interventie_c)
ZA$Impact_per_interventie_c = NULL

ZA$Type_proxy = as.character(ZA$Type_proxy)
van_naar_ZA$Type_proxy = as.character(van_naar_ZA$Type_proxy)


setnames(van_naar_ZA, old = c('Vaatscenario',"Variabele", "Specialisme", "Type_proxy"), 
         new = c('Hoofdscenario', "Variabele_c", "Specialisme_c","Type_proxy_c"))
van_naar_ZA = van_naar_ZA[grepl("endoscopisch", van_naar_ZA$Interventie)]
ZA = left_join(ZA, van_naar_ZA, by = c('Instelling', 'Interventie', 'Hoofdscenario'))
ZA$Specialisme = ifelse(is.na(ZA$Specialisme), ZA$Specialisme_c, ZA$Specialisme)
ZA$Variabele   = ifelse(is.na(ZA$Variabele), ZA$Variabele_c, ZA$Variabele)
ZA$Type_proxy  = ifelse(is.na(ZA$Type_proxy), ZA$Type_proxy_c, ZA$Type_proxy)
ZA$Impact_per_interventie   = ifelse(is.na(ZA$Impact_per_interventie), ZA$Impact_per_interventie_c, ZA$Impact_per_interventie)
ZA = subset(ZA, select = -c(Impact_per_interventie_c, Specialisme_c, Variabele_c, Type_proxy_c))

# We zetten de impact per interventie af tegen drie verschillende totalen (totale OK-tijd, totaal aantal IC-dagen, etc.)
# - Totalen per specialisme, bijv. totale OK-tijd voor chirurgie voor de gehele organisatie
# - Totale OK-tijd van de organisatie (dus van alle specialismen opgeteld)
# - Totale OK-tijd van de locatie van het ziekenhuis waar de interventie wordt uitgevoerd (is minder dan totaal organisatie als ziekenhuis meerdere operatielocaties heeft)
# Eerst de kengetallen op specialismeniveau koppelen:
ZA1 = ZA[ZA$Specialisme != 'Totaal',]
ZA1 = ZA1[!ZA1$Variabele %in% c('Bruto schadelast', 'SEH-bezoeken'),] #Bekijken we relatief gezien alleen t.o.v. totaal en niet per specialisme
ZA1 = merge(ZA1, data_spl, by = c('Instelling', 'Specialisme', 'Variabele'), all.x = T)
# Nu kengetallen op totaalniveau - organisatie
ZA2 = ZA[ZA$Specialisme == 'Totaal',]
data_tot_org     = data_tot[data_tot$Specialisme == 'Totaal ziekenhuis',]
ZA2$Specialisme = 'Totaal ziekenhuis'
ZA2 = merge(ZA2, data_tot_org, by = c('Instelling', 'Variabele', 'Specialisme'), all.x = T)
ZA2$Locatienaam = 'Totaal ziekenhuis'
ZA2$Locatieplaats = 'Totaal ziekenhuis'
# En alle totalen voor de locaties
ZA3 = ZA[ZA$Specialisme == 'Totaal',]
ZA3 = ZA3[!ZA3$Variabele %in% c('Bruto schadelast', 'Scopietijd', 'Kosten'),] #Deze info hebben we niet op locatieniveau
loc_interventies$pc4_locatie = NULL
ZA3 = merge(ZA3, loc_interventies, by = c('Instelling', 'Aandoening', 'Interventie'), all.x = T)
data_tot_loc     = data_tot[data_tot$Specialisme != 'Totaal ziekenhuis',] #Totalen (kengetallen) per locatie
# Filter netjes maken voor PowerBI:
ZA3$Locatieplaats2 = sub(".*, ", "", ZA3$Locatienaam)
ZA3$Locatieplaats2 = sub(".*locatie ", "", ZA3$Locatieplaats2)
ZA3$Specialisme = paste0(ZA3$Instelling, " locatie ", ZA3$Locatieplaats2)  #Ik zou dit een andere naam geven of Specialisme/locatie
ZA3$Locatieplaats2 = NULL
ZA3 = merge(ZA3, data_tot_loc, by = c('Instelling', 'Variabele', 'Specialisme', 'Locatienaam'), all.x = T)
ZA3 = ZA3[!is.na(ZA3$Waarde),] #Alles waarvoor geen totalen per locatie beschikbaar zijn verwijderen

ZA = bind_rows(ZA1, ZA2, ZA3)
rm(ZA1, ZA2, ZA3, data_ZA, data_spl, data_tot, data_tot_loc, data_tot_org)

# Nu vermenigvuldigen met de aantallen om de impact te bepalen:
ZA$`verandering absoluut` = ZA$Aantal_verschil * ZA$Impact_per_interventie
ZA$`verandering relatief (%)` = ZA$`verandering absoluut` / ZA$Waarde
ZA$`verandering relatief (%)`[is.na(ZA$`verandering relatief (%)`)] = 0 #Voor t dashboard moeten hier de NA's op 0 stana

# Handmatig eventuele aanpassingen doen, bijv. omdat focale therapie niet op OK plaatsvindt
# Instelling specifieke informatie uit code gehaald

# Namen aanpassen
setnames(ZA, 'Waarde', 'huidig')
setnames(ZA, 'Impact_per_interventie', 'proxy')

# Totalen toevoegen (dus totale impact per scenario per instelling ipv per interventie)
ZA_tot = ZA %>% dplyr::group_by(Combinatie_hoofd_sub_vaat_scenario, Combinatie_regios, Variabele, Specialisme, Instelling,
                                  Type_proxy, Hoofdregio, Hoofdscenario, Subregio, Vaatnetwerk, Subscenario,
                                  Vaatscenario, `Regio impactanalyse`, Locatienaam, Locatieplaats) %>%
  dplyr::summarise(`verandering absoluut` = sum(`verandering absoluut`, na.rm = T),
                   `verandering relatief (%)` = sum(`verandering relatief (%)`, na.rm = T))

# Ranking bepalen
# Per regio/scenario/variabele: bij welk ziekenhuis neemt variabele relatief gezien het meest toe
# En bij welke het meest af
rang = copy(ZA_tot)
rang = rang[rang$Variabele %in% c('IC-dagen', 'OK-tijd', 'SEH-bezoeken', 'Verpleegdagen', 'Polikliniekbezoeken', 'Scopietijd', 'Bruto schadelast'),]
rang = rang[rang$Specialisme == 'Totaal ziekenhuis',]
rang = subset(rang, select = -c(`verandering absoluut`))
rang_max = rang %>% dplyr::group_by(Combinatie_hoofd_sub_vaat_scenario, Combinatie_regios, Variabele, Specialisme,
                                    Type_proxy, Hoofdregio, Hoofdscenario, Subregio, Vaatnetwerk, Subscenario,
                                    Vaatscenario, `Regio impactanalyse`) %>% slice_max(`verandering relatief (%)`)
rang_max$`ranking totaal` = 'grootste relatieve toename'
rang_min = rang %>% dplyr::group_by(Combinatie_hoofd_sub_vaat_scenario, Combinatie_regios, Variabele,Specialisme,
                                    Type_proxy, Hoofdregio, Hoofdscenario, Subregio, Vaatnetwerk, Subscenario,
                                    Vaatscenario, `Regio impactanalyse`) %>% slice_min(`verandering relatief (%)`)
rang_min$`ranking totaal` = 'grootste relatieve afname'
rang = bind_rows(rang_max, rang_min)
rang$`verandering relatief (%)` = NULL


# Flexibele titels grafieken PowerBI
rang$Titel = ifelse(is.na(rang$`ranking totaal`), NA,
                    ifelse(rang$`ranking totaal` == 'grootste relatieve toename', paste0('Grootste relatieve toename: ', rang$Instelling),
                           ifelse(rang$`ranking totaal` == 'grootste relatieve afname', paste0('Grootste relatieve afname: ', rang$Instelling), NA)))
rang$Variabele2 = rang$Variabele
rang$Variabele2[rang$Variabele2 == 'Verpleegdagen'] = 'verpleegdagen'
rang$Variabele2[rang$Variabele2 == 'Polikliniekbezoeken'] = 'polikliniekbezoeken'
rang$Titel2 = ifelse(is.na(rang$`ranking totaal`), NA,
                     paste0("Maximale impact op ", rang$Variabele2, " per ziekenhuis per scenario [percentage]"))
rang$Variabele2 = NULL

# Totale impact (dus niet per interventie) toevoegen aan dataset:
ZA_tot$Aandoening = 'Totaal'
ZA_tot$Interventie = 'Totaal'
ZA = bind_rows(ZA, ZA_tot)

# Ranking koppelen aan totalen
ZA = merge(ZA, rang, by = c('Combinatie_hoofd_sub_vaat_scenario', 'Combinatie_regios', 'Variabele', 'Specialisme', 'Instelling',
                              'Type_proxy', 'Hoofdregio', 'Hoofdscenario', 'Subregio', 'Vaatnetwerk', "Subscenario", 
                              'Vaatscenario', 'Regio impactanalyse', 'Locatienaam', 'Locatieplaats'), all.x = T)

rm(rang, rang_max, rang_min)

# Afronden:
# Kengetallen op tientallen afronden
ZA$huidig = round_any(ZA$huidig, 10)
# Relatieve verandering op hele percentages afronden
# Kolom onafgerond voor onszelf wel behouden
ZA$`verandering relatief (%) onafgerond` = ZA$`verandering relatief (%)`
ZA$`verandering relatief (%)` = round_any(ZA$`verandering relatief (%) onafgerond`, 0.01)
# Absolute verandering ook op tientallen afronden
ZA$`verandering absoluut` = round_any(ZA$`verandering absoluut`, 10)

# Namen instellingen korter maken:
ZA = merge(ZA, namen_kort, by = 'Instelling', all.x = T)
ZA$Instelling = NULL
setnames(ZA, c('Ziekenhuis_naam_kort'), c('Ziekenhuis'))

# Namen interventies aanpassen (korter)
ZA = merge(ZA, namen_interventies, by = c('Aandoening', 'Interventie'), all.x = T)
ZA$Naam_interventie_nieuw[ZA$Interventie == 'Totaal'] = 'Totaal'
ZA$Interventie = ZA$Naam_interventie_nieuw
ZA$Naam_interventie_nieuw = NULL

# Label type proxy aanpassen - zoals we het willen noemen in PowerBI
ZA$Type_proxy = as.character(ZA$Type_proxy)
ZA$Type_proxy[ZA$Type_proxy == 'Waarde_instelling'] = 'Proxywaarden aangepast door ziekenhuizen'
ZA$Type_proxy[ZA$Type_proxy == 'Waarde_landelijk'] = 'Proxywaarden landelijk'

# Titels absolute impact:
ZA$eenheid = ifelse(ZA$Variabele %in% c('SEH-bezoeken', 'IC-dagen', 'Polikliniekbezoeken','Verpleegdagen'), 'Aantal',
                     ifelse(ZA$Variabele %in% c('OK-tijd', 'Scopietijd'), 'Uur',
                            ifelse(ZA$Variabele %in% c('Bruto schadelast', 'Kosten'), 'Euro', NA)))
ZA$Titel3 = paste0('Absolute verandering in ', ZA$Variabele, ' per ziekenhuis voor geselecteerd scenario [', ZA$eenheid, ']')
ZA$Titel4 = paste0('Impact op totale ziekenhuis voor ', ZA$Variabele, ' [', ZA$eenheid, ']')


# Versie met onafgerond impact opslaan voor later
write.table(ZA, paste0(analyse, '# Databestand impact ZAiliteiten onafgerond'), sep = ";", row.names = F)
# Kolommen in logische volgorde zetten:
ZA = ZA[,c('Regio impactanalyse', 'Hoofdregio', 'Subregio', 'Vaatnetwerk', 'Combinatie_hoofd_sub_vaat_scenario',
             'Hoofdscenario', 'Subscenario', 'Vaatscenario', 'Ziekenhuis', 'Locatienaam', 'Locatieplaats', 'Aandoening',
             'Interventie', 'Specialisme', 'Aantal_huidig', 'Aantal_nieuw', 'Aantal_verschil', 'Variabele',
             'huidig', 'Type_proxy', 'proxy', 'verandering absoluut', 'verandering relatief (%)',
             'ranking totaal', 'Titel', 'Titel2', 'Titel3', 'Titel4')]


# regels besparen
ZA <- ZA[!ZA$Vaatnetwerk == "Alle vaatnetwerken",]
ZA <- ZA[!ZA$Subregio == "Alle subregio's",]
write_xlsx(ZA, paste0(analyse, '# Databestand inputbestand PowerBI faciliteiten'))

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#-------------------- Tabel tijd zorgprofessionals maken ----------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# Nu maken we een aparte tabel voor de impact m.b.t. de tijdsbesteding van zorgprofessionals
# Dus er worden per scenario / Interventie / Ziekenhuis rijen toegevoegd voor de extra tijd per zorgprofessional
data_tijd = melt(setDT(data_tijd), id.vars = c('Instelling', 'Zorgprofessional', 'Interventie'),
                 variable.name = 'Type_proxy', value.name = 'Tijd_per_interventie')
zp = merge(combi_scenarios, data_tijd, by = c('Instelling',  'Interventie'), allow.cartesian = T, all.x=T)

# Bij AAA complex, maag/slokdarmoperaties en nier lokale behandelingen vervangen door proxy's zendend ziekenhuis
setnames(van_naar_tijden, 'Scenario', 'Subscenario')
van_naar_tijden = melt(van_naar_tijden, id.vars = c('Instelling', 'Interventie', 'Subscenario',
                                                    'Zorgprofessional'), variable.name = 'Type_proxy', value.name = 'Tijd_per_interventie_c')
zp = merge(zp, van_naar_tijden, by = c('Instelling', 'Interventie', 'Zorgprofessional',  'Subscenario', 'Type_proxy'), all.x = T)
check = zp[zp$Tijd_per_interventie == 0 & is.na(zp$Tijd_per_interventie_c) == F & zp$Tijd_per_interventie_c != 0,]

zp$Tijd_per_interventie = ifelse(is.na(zp$Tijd_per_interventie_c), zp$Tijd_per_interventie, zp$Tijd_per_interventie_c)
zp$Tijd_per_interventie_c = NULL
setnames(van_naar_tijden, 'Subscenario', 'Hoofdscenario')
zp = merge(zp, van_naar_tijden, by = c('Instelling', 'Interventie', 'Zorgprofessional',  'Hoofdscenario', 'Type_proxy'), all.x = T)
zp$Tijd_per_interventie = ifelse(is.na(zp$Tijd_per_interventie_c), zp$Tijd_per_interventie, zp$Tijd_per_interventie_c)
zp$Tijd_per_interventie_c = NULL
setnames(van_naar_tijden, 'Hoofdscenario', 'Vaatscenario')
zp = merge(zp, van_naar_tijden, by = c('Instelling', 'Interventie', 'Zorgprofessional',  'Vaatscenario', 'Type_proxy'), all.x = T)
zp$Tijd_per_interventie = ifelse(is.na(zp$Tijd_per_interventie_c), zp$Tijd_per_interventie, zp$Tijd_per_interventie_c)
zp$Tijd_per_interventie_c = NULL

zp$Type_proxy <- as.character(zp$Type_proxy)
van_naar_tijden$Type_proxy <- as.character(van_naar_tijden$Type_proxy)

setnames(van_naar_tijden, old=c('Vaatscenario',"Zorgprofessional", "Type_proxy"), new=c('Hoofdscenario', "Zorgprofessional_c","Type_proxy_c"))
van_naar_tijden <- van_naar_tijden[grepl("endoscopisch", van_naar_tijden$Interventie),]
zp = merge(zp, van_naar_tijden, by = c('Instelling', 'Interventie', 'Hoofdscenario'), all.x = T)
zp$Zorgprofessional <- ifelse(is.na(zp$Zorgprofessional), zp$Zorgprofessional_c, zp$Zorgprofessional)
zp$Type_proxy <- ifelse(is.na(zp$Type_proxy), zp$Type_proxy_c, zp$Type_proxy)
zp$Tijd_per_interventie <- ifelse(is.na(zp$Tijd_per_interventie), zp$Tijd_per_interventie_c, zp$Tijd_per_interventie)
zp$Tijd_per_interventie_c = NULL
zp$Zorgprofessional_c = NULL
zp$Type_proxy_c = NULL

# We hebben geen totalen voor tijd van zorgprofessionals dus berekenen alleen de absolute impact
zp$`verandering absoluut` = zp$Aantal_verschil * zp$Tijd_per_interventie
setnames(zp, 'Tijd_per_interventie', 'proxy')

# Totalen toevoegen aan bestand
zp_tot = zp %>% dplyr::group_by(Combinatie_hoofd_sub_vaat_scenario, Combinatie_regios, Zorgprofessional, Instelling,
                                Type_proxy, Hoofdregio, Hoofdscenario, Subregio, Vaatnetwerk, Subscenario,
                                Vaatscenario, `Regio impactanalyse`) %>%
  dplyr::summarise(`verandering absoluut` = sum(`verandering absoluut`, na.rm = T))

# Ranking bepalen
rang = copy(zp_tot)
rang_max = rang %>% dplyr::group_by(Combinatie_hoofd_sub_vaat_scenario, Combinatie_regios, Zorgprofessional, 
                                    Type_proxy, Hoofdregio, Hoofdscenario, Subregio, Vaatnetwerk, Subscenario,
                                    Vaatscenario, `Regio impactanalyse`) %>% slice_max(`verandering absoluut`)
rang_max$`ranking totaal` = 'grootste absolute toename'
rang_min = rang %>% dplyr::group_by(Combinatie_hoofd_sub_vaat_scenario, Combinatie_regios, Zorgprofessional,
                                    Type_proxy, Hoofdregio, Hoofdscenario, Subregio, Vaatnetwerk, Subscenario,
                                    Vaatscenario, `Regio impactanalyse`) %>% slice_min(`verandering absoluut`)
rang_min$`ranking totaal` = 'grootste absolute afname'
rang = bind_rows(rang_max, rang_min)
rang$`verandering absoluut` = NULL


# Flexibele titels grafieken toevoegen
rang$Titel = ifelse(is.na(rang$`ranking totaal`), NA,
                    ifelse(rang$`ranking totaal` == 'grootste absolute toename', paste0('Grootste absolute toename: ', rang$Instelling),
                           ifelse(rang$`ranking totaal` == 'grootste absolute afname', paste0('Grootste absolute afname: ', rang$Instelling), NA)))
rang$Titel2 = ifelse(is.na(rang$`ranking totaal`), NA,
                     paste0("Maximale impact op ", rang$Zorgprofessional, " per ziekenhuis per scenario [percentage]"))

zp_tot$Aandoening = 'Totaal'
zp_tot$Interventie = 'Totaal'

zp = bind_rows(zp, zp_tot)

zp = merge(zp, rang, by = c('Combinatie_hoofd_sub_vaat_scenario', 'Combinatie_regios', 'Zorgprofessional', 'Instelling',
                            'Type_proxy', 'Hoofdregio', 'Hoofdscenario', 'Subregio', 'Vaatnetwerk', "Subscenario", 
                            'Vaatscenario', 'Regio impactanalyse'), all.x = T)

rm(rang, rang_max, rang_min, zp_tot)

# Namen zorgprofessionals aanpassen zodat je labels in PowerBI kan sorteren op type zorgprofessional
# Assistent/medisch specialist/verpleegkundige
koppel_zp = unique(subset(koppel_zp, select = -c(Interventie, Variabele)))
zp = merge(zp, koppel_zp, by = c('Zorgprofessional'), all.x = T)
zp$Zorgprofessional = NULL
setnames(zp, 'Zorgprofessional2', 'Zorgprofessional')

# Namen interventies aanpassen (korter)
zp = merge(zp, namen_interventies, by = c('Aandoening', 'Interventie'), all.x = T)
zp$Naam_interventie_nieuw[zp$Interventie == 'Totaal'] = 'Totaal'
zp$Interventie = zp$Naam_interventie_nieuw
zp$Naam_interventie_nieuw = NULL

# Labels voor tijd maken voor PowerBI (format DD:HH:MM)
zp$Label <- format(as.POSIXct("1970-01-01") + abs(zp$`verandering absoluut`), "%T")
zp$Aantal_huidig = NULL
zp$Aantal_nieuw = NULL
zp$Aantal_verschil = NULL

# Labels voor ziekenhuisnamen korter maken
zp = merge(zp, namen_kort, by = 'Instelling', all.x = T)
zp$Instelling = NULL
setnames(zp, c('Ziekenhuis_naam_kort'), c('Ziekenhuis'))

# Label type proxy aanpassen
zp$Type_proxy = as.character(zp$Type_proxy)
zp$Type_proxy[zp$Type_proxy == 'Waarde_instelling'] = 'Proxywaarden aangepast door ziekenhuizen'
zp$Type_proxy[zp$Type_proxy == 'Waarde_landelijk'] = 'Proxywaarden landelijk'

# Afronden op 50 uur
# Eerst onafgeronde tabel opslaan om te kunnen filteren
write.table(zp, paste0(analyse, "# Databestand impact zorgprofessionals onafgerond.csv"), sep=";", row.names = F)
zp$`verandering absoluut` = round_any(zp$`verandering absoluut`, 50)

# Kolommen in logische volgorde zetten:
zp = zp[,c('Regio impactanalyse', 'Hoofdregio', 'Subregio', 'Vaatnetwerk', 'Combinatie_hoofd_sub_vaat_scenario',
           'Hoofdscenario', 'Subscenario', 'Vaatscenario', 'Ziekenhuis', 'Aandoening',
           'Interventie', 'Zorgprofessional', 'Type_proxy', 'proxy', 'verandering absoluut', 
           'ranking totaal', 'Titel', 'Titel2')]

write_xlsx(zp, paste0(analyse, '# Databestand inputbestand PowerBI zorgprofessionals'))

