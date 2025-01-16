#-------------------------------------------------------------------------------------------------------------------------------------------------------------
#Project: Impactanalyse concentratie en spreiding
#Auteur:  SiRM
#Datum:   januari 2025
#Doel:    Berekening impact per interventie (2a)
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

#----------------------------- Tabellen inladen -------------------------------#

# Volumenormen
v_normen            = read_excel(paste0(analyse, "# Databestand scenario's C&S.xlsx"), sheet = 'Volumenormen')
# Koppeltabel specialismes
koppel_specialisme  = read_excel(paste0(input_model, "# Koppeltabellen"), sheet = "Specialisme")
koppel_zp           = read_excel(paste0(input_model, '# Koppeltabellen'), sheet = 'Zorgprofessionals')
# Koppeltabel proxy's - interventies
proxy_interventie   = read_excel(paste0(input_model, "# Koppeltabellen"), sheet = "Proxy's en interventies")
# Weging maag/slokdarm:
weging_MS_O         = read_excel(paste0(input_model, "# Bronbestand overige gegevens"), sheet = "Weging MS operaties")
weging_MS_ER        = read_excel(paste0(input_model, "# Bronbestand overige gegevens"), sheet = "Weging MS end. resecties")
weging_MS           = bind_rows(weging_MS_O, weging_MS_ER)
rm(weging_MS_O, weging_MS_ER)
# Proxy's inladen:
proxy_tot           = read_excel(paste0(analyse, "# Databestand samengevoegde proxy's"))
# Volumes per instelling
volumes = read_excel(paste0(input_model, '# Bronbestand volumes instellingen.xlsx'))

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#---------------------- Maag/slokdarm weer samenvoegen ------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# In de data-uitvraag hebben we de mogelijkheid gegeven om aparte proxy's aan te leveren / aan te passen
# voor maag en slokdarm
# Dit is alleen ingewikkeld in de berekening omdat de NZa aantallen de som zijn van maag en slokdarm
# Voor operaties maakt het niet uit - in Zorginzicht staan aparte aantallen voor maag en slokdarm
# Maar voor endoscopische resecties kunnen we de opsplitsing niet maken
# We checken of ziekenhuizen verschillende proxy's hebben aangeleverd voor maag en slokdarm
# Zo niet - dan is er geen probleem
x = proxy_tot[proxy_tot$Interventie %in% c('Maag - oncologische, endoscopische resecties (vroeg)carcinoom',
                                           'Slokdarm - oncologische, endoscopische resecties (vroeg)carcinoom'),]
x$aangepast = ifelse(x$Waarde_instelling != x$Waarde_landelijk, 'Aangepast', 'Niet aangepast')
# Check of de aangepaste getallen verschillen voor maag en slokdarm
instellingen_aangepast = unique(x$Instelling[x$aangepast == 'Aangepast'])
variabelen_aangepast = unique(x$Variabele[x$aangepast == 'Aangepast'])
x = x[x$Instelling %in% instellingen_aangepast & x$Variabele %in% variabelen_aangepast,]
x = setDT(x) %>% dcast(Instelling + Variabele ~ Interventie, value.var = 'Waarde_instelling', fun.aggregate = sum)
x$Verschil = ifelse(x$`Maag - oncologische, endoscopische resecties (vroeg)carcinoom` != x$`Slokdarm - oncologische, endoscopische resecties (vroeg)carcinoom`, 1, 0)
ifelse(sum(x$Verschil) > 0,  print('Maag/slokdarm endoscopische resecties proxy is aangepast - opnieuw uitvragen'),  print('Maag/slokdarm endoscopische resecties proxy gaat goed!'))
# Voor endoscopische resecties zijn dus alle proxy's voor maag en slokdarm gelijk
# dus we kunnen het weer samenvoegen
rm(x, instellingen_aangepast, variabelen_aangepast)

# Proxy's maag en slokdarm weer samen nemen obv gewogen gemiddelde
# Voor endoscopische resecties pakken we 50/50, wat niet uitmaakt als de proxy's gelijk zijn
proxy_tot = merge(proxy_tot, weging_MS, by = c('Instelling', 'Interventie'), all.x=T)
proxy_tot$Interventie_tot2 = ifelse(is.na(proxy_tot$Interventie_tot), proxy_tot$Interventie, proxy_tot$Interventie_tot)
proxy_tot$W[is.na(proxy_tot$W)] = 1 #W is de wegingsfactor, 1 als we niks op hoeven te tellen obv gewogen gemiddelde
proxy_tot$Waarde_instelling = proxy_tot$Waarde_instelling * proxy_tot$W
proxy_tot$Waarde_landelijk = proxy_tot$Waarde_landelijk * proxy_tot$W
# Som pakken over 'interventie_tot2', zodat maag/slokdarm gezamenlijke proxywaarden krijgen
proxy_tot = proxy_tot %>% dplyr::group_by(Instelling, Interventie_tot2, Onderwerp, Variabele) %>%
  dplyr::summarise(Waarde_landelijk = sum(Waarde_landelijk, na.rm = T),
                   Waarde_instelling = sum(Waarde_instelling, na.rm = T))
setnames(proxy_tot, 'Interventie_tot2', 'Interventie')

# Niet gekoppelde interventies maag/slokdarm kunnen eruit
# komt doordat die instellingen die interventies niet doen
proxy_tot = proxy_tot[!proxy_tot$Interventie %in% c('Maag - oncologische, chirurgische maagresecties',
                                                   'Maag - oncologische, endoscopische resecties (vroeg)carcinoom',
                                                   'Slokdarm - oncologische, chirurgische slokdarmresecties',
                                                   'Slokdarm - oncologische, endoscopische resecties (vroeg)carcinoom'),]

rm(weging_MS)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#---------------------------- Combineren met volumes --------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# Nier focale therapie heeft een aparte proxy voor aantal operateurs
# Die omzetten
proxy_tot$Variabele[proxy_tot$Variabele == 'Aantal uitvoerend operateurs (specialist/arts-assistent) dat interventie uitvoert bij operatieve ingrepen' &
                      proxy_tot$Interventie == 'Nier - lokale behandeling: focale therapie'] = 
  'Aantal uitvoerend operateurs (specialist/arts-assistent) dat interventie uitvoert bij operatieve ingrepen - focale therapie nier'

#---------------- Per interventie de relevante proxy's koppelen ---------------#

# Onderstaande tabel geeft aan welke proxy's relevant zijn voor welke interventies
proxy_interventie = melt(proxy_interventie, id.vars = c('Interventie', 'Onderwerp', 'Variabele'),
                         variable.name = 'Interventie2')
proxy_interventie = proxy_interventie[is.na(proxy_interventie$value) == F & proxy_interventie$value == 1,]
proxy_interventie$value = NULL
proxy_tot = merge(proxy_tot, proxy_interventie, by = c('Interventie', 'Onderwerp', 'Variabele'), all.x = T, allow.cartesian = T)
# Als proxy's bij geen interventie zijn ingevuld, dan niet relevant, dus verwijderen
proxy_tot = proxy_tot[!is.na(proxy_tot$Interventie2),]
proxy_tot$Interventie = NULL
setnames(proxy_tot, 'Interventie2', 'Interventie')
proxy_tot$Variabele[proxy_tot$Variabele == 'Aantal uitvoerend operateurs (specialist/arts-assistent) dat interventie uitvoert bij operatieve ingrepen - focale therapie nier'] =
                      'Aantal uitvoerend operateurs (specialist/arts-assistent) dat interventie uitvoert bij operatieve ingrepen'
rm(proxy_interventie)

# ---------------------------- Volumes koppelen -------------------------------#

proxy_tot = merge(proxy_tot, volumes, by = c('Instelling', 'Interventie'), all.x = T)

# Proxy's verwijderen als volume 0 is
# Maar alleen voor interventiespecifieke proxy's, niet de totalen
# --> niet relevante proxy's verwijderen we dus
proxy_tot = proxy_tot[!(is.na(proxy_tot$aantal) | proxy_tot$aantal == 0),]
proxy_tot$aantal = NULL

# Nier radiotherapie verschuivingen zijn hele kleine aantallen
# En überhaupt niet zeker of je dit in een regio kan verschuiven omdat het maar 5 aanbieders zijn in NL
# Dus impact (proxy's) verwijderen
proxy_tot = proxy_tot[!proxy_tot$Interventie == 'Nier - radiotherapeutische interventies',]

# Vermenigvuldigen met weging (voor interventies waarbij norm som van meerdere dingen is)
# Dus gewogen gemiddelde o.b.v. volumes per instelling
# Geldt voor AAA complex en nier lokale behandelingen
proxy_tot$Waarde_landelijk = proxy_tot$Waarde_landelijk * proxy_tot$W
proxy_tot$Waarde_instelling = proxy_tot$Waarde_instelling * proxy_tot$W
proxy_tot = proxy_tot %>% dplyr::group_by(Instelling, Aandoening, Norm_optellen, Onderwerp, Variabele) %>%
  dplyr::summarise(Waarde_landelijk = sum(Waarde_landelijk, na.rm = T),
                   Waarde_instelling = sum(Waarde_instelling, na.rm = T))
setnames(proxy_tot, 'Norm_optellen', 'Interventie')

# Alleen proxy's voor interventies selecteren die in scenario's voorkomen
proxy_tot = proxy_tot[proxy_tot$Interventie %in% v_normen$Interventie,]
rm(volumes)

# Tijd voor endoscopische resectie is niet OK-tijd maar scopietijd
proxy_tot$Variabele[grepl("endoscopisch", proxy_tot$Interventie) & proxy_tot$Variabele == 'OK-tijd'] = 'Scopietijd'

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------ Dataset impact zorgaanbieder klaarzetten ------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

proxy_ZA = proxy_tot[proxy_tot$Onderwerp %in% c("Polikliniek", "Verpleegafdeling",
                                                 "IC", "SEH",  "OK en scopiekamer", 'Financieel'),]

# Omzetten - variabelen (i.p.v. interventies) als kolommen zodat we kunnen rekenen
proxy_ZA = setDT(proxy_ZA) %>% melt(id.vars=c('Instelling', 'Aandoening', 'Onderwerp', 'Variabele', 'Interventie'),
                                      variable.name = 'Type_proxy', value.name = 'Waarde')
proxy_ZA = setDT(proxy_ZA) %>% dcast(Instelling + Interventie + Type_proxy ~ Variabele,
                                       value.var = 'Waarde', fun.aggregate = sum)
proxy_ZA[is.na(proxy_ZA)] = 0

# Proxy OK-tijd - verschil academisch/algemeen nog doorvoeren
proxy_ZA$OK_plus = ifelse(proxy_ZA$Instelling %in% c('Amsterdam UMC', 'Erasmus MC', 'LUMC', 'Maastricht UMC+', 'UMCG Groningen', 'UMC Utrecht', 'Radboudumc'), proxy_ZA$`OK-tijd - academische ziekenhuizen`, proxy_ZA$`OK-tijd - algemene ziekenhuizen`)
proxy_ZA$`OK-tijd` = proxy_ZA$`OK-tijd` + proxy_ZA$OK_plus
proxy_ZA = subset(proxy_ZA, select = -c(OK_plus, `OK-tijd - academische ziekenhuizen`, `OK-tijd - algemene ziekenhuizen`))

names(proxy_ZA)

# Nu komen de berekeningen
# Totaal aantal polikliniekbezoeken
proxy_ZA$Polikliniekbezoeken = proxy_ZA$`Aantal pre-operatieve polikliniekbezoeken per interventie door chirurg` +
                                proxy_ZA$`Aantal polikliniekbezoeken na operatie per interventie door chirurg` +
                                proxy_ZA$`Aantal polikliniekbezoeken voor interventie per interventie - ERCP` +
                                proxy_ZA$`Aantal polikliniekbezoeken na interventie per interventie - ERCP` +
                                proxy_ZA$`Aantal pre-operatieve polikliniekbezoeken per interventie door anesthesist`

# Duur polikliniekbezoek chirurg (voor + na)
# Alle tijden omzetten naar uren, dus hier delen door 60
proxy_ZA$tijd_poli_chirurg  = ((proxy_ZA$`Aantal pre-operatieve polikliniekbezoeken per interventie door chirurg` *
                                 proxy_ZA$`Duur pre-operatieve polikliniekbezoeken per interventie door chirurg`) +
                                (proxy_ZA$`Aantal polikliniekbezoeken na operatie per interventie door chirurg` *
                                 proxy_ZA$`Duur polikliniekbezoeken na operatie per interventie door chirurg`)) / 60

#Duur polikliniekbezoek ERCP (voor + na)
proxy_ZA$tijd_poli_ERCP = ((proxy_ZA$`Aantal polikliniekbezoeken voor interventie per interventie - ERCP` *
                             proxy_ZA$`Duur polikliniekbezoeken voor interventie per interventie - ERCP`) +
                            (proxy_ZA$`Aantal polikliniekbezoeken na interventie per interventie - ERCP` *
                             proxy_ZA$`Duur polikliniekbezoeken na interventie per interventie - ERCP`)) / 60

# Duur polikliniekbezoek anesthesist (voor)
proxy_ZA$tijd_poli_anesthesist = ((proxy_ZA$`Aantal pre-operatieve polikliniekbezoeken per interventie door anesthesist` *
                                    proxy_ZA$`Duur pre-operatieve polikliniekbezoeken per interventie door anesthesist`)) / 60

# Duur polikliniekbezoek VS/PA (voor + na)
proxy_ZA$`VS/PA` = ((proxy_ZA$`Aantal pre-operatieve polikliniekbezoeken per interventie door verpleegkundig specialist (of PA)`*
                           proxy_ZA$`Duur pre-operatieve polikliniekbezoeken per interventie door verpleegkundig specialist (of PA)`)+
                          (proxy_ZA$`Aantal polikliniekbezoeken na operatie per interventie door verpleegkundig specialist (of PA)` *
                           proxy_ZA$`Duur polikliniekbezoeken na operatie per interventie door verpleegkundig specialist (of PA)`)) / 60

# Gemiddeld aantal verpleegdagen - algemeen
# Gewogen gemiddelde o.b.v. wel/niet IC
proxy_ZA$Verpleegdagen = ((proxy_ZA$`Percentage opname IC` * proxy_ZA$`Gemiddelde opnameduur patiënten na IC`) + 
                          ((proxy_ZA$`Percentage klinische opname`-proxy_ZA$`Percentage opname IC`)*proxy_ZA$`Gemiddelde opnameduur patiënten zonder IC`)) *
                            proxy_ZA$`Percentage klinische opname`

# Gemiddeld aantal IC-dagen
proxy_ZA$`IC-dagen` = proxy_ZA$`Percentage opname IC` *
                       proxy_ZA$`Gemiddelde IC-opnameduur`

# Gemiddeld aantal SEH-bezoeken
setnames(proxy_ZA, 'Gemiddeld aantal SEH-bezoeken', 'SEH-bezoeken')

# Gemiddelde scopietijd
proxy_ZA$Scopietijd = proxy_ZA$Scopietijd / 60

# Gemiddelde OK-tijd (nog omzetten naar uren)
proxy_ZA$`OK-tijd` = proxy_ZA$`OK-tijd` / 60

# Bruto schadelast en kosten
proxy_ZA$Omzet  = proxy_ZA$`Proxywaarde voor gemiddelde omzet`
proxy_ZA$Kosten = proxy_ZA$`Proxywaarde voor gemiddelde kosten`

# Tabel opslaan voor overzicht proxy's
write_xlsx(proxy_ZA, paste0(analyse, '#Databestand proxy_ZA'))

# Tabel opslaan met inzet zorgprofessionals
# Hebben we nodig voor impact op uren zorgprofessionals
inzet_zorg = proxy_ZA[,c('Instelling',
                          'Interventie',
                          'Type_proxy',
                          'tijd_poli_chirurg',
                          'tijd_poli_ERCP',
                          'tijd_poli_anesthesist',
                          'VS/PA')]

# Tabel opslaan met impact op zorgaanbieders
proxy_ZA = proxy_ZA[,c('Instelling',
                         'Interventie',
                         'Type_proxy',
                         'Polikliniekbezoeken', 
                         'Verpleegdagen',
                         'IC-dagen',
                         'SEH-bezoeken',
                         'Scopietijd',
                         'OK-tijd',
                         'Omzet',
                         'Kosten')]

# Opslaan voor inzet zorgprofessionals
# Hebben we later ook nodig voor impact op uren
proxy_tijden = proxy_ZA

proxy_ZA = setDT(proxy_ZA) %>% melt(id.vars=c('Instelling', 'Interventie', 'Type_proxy'),
                                      variable.name = 'Variabele',
                                      value.name = 'Waarde')
proxy_ZA = proxy_ZA %>% dcast(Instelling + Interventie + Variabele ~ Type_proxy)

# Waarden gelijk aan 0 verwijderen (geen impact) 
proxy_ZA = proxy_ZA[(proxy_ZA$Waarde_landelijk + proxy_ZA$Waarde_instelling) > 0,]

# Specialisme koppelen
# Voor OK-tijd, polikliniekbezoeken en verpleegdagen
koppel_specialisme = unique(koppel_specialisme[,c('Interventie', 'Variabele', 'Specialisme_optellen')])
setnames(koppel_specialisme, 'Specialisme_optellen', 'Specialisme')
#Specialisme 'totaal' kiezen voor overige variabelen (bijv. IC-dagen, niet gekoppeld aan een specialisme)
koppel_specialisme2 = copy(koppel_specialisme)
koppel_specialisme2$Specialisme = 'Totaal'
koppel_specialisme = bind_rows(koppel_specialisme, koppel_specialisme2)
proxy_ZA = merge(proxy_ZA, koppel_specialisme, by = c('Interventie', 'Variabele'), all = T, allow.cartesian = T)
# Niet alle variabelen zijn aan een specialisme gekoppeld - Dus zitten NA's bij
proxy_ZA$Specialisme[is.na(proxy_ZA$Specialisme)] = 'Totaal'
rm(koppel_specialisme, koppel_specialisme2)

proxy_ZA$Variabele[proxy_ZA$Variabele == 'Omzet'] = 'Bruto schadelast'

proxy_ZA = proxy_ZA[,c('Instelling', 'Variabele',
                         'Interventie', 
                         'Specialisme', 'Waarde_instelling', 'Waarde_landelijk')]

write_xlsx(proxy_ZA, paste0(analyse, 'Databestand gebruik faciliteiten'))

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#----------------- Dataset inzet zorgprofessionals klaarzetten ----------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#Impact op uren zorgprofessionals

proxy_tijden = proxy_tijden[,c('Instelling', 'Interventie', 'Type_proxy', 
                               'Verpleegdagen', 'IC-dagen', 'OK-tijd',
                               'Scopietijd')]

proxy_inzet = proxy_tot[proxy_tot$Onderwerp %in% c('Personeelsinzet', 'Capaciteit'),]

proxy_inzet = setDT(proxy_inzet) %>% 
  melt(id.vars = c('Instelling', 'Aandoening', 'Interventie', 
                   'Onderwerp', 'Variabele'), variable.name = 'Type_proxy',
       value.name = 'Waarde')
proxy_inzet = setDT(proxy_inzet) %>% dcast(Instelling + Interventie + Type_proxy ~ Variabele,
                                           value.var = 'Waarde', fun.aggregate = sum)

proxy_tijden = merge(proxy_tijden, proxy_inzet, by = c('Instelling', 'Interventie', 'Type_proxy'), all.x=T)
proxy_tijden = merge(proxy_tijden, inzet_zorg, by = c("Instelling", 'Interventie', 'Type_proxy'), all.x=T)

rm(proxy_inzet, inzet_zorg)

# Nu komt de berekening
# Alle tijden omrekenen naar uren

# Verpleegkundigen - IC/kliniek/verkoeverkamer
proxy_tijden$`IC-verpleegkundige` = (proxy_tijden$`IC-dagen` *24) * 
                                     proxy_tijden$`Aantal ingezette verpleegkundigen per patiënt per IC-dag`
proxy_tijden$`Verpleegkundige kliniek` = (proxy_tijden$Verpleegdagen *24) *
                                          proxy_tijden$`Aantal ingezette verpleegkundigen per patiënt per verpleegdag`
proxy_tijden$`Verpleegkundige verkoeverkamer` = proxy_tijden$`Duur verkoeverkamer per patiënt per operatieve ingreep` * # hier stond (in uren) achter, dus deed het niet
                                                proxy_tijden$`Aantal ingezette verpleegkundigen per patiënt op de verkoeverkamer`

# OK
# Voor uitvoerend specialist niet-patiëntgebonden tijd meenemen
# Uitvoerende specialist
proxy_tijden$tijd_ms_OK = (1 + proxy_tijden$`Aandeel niet-patiëntgebonden tijd van uitvoerend specialist operatieve ingreep / ERCP`) *
                          (proxy_tijden$`OK-tijd`) * 
                           proxy_tijden$`Aantal uitvoerend operateurs (specialist/arts-assistent) dat interventie uitvoert bij operatieve ingrepen`
# Anesthesioloog
#Proxy moet 1 zijn als we geen afslag nemen
proxy_tijden$`Afslag op aantal anesthesiologen`[proxy_tijden$`Afslag op aantal anesthesiologen` == 0] = 1
proxy_tijden$tijd_anesth_OK = (proxy_tijden$`OK-tijd`) * 
                               proxy_tijden$`Aantal anesthesiologen dat interventie uitvoert bij operatieve ingrepen` * 
                               proxy_tijden$`Afslag op aantal anesthesiologen`
# Operatieassistent
proxy_tijden$Operatieassistent = (proxy_tijden$`OK-tijd`) * 
                                  proxy_tijden$`Aantal OK-assistenten per interventie`
# Anesthesiemedewerker
proxy_tijden$Anesthesiemedewerker = (proxy_tijden$`OK-tijd`) * 
                                     proxy_tijden$`Aantal anesthesiemedewerkers per interventie`

# ERCP
# Uitvoerende specialist
proxy_tijden$tijd_ms_ERCP = (1 + proxy_tijden$`Aandeel niet-patiëntgebonden tijd van uitvoerend specialist operatieve ingreep / ERCP`)*
                             proxy_tijden$`Scopietijd` * 
                             proxy_tijden$`Aantal uitvoerend specialisten dat interventie uitvoert bij ERCP`
#Endoscopische resectie maag/slokdarm
proxy_tijden$tijd_ms_ERMS = (1 + proxy_tijden$`Aandeel niet-patiëntgebonden tijd van uitvoerend specialist operatieve ingreep / ERCP`)*
                             proxy_tijden$`Scopietijd` * 
                             proxy_tijden$`Aantal uitvoerend operateurs (specialist/arts-assistent) dat interventie uitvoert bij operatieve ingrepen`
#Verpleegkundige voor ERCP en endoscopische resectie
proxy_tijden$`Verpleegkundige voor ERCP` = proxy_tijden$`Scopietijd` *
                                           proxy_tijden$`Aantal scopiemedewerkers / verpleegkundigen per interventie` # Hier gaat het wel goed. 

# Tijden poli en OK optellen
proxy_tijden$`Uitvoerend specialist` = proxy_tijden$tijd_poli_chirurg + 
                                       proxy_tijden$tijd_ms_OK +
                                       proxy_tijden$tijd_ms_ERCP + 
                                       proxy_tijden$tijd_ms_ERMS
proxy_tijden$Anesthesioloog =          proxy_tijden$tijd_poli_anesthesist + 
                                       proxy_tijden$tijd_anesth_OK 

 
# Tabel opslaan voor overzicht proxy's
write_xlsx(proxy_tijden, paste0(analyse, '# Databestand proxy_tijden'))

proxy_tijden = proxy_tijden[,c('Instelling',
                               'Interventie',
                               'Type_proxy',
                               'VS/PA',
                               'IC-verpleegkundige', 
                               'Verpleegkundige kliniek',
                               'Verpleegkundige verkoeverkamer',
                               'Uitvoerend specialist',
                               'Anesthesioloog',
                               'Operatieassistent',
                               'Anesthesiemedewerker',
                               'Verpleegkundige voor ERCP')]  

proxy_tijden = setDT(proxy_tijden) %>% melt(id.vars = c('Instelling', 'Interventie', 'Type_proxy'),
                                            variable.name = 'Variabele',
                                            value.name = 'Waarde')

# Verwijderen als proxy 0 is - geen impact
proxy_tijden = proxy_tijden[!(is.na(proxy_tijden$Waarde) | proxy_tijden$Waarde == 0),]

# Juiste zorgprofessionals koppelen aan uitvoerende specialisten
koppel_zp$Zorgprofessional2 = NULL
proxy_tijden = merge(proxy_tijden, koppel_zp, by = c('Interventie', 'Variabele'), all.x = T)
proxy_tijden$Variabele = NULL

proxy_tijden = proxy_tijden %>% dcast(Instelling + Interventie + Zorgprofessional ~ Type_proxy,
                                      value.var = 'Waarde', fun.aggregate = sum)

write_xlsx(proxy_tijden, paste0(analyse, '# Databestand tijd zorgprofessionals'))