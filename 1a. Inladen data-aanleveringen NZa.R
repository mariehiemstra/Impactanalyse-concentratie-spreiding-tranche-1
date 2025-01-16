#-------------------------------------------------------------------------------------------------------------------------------------------------------------
#Project: Impactanalyse concentratie en spreiding
#Auteur:  SiRM
#Datum:   januari 2025
#Doel:    Inladen data-aanleveringen NZa (1a)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------

cat("\014")                 # Clears the console
rm(list = ls())             # Remove all variables of the work space

#Libraries
library(readxl)
library(data.table)
library(writexl)
library(tidyverse)
library(dplyr)

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

# Inlezen bestanden -------------------------------------------------------

#We hebben verschillende aanleveringen ontvangen van de NZa
#We lezen ze allemaal in zodat we kunnen vergelijken en kijken of alles compleet is
NZa_2okt      = read.csv2(paste0(bron_NZa, "#naam bronbestand.csv"))
NZa_3sep      = read.csv2(paste0(bron_NZa, "#naam bronbestand.csv"))
NZa_28aug     = read.csv2(paste0(bron_NZa, "#naam bronbestand.csv"))
NZa_30juli    = read.csv2(paste0(bron_NZa, "#naam bronbestand.csv"))
NZa_23juli    = read.csv2(paste0(bron_NZa, "#naam bronbestand.csv"))
NZa_18juli    = read.csv2(paste0(bron_NZa, "#naam bronbestand.csv"))
NZa_9juli     = read.csv2(paste0(bron_NZa, "#naam bronbestand.csv"))
NZa_25juni    = read.csv2(paste0(bron_NZa, "#naam bronbestand.csv"))

koppel_zkh    = read_excel(paste0('#input koppeltabel'), sheet = 'Ziekenhuisnamen')
koppel_zkh    = koppel_zkh[,c('Instelling', 'nza_naam', 'nza_naam2')]
koppel_zkh    = setDT(koppel_zkh) %>% melt(id.vars = c('Instelling'),
                                        variable.name = 'x',
                                        value.name = 'naam_instelling_lijst')
koppel_zkh$x = NULL
koppel_iv     = read_excel(paste0('# Koppeltabel'), sheet = 'Interventies NZa')
normen_optel  = read_excel(paste0('# Koppeltabel'), sheet = 'Aantallen optellen voor norm')

NZa_2okt$datum_aanlevering = '2 oktober'
NZa_3sep$datum_aanlevering = '3 september'
NZa_28aug$datum_aanlevering = '28 augustus'
NZa_30juli$datum_aanlevering = '30 juli'
NZa_23juli$datum_aanlevering = '23 juli'
NZa_18juli$datum_aanlevering = '18 juli'
NZa_9juli$datum_aanlevering  = '9 juli'
NZa_25juni$datum_aanlevering = '25 juni'

#--------------------------- Data samenvoegen ---------------------------------#

NZa = bind_rows(NZa_2okt,NZa_3sep, NZa_28aug, NZa_30juli, NZa_23juli, NZa_18juli, NZa_9juli, NZa_25juni)
rm(NZa_2okt,NZa_3sep, NZa_28aug, NZa_30juli, NZa_23juli, NZa_18juli, NZa_9juli, NZa_25juni)

# Ziekenhuisnamen omzetten naar namen die wij gebruiken zodat deze gelijk zijn
NZa = merge(NZa, koppel_zkh, by='naam_instelling_lijst', all.x=T)
NZa$naam_instelling_lijst[is.na(NZa$Instelling)] #Check of alles gekoppeld is

# Interventies hebben niet in elke aanlevering dezelfde naam
# Omzetten naar namen interventies die wij ook in data-uitvraag en model gebruiken
NZa = merge(NZa, koppel_iv, by=c('zorgtype', 'interventie', 'datum_aanlevering'), all.x=T)
NZa = subset(NZa, select = -c(interventie, zorgtype))

# Niet relevante instellingen en interventies verwijderen
NZa = NZa[!is.na(NZa$Instelling),]
NZa = NZa[!NZa$Interventie == 'Excluderen',]

# Relevante kolommen selecteren
NZa = NZa[,c('Instelling', 'Aandoening', 'Interventie', 'aantal', 'datum_aanlevering', 'meenemen_impact')]

rm(koppel_iv, koppel_zkh)

#--------------------------- Data klaarzetten ---------------------------------#

# Aantallen gelijk aan 0 of NA verwijderen
NZa = NZa[!(is.na(NZa$aantal) | NZa$aantal == 0),]

write_xlsx(NZa, paste0(output, 'Nieuwe data NZa zonder correcties.xlsx'))

# Voor sommige interventies (nier, AAA) telt de norm voor een combinatie van interventies
NZa = merge(NZa, normen_optel, by = c('Aandoening', 'Interventie'), all.x=T)

# Koppelen aan aantallen reikwijdteverbreding
# Uiteindelijk alleen één uitbreiding in één netwerk
verbreding = read_excel(paste0(analyse, 'Model C&S/Inputbestanden code/Correcties op aantallen NZa.xlsx'), sheet = "Aantallen reikwijdteverbreding")
verbreding = verbreding[verbreding$`Meenemen in impactanalyse?` == 1,]
verbreding$Jaar = NULL
NZa = bind_rows(NZa, verbreding)
rm(verbreding, normen_optel)

#Aantallen benigne longchirurgie toevoegen (we gaan uit van  aantallen  onc. longresecties * factor)
#Alleen voor één regio (A)
factor_benigne   = 1.5
NZa_long = NZa[NZa$Interventie == 'Long - oncologische longresecties',]
NZa_long = NZa_long[NZa_long$Instelling %in% c("ziekenhuizen in regio A"),]
NZa_long$Interventie = 'Long - benigne longchirurgie'
NZa_long$Norm_optellen = 'Long - benigne longchirurgie'
NZa_long$aantal = NZa_long$aantal * factor_benigne
NZa = bind_rows(NZa, NZa_long)

#------------------------- Correcties doorvoeren ------------------------------#

# Nu de correcties inladen en doorvoeren
correcties = read_excel(paste0(analyse, '# naam bestand Correcties op aantallen NZa'))
# Soms zijn er extra cijfers aangeleverd, we nemen alleen geselecteerde deel mee:
correcties = correcties[correcties$`Meenemen in impactanalyse?` == 1,]
# Als het aantal eerder 0 was moeten we deze opnieuw als rij toevoegen (in regels hierna)
# De andere kunnen we direct koppelen en vervangen
correcties = correcties[correcties$`Was aantal eerder 0?` == 'Nee',]
correcties = correcties[,c('Instelling', 'Aandoening', 'Interventie', 'Jaar', 'aantal_correctie')]
# Endoscopische resecties koppelen niet goed, dus aparte regel om klaar te zetten:
correcties$Interventie[grepl('endoscopisch', correcties$Interventie)] = unique(NZa$Interventie[grepl('endoscopisch', NZa$Interventie)])

# Correcties koppelen en vervangen
NZa = merge(NZa, correcties, by = c('Instelling', 'Aandoening', 'Interventie'), all.x = T)
NZa$aantal_meenemen = ifelse(is.na(NZa$aantal_correctie), NZa$aantal, NZa$aantal_correctie)
NZa$Jaar[is.na(NZa$Jaar)] = 2022
NZa = subset(NZa, select = -c(aantal, aantal_correctie))
setnames(NZa, 'aantal_meenemen', 'aantal')

# Rijen toevoegen voor aantallen die eerder 0 waren
correcties = read_excel(paste0(analyse, '# naam bestand Correcties op aantallen NZa'))
correcties = correcties[correcties$`Meenemen in impactanalyse?` == 1,]
correcties = correcties[correcties$`Was aantal eerder 0?` == 'Ja',]
correcties$meenemen_impact <- 1
correcties = correcties[,c('Instelling', 'Aandoening', 'Interventie', 'meenemen_impact','datum_aanlevering','Norm_optellen', 
                           'Meenemen in impactanalyse?','Jaar', 'aantal_correctie')]
setnames(correcties, 'aantal_correctie', 'aantal')
NZa = bind_rows(NZa, correcties)

rm(NZa_orig, correcties)

# Tabel maken met welke nier behandelingen elk ziekenhuis doet
# Hebben we later nodig zodat we de proxy's van het zendende ziekenhuis
# Kunnen koppelen aan het ontvangende ziekenhuis
nier = NZa[NZa$Norm_optellen == 'Nier - lokale behandelingen' & NZa$datum_aanlevering == '2 oktober',]
nier = setDT(nier) %>% dcast(Instelling ~ Interventie)
nier[is.na(nier)] = 0
nier$Behandelingen_nier = ifelse(nier$`Nier - lokale behandeling: (partiële) nefrectomie` > 0 & 
                                   nier$`Nier - lokale behandeling: focale therapie` > 0,
                                 '(partiële) nefrectomie en focale therapie',
                                 ifelse(nier$`Nier - lokale behandeling: (partiële) nefrectomie` > 0,
                                        '(partiële) nefrectomie',
                                        ifelse(nier$`Nier - lokale behandeling: focale therapie` > 0,
                                               'focale therapie', NA)))
nier = nier[!nier$Behandelingen_nier == 0,]
nier = nier[,c('Instelling', 'Behandelingen_nier')]
write_xlsx(nier, paste0(analyse, '# bestand wegschrijven nier'))

#----------------------- Wegingsfactoren voor model ---------------------------#

# Wegingsfactor berekenen (AAA complex en nier lokale therapie)
# Dus aandeel binnen de norm dat bijv EVAR is en TEVAR
# Hebben we nodig omdat onder een norm meerdere interventies vallen
# In de berekening op te lossen door proxy's van onderliggende
# interventies o.b.v. een gewogen gemiddelde samen te voegen
NZa_w = NZa %>% dplyr::group_by(Instelling, Aandoening, Norm_optellen, Jaar, datum_aanlevering, meenemen_impact) %>%
  dplyr::summarise(aantal_norm = sum(aantal, na.rm=T))
NZa = merge(NZa, NZa_w, by = c('Instelling', 'Aandoening', 'Jaar', 'Norm_optellen', 'datum_aanlevering', 'meenemen_impact'), all.x = T)
NZa$W = NZa$aantal / NZa$aantal_norm

# Alleen interventies meenemen waarvoor we impact gaan bepalen
NZa = NZa[NZa$meenemen_impact == 1,]
# Carotis zit niet in de nieuwste aanleveringen, dus oude pakken
# Voor de andere interventies de meest nieuwe aanlevering
NZa$meenemen = ifelse(NZa$Interventie == 'Carotis - chirurgische interventies', '23 juli', '2 oktober')
NZa$meenemen2 = ifelse(NZa$datum_aanlevering == NZa$meenemen, 1, 0)
NZa = NZa[NZa$meenemen2 == 1,]
NZa = subset(NZa, select = -c(aantal_norm, meenemen_impact, meenemen, meenemen2, 
                              datum_aanlevering, `Meenemen in impactanalyse?`, Jaar))

#----------------------------- Tabellen opslaan -------------------------------#

# Deze tabel opslaan voor het model:
write_xlsx(NZa, paste0(analyse, '# Databestand Volumes NZa (model)'))

# Andere tabel opslaan voor overzicht volumes in excel
setnames(NZa_w, 'Norm_optellen', 'Interventie')
setnames(NZa_w, 'aantal_norm', 'aantal')
NZa_w$meenemen_impact = NULL

write_xlsx(NZa_w, paste0(analyse, '# Databestand Volumes NZa (Excel)'))

