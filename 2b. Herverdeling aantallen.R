#-------------------------------------------------------------------------------------------------------------------------------------------------------------
#Project: Impactanalyse concentratie en spreiding
#Auteur:  SiRM
#Datum:   januari 2025
#Doel:    Herverdeling interventies
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

geodan              = # geodan bestand inladen
geodan$X            = NULL
setnames(geodan, c('FROMPC4', 'TOPC4', 'TIME'), c('pc4', 'pc4_zh', 'reistijd'))

source(paste0(analyse, "# R bestand Samenvoegen scenario's"))
overschrijvingen = data.table(read_excel(paste0("# Databestand aantallen herverdeling overschrijven"), sheet = "input R"))

#----------------------------- Koppeltabellen ---------------------------------#

# Locaties instellingen
locaties = read_excel(paste0(input_model, '# Koppeltabellen'), sheet = "Interventies - locaties")
locaties = locaties[!duplicated(locaties[,c("Instelling", "Interventie")]),] # voor als instelling op 2 locaties werkt
locaties = locaties[,c('Instelling', 'Interventie', 'pc4_locatie')]  # pc4 per instelling kan verschillen voor interventies
setnames(locaties, c('pc4_locatie'), c('pc4_zh'))

# Aantallen per interventie - NZa (evt. gecorrigeerd)
NZa = read_excel(paste0(analyse, '# Databestand nieuwe data NZa'))
NZa$meenemen = ifelse(NZa$Interventie == 'Carotis - chirurgische interventies', '23 juli', '2 oktober')
NZa$meenemen2 = ifelse(NZa$datum_aanlevering == NZa$meenemen | NZa$datum_aanlevering == 'n.v.t.', 1, 0)
NZa = NZa[NZa$meenemen2 == 1,]
NZa$meenemen          = NULL
NZa$meenemen2         = NULL
NZa$datum_aanlevering = NULL
NZa$Jaar              = NULL

# Aantal inwoners per pc4-gebied (2022)
inwoners = read.csv(paste0(sogiz, 'bevolking (CBS).csv'), sep = ';')
inwoners = inwoners[inwoners$jaar == 2022,]
inwoners = inwoners %>% dplyr::group_by(pc4) %>% dplyr::summarise(inwoners = sum(inwoners, na.rm = T))

# Volumenormen
v_normen = read_excel(paste0(analyse, "# Databestand Scenario's C&S"), sheet = 'Volumenormen')

scenarios = merge(scenarios, NZa, by = c('Instelling', 'Aandoening', 'Interventie'), all.x = T)
setnames(scenarios, 'aantal', 'aantal_2022')
scenarios$aantal_2022[is.na(scenarios$aantal_2022)] = 0

# Een deel gaan we verdelen o.b.v. reistijd
herverdeling_rt = scenarios

#------------------------------------------------------------------------------#
#------------------------ Adherentiegebied per tumortype ----------------------#
#------------------------------------------------------------------------------#
# Adherentiegebied per interventie bepalen
locaties_interventies = NZa[NZa$Interventie %in% herverdeling_rt$Interventie,]
# pc4 koppelen
locaties_interventies = left_join(locaties_interventies, locaties, by = c('Instelling', 'Interventie'))
locaties_interventies <- locaties_interventies[locaties_interventies$aantal >0,]

# Leeg dataframe maken om adherentiegebieden in te zetten
adh_onco = data.frame(matrix(data = NA, nrow = 0, ncol = 5))
names(adh_onco) = c('Interventie', 'pc4', 'pc4_zh', 'Instelling', 'reistijd')
adh_onco = adh_onco %>% mutate_at(c('Interventie', 'Instelling'), 'as.character')
adh_onco = adh_onco %>% mutate_at(c('pc4', 'pc4_zh', 'reistijd'), 'as.numeric')

interventies = unique(herverdeling_rt$Interventie)

for(i in 1:length(interventies)){
  # Per interventie het adherentiegebied bepalen
  # Eerst selecteren voor interventie = i welke ziekenhuizen de interventie aanbieden (2022)
  selectie_pc4 = locaties_interventies$pc4_zh[locaties_interventies$Interventie == interventies[i]]
  
  # Alleen de relevante ziekenhuis pc4-gebieden selecteren
  geodan0 = geodan[geodan$pc4_zh %in% selectie_pc4,]
  geodan0$Interventie = interventies[i]
  
  # Per pc4-gebied het dichtstbijzijnde ziekenhuis dat interventie i aanbiedt bepalen
  geodan0[,combi:=reistijd*10^9+DISTANCE+pc4_zh/1000]
  geodan0[,ni:=frankv(combi, order=1L),
          by=c("pc4")][order(`pc4`,combi)]
  geodan0               = geodan0[geodan0$ni == 1,]
  geodan0               = geodan0[,c('pc4','pc4_zh', 'Interventie', 'reistijd')]
  geodan0               = merge(geodan0, locaties, by=c('pc4_zh', 'Interventie'), all.x=T, all.y=F)
  
  # Adherentiegebieden voor alle interventies samenvoegen in één tabel:
  adh_onco              = bind_rows(adh_onco, geodan0)
}

rm(interventies)

#------------------------------------------------------------------------------#
#------------------------ Aantallen toewijzen aan pc4 -------------------------#
#------------------------------------------------------------------------------#

# Samenvoegen met de aantallen per ziekenhuis
adh_onco_aantallen = distinct(merge(adh_onco, locaties_interventies, by = c('Instelling', 'pc4_zh', 'Interventie'), all.x = T))
check_aantal <- distinct(adh_onco_aantallen[,c("Instelling", "Interventie", "aantal")])
sum(check_aantal$aantal)

# Combineren met inwoners
# We doen een aanname waar patiënten vandaan komen - gaan naar dichtstbijzijnde ziekenhuis
# En dan verdeling o.b.v. inwoners per pc4-gebied
adh_onco_aantallen = data.table(merge(adh_onco_aantallen, inwoners, by = 'pc4', all.x = T))
adh_onco_aantallen <- drop_na(adh_onco_aantallen) 
inw_adh = adh_onco_aantallen[,.(inwoners_tot = sum(inwoners, na.rm = T)), by = list(Instelling, pc4_zh, Aandoening, Interventie)]
adh_onco_aantallen = left_join(adh_onco_aantallen, inw_adh, by = c('Instelling', 'pc4_zh', 'Aandoening', 'Interventie'))
adh_onco_aantallen$aantal_pc4 = adh_onco_aantallen$aantal * (adh_onco_aantallen$inwoners / adh_onco_aantallen$inwoners_tot)
adh_onco_aantallen$inwoners = NULL
adh_onco_aantallen$inwoners_tot = NULL
rm(inw_adh)

# Check aantallen: totaal van alle pc4's moet gelijk zijn aan totaal aantal interventies
check_aantal <- distinct(adh_onco_aantallen[,c("Instelling", "Interventie", "aantal")])
sum(check_aantal$aantal)
sum(adh_onco_aantallen$aantal_pc4)
sum(locaties_interventies$aantal)

#------------------------------------------------------------------------------#
#--- Stap 1: zenden vanuit ziekenhuizen die stoppen met interventie -----------#
#------------------------------------------------------------------------------#

alle_scenarios <- distinct(herverdeling_rt[,c("Instelling", "Interventie", "Scenario", "Subregio")])
stoppende_zkh <- herverdeling_rt[herverdeling_rt$Herverdeling == "Verdelen o.b.v. reistijd",
                                 c("Instelling", "Interventie", "Scenario", "aantal_2022")]
stoppende_zkh$stopt = "Ja"

#-----------locaties die nog wel open blijven bepalen---------------------------------- 
locaties_scenarios <- left_join(alle_scenarios, locaties_interventies, by = c("Instelling", "Interventie")) 
locaties_na_stap_1 <- left_join(locaties_scenarios, stoppende_zkh[,-c("aantal_2022")], by = c("Instelling", "Interventie", "Scenario"))
locaties_na_stap_1 <- locaties_na_stap_1[is.na(locaties_na_stap_1$stopt), -c("stopt")]

locaties_na_stap_1 <- locaties_na_stap_1[!is.na(locaties_na_stap_1$pc4_zh)]
alle_locaties_nieuw <- distinct(herverdeling_rt[herverdeling_rt$Type_verdeling != "Verdelen o.b.v. reistijd",
                                                c("Instelling", "Interventie", "Scenario", "Subregio", "Aandoening")])
nieuwe_locaties <- anti_join(alle_locaties_nieuw, locaties_interventies, by = c("Instelling", "Interventie"))
nieuwe_locaties$aantal = 0
nieuwe_locaties <- left_join(nieuwe_locaties, locaties[!duplicated(locaties$Instelling),c("Instelling", "pc4_zh")], by = c("Instelling"))
locaties_na_stap_1 <- rbind(locaties_na_stap_1, nieuwe_locaties)

#-----------pc4s op basis van adherentie verdelen ---------------------------------- 

# Leeg dataframe maken om adherentiegebieden in te zetten
adh_na_stoppen = data.frame(matrix(data = NA, nrow = 0, ncol = 7))
names(adh_na_stoppen) = c('Interventie', 'pc4', 'pc4_zh','Subregio', 'Scenario','reistijd', 'Instelling')
adh_na_stoppen = adh_na_stoppen %>% mutate_at(c('Interventie', 'Instelling', 'Scenario', 'Subregio'), 'as.character')
adh_na_stoppen = adh_na_stoppen %>% mutate_at(c('pc4', 'pc4_zh', 'reistijd'), 'as.numeric')


interventies = distinct(alle_scenarios[,c("Interventie","Scenario", "Subregio")])
pc4_gebieden <- left_join(adh_onco_aantallen[,c("Instelling", "pc4", "Interventie")], 
                          distinct(herverdeling_rt[,c("Instelling", "Interventie", "Subregio")]),
                          by =c ("Instelling", "Interventie"))

pc4_gebieden <- pc4_gebieden[!is.na(pc4_gebieden$pc4),]

for(i in 1:nrow(interventies)){
  interventie = interventies[i,`Interventie`]
  scenario = interventies[i,`Scenario`]
  subregio = interventies[i, `Subregio`]
  # Per interventie het adherentiegebied bepalen
  # Eerst selecteren welke ziekenhuizen de interventie nog steeds aanbieden en binnen welke subregio (daarbinnen verdelen)
  selectie_pc4_zh = locaties_na_stap_1$pc4_zh[locaties_na_stap_1$Interventie == interventie & 
                                                locaties_na_stap_1$Scenario == scenario & 
                                                locaties_na_stap_1$Subregio == subregio] 

  # Filteren op PC4s van de regio
  pc4_subregio <- pc4_gebieden[pc4_gebieden$Subregio == subregio & pc4_gebieden$Interventie == interventie,]$pc4
  
  # Alleen de relevante ziekenhuis pc4-gebieden selecteren
  geodan1 = geodan[geodan$pc4_zh %in% selectie_pc4_zh & geodan$pc4 %in% pc4_subregio,]
  geodan1$Interventie = interventie
  geodan1$Scenario = scenario
  geodan1$Subregio = subregio
  
  # Per pc4-gebied het dichtstbijzijnde ziekenhuis dat interventie aanbiedt bepalen
  geodan1[,combi:=reistijd*10^9+DISTANCE+pc4_zh/1000]
  adher1 <- geodan1[,ni1:=frankv(combi, order=1L),
                    by=c("pc4")][order(`pc4`,combi)]
  adher1               = adher1[adher1$ni1 == 1,]
  adher1               = adher1[,c('Interventie','pc4','pc4_zh', 'Subregio','Scenario','reistijd')]
  adher1               = merge(adher1, distinct(locaties_na_stap_1[,c("Instelling", "Interventie", "pc4_zh")]), 
                               by=c('pc4_zh', 'Interventie'), all.x=T, all.y=F)
  
  # Adherentiegebieden voor alle interventies samenvoegen in één tabel:
  adh_na_stoppen              = bind_rows(adh_na_stoppen, adher1)
}

rm(interventies)

adh_na_stoppen <- setnames(adh_na_stoppen, old=c("Instelling","pc4_zh", "reistijd"), 
                           new = c("Instelling_na_stap_1","pc4_zh_na_stap_1", "reistijd_na_stap_1"), skip_absent = T)

adh_na_stap_1 <- left_join(adh_onco_aantallen, distinct(alle_scenarios[,c("Instelling","Interventie", "Subregio")]), by = c("Instelling", "Interventie"))
adh_na_stap_1 <- left_join(adh_na_stap_1[!is.na(adh_na_stap_1$pc4)], adh_na_stoppen, by = c("pc4", "Interventie", "Subregio"))
adh_na_stap_1 <- adh_na_stap_1[!is.na(adh_na_stap_1$Scenario),]

#----------Overschrijvingen verwerken ---------------------------------- 
overschrijvingen_concentratie <- distinct(left_join(overschrijvingen, locaties_interventies[,c("Instelling", "pc4_zh", "Interventie")], 
                                                    by = c("Instelling naar" = "Instelling", "Interventie")))

for(i in 1:nrow(overschrijvingen_concentratie)){
  zkh_van <- overschrijvingen_concentratie[i,`Instelling van`]
  zkh_naar <- overschrijvingen_concentratie[i,`Instelling naar`]
  pc4_naar <- overschrijvingen_concentratie[i,`pc4_zh`]
  interventie <- overschrijvingen_concentratie[i,`Interventie`]
  scenario <- overschrijvingen_concentratie[i,`Scenario`]
  
  # Omdat je ook binnen een scenario naar twee ziekenhuizen kan concentreren. 
  # De eerste overschrijving heeft rank 1 gekregen, de tweede 2 enz.
  # Na de eerste overschrijving  de aantallen per pc4 in verhouding verdelen over het eerste ziekenhuis en het nieuwe ziekenhuis.  
  rank <- overschrijvingen_concentratie[i,`Rank verplaatsen`]
  if(rank ==1){
    adh_na_stap_1[adh_na_stap_1$Instelling == zkh_van & adh_na_stap_1$Interventie == interventie &
                      adh_na_stap_1$Scenario == scenario,]$Instelling_na_stap_1 = zkh_naar
    adh_na_stap_1[adh_na_stap_1$Instelling == zkh_van & adh_na_stap_1$Interventie == interventie &
                      adh_na_stap_1$Scenario == scenario,]$pc4_zh_na_stap_1 = pc4_naar
  } else {
    zkh_rank_1 <- overschrijvingen_concentratie[overschrijvingen_concentratie$`Instelling van` == zkh_van & 
                                                  overschrijvingen_concentratie$Interventie == interventie &
                                                  overschrijvingen_concentratie$Scenario == scenario &
                                                  overschrijvingen_concentratie$`Rank verplaatsen` == 1,]$`Instelling naar`
    
    aandeel_over_bij_rank_1 <- sum(overschrijvingen_concentratie[overschrijvingen_concentratie$`Instelling van` == zkh_van & 
                                                  overschrijvingen_concentratie$Interventie == interventie &
                                                  overschrijvingen_concentratie$Scenario == scenario &
                                                  (overschrijvingen_concentratie$`Rank verplaatsen`> rank|
                                                     overschrijvingen_concentratie$`Rank verplaatsen` ==1),]$`Aandeel verplaatsen`)
    
    tabel_toevoegen <-  adh_na_stap_1[adh_na_stap_1$Instelling == zkh_van & adh_na_stap_1$Interventie == interventie &
                                        adh_na_stap_1$Scenario == scenario & 
                                        adh_na_stap_1$Instelling_na_stap_1 == zkh_rank_1,]
    
    if(rank ==2){
      aandeel_blijft <- 1 - overschrijvingen_concentratie[i, `Aandeel verplaatsen`]
      aandeel_verplaatst <-  overschrijvingen_concentratie[i, `Aandeel verplaatsen`]
    } else {
      aandeel_verplaatst <- overschrijvingen_concentratie[i, `Aandeel verplaatsen`] / (aandeel_over_bij_rank_1 + overschrijvingen_concentratie[i, `Aandeel verplaatsen`])
      aandeel_blijft <- 1 - aandeel_verplaatst
    }
    
    adh_na_stap_1$aantal_pc4 <- ifelse(adh_na_stap_1$Instelling == zkh_van & adh_na_stap_1$Interventie == interventie &
                                         adh_na_stap_1$Scenario == scenario &
                                         adh_na_stap_1$Instelling_na_stap_1 == zkh_rank_1, 
                                       adh_na_stap_1$aantal_pc4 * aandeel_blijft, adh_na_stap_1$aantal_pc4)
    
    tabel_toevoegen$aantal_pc4 <- tabel_toevoegen$aantal_pc4 * aandeel_verplaatst
    tabel_toevoegen$Instelling_na_stap_1 <- zkh_naar
    tabel_toevoegen$pc4_zh_na_stap_1 <- pc4_naar
    adh_na_stap_1 <- rbind(adh_na_stap_1, tabel_toevoegen)
  }
  
  reistijden_overschrijven <- setnames(geodan[geodan$pc4_zh == pc4_naar,-c("DISTANCE", "pc4_zh")], 
                                       old = c("reistijd"), new= c("reistijd_overschrijven"))
  adh_na_stap_1 <- left_join(adh_na_stap_1, reistijden_overschrijven, by = c("pc4"))
  adh_na_stap_1$reistijd_na_stap_1 <- ifelse(
    adh_na_stap_1$Instelling == zkh_van & adh_na_stap_1$Interventie == interventie &
      adh_na_stap_1$Scenario == scenario & adh_na_stap_1$pc4_zh_na_stap_1 == pc4_naar, 
    adh_na_stap_1$reistijd_overschrijven, adh_na_stap_1$reistijd_na_stap_1
  )
  adh_na_stap_1$reistijd_overschrijven = NULL
}

#----------Correctie voor ERCP's ---------------------------------- 
# Correctie voor ERCP: alleen 10% van de interventies herverdelen
adh_na_stap_1$aantal_herverdelen = ifelse(adh_na_stap_1$Interventie == "Pancreas - aantal ERCP’s ongeacht indicatie"&
                                            adh_na_stap_1$Instelling != adh_na_stap_1$Instelling_na_stap_1,
                                          adh_na_stap_1$aantal_pc4 * 0.1, adh_na_stap_1$aantal_pc4)

adh_na_stap_1$ercp_resterend = ifelse(adh_na_stap_1$Interventie == "Pancreas - aantal ERCP’s ongeacht indicatie"&
                                        adh_na_stap_1$Instelling != adh_na_stap_1$Instelling_na_stap_1,
                                      adh_na_stap_1$aantal_pc4 * 0.9, 0)

# ERCP's van ziekenhuizen die niet meer meedoen opslaan zodat deze op het eind weer kunnen worden samengevoegd
ercp_resterend <- adh_na_stap_1[,.(aantal_ercp_resterend = sum(ercp_resterend)), 
                                by = c("Instelling","Interventie", "Subregio", "Scenario")]
ercp_resterend <- ercp_resterend[ercp_resterend$aantal_ercp_resterend >0,]

sum(adh_na_stap_1$aantal_pc4)
sum(adh_na_stap_1$aantal_herverdelen, adh_na_stap_1$ercp_resterend)
sum(herverdeling_rt$aantal_2022)

#----------Reistijdentabel maken ---------------------------------- 
# Meerdere regels voor ercp, waarin we onderscheid maken naar ercps die nog wel in het oude ziekenhuis blijven en die verplaatsen
reistijdentabel <- data.table(pivot_longer(adh_na_stap_1, cols = c("aantal_herverdelen", "ercp_resterend"), values_to ="aantal_na_stap_1"))
reistijdentabel <- reistijdentabel[!reistijdentabel$aantal_na_stap_1 ==0,]
reistijdentabel <- setnames(reistijdentabel, old = c("reistijd"), new=c("reistijd_voor_concentratie"))
reistijdentabel$reistijd_na_concentratie <- ifelse(reistijdentabel$name == "aantal_herverdelen", 
                                                   reistijdentabel$reistijd_na_stap_1, reistijdentabel$reistijd_voor_concentratie)
reistijdentabel <- reistijdentabel[,-c("pc4_zh", "pc4_zh_na_stap_1", "reistijd_na_stap_1")]

#------------------------------------------------------------------------------#
#--- Stap 2: zenden zodat alle huizen aan de norm gaan voldoen ----------------#
#------------------------------------------------------------------------------#
aantallen_na_stap_1 <- adh_na_stap_1[,.(aantal_interventies_na_stap_1 = sum(aantal_herverdelen)), 
                                     by = c("Instelling_na_stap_1","Interventie", "Subregio", "Scenario", "pc4_zh_na_stap_1")]

aantallen_na_stap_1 <- left_join(aantallen_na_stap_1, v_normen, by = c("Interventie"))

ziekenhuizen_met_tekort <- aantallen_na_stap_1[aantallen_na_stap_1$aantal_interventies_na_stap_1 < aantallen_na_stap_1$Volumenorm,]
ziekenhuizen_met_tekort$tekort <- ziekenhuizen_met_tekort$Volumenorm - ziekenhuizen_met_tekort$aantal_interventies_na_stap_1

# Beginnen met het kleinste tekort, voor als een ziekenhuis naar meerdere moet zenden
ziekenhuizen_met_tekort <- ziekenhuizen_met_tekort[order(ziekenhuizen_met_tekort$tekort),]

aantallen_na_stap_2 <- aantallen_na_stap_1
aantallen_na_stap_2$aantal_interventies_na_stap_2 <- aantallen_na_stap_2$aantal_interventies_na_stap_1

reistijdentabel_2 <- reistijdentabel
reistijdentabel_2$reistijd_na_concentratie_en_spreiding <- reistijdentabel_2$reistijd_na_concentratie
reistijdentabel_2$aantal_na_stap_2 <- reistijdentabel_2$aantal_na_stap_1
reistijdentabel_2$Instelling_na_stap_2 <- reistijdentabel_2$Instelling_na_stap_1

for(i in 1:nrow(ziekenhuizen_met_tekort)){
  ziekenhuis_met_tekort <- ziekenhuizen_met_tekort[i, `Instelling_na_stap_1`]
  interventie <- ziekenhuizen_met_tekort[i, `Interventie`]
  scenario <- ziekenhuizen_met_tekort[i, `Scenario`]
  subregio <- ziekenhuizen_met_tekort[i, `Subregio`]
  pc4_zh_tekort <- ziekenhuizen_met_tekort[i, `pc4_zh_na_stap_1`]
  tekort <- ziekenhuizen_met_tekort[i, `tekort`]
  
  
  #dichtstbijzijnde zkh bepalen
  andere_zkh <- distinct(aantallen_na_stap_2[aantallen_na_stap_1$Interventie == interventie &
                                               aantallen_na_stap_1$Subregio == subregio &
                                               aantallen_na_stap_1$Scenario == scenario &
                                               aantallen_na_stap_1$Instelling_na_stap_1 != ziekenhuis_met_tekort,
                                             c("Instelling_na_stap_1","pc4_zh_na_stap_1", "aantal_interventies_na_stap_2", "Volumenorm")])
  geodan2 <- geodan[geodan$pc4 == pc4_zh_tekort & geodan$pc4_zh %in% andere_zkh$pc4_zh_na_stap_1,]
  geodan2 <- left_join(geodan2, andere_zkh, by = c("pc4_zh" = "pc4_zh_na_stap_1"))
  geodan2 <- geodan2[order(geodan2$reistijd),]
  
  # Verdelen vanuit het dichtstbijzijnde ziekenhuis
  
  j<-1 # Als scenario niet haalbaar is niet vastlopen
  while((tekort > 0) & (j<= nrow(geodan2))){
    ziekenhuis_dat_zendt <- geodan2[j,`Instelling_na_stap_1`]
    overschot <- geodan2[j,`aantal_interventies_na_stap_2`] - geodan2[j,`Volumenorm`]
    
    if(overschot <= 0){
      verplaatsing = 0
    }else if(tekort <= overschot){
      verplaatsing = tekort
    } else {
      verplaatsing = overschot
    }

    aantallen_na_stap_2$aantal_interventies_na_stap_2 <- 
      ifelse(aantallen_na_stap_1$Instelling_na_stap_1 == ziekenhuis_met_tekort &
               aantallen_na_stap_1$Interventie == interventie &
               aantallen_na_stap_1$Scenario == scenario, 
             aantallen_na_stap_2$aantal_interventies_na_stap_2 + verplaatsing, 
             aantallen_na_stap_2$aantal_interventies_na_stap_2)
    
    aantallen_na_stap_2$aantal_interventies_na_stap_2 <- 
      ifelse(aantallen_na_stap_1$Instelling_na_stap_1 == ziekenhuis_dat_zendt &
               aantallen_na_stap_1$Interventie == interventie &
               aantallen_na_stap_1$Scenario == scenario, 
             aantallen_na_stap_2$aantal_interventies_na_stap_2 - verplaatsing, 
             aantallen_na_stap_2$aantal_interventies_na_stap_2)
    
    #reistijden aanpassen: deel van de interventies uit pc4 gebieden van adher zendend zkh krijgt hogere reistijd
    aandeel_zenden <- verplaatsing / geodan2[geodan2$Instelling_na_stap_1 == ziekenhuis_dat_zendt,`aantal_interventies_na_stap_2`]
    pc4s_met_hogere_reistijd <- reistijdentabel_2[reistijdentabel_2$Instelling_na_stap_1 == ziekenhuis_dat_zendt &
                                                    reistijdentabel_2$Interventie == interventie & 
                                                    reistijdentabel_2$Scenario == scenario,]
    pc4s_met_hogere_reistijd$reistijd_na_concentratie_en_spreiding = NULL
    geodan3 <- setnames(geodan[geodan$pc4_zh == pc4_zh_tekort & geodan$pc4 %in% pc4s_met_hogere_reistijd$pc4,c("pc4", "reistijd")], 
                        old = c("reistijd"), new=c("reistijd_na_concentratie_en_spreiding"))
    pc4s_met_hogere_reistijd <- left_join(pc4s_met_hogere_reistijd, geodan3, by = c("pc4"))
    pc4s_met_hogere_reistijd$aantal_na_stap_2 = pc4s_met_hogere_reistijd$aantal_na_stap_2 * aandeel_zenden
    pc4s_met_hogere_reistijd$Instelling_na_stap_2 = ziekenhuis_met_tekort
    
    
    reistijdentabel_2$aantal_na_stap_2 <- ifelse(reistijdentabel_2$Instelling_na_stap_1 == ziekenhuis_dat_zendt & 
                                                   reistijdentabel_2$Interventie == interventie & 
                                                   reistijdentabel_2$Scenario == scenario, 
                                                 (1-aandeel_zenden)*reistijdentabel_2$aantal_na_stap_2, reistijdentabel_2$aantal_na_stap_2)
    reistijdentabel_2 <- rbind(reistijdentabel_2, pc4s_met_hogere_reistijd)
    
    tekort = tekort - verplaatsing
    j <- j + 1

  }
}

#---------------------------- Alles samenvoegen -------------------------------#

scenarios_output <- left_join(scenarios, aantallen_na_stap_2[,-c("pc4_zh_na_stap_1", "aantal_interventies_na_stap_1", "Volumenorm")], 
                       by = c("Instelling" = "Instelling_na_stap_1", "Interventie", "Subregio", "Aandoening", "Scenario"))

scenarios_output <- left_join(scenarios_output, ercp_resterend, by = c("Instelling", "Interventie","Subregio", "Scenario"))
scenarios_output$aantal_interventies_na_stap_2 <- ifelse(!is.na(scenarios_output$aantal_ercp_resterend), 
                                                         scenarios_output$aantal_ercp_resterend, scenarios_output$aantal_interventies_na_stap_2)
sum(scenarios_output$aantal_interventies_na_stap_2, na.rm=T) 
scenarios_output$aantal_ercp_resterend = NULL

scenarios_output$aantal_stap2 <- ifelse(is.na(scenarios_output$aantal_interventies_na_stap_2),0, scenarios_output$aantal_interventies_na_stap_2)
scenarios_output$aantal_interventies_na_stap_2 = NULL
scenarios_output$aantal_extra = scenarios_output$aantal_stap2 - scenarios_output$aantal_2022


# Voor bovenregionale scenarios moeten alle instellingen erin zitten
voert_niet_uit = voert_niet_uit[,c('Scenario', 'Aandoening', 'Interventie', 'Instelling', 'Subregio', 'Hoofdregio')]
voert_niet_uit$aantal_2022 = 0
voert_niet_uit$aantal_stap2 = 0
voert_niet_uit$aantal_extra = 0
voert_niet_uit <- voert_niet_uit[voert_niet_uit$Subregio %in% c("CCN NO", "CCN Zuid-West")]
scenarios_output = bind_rows(scenarios_output, voert_niet_uit)

write_xlsx(scenarios_output, paste0(analyse, '# Databestand Scenarios en aantallen'))


#-------------------------Tabel verplaatsing-----------------------------------#
verplaatsingen <- reistijdentabel_2[!reistijdentabel_2$Instelling == reistijdentabel_2$Instelling_na_stap_2,
                                    .(aantal_verplaatsing = sum(aantal_na_stap_2)), by = c("Interventie", "Scenario", "Instelling", "Instelling_na_stap_2")]

write_xlsx(verplaatsingen, paste0(analyse, '# Databestand Herverdeling van en naar'))

reistijdentabel_2$Instelling_na_stap_2 = NULL 

write_xlsx(reistijdentabel_2, paste0(analyse, '# Databestand input code voor reistijden'))
