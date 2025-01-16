#-------------------------------------------------------------------------------------------------------------------------------------------------------------
#Project: Impactanalyse concentratie en spreiding
#Auteur:  SiRM
#Datum:   januari 2025
#Doel:    Inladen scenario's (1d)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------#
#------------------------ Scenario's samenvoegen ------------------------------#
#------------------------------------------------------------------------------#

tabbladen = read_excel(paste0(analyse, "# Databestand scenario's C&S"), sheet = 'Tabbladen inlezen')

scenarios <- setNames(data.table(matrix(ncol = 8, nrow = 0)), c("Scenario", "Aandoening", "Interventie", "Type_verdeling", "Instelling", "Herverdeling", "Subregio", "Hoofdregio"))
scenarios <- mutate_all(scenarios, as.character)

hoofdscenarios <- setNames(data.table(matrix(ncol = 4, nrow = 0)), c("Hoofdscenario","Scenario", "Subregio", "Hoofdregio"))
hoofdscenarios <- mutate_all(hoofdscenarios, as.character)

for(i in 1:nrow(tabbladen)){
  tabblad <- tabbladen[i,]$Tabbladnaam
  subregio <- tabbladen[i,]$Subregio
  hoofdregio <- tabbladen[i,]$Hoofdregio
  
  scenarios_subregio = read_excel(paste0(analyse, "# Databestand scenario's C&S"), sheet = tabblad)
  scenarios_subregio$Herverdelingsmogelijkheid = NULL
  scenarios_subregio$Type_verdeling = ifelse(rowSums(scenarios_subregio == 'Verdelen o.b.v. reistijd') > 0 & rowSums(scenarios_subregio == 'Ontvangen o.b.v. reistijd') > 0, 'Concentreren en spreiden', 
                                        ifelse(rowSums(scenarios_subregio == 'Ontvangen o.b.v. reistijd') > 0, 'Spreiden', 
                                               ifelse(rowSums(scenarios_subregio == 'Verdelen o.b.v. reistijd') > 0, 'Concentreren','Ziekenhuis blijft aanbieder')))
  hoofdscenarios_subregio <- distinct(scenarios_subregio[,c("Hoofdscenario", "Scenario")])
  scenarios_subregio$Hoofdscenario = NULL
  scenarios_subregio = setDT(scenarios_subregio) %>% melt(id.vars = c('Scenario', 'Aandoening', 'Interventie', 'Type_verdeling'), variable.name = 'Instelling', value.name = 'Herverdeling')
  scenarios_subregio$Subregio = subregio
  scenarios_subregio$Hoofdregio = hoofdregio
  hoofdscenarios_subregio$Subregio <- subregio
  hoofdscenarios_subregio$Hoofdregio <- hoofdregio
  
  scenarios <- rbind(scenarios, scenarios_subregio)
  hoofdscenarios <- rbind(hoofdscenarios, hoofdscenarios_subregio)
  rm(scenarios_subregio, hoofdscenarios_subregio)
}

write_xlsx(hoofdscenarios, paste0(analyse, '# Databestand koppeltabel hoofdscenarios.xlsx'))

rm(hoofdscenarios)


scenarios$Type_verdeling2 = ifelse(scenarios$Herverdeling == 'Voert behandeling niet uit', 'Voert behandeling niet uit', scenarios$Type_verdeling)
scenarios$Type_verdeling = scenarios$Type_verdeling2
scenarios$Type_verdeling2 = NULL

excluderen        = scenarios[scenarios$Herverdeling == 'Excluderen',]
voert_niet_uit    = scenarios[scenarios$Herverdeling == 'Voert behandeling niet uit',]
geen_herverdeling = scenarios[scenarios$Herverdeling == 'Geen herverdeling',]

scenarios = scenarios[!scenarios$Herverdeling == 'Excluderen',]
scenarios = scenarios[!scenarios$Herverdeling == 'Voert behandeling niet uit',]
scenarios = scenarios[!scenarios$Herverdeling == 'Geen herverdeling',]

