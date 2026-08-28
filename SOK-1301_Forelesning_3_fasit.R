# SOK-1301 Kode til Forelesning 3

# Løsningsforslag

############################
### R-kode forelesning 3 ###
### Skrevet av:          ###
### Derek J. Clark,      ###
### Mikko Moilanen og    ###
### Even S. Hvinden      ###
### Tidligere UiT        ###
### Nå: Forsvarets       ### 
### forskningsinstitutt  ###
############################

# rydd opp
rm(list=ls())

# last inn tidyverse
library(tidyverse)

############################
### data i tibble-format ### 
############################



# les CO2 data i .csv-format fra OWID
co2data <- read_csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")


# se: https://github.com/owid/co2-data
# se en beskrivelse av data her: https://github.com/owid/co2-data/blob/master/owid-co2-codebook.csv

# se på dataene i konsollen
co2data

###########################
### kommandoen select() ### 
###########################

select(co2data,year)

# beskriv hva kommandoen gjør

###########################################################
### Oppgave 1: Kan du skrive om kommandoen med en pipe? ### 
###########################################################

co2data %>% 
  select(year)


###########################
### kommandoen filter() ### 
###########################

filter(co2data,iso_code == "SWE")


# beskriv hva kommandoen gjør

# for hver observasjon/rad i co2data, vi returnerer de som har SWE som iso_code
# == er en logisk test (True eller False)
# vi bruker == for å sammenligne, = for å fastsette et argument i en funksjon, og <- for å lagre et objekt.


co2data <- co2data %>%
  filter(!is.na(iso_code))

# beskriv hva denne kommandoen gjør
# 1. Vi begynner med co2data, 
# 2. Filtrer: 
# is.na(iso_code) sjekker om observasjonen mangler en iso_code (som Africa, Asia osv).
# NA er et manglende datapunkt. 
# Anta at vi har NOR, NA, SWE, NA, DNK og bruker is.na(iso-code)
# Dette gir FALSE, TRUE, FALSE, TRUE, FALSE
# ! bytter om TRUE og FALSE
# !is.na(iso-code) er da observasjoner som har en iso_code.
# Da filtreres ut observasjoner uten iso_code og lagrer reslutatet som co2data.



# Bonus: sjekk hvor mange ulike land/territorier vi har nå i datasettet
n_distinct(co2data$iso_code)


# 218 er svaret (noen er land og noen er territorier, som Grønland (GRL), Gibraltar (GIB) og Hong Kong (HKG))


#############################################################################
### Oppgave 2A: Kan du lage en tabell som viser verdiene for Norge mellom ###
### årene 2021 og 2023? Bruk en pipe!                                     ###
### Hint: between(year, 2021, 2023) er det samme som å si at year ligger  ###
### mellom 2021 og 2023                                                   ###
#############################################################################


co2data %>% 
  filter(iso_code == "NOR") %>% 
  filter(between(year, 2021, 2023))


# svar hvorfor følgende ikke fungerer:

co2data %>% 
  filter(iso_code=="NOR" & year==2018:2020)

# Anta at vi har disse 6 radene:
# year
# 2016
# 2017
# 2018
# 2019
# 2020
# 2021

# year==2018:2020 lager en vektor 2018 2019 2020
# Ettersom year har 6 rader, må R gjenta de tre tallene:

# year        det R sammenligner med

# 2016        2018
# 2017        2019
# 2018        2020
# 2019        2018
# 2020        2019
# 2021        2020

#R spør rad for rad

# 2016 == 2018   FALSE
# 2017 == 2019   FALSE
# 2018 == 2020   FALSE
# 2019 == 2018   FALSE
# 2020 == 2019   FALSE
# 2021 == 2020   FALSE




# Bonus: denne lager en figur med  Norge
co2data %>%
  filter(country =="Norway") %>% 
  ggplot(aes(x=year, y=co2)) +
   geom_line() +
   theme_minimal()

################################################################################
### Oppgave 2B.Lag en tabell for Norge som kun viser hvert femte år mellom   ###
### 2000 og 2020 . Bruk c() til å lage listen.                               ###
### Hint: c(2000, 2005, 2010, 2015, 2020) lager en kort liste med de årene   ###
### du vil ha.Bruk så year %in% <den listen> i filter().                     ###
################################################################################
######################
### Kommandoen c() ###
######################
# c() er kort for "concatenate" -- et fancy begrep for å koble sammen
# nyttig for å lage lister
liste <- c("Ola", "Geir")
liste

### Tips: Bruk filter(var %in% c("verdi1", "verdi2))

# Vi lager først en vektor med årene vi vil beholde:
aar <- c(2000, 2005, 2010, 2015, 2020)

# Deretter bruker vi %in% til å beholde rader der year er ett av disse årene:
co2data %>%
  filter(country == "Norway") %>%
  filter(year %in% aar)

# Vi kunne også skrevet alt direkte i filter():

co2data %>%
  filter(country == "Norway") %>%
  filter(year %in% c(2000, 2005, 2010, 2015, 2020))




### klarer du også å lage en figur med disse årene?####

co2data %>%
  filter(country == "Norway") %>%
  filter(year %in% aar) %>%
  ggplot(aes(x = year, y = co2)) +
  geom_line() +
  geom_point() +
  theme_minimal()


##########################################################################
### Oppgave 3: Kan du lage en figur med CO2 utslipp for Norge og Kina? ###
##########################################################################

### Tips: Bruk filter(var %in% c("verdi1", "verdi2))
### Tips: Bruk color = country i aes()

co2data %>%
  filter(country %in% c("Norway", "China")) %>%
  ggplot(aes(x = year, y = co2, color = country)) +
  geom_line() +
  theme_minimal()

# kommenter figuren. gir den en god sammenligning?

# Figuren viser totale CO2-utslipp. Kina har langt høyere totale utslipp
# enn Norge, slik at variasjonen i Norges utslipp blir vanskelig å se.
# Figuren er derfor ikke spesielt god dersom målet er å sammenligne
# utviklingen i de to landene relativt til deres størrelse.

#############################################################################
### Oppgave 4: Kan du lage en figur fra 1990 med CO2 utslipp pr. person for Norge, ###
### Kina, Frankrike, USA og Saudi Arabia?                                 ###
#############################################################################

countries <- c("Norway", "China", "France", "United States", "Saudi Arabia")

co2data %>%
  filter(country %in% countries) %>%
  filter(year >= 1990) %>%
  ggplot(aes(x = year, y = co2_per_capita, color = country)) +
  geom_line() +
  theme_minimal()

# beskriv figuren. hva er det vi ser? hva legger du merke til? 
# hva tror du ligger bak forskjellene? 

# Her sammenligner vi utslipp per person, og sammenligningen blir derfor
# mer meningsfull enn når vi bruker totale utslipp.
# USA og Saudi-Arabia ligger høyt per person, Kina har steget tydelig,
# og Frankrike ligger lavere. Norge ligger under de høyeste landene.
#
# Mulige forklaringer kan være forskjeller i inntekt, energimiks,
# næringsstruktur, transportbehov og klimapolitikk.
# Figuren alene kan imidlertid ikke fortelle oss hva som forårsaker forskjellene.

############################
### kommandoen arrange() ### 
############################

# vi bruker arrange() til å lage en ordnet tabell

# eksempel 
arrange(co2data, co2)

# prøv med flere variabler

df <- co2data %>%
  arrange(gdp,methane,co2)

# skriver den ut i konsollen. 
df

# Eksempel utenom datasettet

df1 <- tibble(x = c(3, 3, 4, 2), y = c("B", "A", "D", "C"))

df1

arranged_df1_x <- df1 %>% arrange(x)

arranged_df1_x

arranged_df2_x <- df1 %>% arrange(desc(x))

arranged_df2_x                                  

arranged_df1_y <- df1 %>% arrange(y)

arranged_df1_y

arranged_df1_x_y <- df1 %>% arrange(x,y)

arranged_df1_x_y

##########################################################################
### Oppgave 5: Hvilke land hadde høyest CO2-utslipp per person i 2023? ###
### Filtrer først på 2023 og sorter deretter fra høyest til lavest.    ###
##########################################################################

co2data %>%
  filter(year == 2023) %>%
  arrange(desc(co2_per_capita)) %>%
  select(country, co2_per_capita)

###########################
### kommandoen mutate() ### 
###########################

# Variabelen co2 viser utslipp i millioner tonn. 
# For å få utslipp målt i tonn ganger vi med 1 000 000:

# dersom du får an y-aksen med tall i vitenskapelig notasjon (2e+07 for eksempel)
# kan du sette options(scipen=999) før koden


options(scipen = 999)

co2data %>%
  mutate(co2_tonn=1e6*co2) %>% 
  filter(country =="Norway") %>% 
  ggplot(aes(x=year, y=co2_tonn)) +
   geom_line()  +
   theme_minimal()


#################################################################################################################################################################
### Oppgave 6: Variabelen consumption_co2_per_capita måler utslipp knyttet                                                                                    ###
### til konsum. Bruk mutate() til å lage en variabel som viser utslipp per                                                                                    ###
### capita utenom konsum. Gjenskap figuren med Norge, USA, Kina, Frankrike                                                                                    ###
### og Saudi Arabia.                                                                                                                                          ###
### Hvis differansen er positiv → landet er en nettoeksportør av karbonutslipp (slipper ut mer hjemme enn som trengs til eget konsum).                        ###
### Hvis differansen er negativ → landet er en nettoimportør av karbonutslipp (forbruket gir opphav til mer utslipp i andre land enn det man produserer selv).###
### Kommenter. Ble du overrasket?                                                                                                                             ###
### Hint: du trenger!is.na() for å fjerne manglende observasjoner                                                                                             ###
#################################################################################################################################################################

countries <- c("Norway", "China", "France", "United States", "Saudi Arabia")


# beskriv figuren. hva er det vi ser? hva legger du merke til?

co2data %>%
  mutate(
    netto_co2_per_capita = co2_per_capita - consumption_co2_per_capita
  ) %>%
  filter(country %in% countries) %>%
  filter(year >= 1990) %>%
  filter(!is.na(netto_co2_per_capita)) %>%
  ggplot(aes(x = year, y = netto_co2_per_capita, color = country)) +
  geom_line() +
  theme_minimal()

# Tolkning:
# netto_co2_per_capita > 0:
#   produksjonsbaserte utslipp er høyere enn konsumrelaterte utslipp.
#   Landet er nettoeksportør av utslipp gjennom handel.
#
# netto_co2_per_capita < 0:
#   konsumrelaterte utslipp er høyere enn produksjonsbaserte utslipp.
#   Landet er nettoimportør av utslipp gjennom handel.
#
# Et interessant spørsmål er om landets rolle som
# eksportør/importør av varer samsvarer med det vi forventet.
# Figuren beskriver mønsteret, men forklarer ikke i seg selv hvorfor det oppstår.


##############################
### kommandoen summarise() ### 
##############################

# n() gir antall observasjoner
co2data %>%
  group_by(year) %>% 
  summarise(sum = sum(co2), n=n())



###################################################################
### Oppgave 7: Lag en figur med totale globale utslipp over tid ### 
###################################################################

# Vi har tidligere fjernet rader uten iso_code. Dermed unngår vi
# å summere egne OWID-aggregater som "World" og kontinenter sammen
# med landene.

dfglobal_co2 <- co2data %>%
  group_by(year) %>%
  summarise(
    global_co2 = sum(co2, na.rm = TRUE)
  )

dfglobal_co2

dfglobal_co2 %>%
  ggplot(aes(x = year, y = global_co2)) +
  geom_line() +
  theme_minimal()


# Figuren viser en kraftig langsiktig økning i globale CO2-utslipp.
# Det er nivået på de samlede utslippene fra landene i datasettet
# som summeres for hvert år.


##########################################################################
### Oppgave 8: Lag en vakker graf. Legg til ny farge, tittel, lag egne ###
### benevninger på aksene, skalering, tykkere linjer. Prøv deg frem!   ### 
##########################################################################

# Ett mulig løsningsforslag basert på figuren i oppgave 7:

dfglobal_co2 %>%
  ggplot(aes(x = year, y = global_co2)) +
  geom_line(
    color = "steelblue",
    linewidth = 1.2
  ) +
  labs(
    title = "Globale CO2-utslipp over tid",
    x = "År",
    y = "CO2-utslipp (millioner tonn)"
  ) +
  theme_minimal()



# Poenget i oppgaven er ikke at figuren må se akkurat slik ut.
# Dere kan prøve ulike farger, temaer, linjetykkelser,
# aksetitler og andre ggplot-innstillinger.

# sjekk for eksempel her: https://r-charts.com/ggplot2/
# eller her: https://ggplot2.tidyverse.org/reference/ggtheme.html
# tips: bruk xlab(), ylab(), ggtitle()...