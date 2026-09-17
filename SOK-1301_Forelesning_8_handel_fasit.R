# JSON API spørring til SSB

# SOK-1004 Forelesning 8 
# Internasjonal handel

# Løsningsforslag

# Hent data fra SSBs tabell 10482

rm(list=ls()) 

library(rjstat)
library(httr)
library(tidyverse)

url <- "https://data.ssb.no/api/pxwebapi/v2/tables/10482/data?lang=no&outputFormat=json-stat2&valuecodes[ContentsCode]=*&valuecodes[Tid]=2025&valuecodes[Region]=*&valuecodes[SITC]=*&heading=ContentsCode,Tid,SITC&stub=Region"

df <- GET(url) %>%
  content(as = "text", encoding = "UTF-8") %>%
  fromJSONstat() %>%
  as_tibble()

unique(df$region)

# SSB-tabellen inneholder både dagens fylker og tidligere fylkesinndelinger.
# Vi velger derfor ut fylkene vi ønsker å sammenligne.

utvalgte_regioner <- c(
  "Oslo - Oslove",
  "Agder",
  "Rogaland",
  "Vestland",
  "Møre og Romsdal",
  "Trøndelag - Trööndelage",
  "Nordland - Nordlánnda",
  "Troms - Romsa - Tromssa",
  "Finnmark - Finnmárku - Finmarkku"
)


df_utvalgt <- df %>% 
  filter(region %in% utvalgte_regioner) 


# OPPGAVE 1. Bruk df_utvalgt og lag en stolpediagram som viser "Varer i alt" for hvert fylke

# La oss først gi nye navn til noen av fylkene

df_utvalgt <- df_utvalgt %>% 
  mutate(region = recode(
    region,
    "Oslo - Oslove" = "Oslo",
    "Trøndelag - Trööndelage" = "Trøndelag",
    "Nordland - Nordlánnda" = "Nordland",
    "Troms - Romsa - Tromssa" = "Troms",
    "Finnmark - Finnmárku - Finmarkku" = "Finnmark"
  ))

options(scipen = 999)
df_utvalgt %>% 
  filter(varegruppe == "Varer i alt") %>%
  ggplot(aes(x = region, y = value, fill = region)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Fastlandseksport etter produksjonsfylke (varer i alt), 2025",
       x = "Fylke",
       y = "Millioner NOK") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# OPPGAVE 2. Nå skal du plotte alle varegruppene for hvert fylke. 
# Vi tar først bort "Varer i alt"

df_utvalgt_ny <- df_utvalgt %>% 
  filter(varegruppe != "Varer i alt")

# vi bruker facet_wrap(~ varegruppe) for å få en graf per varegruppe

df_utvalgt_ny %>% 
  ggplot(aes(x = region, y = value, fill = region)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Fastlandseksport etter produksjonsfylke og varegruppe, 2025",
       x = "Fylke",
       y = "Millioner NOK") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  facet_wrap(~ varegruppe)


# dersom vi får vitenskapelig notasjon på aksen kan vi fjerne med options(scipen = 999)

# vi ser at det er en varegruppe som heter "¬ Fisk"
# som er en undergruppe av "Matvarer, drikkevarer, tobakk"

# bruk ifelse() som tar 3 argumenter
# ifelse (test, value_if_TRUE, value_if_FALSE)

df_endelig <- df_utvalgt_ny %>% 
  mutate(varegruppe = ifelse(varegruppe == "¬ Fisk", "Matvarer, drikkevarer, tobakk (Fisk)", varegruppe))


# OPPGAVE 3. Tegn samme figur som i oppgave 2 ved å bruke df_endelig.
# Denne gangen skal du bruke facet_wrap(~ varegruppe, scales = "free_y")
# Hva skjer?



df_endelig %>% 
  ggplot(aes(x = region, y = value, fill = region)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "Fastlandseksport etter produksjonsfylke og varegruppe, 2025",
       x = "Fylke",
       y = "Millioner NOK") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position ="none") +
  facet_wrap(~ varegruppe)

# legg merke til at vi har samme skala på y-aksen for alle varegruppene
# dette kan være misvisende, vi kan endre dette ved å legge til scales = "free_y"

df_endelig %>% 
  ggplot(aes(x = region, y = value, fill = region)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "Fastlandseksport etter produksjonsfylke og varegruppe, 2025",
       x = "Fylke",
       y = "Millioner NOK") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position ="none") +
  facet_wrap(~ varegruppe, scales = "free_y")


