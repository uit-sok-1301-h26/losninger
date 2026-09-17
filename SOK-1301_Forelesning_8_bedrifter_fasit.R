# JSON API spørring til SSB

# SOK-1301 Forelesning 8 - Løsningsforslag
# Bedrifter i Tromsø


rm(list=ls()) 

library(rjstat)
library(httr)
library(tidyverse)

url <- "https://data.ssb.no/api/pxwebapi/v2/tables/07091/data?lang=no&outputFormat=json-stat2&valuecodes[ContentsCode]=*&valuecodes[Tid]=*&valuecodes[Region]=F-55&codelist[Region]=agg_KommFylker&valuecodes[AntAnsatte]=99&valuecodes[NACE2007]=*&heading=ContentsCode,Tid&stub=Region,NACE2007,AntAnsatte"

df <- GET(url) %>%
  content(as = "text", encoding = "UTF-8") %>%
  fromJSONstat() %>%
  as_tibble()

# jeg vil slippe å skrive `` rundt "næring (SN2007)"
# R liker ikke tomrom, parentes osv i variabelnavn og ``
# forteller programmet å lese dette uansett
# norske bokstaver er vanligvis ikke problematisk

# filtrer for år 2026

df_2026 <- df %>% 
  filter(år == 2026) %>% 
  rename(næring = `næring (SN2007)`)

# hent ut tallet som viser totalt antall bedrifter

total_bedrifter <- df_2026 %>% 
  filter(næring == "Total") %>% 
  pull(value)

# regn ut prosent av totalt antall bedrifter

df_2026 <- df_2026 %>%
  filter(næring != "Total") %>%
  mutate(prosent = (value / total_bedrifter) * 100)

# lag plott

df_2026 %>% 
  ggplot(aes(x = næring, y = prosent)) +
  geom_col(fill = "green") +
  theme_minimal() +
  labs(title = "Andel av bedrifter etter næring, Tromsø kommune 2026",
       x = "Næring",
       y = "Prosent %") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# veldig uoversiktlig!

# la oss se på andeler som er minst 1%



df_2026 %>% 
  filter(prosent>=1) %>%
  ggplot(aes(x = næring, y = prosent)) +
  geom_col(fill = "green") +
  theme_minimal() +
  labs(title = "Andel av bedrifter (over 1%) etter næring, Tromsø kommune 2026",
       x = "Næring",
       y = "Prosent %") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bedre, men vi behøver ikke bruke plass på "Uoppgitt"


df_2026 %>% 
  filter(prosent>=1) %>%
  filter(næring != "Uoppgitt") %>%
  ggplot(aes(x = næring, y = prosent)) +
  geom_col(fill = "green") +
  theme_minimal() +
  labs(title = "Andel av bedrifter (over 1%) etter næring, Tromsø kommune 2026",
       x = "Næring",
       y = "Prosent %") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# blir det bedre dersom vi ordner fra størst til minst i figuren?

# bruker fct_reorder fra forcats pakken

# forcats er en del av tidyverse, så vi trenger ikke å laste inn pakken


df_2026 %>% 
  filter(prosent >= 1) %>%
  filter(næring != "Uoppgitt") %>%
  mutate(næring = fct_reorder(næring, prosent, .desc = TRUE)) %>%
  ggplot(aes(x = næring, y = prosent)) +
  geom_col(fill = "green") +
  theme_minimal() +
  labs(title = "Andel av bedrifter (over 1%) etter næring, Tromsø kommune 2026",
       x = "Næring",
       y = "Prosent %") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# til slutt kan vi ta med tallet på stolpen

df_2026 %>% 
  filter(prosent >= 1) %>%
  filter(næring != "Uoppgitt") %>%
  mutate(næring = fct_reorder(næring, prosent, .desc = TRUE)) %>%
  ggplot(aes(x = næring, y = prosent)) +
  geom_col(fill = "green") +
  geom_text(aes(label = round(prosent, 1)), vjust = 1) +
  theme_minimal() +
  labs(title = "Andel av bedrifter (over 1%) etter næring, Tromsø kommune 2026",
       x = "Næring",
       y = "Prosent %") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# en annen mulighet er å velge ut topp 10 for eksempel

df_topp_10 <- df_2026 %>% 
  filter(næring != "Uoppgitt") %>%
  arrange(desc(prosent)) %>% 
  slice(1:10)

# plott med forrige kode (tilpasset)

# plot.margin = margin(t = 10, r = 10, b = 10, l = 80)
# gir mer plass på venstre side (l=80


df_topp_10 %>% 
  mutate(næring = fct_reorder(næring, prosent, .desc = TRUE)) %>%
  ggplot(aes(x = næring, y = prosent)) +
  geom_col(fill = "green") +
  geom_text(aes(label = round(prosent, 1)), vjust = 1) +
  theme_minimal() +
  labs(title = "Andel av bedrifter etter næring, topp 10 i Tromsø kommune 2026",
       x = "Næring",
       y = "Prosent %") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 12),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 80)
        )

# liggende søyler med coord_flip()

df_topp_10 %>% 
  mutate(næring = fct_reorder(næring, prosent)) %>%
  ggplot(aes(x = næring, y = prosent)) +
  geom_col(fill = "green") +
  geom_text(aes(label = round(prosent, 1)), hjust = 1) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Andel av bedrifter etter næring, topp 10 i Tromsø kommune 2026",
    x = "Næring",
    y = "Prosent %"
  ) +
  theme(axis.text = element_text(size = 12))
