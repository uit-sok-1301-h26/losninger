# SOK-1301 Forelesning 6
# Måling av ulikhet

# Lag figur med P90/P10, S80/S20 og GINI over tid

rm(list = ls())

library(rjstat)
library(httr)
library(tidyverse)

url <- "https://data.ssb.no/api/pxwebapi/v2/tables/07756/data?lang=no&outputFormat=json-stat2&valuecodes[Tid]=*&valuecodes[ContentsCode]=*&valuecodes[Forbruksenhet2]=*&heading=Tid,Forbruksenhet2&stub=ContentsCode"

df <- GET(url) %>%
  content(as = "text", encoding = "UTF-8") %>%
  fromJSONstat() %>%
  as_tibble()

unique(df$statistikkvariabel)

# vi velger ut de dataene som er interessant for oss og lage et nytt datasett
# endrer navn på variable og år som heltall

utvalgt_data <- df %>%
  mutate(år = as.integer(år)) %>% 
  rename(var = statistikkvariabel, verdi = value) %>% 
  filter(var %in% c("P90/P10", "S80/S20", "Ginikoeffisient")) %>% 
  # kunne evt ha bruktr filter(var != "Standardavvik Ginikoeffisient") %>% 
  filter(person == "Hele befolkningen")

# plott verdiene

utvalgt_data %>% 
  ggplot(aes(x = år, y = verdi, color = var)) +
  geom_line() +
  geom_point() +
  labs(
    title = "P90/P10, Ginikoeffisient og S80/S20",
    subtitle = "Norge, 1986-2024 (Hele befolkningen)",
    x = "År",
    y = "Verdi",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle =  element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 14),
    legend.position = "bottom"
  )



# dersom vi vil endre på hvilke år som vises på x-aksen, kan vi gjøre det slik:
# scale_x_continuous(breaks = seq(1986, 2024, by = 4))
# da får vi hvert 4. år


# Ta med en dynamisk tittel - vi bruker min og maks år fra datasettet

utvalgt_data %>% 
  ggplot(aes(x = år, y = verdi, color = var)) +
  geom_line() +
  geom_point() +labs(
    title = paste0(
      "P90/P10, Gini og S80/S20 i Norge, ",
      min(utvalgt_data$år),
      "-",
      max(utvalgt_data$år),
      " (Hele befolkningen)"
    ),
    x = "År",
    y = "Verdi",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = seq(min(utvalgt_data$år), max(utvalgt_data$år), by = 4)) # ta med hvert 4. år
