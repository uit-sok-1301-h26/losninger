# JSON API spørring til SSB

# SOK-1301 Forelesning 7
# Produktivitet

# Utvalg av næringer er opp til deg
# Jeg har valgt ut: 
# Bygge- og anleggsvirksomhet
# Elektrisitets-, gass- og varmtvannsforsyning
# Fiske, fangst og akvakultur
# Undervisning
# Varehandel og reparasjon av motorvogner


rm(list=ls()) 

library(rjstat)
library(httr)
library(tidyverse)


url <- "https://data.ssb.no/api/pxwebapi/v2/tables/09174/data?lang=no&outputFormat=json-stat2&valuecodes[Tid]=*&valuecodes[NACE]=pub2X03,pub2X35,pub2X41_43,pub2X45_47,pub2X85&codelist[NACE]=vs_NRNaeringPubAgg&valuecodes[ContentsCode]=ProduksjonTimev&heading=Tid,ContentsCode&stub=NACE"

df <- GET(url) %>%
  content(as = "text", encoding = "UTF-8") %>%
  fromJSONstat() %>%
  as_tibble()


# endre år til numerisk verdi

df <- df %>% 
  mutate(år = as.integer(år))

df %>% 
  ggplot(aes(x = år, y = value, color = næring)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Produktivitet i utvalgte næringer, 1970-2025",
    subtitle = "Produksjon per utførte timeverk. Endring fra året før (prosent). Faste priser",
    x = "År",
    y = "Prosent (%)",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = seq(min(df$år), max(df$år), by = 5)) # ta med hvert 5. år

# prøv å lage bedre oversikt med facet_wrap()

df %>% 
  ggplot(aes(x = år, y = value, color = næring)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Produktivitet i utvalgte næringer, 1970-2025",
    subtitle = "Produksjon per utførte timeverk. Endring fra året før (prosent). Faste priser",
    x = "År",
    y = "Prosent (%)",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = seq(min(df$år), max(df$år), by = 5)) + 
  facet_wrap(~ næring)

# litt vanskelig å lese fordi én næring har store endringer
# bruk scales = "free" eller scales = "free_y" i facet_wrap()
# ser du forskjellen?

df %>% 
  ggplot(aes(x = år, y = value, color = næring)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Produktivitet i utvalgte næringer, 1970-2025",
    subtitle = "Produksjon per utførte timeverk. Endring fra året før (prosent). Faste priser",
    x = "År",
    y = "Prosent (%)",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = seq(min(df$år), max(df$år), by = 5)) + 
  facet_wrap(~ næring, scales = "free_y")

# ta med en linje på y=0?
# bruk axes = "all_x" i facet wrap() dersom du vil ha årstall i alle paneler
# scales = "free" gir samme resultat her, men brukes dersom du vil ha forskjellige skala
# på begge aksene

df %>% 
  ggplot(aes(x = år, y = value, color = næring)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Produktivitet i utvalgte næringer, 1970-2025",
    subtitle = "Produksjon per utførte timeverk. Endring fra året før (prosent). Faste priser",
    x = "År",
    y = "Prosent (%)",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = seq(min(df$år), max(df$år), by = 5)) + 
  facet_wrap(~ næring, scales = "free_y", axes = "all_x")




# forbedre utseendet på figuren ytterligere
# Her er en penere variant med ryddigere typografi, bedre luft, tydeligere titler/undertitler, strip-overskrifter på fasettene og en kort kilde-caption. 
# vi har en fargeblind-vennlig palett (Okabe–Ito), tynnet ut gridlinjer og lagt på litt større linjer/punkter.
# Vi legger også på en trenglinje


# Okabe–Ito-palett (fargeblind-vennlig)
okabe_ito <- c(
  "Bygge- og anleggsvirksomhet" = "#E69F00",
  "Elektrisitets-, gass- og varmtvannsforsyning" = "#56B4E9",
  "Fiske, fangst og akvakultur" = "#009E73",
  "Undervisning" = "#0072B2",
  "Varehandel og reparasjon av motorvogner" = "#CC79A7"
)

df %>% 
  ggplot(aes(x = år, y = value, color = næring)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4, color = "grey35") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.1, color = "grey35") +  # trendlinje
  facet_wrap(~ næring, scales = "free_y", ncol = 2,
             labeller = labeller(
               næring = label_wrap_gen(width = 35)
             )) +
  scale_x_continuous(breaks = seq(min(df$år, na.rm = TRUE), max(df$år, na.rm = TRUE), by = 5),
                     expand = expansion(mult = c(0.01, 0.03))) +
  scale_color_manual(values = okabe_ito, guide = "none") +  # fjerner legenden (én serie per fasett)
  labs(
    title = "Produktivitet i utvalgte næringer (1970–2025)",
    subtitle = "Produksjon per utførte timeverk. Årsendring i faste priser (prosent)",
    x = "År",
    y = "Prosent (%)",
    caption = "Kilde: SSB, Tabell 09174."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "grey20", lineheight = 1.15),
    plot.caption = element_text(size = 9, color = "grey35"),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.25, color = "grey88"),
    panel.grid.major.y = element_line(linewidth = 0.25, color = "grey88"),
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text = element_text(face = "bold", size = 10,
                              lineheight = 0.95,
                              margin = margin(t = 4, b = 4)),
    plot.margin = margin(10, 12, 10, 12)
  )

# Du kan eksperimentere med facet_wrap(~ næring, scales = "free_y", ncol = 2, labeller = labeller(næring = label_wrap_gen(width = 35)))
# ncol er antall kolonner, og width er bredden
