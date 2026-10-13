# SOK-1301 Forelesning 10 - løsningsforslag

# Arbeidstid


rm(list=ls()) 


library(tidyverse)
library(eurostat) 



df <- get_eurostat("tps00071")  %>% 
  label_eurostat() 



# OPPGAVE 1: Lag en liggende stolpediagram av gjennomsnittlig arbeidstid per uke i 2025 for hvert land

# Først en enkel tilnærming
df %>% 
  filter(TIME_PERIOD == "2025-01-01") %>%                 # behold kun observasjoner for 2024
  ggplot(
    aes(x = geo, y = values) 
    ) +
  geom_col(width = 0.7) +                                 # stolpediagram; litt smalere søyler for luft
  coord_flip() +                              # gjør plottet liggende (horisontale søyler)
  geom_text(
    aes(label = sprintf("%.1f", values),                  # etikett: vis verdien med én desimal
        ),                               
    hjust = -0.1,                                         # plasser etiketten litt til høyre for stolpen
    size = 3
  ) +
  labs(
    title = "Gjennomsnittlig arbeidstid per uke 2025",    # tittel
    x = NULL,                                             # fjern x-aksens tittel (ryddigere)
    y = "Antall timer"                                    # y-aksens tittel
  ) +
  theme_minimal() 



# Vi farger også Norge-stolpen rød.
# Bruker scale_fill_manual for å spesifisere fargene på stolpene

# Filtrer til 2025, merk Norge for egen farge, og lag liggende, sortert stolpediagram
df %>% 
  filter(TIME_PERIOD == "2025-01-01") %>%                 # behold kun observasjoner for 2024
  mutate(is_norway = ifelse(geo == "Norway",              # ny variabel som markerer Norge
                            "Norway", "Other")) %>%
  ggplot(
    aes(x = reorder(geo, values),                         # sorter land (geo) etter verdi (values)
        y = values,                                       # høyden på stolpen = verdien
        fill = is_norway)                                 # fyllfarge styres av om det er Norge/ikke
  ) +
  geom_col(width = 0.7) +                                 # stolpediagram; litt smalere søyler for luft
  coord_flip(clip = "off") +                              # gjør plottet liggende (horisontale søyler)
  geom_text(
    aes(label = sprintf("%.1f", values),                  # etikett: vis verdien med én desimal
        color = is_norway),                               # tekstfarge følger samme Norge/andre-logikk
    hjust = -0.1,                                         # plasser etiketten litt til høyre for stolpen
    size = 3
  ) +
  scale_fill_manual(                                      # manuelle farger for stolpene
    values = c(Norway = "red", Other = "lightblue"),
    guide = "none"                                        # skjul forklaringsboks (legend)
  ) +
  scale_color_manual(                                     # manuelle farger for etikett-teksten
    values = c(Norway = "red", Other = "black"),
    guide = "none"                                        # skjul legend
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.06))                 # litt ekstra plass til høyre for etikettene
  ) +
  labs(
    title = "Gjennomsnittlig arbeidstid per uke 2025",    # tittel
    x = NULL,                                             # fjern x-aksens tittel (ryddigere)
    y = "Antall timer"                                    # y-aksens tittel
  ) +
  theme_minimal() +                                       # ren bakgrunn
  theme(
    panel.grid.major.y = element_blank(),                 # fjern horisontale rutenettlinjer (mindre støy)
    axis.text.x = element_text(size = 10),                # juster skriftstørrelser
    axis.text.y = element_text(size = 10),
    plot.title  = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title  = element_text(size = 10)
  )


# OPPGAVE 2: Vis utviklingen over tid i gjennomsnittlig arbeidstid i Norge

df %>% 
  filter(geo %in% c("Norway", "Sweden", "Finland", "Denmark")) %>%    # behold kun de fire landene
  ggplot(aes(x = TIME_PERIOD, y = values, color = geo)) +              # tidsakse (x), verdiakse (y), farge = land
  geom_line(linewidth = 1.5) +                                             # tegn en linje per land
  geom_point(size = 2) +                                              # marker hvert datapunkt
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +         # år-etiketter annenhvert år (format YYYY)
  coord_cartesian(ylim = c(38, 42)) +                                 # zoom til 38–42 timer uten å fjerne data
  labs(
    title = "Gjennomsnittlig arbeidstid per uke: \n Norge, Sverige, Finland, Danmark (2014–2025)",  # tittel
    x = "År",                                                         # x-aksens navn
    y = "Antall timer",                                               # y-aksens navn
    color = "Land"                                                    # legend-tittel
  ) +
  theme_minimal() +                                                   # lys/minimalistisk tema
  theme(
    panel.grid.minor = element_blank(),                               # fjern svake rutenettlinjer
    panel.grid.major.y = element_blank(),                             # fjern horisontale hovedlinjer (ryddigere)
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # sentrer og fremhev tittel
    axis.title = element_text(size = 14),                              # større aksetitler
    axis.text  = element_text(size = 12),       # større aksetekster
    legend.position = "bottom"
  )

# Bare for Norge (Forelesning 11)
df %>% 
  filter(geo == "Norway") %>%                              # kun Norge  
  filter(TIME_PERIOD>="2014-01-01") %>%                 # fra 2014 og fremover
  ggplot(aes(x = TIME_PERIOD, y = values)) +              
  geom_line(size = 1.5, color = "steelblue") +           # linje for Norge  
  geom_point(size = 2, color = "steelblue") +            # datapunkter  
  geom_text(aes(label = round(values,1)),                # tekst-etiketter (verdier)  
            vjust = -0.5, size = 3) +                     # plasser litt over punktene  
  # scale_x_date(date_breaks = "2 years", date_labels = "%Y") +  
  coord_cartesian(ylim = c(38, 41)) +                    
  labs(
    title = "Gjennomsnittlig arbeidstid per uke: Norge (2014–2025)",
    x = "År",
    y = "Antall timer"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text  = element_text(size = 12)
  )

##########################################################
# NB tallene er gjennomsnittlig antall timer som en person
# i heltidsarbeid jobber
# (betalt og ikke betalt)
#########################################################

# Tabell lfsa_ewhan2 gir gjennomsnittlig antall timer jobbet
# for alle ansatte (heltid og deltid)

df2 <- get_eurostat("lfsa_ewhan2")  %>% 
  label_eurostat() 

names(df2)
unique(df2$nace_r2)
unique(df2$wstatus)
unique(df2$worktime)
unique(df2$sex)
unique(df2$age)

# Vi vil bruke nace_r2 "Total - all NACE activities"
# og wstatus "Employed persons"
# og worktime "Total"
# og sex "Total"
# og age "From 20 to 64 years"

# lag et mindre datasett:

df_norge <- df2 %>% 
  filter(geo == "Norway",                               
  TIME_PERIOD>= "2014-01-01", 
  nace_r2 == "Total - all NACE activities",
  wstatus == "Employed persons",
  worktime == "Total",
  sex == "Total",
  age == "From 20 to 64 years"
  )

df_norge %>% 
  ggplot(aes(x = TIME_PERIOD, y = values)) +              
  geom_line(size = 1.5, color = "steelblue") +           # linje for Norge  
  geom_point(size = 2, color = "steelblue") +            # datapunkter  
  geom_text(aes(label = round(values,1)),                # tekst-etiketter (verdier)  
            vjust = -0.5, size = 3) +                     # plasser litt over punktene  
  # scale_x_date(date_breaks = "2 years", date_labels = "%Y") +  
  coord_cartesian(ylim = c(33, 36)) +                    
  labs(
    title = "Gjennomsnittlig arbeidstid per uke: Norge (2014–2025)",
    subtitle = "Full- og heltid, alder 20-64",
    x = "",
    y = "Antall timer"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text  = element_text(size = 12)
  )

# Ta med sammenligningsland

df_sammenl <- df2 %>% 
  filter(geo %in% c("Norway", "Sweden", "Finland", "Denmark"),                               
         TIME_PERIOD>= "2014-01-01", 
         nace_r2 == "Total - all NACE activities",
         wstatus == "Employed persons",
         worktime == "Total",
         sex == "Total",
         age == "From 20 to 64 years"
  )

df_sammenl %>% 
  ggplot(aes(x = TIME_PERIOD, y = values, color = geo)) +              # tidsakse (x), verdiakse (y), farge = land
  geom_line(linewidth = 1.5) +                                             # tegn en linje per land
  geom_point(size = 2) +                                              # marker hvert datapunkt
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +         # år-etiketter annenhvert år (format YYYY)
  coord_cartesian(ylim = c(33, 37)) +                                 # zoom til 38–42 timer uten å fjerne data
  labs(
    title = "Gjennomsnittlig arbeidstid per uke: \n Norge, Sverige, Finland, Danmark (2014–2025)",  # tittel
    x = "År",                                                         # x-aksens navn
    y = "Antall timer",                                               # y-aksens navn
    color = "Land"                                                    # legend-tittel
  ) +
  theme_minimal() +                                                   # lys/minimalistisk tema
  theme(
    panel.grid.minor = element_blank(),                               # fjern svake rutenettlinjer
    panel.grid.major.y = element_blank(),                             # fjern horisontale hovedlinjer (ryddigere)
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # sentrer og fremhev tittel
    axis.title = element_text(size = 14),                              # større aksetitler
    axis.text  = element_text(size = 12),       # større aksetekster
    legend.position = "bottom"
  )

