# SOK-1301 Forelesning 5

# Produksjon i Norge 

# Data fra SSB, tabell 09171
# Produksjon i basisverdi 1. kvartal 2026. Faste 2023-priser (mill. kr)
# Basisverdi er prisen selgeren mottar for varen, uten mva og avgifter med evt subsidier.


# rydd

rm(list=ls())

# last inn pakken
library(tidyverse)



# URL (csv fil)
url <- "https://raw.githubusercontent.com/uit-sok-1301-h26/uit-sok-1301-h26.github.io/refs/heads/main/data/prod_f5.csv"

# last inn fila


df <- read.csv(url, header=FALSE, sep=";")


# endre kolonnenavn

df <- df %>%
  rename(Næring = V1, Verdi = V2)

################
#  OPPGAVE    #
################

# 1 Bruk geom_col() i ggplot 2 for å lage en stolpediagram av produksjonsverdier for hver næring
# 2 Er det lett å tolke denne visualiseringen? Hva kan gjøres for å forbedre den?



# LØSNING 1

# stolpediagram av verdier
# bruk "stat = "identity"" for å plotte verdiene direkte

# stat="count" er defauilt verdi for geom_bar(), og teller antall observasjoner
# bedre å bruke geom_col()

# her er begge deler

# element_text() brukes for å endre utseende til noen tekst i figuren

options(scipen = 999)

fig0 <- df %>% 
  ggplot(aes(x = Næring, y = Verdi)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(
    title = "Produksjon i Norge i 2026 (mill. kr)",
    x = "Næring",
    y = "mill. kr"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Roter x-akse labels for lesbarhet
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )
fig0

# bruk geom_col

fig1 <- df %>% 
  ggplot(aes(x = Næring, y = Verdi)) +
  geom_col(fill = "skyblue") +
  labs(
    title = "Produksjon i Norge i 2026 (mill. kr)",
    x = "Næring",
    y = "mill. kr"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Roter x-akse labels for lesbarhet
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

fig1


#LØSNING 2

# Det er noe vanskelig å tyde verdiene for hver næring i stolpediagrammet.

# Forslag til forbedring:

# Skriv verdien på hver stolpe for å gjøre det tydeligere

# bruk scale_y_continuous(labels = scales::label_number(big.mark = " "))
# for å skille store tall, dvs 100000 blir 100 000



fig2 <- fig1 +
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = " "
    )
  )+
  geom_text(aes(label = Verdi), 
            vjust = -0.2,
            size = 3, color = "black") # Legg til verdi på stolpene

fig2

# Ble dette bedre? Hva kan gjøres for å forbedre visualiseringen ytterligere?

# Forslag til forbedring:
# Regn om verdiene til prosentandeler for å sammenligne næringenes relative bidrag

df <- df %>%
  mutate(Prosent = (Verdi / sum(Verdi)) * 100)

# sjekk at summen er 100

sum(df$Prosent)


# Lag en ny stolpediagram med prosentandeler

df %>% 
  ggplot(aes(x = Næring, y = Prosent)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Produksjon i Norge i 2026 (%)",
    x = "Næring",
    y = "Prosent (%)"
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = " "
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Roter x-akse labels for lesbarhet
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  ) +
  geom_text(aes(label = sprintf("%.1f%%", Prosent)), 
            vjust = -0.2, 
            size = 3, color = "black") # Legg til prosent på stolpene

# sprintf() hjelper med å formattere karakterer
# prøv å bruke samme kode som for verdi for å skjønne hvorfor dette kan være nyttig

# strengen jeg vil plotte er i ""
# % er en plassholder - forteller at vi vil sette noe her
# .1 betyr at vi begrenser oss til ett desimaltall
# f forteller at verdien er et tall (float)
# %% fordi vi vil ha % med for å vise prosent ("literal")
# siste % betyr "escape" her 

# dersom du ikke vil ha % med i strengen kan du skrive sprintf("%.1f", Prosent))



