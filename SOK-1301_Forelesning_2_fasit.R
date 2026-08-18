
# SOK-1301 Kode til Forelesning 2

# Løsningsforslag


# slett minne, last inn tidyverse og Ecdat

rm(list=ls())

library(tidyverse)
library(Ecdat)


# henter ferdiglagret data
data(Wages)

# forklaring: https://rdrr.io/cran/Ecdat/man/Wages.html
# NB! Les denne.

# se på data 
Wages

# dette er lite oversiktlig

# vi lagrer dataene som en tibble, se kapittel 3 i pensum

Wages <- as_tibble(Wages)

Wages

# dette er mye mer oversiktlig!

# se på type objekt i hver kolonne (str for structure/struktur)
str(Wages)


# Int er integer (heltall), num er numeric, 
# Factor er en faktor 
# (en datatype som brukes til å representere kategoriske data som deles inn i kategorier uten en naturlig orden, 
# for eksempel kjønn (mann, kvinne)).

# Vi bruker "piping" fra tidyverse for å gjøre koden lettere å arbeide med. 
# Pipesymbolet er %>% : Ctrl + Shift + M i Windows, Cmd + Shift + M på Mac.



# først et enkelt eksempel:
select(Wages,lwage)

# forklar kommandoen. hva skjedde?

# prøv denne:
Wages %>%
  select(lwage)

# hva skjedde?

# piping lar oss dele opp koden, og utføre én kommando av gangen.
# symbolet %>% betyr: bruk dette som første argument i neste kommando.
# x %>% f(y) er det samme som f(x,y)!

# Hvorfor bruke piping?
# Jo, det er lettere å lese vertikalt enn horisontalt!


# I dette datasettet har vi en variabel som heter lwage  som er logaritmen til lønna. 
# For å finne lønna i USD må vi "nøytralisere" logaritmen med exp (dere vil lære om dette i BED-1007).

# Legg til en kolonne med faktisk lønn (ikke log)
Wages <- Wages %>%
  mutate(wage = exp(lwage))

# Forklar hva som skjedde her!
# mutate() fra dplyr brukes for å legge til nye kolonner i et datasett (eller overskrive en eksisterende kolonne om navnet er det samme)
# Det originale datasettet Wages trykkes ut av pipen (%>%), og en ny kolonne wage legges til med faktisk lønn. 

# Det ser ut som om wage gir oss lønn pr uke målt i USD.

# La oss gi nye navn til noen variabler

rename(Wages, utdanning = ed)
rename(Wages, ukeslønn = wage)

# Se i konsollen, og forklar hva som skjedde nå.

#############################################################
### SPØRSMÅL 1: Kan du lage denne kommandoen med en pipe? ###
#############################################################


Wages %>% 
  rename(utdanning = ed) %>%
  rename(ukeslønn = wage)



#####################################################################
### SPØRSMÅL 2: Kan du skrive om kommandoen så endringene lagres? ###
#####################################################################


Wages <- Wages %>%
  rename(utdanning = ed) %>%
  rename(ukeslønn = wage)  

# la oss prøve å lage en graf, med piping




Wages %>%
  
  ggplot(aes(x=utdanning,y=ukeslønn)) +
  
  geom_point() +
  
  theme_minimal()


# I tidyverse er det mest vanlig å bruke %>% kun frem til ggplot(), og deretter bygge opp plottet med +
# ggplot trenger kun "+" på slutten av linjen


#######################################################################
### SPØRSMÅL 3: Hva er det vi ser? Forklar så konkret som mulig. ###
#######################################################################

# På x-aksen har vi utdanning og på y-aksen har vi ukeslønn.
# Hver prikk er en observasjon (person) i et bestemt år (såkalt paneldata).
# I disse dataene ser vi en positiv sammenheng mellom utdanning og lønn i snitt.
# Vi ser spredning i data - ikke alle med samme utdanning tjener det samme.

# se på denne grafen

Wages %>%
  ggplot(aes(x = utdanning, y = ukeslønn), color = sex)) +
  
  geom_point() +
  
  theme_minimal()



# det fungerte ikke.

# Se på feilmeldingen i konsollen som antyder at vi har et problem med parentes.
# Vi ser at vi har to venstre og 3 høyre parentes.



#####################################################
### SPØRSMÅL 4: Kan du fikse koden så den kjører? ###
#####################################################

# La oss prøve å fjerne siste høyre-parentes - hjelper det?

Wages %>%
  
  ggplot(aes(x = utdanning, y = ukeslønn), color = sex) +
  
  geom_point() +
  
  theme_minimal()

# koden kjører, men hva skjer med fargen?
# vi har jo color=sex, men alle prikker er sort

# se på denne grafen
# se du hva som er annerledes?

Wages %>%
  
  ggplot(aes(x = utdanning, y = ukeslønn, color = sex)) +
  
  geom_point() +
  
  theme_minimal()

# Vi hadde satt color = sex utenfor aes().
# I ggplot2 må estetiske mappinger (som farger basert på en variabel) ligge inne i aes().

# I denne koden ligger color = sex inne i aes(),
# da brukes sex som en variabel for farge → punktene får ulike farger etter kjønn.


#####################################################
## SPØRSMÅL 5: Lag en ny figur med lwage på y-aksen##
#####################################################

# Ser du noen fordeler med å bruke logaritmen til lønna?


Wages %>%
  
  ggplot(aes(x = utdanning, y = lwage, color = sex)) +
  
  geom_point() +
  
  theme_minimal()

# det er flere fordeler med å bruke logaritmen av lønn (lwage) i stedet for selve lønna:
  
# Demper ekstremverdier:
# Store lønninger trekker ikke skalaen så mye, slik at mønstre i “vanlige” lønninger blir lettere å se.



# Bedre sammenligninger:
# Relativt små forskjeller i lav lønn blir tydeligere, mens store forskjeller i høy lønn tones ned.

# ***********************
# For de av dere som har hatt noe statistikk før:
# Mer normalfordelt:
# Lønnsdata er ofte høyreskjeve (mange med lav/moderat lønn, få med veldig høy). 
# Logaritmen gjør fordelingen mer symmetrisk og nærmere normalfordelt.

# Enklere tolkning i regresjon:
# Koeffisienter på log-lønn kan tolkes som prosentvise endringer i lønn, ikke absolutte beløp.

# *************************

# Så i et plott som dette blir sammenhengen mellom utdanning og lønn mer lesbar, 
# fordi de ekstreme toppene ikke “sprenger skalaen”

# La oss gjøre noen beregninger

Wages <- Wages %>%
  mutate(av_lønn = mean(ukeslønn))


################################################
### SPØRSMÅL 6: Hva gjorde denne kommandoen? ###
################################################
# Den koden legger til en ny kolonne i datasettet Wages som heter av_lønn, 
# og fyller den med samme verdi i hver rad: gjennomsnittet av ukeslønn for hele datasettet.

# La oss beregne prosentvis avvik fra av_lønn

Wages <- Wages %>%
  mutate(pc_avvik = 100(ukeslønn - av_lønn)/av_lønn)


#####################################################
### SPØRSMÅL 7: Kan du fikse koden så den kjører? ###
#####################################################


Wages <- Wages %>%
  mutate(pc_avvik = 100*(ukeslønn - av_lønn)/av_lønn)

# matematiske operatørene "+", "-", "/" og "*" brukes.
# legg spesielt merke til at multiplikasjon utføres med "*"

# se til slutt på denne grafen

Wages %>%
  
  ggplot(aes(x = utdanning, y = pc_avvik, color = sex)) +
  
  geom_point() +
  
  theme_minimal()

###########################################################################
### SPØRSMÅL 8: Forklar figuren som vi har laget.                       ###
###########################################################################
# Figuren viser at personer med mer utdanning ser i disse dataene oftere ut 
# til å ha lønn over gjennomsnittet. 
# Forskjellene blir større jo høyere utdanning man har.
# Menn ser ut til å være overrepresentert i de høyeste lønnsavviken

