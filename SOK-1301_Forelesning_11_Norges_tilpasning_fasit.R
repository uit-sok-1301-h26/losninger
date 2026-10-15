# SOK-1301 Forelesning 11

# Norges valg mellom fritid og konsum



# rydd

rm(list=ls())

# last inn pakken
library(tidyverse)

#lag aksen for tegningen

axes_1 <- ggplot()+
  labs(x=expression("Fritid"), y=expression("Konsum"))+
  theme(axis.title = element_text(size = 15),
        plot.title = element_text(size = 15),
        panel.background = element_blank(), # hvit bakgrunn
        axis.line = element_line(colour = "black"), # sett inn akselinjer
        axis.title.x = element_text(hjust = 1),
        axis.title.y = element_text(angle = 0, vjust = 1, hjust=1))+ # juster labels på aksene  
  scale_x_continuous(limits = c(15, 25), expand = c(0, 0))+
  scale_y_continuous(limits = c(0, 200), expand = c(0, 0)) # begrense aksene
# og sikre at akselinjene møttes i (0,0).

axes_1

# nyttefunkjsonen som brukes U=F^.783K^.15 (Cobb-Douglas)
# F=fritid, K=konsum

# vi angir noen indifferenskurver

I_0 <- function(F) (21.802^6.67)/F^(5.22) # nyttenivå 21.802
I_1 <- function(F) (20^6.67)/F^(5.22) # nytte=20


figur_1 <- axes_1 + labs(title="Norges tilpasning, 2021")+
  stat_function(
    fun=I_0,
    mapping = aes()
  ) +
  stat_function(
    fun=I_1,
    mapping = aes()
  ) +
  annotate("text",x=24,y=60, label=expression(u[0]))+
  annotate("text",x=24,y=40, label=expression(u[1]))

figur_1

# vi angir budsjettlinjen

# Vi må regne ut fra oppgitt info

# Det er 24*365 timer pr år = 8760 timer
# Av disse jobber man 1408 timer, dvs. fritid 7352 timer
# Fritid pr. dag er 7352/24=20.14 

# Husholdningsinntekten er 47687 USD
# Lønn pr. time arbeid blir 47687/1408=33.87 USD
# Konsum per dag er 47687/365=130.65 USD 

# Budsjettbetingelsen per dag er K = 33.87*(24-F)
 

buds_0 <- function(F) 33.87*(24-F)

figur_2 <- figur_1+
  stat_function(
    fun=buds_0,
    mapping = aes()
  )

figur_2

# Vi ser at optimal tilpasning er der indifferenskurven er tangent til budsjettlinjen
# verdiene F=20.14 og K=130.65

figur_3 <- figur_2 +
  geom_segment(aes(x=20.14, y=0, xend=20.14, yend=130.65), linetype="dashed")+
  geom_segment(aes(x=15, y=130.65, xend=20.14, yend=130.65), linetype="dashed")+
  annotate("text",x=20.5,y=133, label="Norge")+
  geom_point(aes(x = 20.14, y = 130.65), color = "red", size = 2)

figur_3



# Hvordan tegne U* generelt hvis man har Cobb-Douglas-nyttefunksjon med kjente parametere, og vi vet F og K?

# Pakker
library(tidyverse)

# Kjente parametere, endre til dine egne
alpha <- 0.783
beta  <- 0.15
w     <- 33.87      # timelønn
F_star <- 20.14
K_star <- 130.65

# Definer nyttefunksjonen og budsjettlinjen
U <- function(F, K) F^alpha * K^beta           # Cobb-Douglas nytte
budget <- function(F) w * (24 - F)             # Budsjettlinje

# Finn nyttenivået i optimum (det tangente punktet)
U_star <- U(F_star, K_star)
U_star

# Løs indifferenskurven som funksjon av F
I_star <- function(F) (U_star / F^alpha)^(1/beta)

# Lag figur med aksene, budsjettlinje og indifferenskurve
axes <- ggplot() +
  labs(x = "Fritid (timer per dag)", y = "Konsum per dag (USD)") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(limits = c(15, 25)) +
  scale_y_continuous(limits = c(0, 200))

figur <- axes +
  stat_function(fun = budget, color = "blue", size = 1.1) +
  stat_function(fun = I_star, color = "red", size = 1.1) +
  geom_point(aes(x = F_star, y = K_star), color = "black", size = 3) +
  geom_segment(aes(x = F_star, y = 0, xend = F_star, yend = K_star),
               linetype = "dashed") +
  geom_segment(aes(x = 15, y = K_star, xend = F_star, yend = K_star),
               linetype = "dashed") +
  annotate("text", x = 21, y = 140, label = "Norge (E*)", size = 5) +
  annotate("text", x = 24, y = I_star(24)+7, label = expression(U[0]), color = "red") +
  labs(title = "Norges valg mellom fritid og konsum")

figur

# Sjekk at den faktisk er tangent
MRS_star <- (alpha / beta) * (K_star / F_star)
MRS_star   # Skal være omtrent lik w = 33.87

