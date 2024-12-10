#######################
### OLA1 OPGAVE 2.1 ###
#######################
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(corrplot)
install.packages("reshape2")
library(reshape2)
library(skimr)


# Før vi kan lave beskrivende statistik, bliver vi nødt til at rydde op i datasættet.
    # webscraping giver en meget rå data der typisk er i stykker eller forkert
    # Før vi går i gang med beskrivende statistik, laver vi derfor Data cleaning. 

summary(boligsiden) # tjekker den beskrivende statistik for at få en ide om outliers eller forkert indtastede data
colnames(boligsiden)[11] = "antaldagetilsalg" 
View(boligsiden)

# - pris er ikke numerisk; evt. flere fejl

boligsiden$pris = as.numeric(gsub("[^0-9]", "", boligsiden$pris))


    # - mange outliers med helt forkerte priser (hus til 300k med kvmpris. 825k og størrelse 287kvm?)
    # - såvel som huse 
    # - Det kan være grundet at andelsboliger også er scrapet med. 
    # - fremhævede boliger kommer stadig frem på hjemmesiden (kan resultere i mærkelig scrapedata) 

### PRIS ###
  # - vi sætter en rimelig grænse for normale boliger på 1.5m - 15m
    # vi sætter et højere max end normale boligers pris for at få nogle af de 'normale' dyre huse med, ikke bare strandvillaerne med egen sø (eks. taarbæk-strandvej)


boligsiden.clean = boligsiden[boligsiden$pris >= 1500000 & boligsiden$pris <= 15000000,]


# - mange NA'er i postnummer? 
# - kvmpris (max på 825k) - mange outliers med kvmpris på 100k ish
    # - selv undersøgt på hjemmesiden for boligerne med højest kvmpris - de virker ikke urimelige (eksklusive lokationer)
    # - for at fjerne outliers sætter vi en begræsning på >= 3000 og <= 100000

boligsiden.clean$kvmpris = boligsiden.clean$kvmpris * 1000


boligsiden.clean = boligsiden.clean[boligsiden.clean$kvmpris >= 3000 & boligsiden.clean$kvmpris <= 100000, ]

# - størrelse (min på 1kvm?)
  # antagelse om at boliger ligger mellem 50-300 kvm 
boligsiden.clean = boligsiden.clean[boligsiden.clean$størrelse >= 50 & boligsiden.clean$størrelse <= 300, ]

# - månedlige udgifter (max på 1m?)
  # - antagelse om at der indenfor intervall med boligpris på max 15m, vil det være urimeligt med en månedlig udgift på mere end 20.000
boligsiden.clean$mdudg = boligsiden.clean$mdudg * 1000

boligsiden.clean = boligsiden.clean[boligsiden.clean$mdudg <= 20000, ]

# - grund er helt væk: min på 1.000 max på 1m?
  # - nogle steder agerer punktum som tusindetalsseperator og andre som et komma
  # - fikses vha. følgende loop

boligsiden.clean <- boligsiden.clean %>%
  mutate(
    nygrund = ifelse(
      !is.na(str_extract(grund, "^\\d{3}")),  
      str_extract(grund, "^\\d{3}"),           
      ifelse(
        !is.na(str_extract(grund, "^\\d{2}")),   
        str_extract(grund, "^\\d{2}"),           
        grund * 1000                            
      )
    ))

boligsiden.clean$grund <- as.numeric(boligsiden.clean$nygrund)
boligsiden.clean <- boligsiden.clean[,-c(13)]
  # afgrænser grundstørrelse til mellem 100 - 4000 (tager hensyn til eks. i jylland med store landarealer tilknyttet bolig)


boligsiden.clean = boligsiden.clean[boligsiden.clean$grund >= 100 & boligsiden.clean$grund <= 4000,]

# - værelser ser normalt ud
# - liggetid - fjern dage så vi kan kigge på numerisk værdi
boligsiden.clean$antaldagetilsalg = gsub("[^0-9]", "", boligsiden.clean$antaldagetilsalg)
# - postnr: postnummeret står i forkert kolonne (by) 

rownames(boligsiden.clean) = 1:nrow(boligsiden.clean)
print(boligsiden.clean[1395, ]) 

boligsiden.clean$by[1395] = "hilleroed"
boligsiden.clean$postnr[1395] = 3400 


# - generelt mange NA værdier - skal fjernes fra kolonner af betydning: eks. pris, vejnavn, postnr osv. 
# - definere kolonner hvor NOT NULL skal være TRUE;
    # - pris, vejnavn, postnr, størrelse, grund, kvmpris

boligsiden.clean = boligsiden.clean %>%  
  filter(!is.na(pris) & !is.na(vej) & !is.na(postnr)
         & !is.na(størrelse) & !is.na(grund) & !is.na(kvmpris))

# vis dataframen efter den er blevet cleaned

## Beskrivende statistik 

# Ideer:
  # - barplots med gns. pris pr. x. (eks. værelse  - opdeling af ) - måske histogram i stedet for?
  # - mean, kvartiler, min, max 
  # - måske boxplots for at vise outliers er fjernet? 
  # - sd/spredning
  # - måske frekvens? 

# lav normalt summary over de kolonner hvor det giver mening i opgaven

# Standarddeviation for relevante kolonner

boligsiden.clean$antaldagetilsalg = as.numeric(boligsiden.clean$antaldagetilsalg)

sd.pris = sd(boligsiden.clean$pris)
sd.kvmpris = sd(boligsiden.clean$kvmpris)
sd.størrelse = sd(boligsiden.clean$størrelse)
sd.mdudg = sd(boligsiden.clean$mdudg)
sd.grund = sd(boligsiden.clean$grund)
sd.antaldagetilsalg = sd(boligsiden.clean$antaldagetilsalg, na.rm = T)

str(boligsiden.clean)
# frekvens af boliger til salg pr. område
sjælland.o.postnr = c(1000:4999)
fyn.o.øer = c(5000:5999)
jylland = c(6000:9999)
boligsiden.clean$postnr = as.numeric(boligsiden.clean$postnr)
boligsiden.clean$område = with(boligsiden.clean,
                        ifelse(postnr %in% sjælland.o.postnr, "sjællandogøerne",
                        ifelse(postnr %in% fyn.o.øer, "fynogøerne",
                        ifelse(postnr %in% jylland, "jylland", "særpostnr"))))

ggplot(boligsiden.clean, aes(x=område)) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Fordeling af huse til salg pr. område",
       x = "område",
       y = "huse til salg i området") +
  theme_minimal()

# plot af husalder: 

gamlehuse = c(1200:1900)
ældrehuse = c(1901: 1970)
nyerehuse = c(1971:2010)
heltnyehuse = c(2011:2024)

boligsiden.clean$husalder = with(boligsiden.clean,
                          ifelse(opført %in% gamlehuse, "gammelt hus",
                          ifelse(opført %in% ældrehuse, "ældre hus",
                          ifelse(opført %in% nyerehuse, "nyere hus",
                          ifelse(opført %in% heltnyehuse, "helt nyt hus", "NA")))))

boligsiden.clean$husalder = factor(boligsiden.clean$husalder, levels = c("gammelt hus", 
                                                                         "ældre hus", 
                                                                         "nyere hus",
                                                                         "helt nyt hus"))

ggplot(boligsiden.clean, aes(x=husalder)) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Størstedelen af boligerne til salg opført mellem 1901 - 1970",
       subtitle = "Fordelingen af boligerne, opdelt efter alder",
       x = NULL,
       y = "Frekvens",
       caption = "Kilde: wulfs webscraping af boligsiden.dk") +
  theme_bw() +  theme(
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank(),  
    panel.grid.minor.y = element_blank(),  
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(vjust = 1, size = 12))


 

# plot af gnspris pr. huskategori

gnspris.husalder = boligsiden.clean %>% 
  group_by(husalder) %>% 
  summarize(gnspris = mean(pris, na.rm = T))

options(scipen = 999)

ggplot(gnspris.husalder, aes(x = husalder, y = gnspris, fill = husalder)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "de dyreste boliger er helt nybyggede boliger",
       x = NULL,
       y = "gennemsnitlig pris i kr.",
       subtitle = "Hvilken huskategori har den dyreste gennemsnitspris?",
       caption = "Kilde: Wulfs webscraping af boligsiden.dk") +
  theme_bw() +  theme(
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank(),  
    panel.grid.minor.y = element_blank(),  
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(vjust = 1, size = 12))


# plot af gnspris pr. område 

gnspris.område = boligsiden.clean %>% 
  group_by(område) %>% 
  summarize(gnspris = mean(pris, na.rm = T))

ggplot(gnspris.område, aes(x = område, y = gnspris, fill = område)) + 
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Boliger til salg på sjælland og øerne er dyrere end resterende områder i Danmark",
       subtitle = "Prisforskellen mellem boliger til salg, opdelt efter demografiske forhold",
       x = NULL,
       y = "gennemsnitlig pris i kr.",
       caption = "Kilde: Wulfs webscraping af boligsiden.dk") +
  theme_bw() +  theme(
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank(),  
    panel.grid.minor.y = element_blank(),  
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(vjust = 1, size = 12))



  # plot af pris-kategorier
huse3m = c(0:3000000)
huse6m = c(3000001:6000000)
huse9m = c(6000001:9000000)
huse12m = c(9000001:12000000)
huse15m = c(12000001:15000000) 

boligsiden.clean$priskategori = with(boligsiden.clean,
                                ifelse(pris %in% huse3m, "Boliger til maks 3 mil. kr.",
                                ifelse(pris %in% huse6m, "Boliger mellem 3-6 mil. kr.",
                                ifelse(pris %in% huse9m, "Boliger mellem 6-9 mil. kr.",
                                ifelse(pris %in% huse12m, "Boliger mellem 9-12 mil. kr.",
                                ifelse(pris %in% huse15m, "Boliger mellem 12-15 mil. kr.", "NA"))))))

boligsiden.clean$priskategori = factor(boligsiden.clean$priskategori, levels = c("Boliger til maks 3 mil. kr.",
                                                                                 "Boliger mellem 3-6 mil. kr.",
                                                                                 "Boliger mellem 6-9 mil. kr.",
                                                                                 "Boliger mellem 9-12 mil. kr.",
                                                                                 "Boliger mellem 12-15 mil. kr."))

ggplot(boligsiden.clean, aes(x=priskategori)) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Størstedelen af boligerne til salg koster under 3 mil. kr.",
       subtitle = "Fordelingen af boliger opdelt i prisklasser",
       x = NULL,
       y = "Frekvens",
       caption = "Kilde: Wulfs webscraping af boligsiden.dk") +
  theme_bw() +  theme(
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank(),  
    panel.grid.minor.y = element_blank(),  
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(vjust = 1, size = 12))



# laver en alder kolonne 

boligsiden.clean$husalderiår = 2024 - boligsiden.clean$opført

#######################
### OLA1 OPGAVE 2.2 ###
#######################

# - Korrelation mellem to variabler påviser deres lineære sammenhæng og er den kvadrede version af r^2 

corr.pris.kvm =  cor(data.frame(pris = boligsiden.clean$pris,
                                kvm = boligsiden.clean$størrelse))
corrplot.pris.kvm = corrplot(corr.pris.kvm,
                             type = "upper",
                             method = "color",
                             order = "hclust", 
                             addCoef.col = "black",
                             tl.col = "black",
                             number.cex = 2,
                             tl.cex = 2,
                             diag = F,
                             outline = T)

lm.test = lm(pris ~ størrelse, data = boligsiden.clean)
summary(lm.test)
# gør pænere 


#######################
### OLA1 OPGAVE 2.3 ###
#######################

# 5 SLR mellem kvmpris og 5 andre variabler; 

lm.m2.alder = lm(kvmpris ~ husalderiår, data = boligsiden.clean)
summary(lm.m2.alder) 
  # negativ koefficient da jo højere husalder (ældre hus) jo lavere pris; giver god mening

lm.m2.størrelse = lm(kvmpris ~ størrelse, data = boligsiden.clean)
summary(lm.m2.størrelse)

lm.m2.grund = lm(kvmpris ~ grund, data = boligsiden.clean)
summary(lm.m2.grund)

lm.m2.mdudg = lm(kvmpris ~ mdudg, data = boligsiden.clean)
summary(lm.m2.mdudg)

lm.m2.tidtilsalg = lm(kvmpris ~ antaldagetilsalg, data = boligsiden.clean)
summary(lm.m2.tidtilsalg)

corr.lm = cor(boligsiden.clean[, c("kvmpris", "husalderiår", "størrelse", "grund", "mdudg", "antaldagetilsalg")],
              use = "complete.obs")
corr.lm = cor(boligsiden.clean$kvmpris, 
              boligsiden.clean$husalderiår,
              boligsiden.clean$størrelse,
              boligsiden.clean$grund,
              boligsiden.clean$mdudg,
              boligsiden.clean$antaldagetilsalg, use = "complete.obs")

corrplot(corr.lm, method = "color", 
         type = "upper",
         order = "hclust", 
         addCoef.col = "black",
         tl.col = "black",
         number.cex = 1,
         diag = F,
         outline = T)

#######################
### OLA1 OPGAVE 2.4 ###
#######################


