#####################
### OLA1 OPGAVE 1.1 ###
#####################
library(httr)
library(rvest)

boligsiden = read.csv("boligsiden (1).csv")

View(boligsiden)

  # gør rede for webscraping, misinformation og generelle ulemper ved webscraping. 

which(boligsiden$vej == "egevej" & boligsiden$vejnr == 20) # 2202
which(boligsiden$vej == "tousvej" & boligsiden$vejnr == 106) # 71

toboligerdf = boligsiden[c(71, 2202) ,]


#####################
### OLA1 OPGAVE 1.2 ###
#####################

# URL-EKSEMPEL: https://www.boligsiden.dk/adresse/soevej-17-5792-aarslev-04301523__17_______?udbud=b93822a6-35fa-48fc-87a7-4340b971984c
# problem med webscaping - mangler information for at kunne finde URL'er; information kunne eksempelvis være: 
  # - Link
  # - Unique identificers; mangler efter by (IKKE MED I SCRAPINGEN) 
  # - Kan eventuelt lave et loop gennem alle boliger til valg pr. vej med parametre pris på, men:
      # - Ikke 'feasible' grundet, at vi bliver nødt til, at udføre GET-request for hver row - bliver hurtigt udelukket medmindre vi lader loopet køre i meget lang tid med sys.sleep

  # skriv eksempel med screendumps på hvordan loopet kunne fungeres


  # vi gør det i denne omgang derfor manuelt med søgning på boligsiden. 

  # måske gør jeg det

# opgave 1.3 og 1.4 i selve opgaven

