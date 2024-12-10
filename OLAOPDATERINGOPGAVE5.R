#####################
### OLA1 OPGAVE 5 ###
#####################
library(tidyr)



Klassevektor = c("A", "B", "C", "D") 
Ugevektor = c(1:9) 

Klassedf = data.frame(Klasse = rep(Klassevektor, times = 1, length.out = 36, each = 9),
                      Måned = rep(Ugevektor, times = 4, length.out = 36, each = 1), 
                      Score = round(runif(36, min = 0, max = 100) , 2))




View(Klassedf)

# Loop for at omdanne dataframe til kvartalsvis

klassedf.kvartal = Klassedf[0 , ]

ri = 1
for (i in seq(1, nrow(Klassedf), by = 3)) { 
  klassedf.kvartal[ri , ] = Klassedf[i ,] 
  klassedf.kvartal[ri, 3] = round(mean(Klassedf[(i):(i+2), 3]), 2)
  ri = ri + 1
}


View(klassedf.kvartal)


# Pivot_wider framen med Klasse som kolonne, så de ikke repeterers i rowsne

klassedf.wide = pivot_wider(klassedf.kvartal, names_from = Klasse, values_from = Score)

View(klassedf.wide)
