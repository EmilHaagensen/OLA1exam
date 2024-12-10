######################
### OLA 1 OPGAVE 4 ###
######################
library(dkstat)
library(tidyr)
library(dplyr)
library(ggplot2)
library(corrplot)

      # Hent data fra tabel FU02

# Indhentning af data

forbrugmeta = dst_meta(table = "FU02", lang = "da")

forbrugquery = list(KONSUMGRP = "*",
                    PRISENHED = "Faste priser",
                    Tid = "*")

forbrugdata = dst_get_data(table = "FU02", query = forbrugquery, lang = "da")

# Sortering af data 

forbrugdata = forbrugdata[grepl("^02\\.1", forbrugdata$KONSUMGRP) , ]

forbrugdata.wide = pivot_wider(forbrugdata, names_from = KONSUMGRP, values_from = value)
colnames(forbrugdata.wide) = gsub("[0-9.]", "", colnames(forbrugdata.wide))
forbrugdata.wide = forbrugdata.wide[, -1] 
forbrugdata.wide$TID = format(as.Date(forbrugdata.wide$TID), "%Y") 

    # Grundet mit gsub, så har colnames et leading space. Dette fjerner vi:
colnames(forbrugdata.wide) <- trimws(colnames(forbrugdata.wide))

View(forbrugdata)
View(forbrugdata.wide)


# Illustrering af -UDVIKLING-. Vi normaliserer derfor. 
    # Laver normaliserings-funktion

normfunc = function(x) { 
  vnorm = ((x - min(x)) / (max(x) - min(x)))
      return(vnorm) 
}

forbrugdata.norm = forbrugdata.wide
forbrugdata.norm[, 2:ncol(forbrugdata.norm)] = apply(forbrugdata.wide[, 2:ncol(forbrugdata.wide)], 2, normfunc)


# Plot af forskellige alkohol-grupper; Vin, øl såvel som alkoholfrie øl/vin

    # for Vin
ggplot(forbrugdata.wide, aes(x = TID)) +
  geom_line(aes(y = Hedvin, color = "Hedvin", group = 1), size = 0.5) + 
  geom_line(aes(y = `Vin af andre frugter`, color = "Vin af andre frugter", group = 1), size = 0.5) +
  geom_line(aes(y = `Vin af druer`, color = "Vin af druer", group = 1), size = 0.5) +
  labs(
    title = "Vin af druer er den dominerende vintype",
    subtitle = "Udviklingen for underkategorierne i kategorien, vin",
    x = NULL,
    y = "Forbrug i tkr., faste priser.",
    color = "Vingruppe",
    caption = "Kilde: Statistikbanken, Tabel FU02"
  ) +
  theme_bw() +  theme(
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank(),  
    panel.grid.minor.y = element_blank(),  
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(vjust = 1, size = 12),
    legend.position = "right")







    # for Vin -- NORMALISERET -- 
ggplot(forbrugdata.norm, aes(x = TID)) +
  geom_line(aes(y = Hedvin, color = "Hedvin", group = 1), size = 0.5) + 
  geom_line(aes(y = `Vin af andre frugter`, color = "Vin af andre frugter", group = 1), size = 0.5) +
  geom_line(aes(y = `Vin af druer`, color = "Vin af druer", group = 1), size = 0.5) +
  labs(
    title = "Vin var yderst populært under finanskrisen, med undtagelse af Hedvin",
    x = "År",
    y = "Normaliseret forbrugsudvikling i faste priser",
    color = "Vingruppe"
  ) +
  theme_minimal()


          ### --------------------- ###


# For Øl
ggplot(forbrugdata.wide, aes(x = TID)) +
  geom_line(aes(y = `Pilsnerøl, guldøl`, color = "Pilsnerøl, guldøl", group = 1), size = 0.5) + 
  geom_line(aes(y = `Andre alkoholholdige øl`, color = "Andre alkoholholdige øl", group = 1), size = 0.5) +
  geom_line(aes(y = `Øl-baserede drikkevarer`, color = "Øl-baserede drikkevarer", group = 1), size = 0.5) +
  labs(
    title = "Den generelle øl-popularitet er faldende",
    x = NULL,
    y = "Forbrug i tkr., faste priser.",
    color = "Ølgruppe",
    caption = "Kilde: Statistikbanken, Tabel FU02",
    subtitle = "Udviklingen for underkategorierne i kategorien, Øl"
  ) +
  theme_bw() +  theme(
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank(),  
    panel.grid.minor.y = element_blank(),  
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(vjust = 1, size = 12),
    legend.position = "right")

    # For Øl -- NORMALISERET --
ggplot(forbrugdata.norm, aes(x = TID)) +
  geom_line(aes(y = `Pilsnerøl, guldøl`, color = "Pilsnerøl, guldøl", group = 1), size = 0.5) + 
  geom_line(aes(y = `Andre alkoholholdige øl`, color = "Andre alkoholholdige øl", group = 1), size = 0.5) +
  geom_line(aes(y = `Øl-baserede drikkevarer`, color = "Øl-baserede drikkevarer", group = 1), size = 0.5) +
  labs(
    title = "Pilsnerøls populæritet bliver overtaget af andre ølgrupper i nyere tid",
    x = "År",
    y = "Normaliseret forbrugsudvikling i faste priser",
    color = "Ølgruppe"
  ) +
  theme_minimal()

              ### --------------------- ###

# For Alkoholfrie alternativer
ggplot(forbrugdata.wide, aes(x = TID)) +
  geom_line(aes(y = `Øl med lavt alkoholindhold og alkoholfri øl`, color = "Øl med lavt alkoholindhold og alkoholfri øl", group = 1), size = 0.5) + 
  geom_line(aes(y = `Vinbaserede drikkevarer og alkoholfri vin`, color = "Vinbaserede drikkevarer og alkoholfri vin", group = 1), size = 0.5) +
  labs(
    title = "Populæriteten af alkolfrie øl er stærkt stigende i nyere tid",
    x = NULL,
    y = "Forbrug i tkr., faste priser.",
    color = "Alkoholfrie alternativer",
    subtitle = "Udviklingen af underkategorierne i kategorien, alternativer til alkohol",
    caption = "Kilde: Statistikbanken, Tabel FU02") +
  theme_bw() +  theme(
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank(),  
    panel.grid.minor.y = element_blank(),  
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(vjust = 1, size = 12),
    legend.position = "right")

# For Alkoholfrie alternativer -- NORMALISERET --
ggplot(forbrugdata.norm, aes(x = TID)) +
  geom_line(aes(y = `Øl med lavt alkoholindhold og alkoholfri øl`, color = "Øl med lavt alkoholindhold og alkoholfri øl", group = 1), size = 0.5) + 
  geom_line(aes(y = `Vinbaserede drikkevarer og alkoholfri vin`, color = "Vinbaserede drikkevarer og alkoholfri vin", group = 1), size = 0.5) +
  labs(
    title = "Populæriteten af alkolfrie øl er stærkt stigende i nyere tid",
    x = "År",
    y = "Normaliseret forbrugsudvikling i faste priser",
    color = "Alkoholfrie alternativer"
  ) +
  theme_minimal()


# For Spiritus og læksedrikke

ggplot(forbrugdata.wide, aes(x = TID)) +
  geom_line(aes(y = `Spiritus og likør`, color = "Spiritus og likør", group = 1), size = 0.5) + 
  geom_line(aes(y = `Alkoholiske læskedrikke`, color = "Alkoholiske læskedrikke", group = 1), size = 0.5) +
   labs(title = "Spiritus og likør er den dominerende underkategori i kategorien, spiritus og læskedrikke",
    x = NULL,
    y = "Forbrug i tkr., faste priser.",
    color = "Spiritus og læskedrikke",
    caption = "Kilde: Statistikbanken, Tabel FU02",
    subtitle = "Udviklingen for underkategorierne i kategorien, spiritus og læskedrikke"
  ) +
  theme_bw() +  theme(
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank(),  
    panel.grid.minor.y = element_blank(),  
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(vjust = 1, size = 12),
    legend.position = "right")

### Korrelation mellem forbrugsgrupperne ### 

cormatrix = cor(forbrugdata.wide[, 2:ncol(forbrugdata.wide)])

corrplot(cormatrix, 
         type = "upper",
         method = "color",
         addCoef.col = "black",
         number.cex = 0.8,
         tl.cex = 0.8,
         diag = F
         )


groupdf = data.frame(
  tid = forbrugdata.wide$TID,
  spiritusoglæskedrikke = rowSums(cbind(forbrugdata.wide$`Spiritus og likør`, forbrugdata.wide$`Alkoholiske læskedrikke`)), 
  øl = rowSums(cbind(forbrugdata.wide$`Pilsnerøl, guldøl`, forbrugdata.wide$`Andre alkoholholdige øl`, forbrugdata.wide$`Øl-baserede drikkevarer`)),
  vin = rowSums(cbind(forbrugdata.wide$`Vin af druer`, forbrugdata.wide$`Vin af andre frugter`, forbrugdata.wide$Hedvin)),
  alkoalternativ = rowSums(cbind(forbrugdata.wide$`Vinbaserede drikkevarer og alkoholfri vin`, forbrugdata.wide$`Øl med lavt alkoholindhold og alkoholfri øl`))
)



cormatrixgroup = cor(groupdf[ , 2:ncol(groupdf)])

corrplot(cormatrixgroup, 
         type = "upper",
         method = "color",
         addCoef.col = "black",
         number.cex = 0.8,
         tl.cex = 1.2,
         diag = F
)                   

library(ggplot2)


## FORBRUG FOR GRUPPERING

ggplot(groupdf, aes(x = tid)) +
  geom_line(aes(y = spiritusoglæskedrikke, color = "spiritusoglæskedrikke", group = 1), size = 0.5) +
  geom_line(aes(y = øl, color = "øl", group = 1), size = 0.5) +
  geom_line(aes(y = vin, color = "vin", group = 1), size = 0.5) +
  geom_line(aes(y = alkoalternativ, color = "alkoalternativ", group = 1), size = 0.5) +
  labs(
    title = "Vin er danskernes foretrukkende alkohol",
    x = NULL,
    y = "Forbrug i tkr., faste priser.",
    color = "Alkoholgrupper",
    subtitle = "Udviklingen for de forskellige alkoholgrupper",
    caption = "Kilde: Statistikbanken, Tabel FU02"
  ) + 
  theme_bw() +  theme(
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank(),  
    panel.grid.minor.y = element_blank(),  
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(vjust = 1, size = 12),
    legend.position = "right")

    
## NORMALISERET FOR GRUPPER

groupdf.norm = groupdf
groupdf.norm[, 2:ncol(groupdf.norm)] = apply(groupdf[, 2:ncol(groupdf)], 2, normfunc)

ggplot(groupdf.norm, aes(x = tid)) +
  geom_line(aes(y = spiritusoglæskedrikke, color = "spiritusoglæskedrikke", group = 1), size = 0.5) +
  geom_line(aes(y = øl, color = "øl", group = 1), size = 0.5) +
  geom_line(aes(y = vin, color = "vin", group = 1), size = 0.5) +
  geom_line(aes(y = alkoalternativ, color = "alkoalternativ", group = 1), size = 0.5) +
  labs(
    title = "Alkoholforbruget har ikke været stabilt udvikliende",
    x = "År",
    y = "Alkoholsforbruget i tkr., faste priser.",
    color = "Alkoholgrupper"
  ) + 
  theme_minimal()



