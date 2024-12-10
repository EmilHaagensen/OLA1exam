######################
### OLA 1 OPGAVE 3 ###
######################

# Opgave 3.1 

    # Funktion for kast med én terning

terning = function(antalkast) {
  dice = 1:6 
  rul = sample(dice, size = antalkast, replace = T) 
}

en.terning.frekvens = as.data.frame(table(terning(25000)))

colnames(en.terning.frekvens) = c("Sum", "Frekvens", "procentvisfrekvens")
en.terning.frekvens$procentvisfrekvens = (en.terning.frekvens$Freq / sum(en.terning.frekvens$Freq)) * 100
  
          ## lav barplot over fordelingen 

barplot(table(terning(25000)),
        xlab = "summer", 
        ylab = "frekvens af sum",
        main = "25.000 kast med èn terning skaber en uniform fordeling")


# opgave 3.2 

    # Funktion for kast med 6 terninger

  seksterninger <- function(antalkast) {
    replicate(antalkast, sum(sample(1:6, 6, replace = TRUE)))
  }

# 10.000 kast

barplot(table(seksterninger(10000)),
        xlab = "summer", 
        ylab = "frekvens af summer",
        main = "10.000 kast med seks terninger skaber en normalfordeling")


# Opgave 3.3


# 1.000.000 kast 

barplot(table(seksterninger(1000000)),
        xlab = "summer", 
        ylab = "frekvens af summer",
        main = "1.000.000 kast med seks terninger skaber en normalfordeling")


# opgave 3.4 

talvektor = c(1, 2, 3, 5, 6)
tilfældigetal = sample(talvektor, 5) 
print(tilfældigetal)

talmatrix = matrix(cbind(2:6, tilfældigetal), nrow = 5, ncol = 2, byrow = F) 

View(talmatrix)



                 