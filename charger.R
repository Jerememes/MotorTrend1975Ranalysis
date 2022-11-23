matricule = "2007279"

charger <- function(matricule) {
  set.seed(matricule)
  mondata <- read.csv2("DevoirD_A22.csv")[sample(250,220),-1]
}