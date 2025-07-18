# funktion der kommer med gæt på kønnet baseret på fornavn.
# returnerer TRUE ved foretrukkent køn, FALSE hvis mand.

sex_distr <- read_csv2("data/sex_dist.csv")
get_sex_prob <- function(name){
  name <- toupper(name)
  name <- str_extract(name, pattern = "(.+?)\\b")
  if(!(name %in% sex_distr$Navn)){
    return(NA)
  }else{
    sex_distr[sex_distr$Navn == name,]$sex < 0.5
  }
}
get_sex_prob <- Vectorize(get_sex_prob)

