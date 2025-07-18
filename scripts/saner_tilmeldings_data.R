# funktion til at sanere tilmeldingsdata - fjerner personhenførbare data, og gætter på
# køn

library(digest)
source("scripts/get_sex_prob.R")

saner_tilmeldings_data <- function(df){
  mail_ekst <- "email" %in% names(df)
  navn_ekst <- "first_name" %in% names(df)
  df <- df %>% 
    mutate(last_name = NA)
  if(mail_ekst){
    df <- df %>% 
      rowwise() %>% 
      mutate(email = digest(email))
  }  
  if(navn_ekst){
    df <- df %>% 
      mutate(first_name = get_sex_prob(first_name))
  }
  df
}



