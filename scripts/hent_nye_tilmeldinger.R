# Henter tilmeldingsdata fra API'en.
# for alle ældre 
# Vi har ældre tilmeldingsdata fra gemte excel-ark.


# Bibliteker
library(tidyverse)
library(httr2)
library(httr)
library(here)

source("scripts/get_signup_details.R")
source("scripts/saner_tilmeldings_data.R")

# finder token - enten lokalt eller på github

if(here::here() == "C:/Users/cbk/Documents/R_projekter/kursus_data"){
  client_secret <- keyring::key_get("libcal")
}else{
  client_secret <- Sys.getenv("CLIENT_SECRET")
}

# Get token
# returnerer et access-token. Tager client_secret som input.
get_token <- function(client_secret){
  token_endpoint <- "https://kubkalender.kb.dk/1.1/oauth/token"
  client_id <- "110"
  token <- POST(token_endpoint,
                body = list(grant_type = "client_credentials",
                            client_id = client_id,
                            client_secret = client_secret)) %>% 
    content() 
  token[["access_token"]]
  
}

token <- get_token(client_secret = client_secret)

# henter kursus-id fra metadata filen
meta_data <- read_csv2("data/kursus_metadata.csv") 

# definerer col_spec til indlæsning af tilmeldingsdata

tilm_col_spec <- cols(
  event_id = col_double(),
  tilm_type = col_character(),
  last_name = col_logical(),
  booking_id = col_double(),
  registration_type = col_character(),
  first_name = col_logical(),
  barcode = col_logical(),
  phone = col_logical(),
  email = col_character(),
  registered_date = col_datetime(format = ""),
  attendance = col_character(),
  qid = col_double(),
  question = col_character(),
  answer = col_character()
)
# Henter eksisterende kursus-ider fra tilmeldings filen
tilm_data <- read_csv2("data/tilmeldings_data.csv", col_types = tilm_col_spec) 

# Identificerer manglede tilmeldingsdata
manglende_ids <- setdiff(meta_data$id, tilm_data$event_id)

# definerer hjælpe funktion til at hente sanerede tilmeldingsdata
hent_sanerede_tilm_data <- function(id){
  Sys.sleep(0)
  get_signup_details(id, token) %>% 
    saner_tilmeldings_data()
}

# Hvis der mangler tilmeldingsdata - hent dem.
if(length(manglende_ids)>0){
nye_tilm_data <- do.call(bind_rows, lapply(manglende_ids, hent_sanerede_tilm_data))

nye_tilm_data %>% 
  bind_rows(tilm_data) %>% 
  write_csv2("data/tilmeldings_data.csv")
}

