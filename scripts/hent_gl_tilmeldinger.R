# Henter gamle tilmeldinger fra regnearkene
library(tidyverse)
library(digest)

# indlæs de tilmeldingsdata som vi skal have indlæst
tilmeldings_data <- read_csv2("data/tilmeldings_data.csv")



# Indlæs gamle tilmeldingsdata som vi har gemt løbende


files <- dir("data/old_tilm_data_regneark/", full.names = TRUE, recursive = TRUE)

filer <-   tibble(filename = files) %>% 
  mutate(year = str_extract(filename, "(?<=Tilmeldinger )(\\d{4})")) %>% 
  mutate(kursus_id = str_extract(filename, "(?<=_)(\\d*)(?=_)")) %>% 
  mutate(file_id = str_extract(filename, "(?<=_)(\\d*)(?=\\.csv)"))

# vi skal have oplysning om venteliste eller deltagerliste
# filer %>% filter(str_detect(filename, "lc_attendees|wait_list"))



# minikonferencen.... ------------------------------------------------
# minikonference er et snefnug...
# og skal derfor håndteres separat

skrammel <- filer %>% 
  filter(str_detect(filename, "minikonference"))

# Næste trin
gl_tilmeldings_data <- filer %>% 
  filter(str_detect(filename, "lc_attendees|wait_list")) %>% 
  mutate(tilm_type = case_when(
    str_detect(filename, "lc_attendees") ~ "registrants",
    str_detect(filename, "wait_list") ~ "waitlist",
    .default = NA_character_
  )) 

# der er dubletter blandt registreringerne... eg er event_id 4202465 repræsenteret
# med to regneark. Der er et file_id, der angiver tidspunktet for hvornår vi har 
# hentet den. Vi går med den nyeste.

gl_tilmeldings_data <- gl_tilmeldings_data %>% 
  group_by(tilm_type, kursus_id) %>% 
  arrange(desc(file_id)) %>% 
  slice(1) %>% 
  ungroup()


# funktion til at indlæse og splitte filerne i to efter den tomme linie
read_files <- function(path){
  lines <- readLines(path)
  empty_index <- which(nchar(trimws(lines)) == 0)
  if(length(empty_index) == 0) {
    print(path)
    stop("Filen indeholder ingen tomme linie!")
  }
  split_index <- empty_index[1]
  
  # Del filen op i to dele
  del1 <- lines[1:(split_index - 1)]
  del2 <- lines[(split_index + 1):length(lines)]
  return(list("meta" = del1, "data" = del2))
}

# Så mapper vi, og unnester.
gl_tilmeldings_data <- gl_tilmeldings_data %>% 
  mutate(noget = map(filename, read_files)) %>% 
  unnest_wider(noget)

# så piller vi de egentlige tilmeldingsdata ud.


gl_tilmeldings_data <- gl_tilmeldings_data %>% 
  select(-meta)

gl_tilmeldings_data <- gl_tilmeldings_data %>%
  mutate(data = map(data, ~ read_csv(paste(.x, collapse= "\n"), show_col_types = FALSE))) %>% 
  unnest_longer(data) %>% 
  unnest_wider(data) %>% 
  janitor::clean_names() %>% 
  mutate(kursus_id = str_extract(filename, "(?<=_)(\\d*)(?=_)"))

# Nu har vi data trukket pænt ud på de tilmeldinger vi har gemt manuelt
# og de skal så ændres til at have samme struktur som de tilmeldinger vi 
# trækker automatisk.

gl_tilmeldings_data <- gl_tilmeldings_data %>% 
  select(-c(filename, year, file_id)) %>% 
  rename(event_id = kursus_id) %>% 
  mutate(barcode = NA,
         last_name = NA,
         email = digest(email),
         phone = NA,
         booking_id = NA
         ) %>% 
  rename(registered_date = booking_made) %>% 
  relocate(last_name, .after = tilm_type) %>% 
  relocate(booking_id, .after = last_name) %>% 
  relocate(registration_type, .after = booking_id) %>% 
  relocate(barcode, .after = first_name) %>% 
  relocate(phone, .after = barcode) 

# ret tæt på - men spørgsmål/svar mangler at blive pivoteret.

gl_tilmeldings_data <- gl_tilmeldings_data %>% 
  rename(`15967` = x1_faculty,
         `15968` = x2_programme_line_of_study_if_not_applicable_please_write_none,
         `15969` = x3_programme_level,
         `15970` = x4_level_of_experience_in_regards_to_course_subject,
         `21065` = x5_which_operating_system_do_you_use,
         `15973` = x6_where_did_you_hear_about_this_event,
         `15972` = x7_may_we_contact_you_in_regards_to_future_courses_and_events,
         `15971` = x8_i_hereby_consent_to_the_royal_danish_librarys_privacy_and_personal_data_protection_policy_http_www_kb_dk_en_kb_webstedet_cookiepolitik_html
         ) %>% 
  rownames_to_column( var = "ID")

# vi skal også have gjort noget ved first_name - der skal konverteres til en
# kønsmarkør
source("scripts/get_sex_prob.R")

gl_tilmeldings_data <- gl_tilmeldings_data %>% 
  mutate(first_name = get_sex_prob(first_name))

# så pivoterer vi
gl_tilmeldings_data <- gl_tilmeldings_data %>% 
  pivot_longer(cols = `15967`:`15971`,
               names_to = "qid",
               values_to = "answer") %>% 
  mutate(qid = as.numeric(qid))

# Nu finder vi sammenhængen mellem spørgsmål og qid fra de data vi henter igennem
# apien

question_konkordans <- tilmeldings_data %>% 
  distinct(question, .keep_all = T) %>% 
  select(qid, question) 




gl_tilmeldings_data <-  gl_tilmeldings_data %>%
  left_join(question_konkordans, join_by(qid)) %>% 
  relocate(question, .before = answer) 


# Så skal vi have dem matchet med de data vi henter fra apien.
# Der er ikke booking id i de gamle data. Men det kunne godt se ud som om vi kan 
# matche på tidspunktet. APIen giver os tilmeldingstidspunkter med sekund opløsning.
# Regnearkene med minutopløsning. Og så er der en to timers forskel. De er 
# i samme tidszone, så det er formentlig et spørgsmål om at regnearkene har
# leveret dem i lokal dansk tid, R har indlæst teksten som om den var i 
# utc. Og nu har vi så en forskel på to timer.

gl_tilmeldings_data %>% 
  filter(event_id == 4202463) %>% 
  distinct(registered_date) %>% 
  arrange(registered_date) %>% slice(1) %>% pull(registered_date)

tilmeldings_data %>% 
  filter(event_id == 4202463) %>% 
  distinct(registered_date) %>% 
  arrange(registered_date) %>% slice(1) %>% pull(registered_date)


mutate(registered_date = with_tz(registered_date, "Europe/Copenhagen"))  
  mutate(registered_date = with_tz(registered_date, "UTC"))

# hvis kombinationen af event-id og registered_date er unik. Så kan vi 
# matche på dem.
# udfordringen er at vi skal have skiftet de data vi har fra apien ud med dem
# vi har fra regnearkene. Men dog uden at vi ellers mister data. Så...
  
  
  
  
tilmeldings_data %>% 
  filter(event_id == 4202463)  

gl_tilmeldings_data %>% 
  filter(event_id == 4202463)  

Det vi mangler er i api-tilmeldingerne er fornavn, email ,og answer. 