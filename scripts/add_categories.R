# Identificerer manglende kategorier i datasættet 
# 


add_categories <- function(df) {
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(xml2)
  
  # 1. Liste over kategorier vi leder efter
  desired_categories <- c("voyant", "vosviewer", "sql", "regex", "nvivo",
                          "regexpr", "orange", "python", "metashape", "excel",
                          "r", "openrefine", "gis", "ArcGIS", "værksted") %>% str_to_lower()
  
  # 2. Funktion til at afkode HTML
  decode_html <- function(x) {
    read_html(paste0("<body>", x, "</body>")) |> 
      xml_find_first("//body") |> 
      xml_text()
  }
  
  # 3. Funktion til at trække kategorier ud af tekst
  extract_categories <- function(text, categories) {
    text_clean <- text %>%
      decode_html() %>%
      tolower() %>%
      str_remove_all("<[^>]+>") %>%
      str_replace_all("\\\\[rnt]", " ") %>%
      str_replace_all("[[:cntrl:]]", " ") %>%
      str_replace_all("[[:punct:]]", " ") %>%
      str_squish()
    
    matches <- sapply(categories, function(cat) {
      pattern <- str_c("\\b", cat, "\\b")
      str_detect(text_clean, regex(pattern, ignore_case = TRUE))
    })
    
    unique(categories[matches])
  }
  
  # 4. Find nye kategorier ud fra title + description
  extra_cats <- df %>%
    distinct(id, title, description) %>%
    mutate(detected = map2(title, description, ~ extract_categories(paste(.x, .y), desired_categories))) %>%
    unnest(detected, keep_empty = TRUE) %>%
    filter(!is.na(detected)) %>%
    rename(new_category = detected) 
  
  # 5. Fjern dem der allerede findes (case-insensitivt match)
  existing_cats <- df %>%
    select(id, title, description, category_name) %>%
    distinct() %>%
    mutate(category_name_lower = tolower(category_name))
  
  new_cats <- extra_cats %>%
    mutate(category_name_lower = tolower(new_category)) %>%
    anti_join(existing_cats,
              by = c("id", "title", "description", "category_name_lower"))
  
  # 6. Tag én række per kursus som skabelon
  row_template <- df %>%
    group_by(id, title, description) %>%
    slice(1) %>%
    ungroup() %>% 
    mutate(category_name = str_to_lower(category_name))
  
  # 7. Opret nye rækker
  new_rows <- new_cats %>%
    left_join(row_template, by = c("id", "title", "description")) %>%
    mutate(category_name = new_category) %>%
    select(names(df))  # samme kolonner og rækkefølge
  
  
  # 8. Saml og returnér
  bind_rows(df, new_rows) %>%
    mutate(category_name = str_to_lower(category_name)) %>% 
    distinct()
}


