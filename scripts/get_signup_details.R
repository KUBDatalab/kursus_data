# get_signup_details(event_id, token)
# Funktion der, givet et event_id og et api-adgangstoken, returnerer
# tilmeldinger for det pågældende event.
#

get_signup_details <- function(event_id, token){
  url <- modify_url(
    url = "https://kubkalender.kb.dk",
    path = c("1.1", "events", event_id, "registrations"),
    query = list(
      waitlist = 1,
      custom_answers = 1
      
    )
  ) 
  df <- url %>% 
    GET(, add_headers('Authorization' = paste("bearer", token))) %>% 
    content(as = "text", type = "application/json") %>% 
    jsonlite::fromJSON() %>% 
    as_tibble()
  df <- df %>% 
    pivot_longer(cols = registrants:waitlist, names_to = "tilm_type", values_to = "values") %>% 
    unnest_wider(values,
                 simplify = TRUE) %>% 
    unnest_longer(col = -2)
   if("answers" %in% names(df)){
     df <- df %>% unnest_wider(answers) 
   }
  if("qid" %in% names(df)){
    df <- df %>% unnest_longer(qid:answer)
  }
  df
}







