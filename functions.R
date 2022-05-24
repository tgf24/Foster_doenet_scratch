clean_events <- function(events) {
  # summarize events page
  events <-
    events %>%
    group_by(userId) %>%
    mutate(timestamp = anytime(timestamp)) %>%
    mutate(time = timestamp - min(timestamp))

events <-
  events %>%
  mutate(new = map(context, ~ fromJSON(.) %>% as.data.frame())) %>%
  unnest(new)

events <-
  events %>%
  mutate(new = map(object, ~ fromJSON(.) %>% as.data.frame())) %>%
  unnest(new)
# 
# events <-
#   events %>%
#   separate(componentName, into = c(NA, "section", "answer", "type"))
# 
# events <-
#   events %>%
#   filter(!is.na(documentCreditAchieved))
return(events)
}