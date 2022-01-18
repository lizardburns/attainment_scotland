library(readr)
library(dplyr)

dat <- read_csv("data/scottish_results.csv", na = c("***", "", "NA")) %>% 
  mutate(across(starts_with("pc"), parse_number)) %>% 
  filter(if_all(starts_with("n_"), ~!is.na(.))) %>% 
  relocate(entries, .after = "year") %>% 
  # select(level, subject, year, entries, starts_with("pc_")) %>% 
  tidyr::pivot_longer(
    names_to = c("measure", "grade"), 
    values_to = "value", 
    names_sep = "_",
    cols = -c(level:entries)
  ) %>% 
  tidyr::pivot_wider(names_from = measure, values_from = value) %>% 
  mutate(
    subject = ifelse(subject == "Totals", "All subjects", subject)
  ) %>% 
  dplyr::rename(Year = year, Subject = subject, Qualification = level) %>%
  dplyr::mutate(
    pc = pc / 100,
    `Number of entries` = prettyNum(entries, big.mark = ","),
    `Grade threshold` = dplyr::if_else(grade %in% c("A", "U"), grade, paste(grade, "& above")),
    Qualification = dplyr::case_when(
      Qualification == "scqf7" ~ "Advanced highers (SCQF level 7)",
      Qualification == "scqf6" ~ "Highers (SCQF level 6)",
      Qualification == "scqf5" ~ "National 5 (SCQF level 5)",
      TRUE ~ NA_character_
    )
  ) %>% 
  select(-grade, -entries)

small_entry <- dat %>% 
  filter(`Grade threshold` == "A") %>% 
  transmute(Qualification, Subject, Year, entries = parse_number(`Number of entries`)) %>% 
  tidyr::pivot_wider(names_from = Year, values_from = entries) %>% 
  filter(if_any(where(is.numeric), ~ . < 100)) %>% 
  select(Qualification, Subject)

dat <- anti_join(dat, small_entry)

# dat %>% filter(is.na(n))

write_csv(dat, "app/data.csv")
write_csv(small_entry, "app/small_entry.csv")
