
library(shiny)
library(dplyr)
library(markdown)

source("functions.R")

dat <- readr::read_csv("data.csv")

init_subjects <- dat %>% 
  dplyr::filter(Subject != "All subjects") %>% 
  dplyr::distinct(Subject) %>% 
  dplyr::arrange(Subject) %>% 
  dplyr::pull(Subject) %>% 
  c("All subjects", .)

dat_int <- calculate_grade_dist(dat) %>% 
  dplyr::select(Qualification:Year, Grade, pc) %>% 
  tidyr::pivot_wider(names_from = Year, values_from = pc)
