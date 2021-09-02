#Script to generate orbital launcher dataset for shiny app

library(tidyverse)
library(rvest)
library(caret)
library(plyr)
library(ggplot2)
library(dplyr)
library(plotly)

#----
#Extract data
#----

#export launcher site html from wiki
url = "https://en.wikipedia.org/wiki/Comparison_of_orbital_launch_systems"
launcher_html = read_html(url)
launcher_html %>% html_nodes(css = "table")
#extract launcher table: 1 - current/future; 2 - cancelled/retired
launcher_tbl1 <- 
        launcher_html %>% 
        html_nodes(css = "table") %>%
        nth(1) %>%
        html_table(fill = TRUE)
head(launcher_tbl1)
launcher_tbl2 <- 
        launcher_html %>% 
        html_nodes(css = "table") %>%
        nth(2) %>%
        html_table(fill = TRUE)
head(launcher_tbl2)

#---
#Data pre-processing
#---

#rename columns
names(launcher_tbl1) <- c("vehicle","origin","manufacturer","LEO.payload","GTO.payload","other.payload","launches","first.flight","latest.flight")
launcher_tbl1 <- launcher_tbl1[-1,]
names(launcher_tbl2) <- c("vehicle","origin","manufacturer","LEO.payload","GTO.payload","other.payload","launches","first.flight","latest.flight")
launcher_tbl2 <- launcher_tbl2[-1,]

#add signifier for current/retired
launcher_tbl1$retired <- c(0)
launcher_tbl2$retired <- c(1)

#combine current and retired launcher datasets
launcher_tbl <- rbind(launcher_tbl1,launcher_tbl2)
launcher_tbl

#clean numeric columns - remove references and convert to numeric format
clean_numeric <- function(val,fun,NAs) {
        clean_val <- {sub("\\[.*","",val)} %>% 
                {sub("\\+.*","",.)} %>% 
                {sub("\\(.*","",.)} %>%
                {sub(" .*","",.)} %>%
                {gsub(",","",.)} %>%
                fun() 
        clean_val[is.na(clean_val)] <- NAs
        return(clean_val)
}
#clean character columns - remove references
clean_char <- function(val) {
        clean_val <- gsub("\\[.*?\\]","",val)
        return(clean_val)
}

launcher_tbl_clean <- launcher_tbl %>% select(-other.payload) %>%
        mutate(vehicle = clean_char(vehicle), manufacturer = clean_char(manufacturer),
               LEO.payload = clean_numeric(LEO.payload,as.numeric,0), GTO.payload = clean_numeric(GTO.payload,as.numeric,0),
               launches = clean_numeric(launches,as.integer,0), first.flight = clean_numeric(first.flight,as.integer,NA),
               latest.flight = clean_numeric(latest.flight,as.integer,NA))
launcher_tbl_clean

#clean country of origin
countries <- launcher_tbl_clean$origin %>% {gsub("United States", "US",.)} %>% {gsub("United Kingdom", "UK",.)} 
unique_countries <- unique(countries)
#5 - US & Ukraine
#10 - US & New Zealand
#12 - Australia & Singapore
#16 - Russia & Europe
#20 - Japan & US
#21 - Soviet Union/Russia
#22 - South Korea & Russia
#23 - Soviet Union/Ukraine
str_to_rename <- c(5,10,12,16,20,21,22,23)
str_names <- c("US & Ukraine","US & New Zealand","Australia & Singapore",
               "Russia & Europe","Japan & US","Soviet Union/Russia","South Korea & Russia",
               "Soviet Union/Ukraine")
countries2 <- plyr::mapvalues(countries, from = unique_countries[str_to_rename],
                        to = str_names)
launcher_tbl_clean$origin <- countries2

#remove entries with no first flight date
launcher_tbl_clean2 <- launcher_tbl_clean %>% filter(!is.na(first.flight))

#---
#Feature Creation
#---
launcher_trans <- launcher_tbl_clean2 %>% mutate(origin = factor(origin), 
                              retired = factor(retired),
                              vehicle = factor(vehicle), manufacturer = factor(ifelse(manufacturer=="","N/A",manufacturer)),        
                              LEO.capable = factor(ifelse(LEO.payload >0,1,0)),
                              GTO.capable = factor(ifelse(GTO.payload>0,1,0)),
                              launched = factor(ifelse(launches > 0,1,0)))
#check number of launchers for each country
launcher_tbl_clean2 %>% dplyr::group_by(origin) %>% dplyr::summarise(n = n()) %>% dplyr::arrange(desc(n))
#create new origin factor
launcher_trans <- launcher_trans %>% 
        mutate(origin2 = factor(ifelse(origin %in% c("US","China","Europe","India","Russia","Sovient Union","Japan"), 
                                       as.character(origin), "Other")))
summary(launcher_trans)

# ---
# Export Data
# ---
launchers <- launcher_trans
dir.create(file.path("data/"))
saveRDS(launchers, file = "data/launchers.rds")
