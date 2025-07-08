# Packages -----------------------------------------------------------
if(require(dplyr) == F) install.packages('dplyr'); require(dplyr)
if(require(rvest) == F) install.packages('rvest'); require(rvest)
if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)

# 1. NON-CONTRIBUTORY POLICIES -------------------------------------------

# This replication should be done with caution. Any updates in the websites
# might demand code adjustments in order to obtain valid data

# Last adjustment: 30/08/2024

## 1.1 Conditional Cash Transfer --------------------------------------
ncp_cct <- "https://dds.cepal.org/bpsnc/cct" %>% 
  xml2::read_html() %>% 
  rvest::html_table() %>%
  bind_rows() %>%
  select(-3) %>%
  rename(programme = 1, country = 2) %>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
         ptype = "cct")

# '\\D' removes non-numerical characters
ncp_cct$year <- substr(x = gsub(pattern = '\\D', "", ncp_cct$programme), 1, 4) 

ncp_cct$year[ncp_cct$programme == "Auxílio Brasil Programme"] <- 2021

ncp_cct$year[ncp_cct$programme == "Bono Vida Mejor (ex Bono 10.000 Education, health and nutrition) (2010-)"] <- 2010

ncp_cct <- ncp_cct %>% 
  mutate(year = as.numeric(year)) %>% 
  relocate(2, 3, 5, 4, 1)

## 1.2 Non-contributory pensions -------------------------------------
ncp_sp <- "https://dds.cepal.org/bpsnc/sp" %>% 
  xml2::read_html() %>% 
  rvest::html_table() %>%
  bind_rows() %>%
  select(-3) %>%
  rename(programme = 1, country = 2) %>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
         ptype = "sp")

ncp_sp$year <- substr(x = gsub(pattern = '\\D', "", ncp_sp$programme), 1, 4)

ncp_sp$year[ncp_sp$programme == "Pension for Older People (Pensión para Adultos Mayores) (ex \"70 and over\" programme)"] <- 2007

ncp_sp$year[ncp_sp$programme == "Programme of Food Support for Adults over 68 years old living in Mexico City (2001-)"] <- 2001

ncp_sp$year[ncp_sp$programme == "120 a los 65: Programa Especial de Transferencia Económica a los Adultos Mayores (2009-) (120 to 65: Special programme of economic assistance for the elderly) (2009-)"] <- 2009

ncp_sp$year[ncp_sp$programme == "National Solidarity Assistance Programme “Pension 65“ (2011-)"] <- 2011

ncp_sp$year[ncp_sp$programme == "Programme of Food Support for Adults over 68 years old living in Mexico City (2001-)"] <- 2001

ncp_sp$year[ncp_sp$programme == "Pension for Older People (Pensión para Adultos Mayores) (ex \"70 and over\" programme)"] <- 2007

ncp_sp <- ncp_sp %>% 
  filter(programme != "") %>% 
  mutate(year = as.numeric(year)) %>% 
  relocate(2, 3, 5, 4, 1)

## 1.3 Labour inclusion programmes ----------------------------------
ncp_lpi <- "https://dds.cepal.org/bpsnc/lpi" %>% 
  xml2::read_html() %>% 
  rvest::html_table() %>%
  bind_rows() %>%
  select(-3) %>%
  rename(programme = 1, country = 2) %>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
         ptype = "lpi")

ncp_lpi$year <- substr(x = gsub(pattern = '\\D', "", ncp_lpi$programme), 1, 4)

ncp_lpi$year[ncp_lpi$programme == "Programme 4 to 7 (2011 - )"] <- 2011

ncp_lpi$year[ncp_lpi$programme == "PROMYPE Programme (Pilot and PHASE 2) (2014 - 2023)"] <- 2014

ncp_lpi$year[ncp_lpi$programme == "Joint Opportunities Programme (PCO) \"Youth: Skills and Economic Opportunities for Social Inclusion\"."] <- 2014

ncp_lpi <- ncp_lpi %>%
  drop_na() %>% 
  mutate(year = as.numeric(year)) %>% 
  relocate(2, 3, 5, 4, 1)

ncp_all <- bind_rows(ncp_cct, ncp_lpi, ncp_sp)

ncp_count <- ncp_all %>% 
  complete(year = 1990:2024, country = unique(country)) %>% 
  group_by(country) %>% 
  fill(iso3c, .direction = "updown") %>% 
  group_by(iso3c, year, ptype) %>%
  dplyr::reframe(ncp_count=n(),
                 country = unique(country)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = ptype, values_from = ncp_count) %>% 
  select(-"NA") %>% 
  rename(n_cct = cct, n_sp = sp, n_lpi = lpi) %>% 
  mutate(n_cct = coalesce(n_cct, 0),
         n_sp = coalesce(n_sp, 0),
         n_lpi = coalesce(n_lpi, 0),
         n_ncp = n_cct + n_sp + n_lpi)

## 1.4 Exporting data ---------------------------------------
write_excel_csv2(ncp_all %>% 
                   filter(year %in% c(1900:2024)),
                 "final_data/ncp_programmes.csv", na = '')

write_excel_csv2(ncp_count %>% 
                   filter(year %in% c(1900:2024)),
                 "final_data/ncp_count.csv", na = '')