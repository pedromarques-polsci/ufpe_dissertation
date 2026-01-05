# This must be done before loading other packages
library(demstock) # devtools::install_github("vdeminstitute/demstock")
get_stock(var = "v2x_polyarchy", val = 0.99)

rm(v.dem.sub)

demstock <- v.dem.out %>% 
  rename(demstock_one = 7)

rm(v.dem.out)

write_rds(demstock, "raw_data/demstock.rds")

# Packages -----------------------------------------------------------
library(countrycode)
library(gridExtra)
library(haven)
library(Hmisc)
library(janitor)
library(readxl)
library(rnaturalearth)
library(rvest)
library(sf)
library(tidyr)
library(tidyverse)
library(wbstats)

# Functions ---------------------------------------------------------------
## Summing values while skipping NAs and not imputing zeroes
calc <- function(df, cols, x) {
  # df = a data frame
  # cols = vector of collumns to be sumed
  # x = name of the output collumn
  
  new_column <- ensym(x)
  df %>%
    mutate(
      !!x := ifelse(
        rowSums(is.na(select(., all_of(cols)))) == ncol(select(., all_of(cols))),
        NA,
        rowSums(select(., all_of(cols)), na.rm = TRUE)
      )
    )
}

## Extraction of information directly from Data Bank
wb_etl <- function(y, w, z){
  wb_data(country = "countries_only", indicator = as.character(y), start_date = w, 
          end_date = z) %>% 
#    left_join(correct_names, join_by(country)) %>% 
    rename(year = date) %>% 
    mutate(iso3c = case_when(country == "Kosovo" ~ "KSV", .default = iso3c)) %>% 
    select(2:5)
}

# Country Name Correction -------------------------------------------------
correct_names <- data.frame(
  country = c('Czechoslovakia', 'Netherlands Antilles', 'Serbia and Montenegro',
              "Yugoslavia, Soc. Fed. Rep. of", "Yugoslavia",
              "Pacific Islands, Trust Territory",
              "GDR", "German Democratic Republic", "Germany East",
              "Turk Cyprus", "P. N. Guinea", "eSwatini",
              "Kosovo", "Somaliland", "Zanzibar",
              "Palestine/British Mandate", "Palestine/Gaza", "Channel Islands",
              "Yemen, Democratic", "Yemen (PDR)", "South Yemen", "Yemen South",
              "Yemen North", "Yemen (AR)", "Yemen",
              "Vietnam, South", "Vietnam South",
              "Vietnam North", "Republic of Vietnam"
  ),
  iso3c = c("CSK", "ANT", "SCG",
            "YUG", "YUG",
            "PCI",
            "DDR", "DDR", "DDR",
            0, "PNG", "SSW",
            "KSV", "SML", "ZZB",
            "PSB", "PSG", "CHI",
            "YMD", "YMD", "YMD", "YMD",
            "YAR", "YAR", "YEM",
            "VDR", "VDR",
            "VNR", "VNR"
            )
)

# 1. DEPENDENT VARIABLE ------------------------------------------------
## 1.1 Social Spending per capita (Constant Prices) --------------------
social_spending <- read_xlsx("raw_data/cepal_social_spending_pcp_constant.xlsx", sheet = 1, col_types = c(
  "text", "text", "text", "text", "numeric", "numeric", "text", "text", "numeric")
) %>%
  select(c(-1, -7, -8, -9)) %>%
  tidyr::pivot_wider(names_from = 3,
              values_from = value,
              values_fill = NA) %>%
  select(-4, -8, -9) %>% 
  tidyr::pivot_wider(names_from = 1,
              values_from = c(4:6),
              values_fill = NA) %>% 
  rename(country = 1, year = 2, cg_pcp_edu = 3, gg_pcp_edu = 4, nfps_pcp_edu = 5,
         cg_pcp_security = 6, gg_pcp_security = 7, nfps_pcp_security = 8,
         cg_pcp_health = 9, gg_pcp_health = 10, nfps_pcp_health = 11) %>% 
  calc(c("cg_pcp_edu", "cg_pcp_security", "cg_pcp_health"), "cg_pcp_sexp") %>% 
  calc(c("gg_pcp_edu", "gg_pcp_security", "gg_pcp_health"), "gg_pcp_sexp") %>% 
  calc(c("nfps_pcp_edu", "nfps_pcp_security", "nfps_pcp_health"), "nfps_pcp_sexp") %>% 
  arrange(country, year)

# Warnings are referred to coercion and should be ignored

## 1.2 Public Spending by Function -----------------------------------
# Public spending by function
public_spending <- read_xlsx("raw_data/cepal_public_spending_current.xlsx") %>%
  clean_names() %>% 
  select(-c(indicator, unit, notes_ids, source_id)) %>%
  pivot_wider(names_from = public_spending_by_function,
              values_from = value,
              values_fill = NA) %>%
  clean_names() %>% 
  pivot_wider(names_from = institutional_coverage,
              values_from = c(total_expenditure:defense_n_e_c),
              values_fill = NA) %>% 
  clean_names() %>% 
  rename(country = country_estandar, year = years_estandar, 
         cg_curr = total_expenditure_central_government, 
         gg_curr = total_expenditure_general_government, 
         nfps_curr = total_expenditure_nonfinancial_public_sector,
         cg_curr_health = health_central_government, 
         gg_curr_health = health_general_government, 
         nfps_curr_health = health_nonfinancial_public_sector,
         cg_curr_edu = education_central_government,
         gg_curr_edu = education_general_government, 
         nfps_curr_edu = education_nonfinancial_public_sector,
         cg_curr_security = social_protection_central_government,
         gg_curr_security = social_protection_general_government,
         nfps_curr_security = social_protection_nonfinancial_public_sector) %>% 
  calc(c("military_defense_central_government", 
         "foreign_military_aid_central_government", 
         "r_d_defense_central_government",
         "defense_n_e_c_central_government"), "cg_curr_def") %>% 
  calc(c("military_defense_general_government", 
         "foreign_military_aid_general_government",
         "r_d_defense_general_government",
         "defense_n_e_c_general_government"), "gg_curr_def") %>% 
  calc(c("military_defense_nonfinancial_public_sector", 
         'foreign_military_aid_nonfinancial_public_sector',
         "r_d_defense_nonfinancial_public_sector",
         "defense_n_e_c_nonfinancial_public_sector"), "nfps_curr_def") %>% 
  
# Central government
  mutate(year = as.numeric(year),
         cg_prop_def = cg_curr_def * 100 / cg_curr,
         cg_prop_health = cg_curr_health * 100 / cg_curr,
         cg_prop_edu = cg_curr_edu * 100 / cg_curr,
         cg_prop_security = cg_curr_security * 100 / cg_curr,
  
  # General government
         gg_prop_def = gg_curr_def * 100 / gg_curr,
         gg_prop_health = gg_curr_health * 100 / gg_curr,
         gg_prop_edu = gg_curr_edu * 100 / gg_curr,
         gg_prop_security = gg_curr_security * 100 / gg_curr,
         
  # Non-Financial Public Sector
         nfps_prop_def = nfps_curr_def * 100 / nfps_curr,
         nfps_prop_health = nfps_curr_health * 100 / nfps_curr,
         nfps_prop_edu = nfps_curr_edu * 100 / nfps_curr,
         nfps_prop_security = nfps_curr_security * 100 / nfps_curr) %>% 
    calc(c("cg_prop_health", "cg_prop_edu", "cg_prop_security"), "cg_prop_sexp") %>% 
    calc(c("gg_prop_health", "gg_prop_edu", "gg_prop_security"), "gg_prop_sexp") %>% 
    calc(c("nfps_prop_health", "nfps_prop_edu", "nfps_prop_security"), "nfps_prop_sexp") %>% 
  arrange(country, year) %>% 
  select(-c(military_defense_central_government:
              defense_n_e_c_nonfinancial_public_sector))

## 1.3 Social Spending (% GPD) ---------------------------------------------
social_spending_gdp <- read_xlsx("raw_data/cepal_social_spending_gdp.xlsx",
                                 col_types = c(
  "text", "text", "text", "text", "numeric", "numeric", "text", "text", "numeric")
) %>% 
  select(-1, -7, -8, -9) %>% 
  pivot_wider(names_from = 3,
              values_from = value,
              values_fill = NA) %>%
  select(-4, -5, -6) %>% 
  pivot_wider(names_from = 2,
              values_from = c(4:6),
              values_fill = NA) %>% 
  rename(country = 1, year = 2, 
         cg_gdp_health = 3, gg_gdp_health = 4, nfps_gdp_health = 5,
         cg_gdp_edu = 6, gg_gdp_edu = 7, nfps_gdp_edu = 8,
         cg_gdp_security = 9, gg_gdp_security = 10, nfps_gdp_security = 11) %>% 
  calc(c("cg_gdp_edu", "cg_gdp_security", "cg_gdp_health"), "cg_gdp_sexp") %>% 
  calc(c("gg_gdp_edu", "gg_gdp_security", "gg_gdp_health"), "gg_gdp_sexp") %>% 
  calc(c("nfps_gdp_edu", "nfps_gdp_security", "nfps_gdp_health"), "nfps_gdp_sexp") %>% 
  arrange(country, year)

# Correcting Bolivia's country name
social_spending$country[social_spending$country == 
                          "Bolivia (Plurinational State of)"] <- 'Bolivia'
public_spending$country[social_spending$country == 
                          "Bolivia (Plurinational State of)"] <- 'Bolivia'
social_spending_gdp$country[social_spending$country == 
                          "Bolivia (Plurinational State of)"] <- 'Bolivia'


social_spending<- social_spending %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name",
                             destination = "iso3c"))

social_spending_gdp <- social_spending_gdp %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name",
                             destination = "iso3c"))

public_spending <- public_spending %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name",
                             destination = "iso3c"))

lac <- codelist_panel %>% filter(continent == "Americas") %>% 
  select(country.name.en,year,iso3c) %>% 
  filter(year >= 1990, !country.name.en %in% c("United States", "Canada")) %>% 
  complete(year = 1990:2022) %>% drop_na() %>% 
  arrange(country.name.en, year)

lac_expenditure <- lac %>% 
  left_join(social_spending %>% select(-country), join_by(iso3c, year)) %>% 
  left_join(social_spending_gdp%>% select(-country), join_by(iso3c, year)) %>% 
  left_join(public_spending %>% select(-country), join_by(iso3c, year)) %>% 
  rename(country = country.name.en) %>% 
  mutate(cg_curr_def = ifelse(iso3c %in% c("CRI", "PAN"), 0, cg_curr_def),
         gg_curr_def = ifelse(iso3c %in% c("CRI", "PAN"), 0, gg_curr_def),
         nfps_curr_def = ifelse(iso3c %in% c("CRI", "PAN"), 0, nfps_curr_def),
         cg_prop_def = ifelse(iso3c %in% c("CRI", "PAN"), 0, cg_prop_def),
         gg_prop_def = ifelse(iso3c %in% c("CRI", "PAN"), 0, gg_prop_def),
         nfps_prop_def = ifelse(iso3c %in% c("CRI", "PAN"), 0, nfps_prop_def))

lac_expenditure %>% 
  count(iso3c, year) %>% 
  filter(n > 1)

## Exporting full spending data-set
saveRDS(lac_expenditure, "final_data/lac_expenditure.RDS")

# 2. INDEPENDENT VARIABLE -------------------------------------------------
## 2.1 Deflator -----------------------------------------------------------
cpi <- wb_etl(y = "FP.CPI.TOTL", w = 1980, z = 2022) %>% 
  filter(iso3c == "USA") %>% 
  rename(us_cpi = FP.CPI.TOTL) %>% 
  select(iso3c, year, us_cpi)

xpi <- read_xlsx("raw_data/export_price_index.xlsx", skip = 10) %>% 
  clean_names() %>% 
  pivot_longer(cols = jan:dec, names_to = "month", values_to = "xpi") %>% 
  group_by(year) %>% 
  summarise(xpi = mean(xpi, na.rm = T))

## 2.2 Population ---------------------------------------------------------
population <- read.csv2("raw_data/unctad_population.csv", sep = ",", 
                        na.strings = c(NA, "")) %>% 
  rename(country = 1, pop = 3, year = Year) %>% 
  filter(!country %in% c('Individual economies')) %>% 
  left_join(correct_names, join_by(country))

population <- population %>% 
  filter(!country %in% intersect(population$country, correct_names$country)) %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", 
                             destination = "iso3c")) %>% 
  rbind(population %>% filter(country %in% intersect(population$country, 
                                                 correct_names$country))) %>% 
  drop_na(pop, iso3c)

population %>% 
  count(iso3c, year) %>% 
  filter(n > 1)

## 2.3 Commodity Trade Revenue --------------------------------------------
cmd_raw <- read.csv2("raw_data/unctad_trade_2.csv", sep = ",", 
                       na.strings = c(NA, "")) %>% 
  clean_names() %>% 
  rename(country = economy_label, cmd_stone = 4, cmd_stone_prop = 5, 
         ore_metal = 6, ore_metal_prop = 7, iron_steel = 8, 
         iron_steel_prop = 9) %>% 
  pivot_wider(names_from = flow_label, values_from = 
                c(cmd_stone:iron_steel_prop)) %>% 
  clean_names() %>% 
  filter(!country %in% 
           c("Sudan (...2011)", "Indonesia (...2002)")) %>% 
  mutate(across(c(cmd_stone_exports:iron_steel_prop_imports), 
                ~as.numeric(.x))) %>% 
  calc(paste0(c("cmd_stone", "ore_metal", "iron_steel"), "_imports"), 
       "all_cmd_imports") %>% 
  calc(paste0(c("cmd_stone", "ore_metal", "iron_steel"), "_exports"), 
       "all_cmd_exports") %>%
  mutate(all_cmd_net = all_cmd_exports - all_cmd_imports) %>% 
  left_join(correct_names, join_by(country))

cmd_raw_2 <- cmd_raw %>% 
  filter(!country %in% intersect(cmd_raw$country, correct_names$country)) %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", 
                             destination = "iso3c")) %>% 
  rbind(cmd_raw %>% filter(country %in% intersect(cmd_raw$country, 
                                                     correct_names$country))) %>% 
  drop_na(iso3c)

cmd_raw_2 %>% 
  count(iso3c, year) %>% 
  filter(n > 1)

cmd_trade <- cmd_raw_2 %>% 
  left_join(population %>% select(-country), join_by(year, iso3c)) %>% 
  left_join(xpi, join_by(year)) %>% 
  mutate(real_cmd_exports_pcp = 
           (all_cmd_exports/pop) * (100/xpi)) %>% # USA XPI for 2000 is "100"
  filter(year < 2021)

# 3. POLITICAL COVARIATES-----------------------------------------
## 3.1 Database of Political Institutions ----------------------------
dpi <- read.csv2("raw_data/dpi.csv", sep = ",") %>%
  rename(country = countryname) %>%
  mutate(year = as.double(year),
         maj = as.numeric(maj)) %>%
  left_join(correct_names, join_by(country))

dpi <- dpi %>% 
  filter(!country %in% intersect(dpi$country, correct_names$country)) %>% 
  mutate(country = ifelse(country == 'Cent. Af. Rep.', 'Central African Republic', country),
         country = ifelse(country == 'Dom. Rep.', 'Dominican Republic', country),
         country = ifelse(country == 'PRC', "People's Republic of China", country),
         country = ifelse(country == 'PRK', "Korea (the Democratic People's Republic of)", country),
         country = ifelse(country == 'ROK', 'Republic of Korea', 
                          country),
         country = ifelse(country == 'S. Africa', 'South Africa', country),
  )%>%
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c")) %>% 
  rbind(dpi %>% filter(country %in% intersect(dpi$country, 
                                                    correct_names$country)))

dpi %>% 
  count(iso3c, year) %>% 
  filter(n > 1)

## 3.2 Leaders Global  ------------------------------------------------
leadglob <- read.csv2("raw_data/leadglob.csv", sep = ",") %>%
  mutate(year = as.double(year)) %>%
  left_join(correct_names, join_by(country))

leadglob <- leadglob %>% 
  filter(!country %in% intersect(leadglob$country, correct_names$country)) %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", 
                             destination = "iso3c")) %>% 
  rbind(leadglob %>% filter(country %in% intersect(leadglob$country, 
                                                 correct_names$country)))
  
leadglob %>% 
  count(iso3c, year) %>% 
  filter(n > 1) 

# Panama is duplicated
# Removing duplicates
leadglob <- leadglob[!duplicated(leadglob[c("country","year")]),] 

lead_check <- leadglob %>% 
  left_join(dpi %>% select(ifs, year, execme),
            join_by(iso3c == ifs, year == year))

lead_check %>% 
  mutate(check = ifelse(HoS_party_short == execme, 1, 0)) %>% 
  filter(check == 0, year %in% c(1989:2019)) %>% 
  select(country, iso3c, year, execme, HoS_party_short, HoS_party_id, 
         HoS_name, HoS_party_english,
         HoG_party_source, HoG_party_id)

# Data correction
leadglob_final <- leadglob %>% 
  mutate(
    # BRAZIL
    # Collor de Melo (left office at the end of the year)
    HoS_party_short = ifelse(iso3c == "BRA" & year == 1992,
                             "PRN",
                             HoS_party_short),
    HoS_party_id = ifelse(iso3c == "BRA" & year == 1992, 4410,
                          HoS_party_id),
    HoS_party_english = ifelse(iso3c == "BRA" & year == 1992,
                               "National Reconstruction Party", 
                               HoS_party_english),
    HoS_name = ifelse(iso3c == "BRA" & year == 1992,
                      "Fernando Alfonso Collor de Mello", HoS_name),
    
    # Dilma (left office at August)
    HoS_party_short = ifelse(iso3c == "BRA" & year == 2016,
                             "PT",
                             HoS_party_short),
    HoS_party_id = ifelse(iso3c == "BRA" & year == 2016, 356,
                          HoS_party_id),
    HoS_party_english = ifelse(iso3c == "BRA" & year == 2016,
                               "Worker's Party", 
                               HoS_party_english),
    HoS_name = ifelse(iso3c == "BRA" & year == 2016,
                      "Dilma Vana Rousseff", HoS_name),
    
    # Bolsonaro (wrong party)
    HoS_party_english = ifelse(grepl("Bolsonaro", HoS_name),
                               "Social Liberal Party", HoS_party_english),
    HoS_party_short = ifelse(grepl("Bolsonaro", HoS_name),
                             "PSL", HoS_party_short),
    HoS_party_id = ifelse(grepl("Bolsonaro", HoS_name),
                          6920, HoS_party_id),
    
    HoS_party_english = ifelse(grepl("Bolsonaro", HoS_name),
                             "Social Liberal Party", HoS_party_english),
    HoS_party_short = ifelse(grepl("Bolsonaro", HoS_name),
                             "PSL", HoS_party_short),
    HoS_party_id = ifelse(grepl("Bolsonaro", HoS_name),
                               6920, HoS_party_id),
    
    # BOLIVIA: presidents take office irregularly
    HoS_party_short = ifelse(iso3c %in% c("BOL") &
                               year %in% c(1989:2003, 2019, 2020),
                             dplyr::lag(HoS_party_short, 1),
                             HoS_party_short),
    HoS_party_id = ifelse(iso3c %in% c("BOL") &
                            year %in% c(1989:2003, 2019, 2020),
                          dplyr::lag(HoS_party_id, 1),
                          HoS_party_id),
    HoS_party_english = ifelse(iso3c %in% c("BOL") &
                                 year %in% c(1989:2003, 2019, 2020),
                               dplyr::lag(HoS_party_english, 1),
                               HoS_party_english),
    HoS_name = ifelse(iso3c %in% c("BOL") &
                        year %in% c(1989:2003, 2019, 2020),
                      dplyr::lag(HoS_name, 1),
                      HoS_name),
    
    
    # COLOMBIA: new presidents take office in august
    # DOMINICAN REPUBLIC: new presidents take office in august
    # ARGENTINA: new presidents take office in december
    ## ARGENTINA: Adolfo Saá stayed a few months in office
    # MEXICO: new presidents take office in december
    # PANAMA: new presidents take office in the 2nd semester
    HoS_party_short = ifelse(iso3c %in% c("COL", "DOM", "ARG", "MEX",
                                          "PAN"),
                             dplyr::lag(HoS_party_short, 1),
                             HoS_party_short),
    HoS_party_id = ifelse(iso3c %in% c("COL", "DOM", "ARG", "MEX",
                                       "PAN"),
                          dplyr::lag(HoS_party_id, 1),
                          HoS_party_id),
    HoS_party_english = ifelse(iso3c %in% c("COL", "DOM", "ARG",
                                            "MEX", "PAN"),
                               dplyr::lag(HoS_party_english, 1),
                               HoS_party_english),
    HoS_name = ifelse(iso3c %in% c("COL", "DOM", "ARG", "MEX", "PAN"),
                      dplyr::lag(HoS_name, 1),
                      HoS_name),
    
    # ECUADOR: presidents take office irregularly
    # From aug (1996) to feb (1997) there were three brief terms
    HoS_party_short = ifelse(iso3c %in% c("ECU") &
                               year %in% c(1990:1996,1998,2023),
                             dplyr::lag(HoS_party_short, 1),
                             HoS_party_short),
    HoS_party_id = ifelse(iso3c %in% c("ECU") &
                            year %in% c(1990:1996,1998,2023),
                          dplyr::lag(HoS_party_id, 1),
                          HoS_party_id),
    HoS_party_english = ifelse(iso3c %in% c("ECU") &
                                 year %in% c(1990:1996,1998,2023),
                               dplyr::lag(HoS_party_english, 1),
                               HoS_party_english),
    HoS_name = ifelse(iso3c %in% c("ECU") &
                        year %in% c(1990:1996,1998,2023),
                      dplyr::lag(HoS_name, 1),
                      HoS_name),
    
    # GUATEMALA: 
    ## There is only v-party data for GANA (a coalition) in 2004-7
    HoS_party_short = ifelse(grepl("Óscar José Rafael", HoS_name),
                                    "GANA",
                                  HoS_party_short),
    HoS_party_id = ifelse(grepl("Óscar José Rafael", HoS_name), 538,
                          HoS_party_id),
    HoS_party_english = ifelse(grepl("Óscar José Rafael", HoS_name),
                                    "Grand National Alliance",
                                    HoS_party_english),
    
    # GUATEMALA: Alejandro stayed in office for few months
    HoS_party_short = ifelse(iso3c %in% c("GTM") &
                               year %in% c(2015),
                             dplyr::lag(HoS_party_short, 1),
                             HoS_party_short),
    HoS_party_id = ifelse(iso3c %in% c("GTM") &
                            year %in% c(2015),
                          dplyr::lag(HoS_party_id, 1),
                          HoS_party_id),
    HoS_party_english = ifelse(iso3c %in% c("GTM") &
                                 year %in% c(2015),
                               dplyr::lag(HoS_party_english, 1),
                               HoS_party_english),
    HoS_name = ifelse(iso3c %in% c("GTM") &
                        year %in% c(2015),
                      dplyr::lag(HoS_name, 1),
                      HoS_name),
    
    # GUYANA: presidents take office irregularly
    HoS_party_short = ifelse(iso3c %in% c("GUY") &
                               year %in% c(1990:1996, 1999:2011, 2020),
                             dplyr::lag(HoS_party_short, 1),
                             HoS_party_short),
    HoS_party_id = ifelse(iso3c %in% c("GUY") &
                            year %in% c(1990:1996, 1999:2011, 2020),
                          dplyr::lag(HoS_party_id, 1),
                          HoS_party_id),
    HoS_party_english = ifelse(iso3c %in% c("GUY") &
                                 year %in% c(1990:1996, 1999:2011, 2020),
                               dplyr::lag(HoS_party_english, 1),
                               HoS_party_english),
    HoS_name = ifelse(iso3c %in% c("GUY") &
                        year %in% c(1990:1996, 1999:2011, 2020),
                      dplyr::lag(HoS_name, 1),
                      HoS_name),
    
    # GUYANA: Sam Hinds took office in 1997, he was also from PPP
    HoS_name = ifelse(iso3c %in% c("GUY") &
                        year %in% c(1997),
                      "Sam Hinds",
                      HoS_name),
    
    # JAMAICA: HoG take office irregularly
    # Andrew Holness stayed a few months in office
    HoG_party_short = ifelse(iso3c %in% c("JAM") &
                               year %in% c(2007:2011),
                             dplyr::lag(HoG_party_short, 1),
                             HoG_party_short),
    HoG_party_id = ifelse(iso3c %in% c("JAM") &
                            year %in% c(2007:2011),
                          dplyr::lag(HoG_party_id, 1),
                          HoG_party_id),
    HoG_party_english = ifelse(iso3c %in% c("JAM") &
                                 year %in% c(2007:2011),
                               dplyr::lag(HoG_party_english, 1),
                               HoG_party_english),
    HoG_name = ifelse(iso3c %in% c("JAM") &
                        year %in% c(2007:2011),
                      dplyr::lag(HoG_name, 1),
                      HoG_name),
    
    # PARAGUAY: presidents take office irregularly
    HoS_party_short = ifelse(iso3c %in% c("PRY") &
                               year %in% c(1992:1998, 2003:2011,
                                           2013:2023),
                             dplyr::lag(HoS_party_short, 1),
                             HoS_party_short),
    HoS_party_id = ifelse(iso3c %in% c("PRY") &
                            year %in% c(1992:1998, 2003:2011,
                                        2013:2023),
                          dplyr::lag(HoS_party_id, 1),
                          HoS_party_id),
    HoS_party_english = ifelse(iso3c %in% c("PRY") &
                                 year %in% c(1992:1998, 2003:2011,
                                             2013:2023),
                               dplyr::lag(HoS_party_english, 1),
                               HoS_party_english),
    HoS_name = ifelse(iso3c %in% c("PRY") &
                        year %in% c(1992:1998, 2003:2011,
                                    2013:2023),
                      dplyr::lag(HoS_name, 1),
                      HoS_name)
  )

## 3.3 V-Party ------------------------------------------------------------
vparty <- readRDS("raw_data/v_party.rds") %>%
  rename(country = country_name) %>%
  left_join(correct_names, join_by(country))

vparty <- vparty %>% 
  filter(!country %in% intersect(vparty$country, correct_names$country)) %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", 
                             destination = "iso3c")) %>% 
  rbind(vparty %>% filter(country %in% intersect(vparty$country, 
                                              correct_names$country)))
vparty %>% 
  count(iso3c, year, v2paid) %>% 
  filter(n > 1)

## 3.4 State Capacity ---------------------------------------------------
# https://onlinelibrary.wiley.com/doi/10.1111/ecca.12411
statecap_v14 <- read_dta("raw_data/statecap_finalv14.dta") %>%
  rename(country = country_name) %>%
  left_join(correct_names, join_by(country))

statecap_v14 <- statecap_v14 %>% 
  filter(!country %in% intersect(statecap_v14$country, correct_names$country)) %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", 
                             destination = "iso3c")) %>% 
  rbind(statecap_v14 %>% filter(country %in% intersect(statecap_v14$country, 
                                                 correct_names$country)))

## 3.5 Democratic stock -------------------------------------------------
demstock <- read_rds("raw_data/demstock.rds")

demstock.correct_names <- demstock %>%
  rename(country = country_name) %>%
  left_join(correct_names, join_by(country))

#rm(v.dem.out)

democratic.set <- demstock.correct_names %>% 
  filter(!country %in% intersect(demstock.correct_names$country, 
                                 correct_names$country)) %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", 
                             destination = "iso3c")) %>% 
  rbind(demstock.correct_names %>% 
          filter(country %in% intersect(demstock.correct_names$country, 
                                                 correct_names$country)))

democratic.set %>% 
  filter(!is.na(iso3c)) %>% 
  count(iso3c, year) %>% 
  filter(n > 1)

## 3.6 Defense Expenditure (% GDP) ----------------------------------------
def_prop_gdp <- wb_etl(y = 'MS.MIL.XPND.GD.ZS', w = 1980, z = 2024) %>% 
  clean_names() %>% 
  rename(def_prop_gdp = ms_mil_xpnd_gd_zs) %>% 
  mutate(def_prop_gdp = ifelse(iso3c %in% c("CRI", "PAN"), 0, def_prop_gdp))

intersect(def_prop_gdp$country, correct_names$country)

def_prop_gdp %>% 
  count(iso3c, year) %>% 
  filter(n > 1)

milex <- read_excel("raw_data/sipri_milex_data.xlsx", sheet = 2,
                    na = c("...", "xxx")) %>% 
  clean_names() %>% 
  filter(!is.na(country)) %>% 
  select(country, x1980:x2024) %>% 
  pivot_longer(cols = x1980:x2024, values_to = "milex", names_to = "year") %>% 
  mutate(year = as.numeric(
    gsub(pattern = "x", replacement = "", x = year)),
    milex = as.numeric(milex)) %>% 
  filter(!is.na(milex)) %>% 
  left_join(correct_names, join_by(country))

milex_countries <- milex$country

milex_adj <- milex %>% 
  filter(!country %in% intersect(milex_countries, 
                                 correct_names$country)) %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", 
                             destination = "iso3c"),
         year = as.double(year)) %>%
  rbind(milex %>% filter(country %in% intersect(milex_countries, 
                                                    correct_names$country)))

milex_adj %>% 
  count(iso3c, year) %>% 
  filter(n > 1)

# 4. ECONOMIC COVARIATES-------------------------------------------------
## 4.1 World Economic Outlook ---------------------------------------------
weo <- read.csv2("raw_data/weo_data.csv", na = c("n/a", "", "--"), dec = ".") %>% 
  slice(1:2170)

names(weo) <- substr(names(weo), 2, 5)

weo <- weo %>%  
  pivot_longer(cols = c(6:48))

weo$value <- gsub(",", "", weo$value)

weo <- weo %>%
  rename(country = 2, iso3c = 1, year = "name") %>% 
  mutate(value = as.numeric(value),
         year = as.numeric(year)) %>% 
  unite(variable, 3:4, sep = "_", remove = TRUE, na.rm = FALSE) %>%
  pivot_wider(id_cols = c("country", "iso3c", "year"), names_from = "variable",
              values_from = 'value') %>% 
  rename(gdp_nc = 4, # GDP, Constant Prices, National Currency
         gdp_g = 5, # GDP, Constant prices, Percent Change
         gdp_pcp_nc = 6, # GDP Per Capita, Constant prices, National Currency
         gdp_pcp_ppp = 7, # GDP Per Capita, PPP, International Dollar
         inf_avg_idx = 8, # Inflation, Average Prices Index
         inf_avg_g = 9, # Inflation, Average Prices Percent Change
         inf_eop_idx = 10, # Inflation, End of Period, Index
         inf_eop_g = 11, # Inflation, End of Period, Percent Change
         unemp = 12, # Unemployment Rate % Total Labor Force
         population = 13, # Population
         gg_rev = 14, # General Government Revenue % GDP
         gg_exp = 15, # General Government Total Expenditure % GDP
         gg_debt = 16, # General Government Net Debt % GDP
         acc_balance = 17 # Current Account Balance % GDP
  ) %>% 
  mutate(iso3c = case_when(country == "Kosovo" ~ "KSV", .default = iso3c)) %>% 
  select(country, iso3c, year, gdp_pcp_ppp, inf_eop_g, unemp, unemp, gg_rev)

weo %>% 
  count(iso3c, year) %>% 
  filter(n > 1)

## 4.2 Central Government Debt -------------------------------------------
cg_debt <- read_xls("raw_data/imf_central_debt.xls", sheet = 1, na = 'no data') %>%
  slice(-c(1, 176:177)) %>%
  pivot_longer(cols = c(2:73)) %>%
  rename(country = 1, year = 2, cgov_debt = 3) %>%
  mutate(year = as.numeric(year)) %>% 
  left_join(correct_names, join_by(country))

cg_debt <- cg_debt %>% 
  filter(!country %in% intersect(cg_debt$country, correct_names$country)) %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
         year = as.double(year)) %>%
  rbind(cg_debt %>% filter(country %in% intersect(cg_debt$country, 
                                                 correct_names$country))) %>% 
  relocate(1, 4, 2, 3)

cg_debt %>% 
  count(iso3c, year) %>% 
  filter(n > 1)

## 4.3 Economic Openness and Globalization ---------------------------
globalization <- read_dta('raw_data/kof_globalization.dta') %>% 
  filter(!country %in% c("East Asia and Pacific", "Europe and Central Asia", 
                         "High income", "Latin America and Caribbean", 
                         "Low income", "Lower middle income", 
                         "Middle East and North Africa", "North America", 
                         "South Asia", "Sub-Saharan Africa", 
                         "Upper middle income", "World")) %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", destination = "iso3c")) %>% 
  rename(kof_trade_df = KOFTrGIdf, # Trade globalization idx de facto
         kof_trade_dj = KOFTrGIdj, # Trade globalization idx de jure
         kof_finance_df = KOFFiGIdf, # Financial globaliztion idx de facto
         kof_finance_dj = KOFFiGIdj, # Financial globaliztion idx de jure
         kof_personal_df = KOFIpGIdf, # Interpersonal glob. idx de facto
         kof_personal_dj = KOFIpGIdj, # Interpersonal glob. idx de jure
         kof_info_df = KOFInGIdf, # Informational globalization idx de facto
         kof_info_dj = KOFInGIdj, # Informational globalization idx de jure
         kof_pol_df = KOFPoGIdf, # Political globalization idx de facto
         kof_pol_dj = KOFPoGIdj, # Political globalization idx de jure
         kof_idx = KOFGI, # Globalization Index
         kof_idx_df = KOFGIdf, # Globalization Index de facto
         kof_idx_dj = KOFGIdj) %>% # Globalization Index de jure
  select(-c(KOFCuGIdf, KOFCuGIdj, KOFEcGI, KOFEcGIdf, KOFEcGIdj,
            KOFTrGI, KOFFiGI, KOFSoGI, KOFSoGIdf, KOFSoGIdj,
            KOFIpGI, KOFCuGI, KOFPoGI,code))

globalization %>% 
  count(iso3c, year) %>% 
  filter(n > 1)

intersect(globalization$country, correct_names$country)

## 4.4 Urban population ------------------------------------------------
urban_pop <- wb_etl(y = 'SP.URB.TOTL.IN.ZS', w = 1980, z = 2024) %>% 
  rename(urban_pop = 4)

urban_pop %>% 
  count(country, year) %>% 
  filter(n > 1)

## 4.5 Natural Resources Depletion -------------------------------------
res_depletion <- wb_etl(y = 'NY.ADJ.DRES.GN.ZS', w = 1980, z = 2024) %>% 
  rename(res_depletion = 4)

res_depletion %>% 
  count(country, year) %>% 
  filter(n > 1)

# 5. SOCIETAL COVARIATES -------------------------------------------------
## 5.1 Conflict -------------------------------------------------------
warfare <- read_xlsx("raw_data/csp_political_violence.xlsx") %>% 
  select(-scode) %>% 
  left_join(correct_names, join_by(country))

warfare <- warfare %>% 
  filter(!country %in% intersect(warfare$country, correct_names$country)) %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", 
                             destination = "iso3c")) %>% 
  rbind(warfare %>% filter(country %in% intersect(warfare$country, 
                                                 correct_names$country)))

warfare %>% 
  count(iso3c, year) %>% 
  filter(n > 1)

intersect(warfare$country, correct_names$country)

## 5.2 Dependency Ratio ---------------------------------------------------
dp_ratio_old <- wb_etl(y = 'SP.POP.DPND.OL', w = 1980, z = 2024) %>% 
  rename(dp_ratio_old = 4)

dp_ratio_old %>% 
  count(iso3c, year) %>% 
  filter(n > 1)

dp_ratio_yg <- wb_etl(y = 'SP.POP.DPND.YG', w = 1980, z = 2024) %>% 
  rename(dp_ratio_yg = 4)

dp_ratio_yg %>% 
  count(iso3c, year) %>% 
  filter(n > 1)

## 5.3 Setler Mortality ------------------------------------------------
settler_mortality <- read_csv2("raw_data/qogdata_settler_mortality.csv") %>% 
  rename(country = cname_qog) %>% 
  left_join(correct_names, join_by(country))

settler_mortality <- settler_mortality %>% 
  filter(!country %in% intersect(settler_mortality$country, 
                                 correct_names$country)) %>% 
  mutate(country = countryname(country),
         iso3c = countrycode(country, origin = "country.name", 
                             destination = "iso3c")) %>% 
  rbind(settler_mortality %>% filter(country %in% intersect(settler_mortality$country, 
                                                  correct_names$country)))

# 6. EUCLIDEAN DISTANCE ------------------------------------------------
st_iso3c <- c("USA", "ATG", "BHS", "BRB", "CUB", "DMA", "DOM", "GRD", "HTI", 
           "KNA", "LCA", "VCT", "TTO", "BLZ", "CRI", "SLV", "GTM", "HND", 
           "MEX", "NIC", "PAN", "ARG", "BOL", "BRA", "CHL", "COL", "ECU", 
           "GUY", "PRY", "PER", "SUR", "URY", "VEN", "JAM")

geo_sf <- ne_countries(scale = "large", returnclass="sf") %>% 
  filter(iso_a3 %in% st_iso3c)

df_country_iso <- geo_sf$iso_a3

geo_sf$admin[20]

df_dist <- st_distance(geo_sf[20,], geo_sf) %>% as.numeric()

country_distances <- data.frame(iso3c = df_country_iso,
                                dist = df_dist)

# 7. DATA ENRICHMENT ---------------------------------------------------
## 7.1 Easy bind -----------------------------------------------------
ncp_count <- read.csv2("final_data/ncp_count.csv")

ws_dataset <- lac %>%
  rename(country = country.name.en) %>% 
  
  # Dependent Variables
  left_join(lac_expenditure %>% select(year, iso3c, cg_pcp_sexp, cg_prop_sexp, 
                                       cg_prop_def, cg_gdp_sexp, -country),
            join_by(year, iso3c)) %>%
  
  left_join(ncp_count %>% select(-country), join_by(year, iso3c)) %>% 
  
  # Independent Variables
  left_join(cmd_trade %>% select(year, iso3c, all_cmd_imports, 
                                 real_cmd_exports_pcp,
                                 pop),
            join_by(year, iso3c)) %>% 
  
  # Political Covariates
  left_join(dpi %>% select(iso3c, year, system, maj, execrlc, exelec), 
            join_by(year, iso3c)) %>% 
  left_join(statecap_v14 %>% select(iso3c, year, statecap_baseline,
                                    -country)) %>% 
  left_join(democratic.set %>% select(iso3c, year, 
                                      v2x_polyarchy, demstock_one)) %>% 
  
  # Political Economy
  left_join(weo %>% select(iso3c, year, gdp_pcp_ppp, inf_eop_g, unemp,
                           gg_rev,
                           -country)) %>%
  
  left_join(cg_debt %>% select(-country),
            join_by(year, iso3c)) %>% 
  left_join(globalization %>% select(iso3c, year, kof_trade_df, kof_trade_dj,
                                     -country),
            join_by(year, iso3c)) %>% 
  # Alternate military expenditures
  left_join(def_prop_gdp %>% select(iso3c, year, def_prop_gdp),
            join_by(year, iso3c)) %>% 
  left_join(milex_adj %>% select(iso3c, year, milex),
            join_by(year, iso3c)) %>% 
  
  #left_join(res_depletion %>% select(year, iso3c, res_depletion)) %>%
  left_join(urban_pop %>% select(year, iso3c, urban_pop),
            join_by(year, iso3c)) %>% 
  
  # Societal Covariates
  left_join(dp_ratio_old %>% select(year, iso3c, dp_ratio_old),
            join_by(year, iso3c)) %>% 
  left_join(dp_ratio_yg %>% select(year, iso3c, dp_ratio_yg),
            join_by(year, iso3c)) %>% 
  left_join(warfare %>% select(year, iso3c, 
                               # civviol, # Civil violence
                               # civwar, # Civil war
                               # ethviol, # Ethnic violence
                               # ethwar, # Ethnic war
                               civtot),
            join_by(year, iso3c)) %>% 
  left_join(settler_mortality %>% select(year, iso3c, ajr_settmort),
            join_by(year, iso3c)) %>% 
  
  # Euclidean distance
  left_join(country_distances, join_by(iso3c))

## 7.3 Hard Bind -----------------------------------------------------------
### 7.3.1 Elections --------------------------------------------------------

# Government party
ws_dataset.lead <- leadglob_final %>%
  mutate(year = as.double(year)) %>%
  select(iso3c, year, HoS_name, HoS_party_short, HoS_party_english, 
         HoS_party_id, HoG_name, HoG_party_short, HoG_party_english, 
         HoG_party_id) %>%
  right_join(ws_dataset, join_by(iso3c, year)) %>%
  arrange(country, iso3c, year) %>% 
  mutate(leader = ifelse(system == 'Presidential', HoS_name, NA),
         leader = ifelse(system == 'Assembly-Elected President', HoS_name, leader),
         leader = ifelse(system == 'Parliamentary', HoG_name, leader),
         party_name = ifelse(system == 'Presidential', HoS_party_english, NA),
         party_name = ifelse(system == 'Assembly-Elected President', HoS_party_english, party_name),
         party_name = ifelse(system == 'Parliamentary', HoG_party_english, party_name),
         pf_party_id = ifelse(system == 'Presidential', HoS_party_id, NA),
         pf_party_id = ifelse(system == 'Assembly-Elected President', HoS_party_id, pf_party_id),
         pf_party_id = ifelse(system == 'Parliamentary', HoG_party_id, pf_party_id),
         party_short = ifelse(system == 'Presidential', HoS_party_short, NA),
         party_short = ifelse(system == 'Assembly-Elected President', HoS_party_short, party_short),
         party_short = ifelse(system == 'Parliamentary', HoG_party_short, party_short)) %>% 
  select(-HoS_name, -HoS_party_short, -HoS_party_english, -HoS_party_id, 
         -HoG_name, -HoG_party_short, -HoG_party_english, -HoG_party_id)

### 7.3.3 Party Ideology -----------------------------------------------------
ws_dataset.lead <- ws_dataset.lead %>%
  mutate(year1 = year)

vparty_sel <- vparty %>%
  select(iso3c, country, year, pf_party_id, v2pariglef_ord) %>%
  mutate(year2 = year)

ws_dataset.ideo <- ws_dataset.lead %>%
  left_join(vparty_sel,
            by = join_by(country, pf_party_id, closest(year1 >= year2))) %>%
  mutate(region = countrycode(country, origin = "country.name", 
                              destination = "region23"),
         iso3c = countrycode(country, origin = "country.name", 
                             destination = "iso3c"))

final_ws_dataset <- ws_dataset.ideo %>%
  select(-c(iso3c.x, iso3c.y, year.x, iso3c.y, year.y, year2)) %>%
  rename(year = year1) %>% 
  relocate(region, iso3c, country, year, system,
           leader, party_name, pf_party_id, party_short) %>%
  arrange(region, country, year)

adj_ws_dataset <- final_ws_dataset %>% 
  mutate(region = ifelse(country == "Mexico", 'North America', region),
         v2pariglef_ord = 
           case_when(iso3c == "VEN" & year %in% c(2007:2009) ~ 1,
                     iso3c == "NIC" & year %in% c(1997:2000) ~ 5,
                     iso3c == "GTM" & year %in% c(2011) ~ 2,
                     iso3c == "ECU" & year %in% c(1997) ~ 3,
                     iso3c == "BOL" & year %in% c(1993) ~ 2,
                     .default = v2pariglef_ord),
         milex = ifelse(iso3c %in% c("CRI", "PAN"), 0, milex),
         milex_pop = milex / pop * 100
  )

# 8. DATASET EXPORT ------------------------------------------------------
saveRDS(adj_ws_dataset, "final_data/ws_dataset.RDS")
write.csv2(adj_ws_dataset, "final_data/ws_dataset.csv", na = '')
write_dta(adj_ws_dataset, "final_data/ws_dataset.dta")