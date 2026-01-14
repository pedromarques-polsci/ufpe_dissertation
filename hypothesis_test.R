# Packages -----------------------------------------------------------
library(betareg)
library(broom)
library(car)
library(fastDummies)
library(ggpubr)
library(ggrepel)
library(glue)
library(haven)
library(janitor)
library(lmtest)
library(marginaleffects)
library(plm)
library(sandwich)
library(tidyverse)
library(zoo)

# 1. LOAD AND TRANSFORM -------------------------------------------------------

# Functions
# Filter is applied to the dataset before interpolation
data_sample <- function(df, variable, 
                        old_dataset = dataset,
                        doit = T) {
  
  if(doit == T) {
    df <- old_dataset %>% 
    arrange(iso3c, year) %>% 
    filter(year %in% c(1990:2019)) %>% 
    group_by(iso3c) %>% 
    summarise(mean = mean(!is.na(.data[[variable]]))) %>%
    filter(mean >= 0.6) %>%
    select(iso3c) %>%
    inner_join(df, by = "iso3c") %>% 
    ungroup() %>% 
    filter(year %in% c(1990:2019))}
  
  if(doit == F) {
    df <- ws_dataset
  }
  
  return(df)
}

# I recommend not using proportional commodity trade data in the meanwhile
# since it seems flawed and it is not priority

## 1.1 Data -----------------------------------------------------------

latin_countries <- c(
  "ARG", "BOL", "BRA", "CHL", "COL", "CRI", "CUB", "DOM", "ECU", "SLV", 
  "GTM", "HTI", "HND", "MEX", "NIC", "PAN", "PRY", "PER", "PRI", "URY", "VEN"
)

dataset <- readRDS("final_data/ws_dataset.RDS") %>% 
  mutate(latin = ifelse(iso3c %in% latin_countries, 1, 0),
         log_cg_pcp_sexp = log(cg_pcp_sexp),
         crisis = ifelse(year %in% c(2008, 2009), 1, 0)) %>% 
  arrange(iso3c, year) %>% 
  group_by(iso3c) %>% 
  ungroup()

# Do not interpolate these countries, because of 10 years of data blackout
no_int_iso3c <- c("BLZ", "GRD", "VCT")

dataset_interpol <- dataset %>% 
  arrange(iso3c, year) %>% 
  group_by(iso3c) %>% 
  mutate(cg_pcp_sexp_missing = ifelse(is.na(cg_pcp_sexp), 1, 0),
         cg_prop_sexp_missing = ifelse(is.na(cg_prop_sexp), 1, 0),
         cg_gdp_sexp_missing = ifelse(is.na(cg_gdp_sexp), 1, 0),
         cg_prop_def_missing = ifelse(is.na(cg_prop_def), 1, 0),
         def_prop_gdp_missing = ifelse(is.na(def_prop_gdp), 1, 0),
         
         # Linear Interpolation
         cg_pcp_sexp = na.approx(cg_pcp_sexp, na.rm = FALSE),
         cg_prop_sexp = ifelse(iso3c %in% no_int_iso3c, 
                               cg_prop_sexp, na.approx(cg_prop_sexp, na.rm = FALSE)),
         cg_gdp_sexp = na.approx(cg_gdp_sexp, na.rm = FALSE),
         cg_prop_def = na.approx(cg_prop_def, na.rm = FALSE),
         def_prop_gdp = na.approx(def_prop_gdp, na.rm = FALSE),
         milex_pop = na.approx(milex_pop, na.rm = FALSE),
         
         # Interpolated values and Non-Interpolated values
         cg_pcp_sexp_missing_tl = ifelse(cg_pcp_sexp_missing == 1,
                                         cg_pcp_sexp, NA),
         cg_prop_sexp_missing_tl = ifelse(cg_prop_sexp_missing == 1,
                                         cg_prop_sexp, NA),
         cg_gdp_sexp_missing_tl = ifelse(cg_gdp_sexp_missing == 1,
                                         cg_gdp_sexp, NA),
         cg_prop_def_missing_tl = ifelse(cg_prop_def_missing == 1,
                                         cg_prop_def, NA),
         def_prop_gdp_missing_tl = ifelse(def_prop_gdp_missing == 1,
                                          def_prop_gdp, NA),
         
         # Log
         log_cg_pcp_sexp = log(cg_pcp_sexp),
         log_real_cmd_exports_pcp = log(real_cmd_exports_pcp),
         log_gdp_pcp_ppp = log(gdp_pcp_ppp),
         log_milex_pop = log(milex_pop + 0.01),
         
         # Interaction
         log_cmd_poly = log_real_cmd_exports_pcp * demstock_one,
         cmd_poly = real_cmd_exports_pcp * demstock_one,

         # Alternative Regional Classification
         region2 = ifelse(region %in% c('Central America', 'North America'),
                         'Central America and MEX', region)) %>%
  ungroup()

# Ideology imputation for Belize and Bahamas
gps_19 <- read.csv2("raw_data/gps_2019.csv", sep = ",") %>% 
  clean_names()

dpi_blz_bhs <- read.csv2("raw_data/dpi.csv", sep = ",") %>%
  rename(country = countryname) %>%
  mutate(year = as.double(year)) %>% 
  filter(ifs %in% c("BLZ", "BHS")) %>% 
  select(country, ifs, year, execme) %>% 
  left_join(gps_19 %>% select(iso, partyabb, v4_scale),
            join_by(ifs == iso, execme == partyabb)) %>% 
  rename(party_short = execme) %>% 
  mutate(ideology = case_when(
    v4_scale <= 10/7 ~ 0,
    v4_scale > 10/7 & v4_scale <= 20/7 ~ 1,
    v4_scale > 20/7 & v4_scale <= 30/7 ~ 2,
    v4_scale > 30/7 & v4_scale <= 40/7 ~ 3,
    v4_scale > 40/7 & v4_scale <= 50/7 ~ 4,
    v4_scale > 50/7 & v4_scale <= 60/7 ~ 5,
    v4_scale > 60/7 & v4_scale <= 70/7 ~ 6
  ))

ws_dataset <- dataset_interpol %>% 
  left_join(dpi_blz_bhs %>% select(ifs, year, ideology), 
            join_by(iso3c == ifs, year == year)) %>% 
  mutate(v2pariglef_ord = ifelse(is.na(v2pariglef_ord), ideology,
                                       v2pariglef_ord),
         region2 = case_when(
           region2 == "South America" ~ "América do Sul",
           region2 == "Caribbean" ~ "Caribe",
           region2 == "Central America and MEX" ~ "América Central e MEX"
         )) %>% 
  select(-ideology)

## 1.2 Effective countries ----------------------------------------------------
cgpcpiso3c <- ws_dataset %>% 
  data_sample(variable = "cg_pcp_sexp",
              old_dataset = dataset) %>% distinct(iso3c)

cgpropiso3c <- ws_dataset %>% 
  data_sample(variable = "cg_prop_sexp", 
              old_dataset = dataset) %>% distinct(iso3c)

cggdpiso3c <- ws_dataset %>% 
  data_sample(variable = "cg_gdp_sexp",
              old_dataset = dataset) %>% distinct(iso3c)

eff_iso3c <- rbind(cgpcpiso3c, cgpropiso3c, cggdpiso3c) %>% 
  distinct(iso3c)

## 1.3 Formulas -----------------------------------------------------------
plm.model <- "log_cg_pcp_sexp ~ log_real_cmd_exports_pcp + maj + kof_trade_df + 
dp_ratio_old + v2pariglef_ord + def_prop_gdp"

plm.model.dummy <- "log_cg_pcp_sexp ~ log_real_cmd_exports_pcp + maj + 
kof_trade_df + dp_ratio_old + v2pariglef_ord + def_prop_gdp + as.factor(iso3c)"

plm.model.b <- "log_cg_pcp_sexp ~ log_real_cmd_exports_pcp + maj + 
kof_trade_df + dp_ratio_old + v2pariglef_ord"

prop.budget.model <- "cg_prop_sexp ~ log_real_cmd_exports_pcp + maj + 
kof_trade_df + dp_ratio_old + v2pariglef_ord + def_prop_gdp"

prop.budget.model.b <- "cg_prop_sexp ~ log_real_cmd_exports_pcp + maj + 
kof_trade_df + dp_ratio_old + v2pariglef_ord"

prop.gdp.model <- "cg_gdp_sexp ~ log_real_cmd_exports_pcp + maj + 
kof_trade_df + dp_ratio_old + v2pariglef_ord + def_prop_gdp"

prop.gdp.model <- "cg_gdp_sexp ~ log_real_cmd_exports_pcp + maj + 
kof_trade_df + dp_ratio_old + v2pariglef_ord"

## 1.4 Dataset for GMM analysis --------------------------------------------
gmm_dataset <- ws_dataset %>% data_sample(variable = "log_cg_pcp_sexp",
                           old_dataset = dataset)

write_dta(gmm_dataset, "final_data/gmm_data.dta")

rm(gmm_dataset)

# 2. DATA SCREENING -------------------------------------------------------
# Number of missings before and after interpolation
missing_pre_int <- dataset %>% 
  filter(year %in% c(1990:2019)) %>% group_by(iso3c) %>% 
  summarise(mean_pcp = mean(!is.na(cg_pcp_sexp)),
            mean_prop = mean(!is.na(cg_prop_sexp)),
            mean_gdp = mean(!is.na(cg_gdp_sexp)),
            mean_def = mean(!is.na(cg_prop_def)))

missing_pre_int %>% 
  arrange(desc(mean_pcp + mean_prop + mean_gdp)) %>% 
  print(n = 50)

missing_post_int <- ws_dataset %>% 
  filter(year %in% c(1990:2019)) %>% group_by(iso3c) %>% 
  summarise(pmean_pcp = mean(!is.na(cg_pcp_sexp)),
            pmean_prop = mean(!is.na(cg_prop_sexp)),
            pmean_gdp = mean(!is.na(cg_gdp_sexp)),
            pmean_def = mean(!is.na(cg_prop_def)))

# Interpolated country-years
ws_dataset %>% filter(year < 2020, 
                      !is.na(cg_pcp_sexp_missing_tl)) %>% 
  distinct(iso3c, year) %>% 
  mutate(combined = paste(iso3c, year, sep = "-")) %>% select(combined)

ws_dataset %>% filter(year < 2020,
                      !is.na(cg_prop_sexp_missing_tl)) %>% 
  distinct(iso3c, year) %>% 
  mutate(combined = paste(iso3c, year, sep = "-")) %>% select(combined) %>% 
  print(n = 70)

ws_dataset %>% filter(year < 2020,
                      !is.na(cg_gdp_sexp_missing_tl)) %>% 
  distinct(iso3c, year) %>% 
  mutate(combined = paste(iso3c, year, sep = "-")) %>% select(combined)

ws_dataset %>% filter(year < 2020,
                      !is.na(def_prop_gdp_missing_tl),
                      iso3c %in% eff_iso3c$iso3c) %>% 
  distinct(iso3c, year) %>% 
  mutate(combined = paste(iso3c, year, sep = "-")) %>% select(combined) %>% 
  print(n = 50)

# Number of observations
nobs_count <- function(df, var_name) {
  var_sym <- sym(var_name)
  
  nob <- df %>% 
    filter(year %in% 1990:2019) %>% 
    summarise(nob = sum(!is.na(!!var_sym))) %>% 
    pull(nob)
  
  noc <- df %>% 
    filter(!is.na(!!var_sym), year %in% 1990:2019) %>% 
    summarise(noc = n_distinct(iso3c)) %>% 
    pull(noc)
  
  tibble(variavel = var_name, nob, noc)
}

outcome <- c("cg_pcp_sexp", "cg_prop_sexp", "cg_gdp_sexp")

# Full sample
map_df(outcome, ~ nobs_count(dataset, .x))

# Sub-sample
map_df(outcome, ~ nobs_count(ws_dataset %>% 
                               data_sample(variable = .x,
                                           old_dataset = dataset), .x))

ws_dataset %>% 
  data_sample(variable = "cg_pcp_sexp",
              old_dataset = dataset) %>% distinct(iso3c)

# Time mean
ws_dataset %>% 
  data_sample(variable = "cg_gdp_sexp",
              old_dataset = dataset) %>% group_by(iso3c) %>% 
  summarise(x = sum(!is.na(cg_pcp_sexp))) %>% ungroup() %>% summarise(mean(x))

# Number of missings
ws_dataset %>% 
  filter(iso3c %in% eff_iso3c$iso3c) %>% 
  group_by(iso3c) %>% 
  summarise(across(
    c(cg_pcp_sexp, cg_prop_def, def_prop_gdp, milex_pop,
      real_cmd_exports_pcp, maj, kof_trade_df, 
      v2pariglef_ord, execrlc, dp_ratio_old),
    list(
      miss_mean = ~ mean(is.na(.))
    ),
    .names = "{.col}_{.fn}"
  ))

ideo_miss <- ws_dataset %>% 
  filter(iso3c %in% eff_iso3c$iso3c,
         is.na(v2pariglef_ord),
         year < 2020) %>% 
  select(iso3c, year, leader:party_short, v2pariglef_ord)

# 3. DESCRIPTIVE ANALYSIS -------------------------------------------------

## 3.1 Descriptive Statistics ----------------------------------------------
stats <- ws_dataset %>% filter(year < 2020, 
                               iso3c %in% (eff_iso3c$iso3c)) %>% 
  summarise(across(
    c(cg_pcp_sexp, cg_gdp_sexp, cg_prop_sexp, cg_prop_def, 
      def_prop_gdp,
      real_cmd_exports_pcp,
      maj, kof_trade_df,
      gdp_pcp_ppp, v2pariglef_ord, inf_eop_g, unemp, dp_ratio_old, urban_pop),
    list(
      min = ~ min(., na.rm = TRUE),
      q1 = ~ quantile(., 0.25, na.rm = TRUE),
      q2 = ~ quantile(., 0.50, na.rm = TRUE),
      mean = ~ mean(., na.rm = TRUE),
      q3 = ~ quantile(., 0.75, na.rm = TRUE),
      max = ~ max(., na.rm = TRUE),
      sd = ~ sd(., na.rm = TRUE),
      miss = ~ sum(is.na(.))
    ),
    .names = "{.col}_{.fn}"
  )) %>% 
  mutate(
    across(where(is.numeric), ~ round(., digits = 1)),
    across(where(is.numeric), ~ format(., scientific = FALSE, nsmall = 1))) %>% 
  pivot_longer(
    cols = everything(),
    names_to = c("variavel", "estatistica"),
    names_pattern = "(.*)_(.*)",
    values_to = "valor"
  ) %>%
  pivot_wider(
    names_from = estatistica,
    values_from = valor
  )

stats

## 3.2 Fourth chapter --------------------------------------------------
# Commodity boom
shade_export_boom <- data.frame(x1=c(2003), 
                                x2=c(2013), 
                                y1=c(200), 
                                y2=c(1600))

gg_mean_export_region <- ws_dataset %>% 
  mutate(region2 = 
           fct_relevel(region2, "América do Sul",
                       "Caribe", "América Central e MEX")) %>% 
  group_by(year, region2) %>% 
  reframe(mean_cmd = mean(real_cmd_exports_pcp, na.rm = T)) %>% 
  filter(year < 2020, year > 1994) %>% 
  ggplot(aes(x = year, y = mean_cmd, group = region2)) +
  geom_rect(data = shade_export_boom,
            mapping=aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
            inherit.aes = FALSE,
            color=NA, alpha=0.45, fill = "grey80") +
  geom_line(aes()) +
  geom_point(aes(shape = region2), size = 4) +
  geom_vline(xintercept = 2008, linetype = "dashed") +
  theme_minimal() +
  labs(x = "Ano", y = "Exportação de Commodities per capita", 
       shape = "Subregião") +
  scale_x_continuous(breaks = seq(1995, 2019, by = 1)) +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

ggsave("plot/gg_mean_export_region.jpeg", gg_mean_export_region, 
       width = 15, height = 8, 
       dpi = 300)

# Mean social expenditure
cores_dalton_friendly <- c(
  "#005AFF",   # Azul intenso (mantido, mas se quiser remover, avise!)
  "red",   # Vermelho-alaranjado
  "#FF69E9"   # Rosa-choque (mantido)
)

latamean <- ws_dataset %>% 
  group_by(year) %>% 
  reframe(latam_mean = mean(cg_gdp_sexp, na.rm = T))
  
gg_welfare_region_mean <- ws_dataset %>% 
  mutate(region2 = 
           fct_relevel(region2, "América do Sul",
                       "Caribe", "América Central e MEX")) %>% 
  group_by(year, region2) %>% 
  reframe(mean_sexp = mean(cg_gdp_sexp, na.rm = T)) %>% 
  filter(year < 2020) %>% 
  left_join(latamean) %>% 
  ggplot(aes(x = year, y = mean_sexp, group = region2)) +
  annotate("rect", 
           xmin = 2003, xmax = 2013,
           ymin = -Inf, ymax = Inf,
           fill = "grey80", alpha = 0.45) +
  geom_line(aes(colour = region2), linewidth = 1.5,
            linetype = "solid") +
  geom_line(aes(y = latam_mean, linetype = "América Latina e Caribe"), 
            linewidth = 1.5) +
  scale_colour_manual(values = cores_dalton_friendly) +
  scale_linetype_manual(values = "dashed", name = "") +
  geom_vline(xintercept = 2008, linetype = "dashed") +
  theme_minimal() +
  labs(x = "Ano", y = "Gastos Sociais (% PIB)", 
       colour = "Recorte") +
  scale_x_continuous(breaks = seq(1990, 2019, by = 1)) +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

gg_welfare_region_mean

ggsave("plot/gg_welfare_region_mean.jpeg", gg_welfare_region_mean, 
       width = 11, height = 6, 
       dpi = 300)

gg_welfare_latam_mean <- ws_dataset %>% 
  group_by(year) %>% 
  reframe(mean_sexp = mean(cg_pcp_sexp, na.rm = T)) %>% 
  filter(year < 2020) %>% 
  ggplot(aes(x = year, y = mean_sexp)) +
  annotate("rect", 
           xmin = 2003, xmax = 2013,
           ymin = -Inf, ymax = Inf,
           fill = "grey80", alpha = 0.45) +
  geom_line(aes(), linewidth = 1) +
  geom_smooth(method = "lm", color = "black", linetype = "dashed",
              linewidth = 0.5, fill = "lightblue") +
  geom_point(size = 3, shape = 15) +
  #geom_vline(xintercept = 2008, linetype = "dashed") +
  theme_minimal() +
  labs(x = "Ano", y = "Gastos Sociais per capita") +
  scale_x_continuous(breaks = seq(1990, 2019, by = 1)) +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

gg_welfare_latam_mean

ggsave("plot/gg_welfare_latam_mean.jpeg", gg_welfare_latam_mean, 
       width = 11, height = 6, 
       dpi = 300)

# Mean of p.p variation by decade and region
ws_dataset %>% 
  mutate(decade = paste0(substr(year, 1, 3), 0),
         year_last_digit = substr(year, 4, 4)) %>% 
  arrange(iso3c, year) %>% 
  group_by(iso3c, decade) %>% 
  reframe(cg_gdp_sexp = cg_gdp_sexp[which(year_last_digit==9)] -
           cg_gdp_sexp[which(year_last_digit==0)],
          region2 = first(region2)) %>% 
  group_by(decade, region2) %>% 
  reframe(x = mean(cg_gdp_sexp, na.rm = T)) %>% filter(decade != 2020) %>% 
  pivot_wider(names_from = decade, names_prefix = "d", values_from = x) %>% 
  mutate(across(d1990:d2010, ~round(.x, 2)))

# Mean of p.p variation by decade
ws_dataset %>% 
  mutate(decade = paste0(substr(year, 1, 3), 0),
         year_last_digit = substr(year, 4, 4)) %>% 
  arrange(iso3c, year) %>% 
  group_by(iso3c, decade) %>% 
  reframe(cg_gdp_sexp = cg_gdp_sexp[which(year_last_digit==9)] -
            cg_gdp_sexp[which(year_last_digit==0)],
          region2 = first(region2)) %>% 
  group_by(decade) %>% 
  reframe(x = mean(cg_gdp_sexp, na.rm = T)) %>% filter(decade != 2020) %>% 
  pivot_wider(names_from = decade, names_prefix = "d", values_from = x) %>% 
  mutate(across(d1990:d2010, ~round(.x, 2)))

# Mean of p.p variation during commodity boom by region
ws_dataset %>% 
  arrange(iso3c, year) %>% 
  group_by(iso3c) %>% 
  reframe(cg_gdp_sexp = cg_gdp_sexp[which(year==2013)] -
            cg_gdp_sexp[which(year==2003)],
          region2 = first(region2)) %>% 
  group_by(region2) %>% 
  reframe(x = mean(cg_gdp_sexp, na.rm = T))

# Mean of p.p variation during commodity boom
ws_dataset %>% 
  arrange(iso3c, year) %>% 
  group_by(iso3c) %>% 
  reframe(cg_gdp_sexp = cg_gdp_sexp[which(year==2013)] -
            cg_gdp_sexp[which(year==2003)],
          region2 = first(region2)) %>% 
  ungroup() %>% 
  reframe(x = mean(cg_gdp_sexp, na.rm = T))

# Net advancement
ws_net_advance <- ws_dataset %>% 
  group_by(iso3c) %>% 
  reframe(boom = cg_gdp_sexp[which(year==2013)] -
            cg_gdp_sexp[which(year==2003)],
          
          post_boom = cg_gdp_sexp[which(year==2019)] -
            cg_gdp_sexp[which(year==2014)],
          
          region2 = first(region2))

gg_welfare_net_advance <- ws_net_advance %>%
  filter(!is.na(boom),
         !is.na(post_boom)) %>% 
  ggplot(aes(x = reorder(iso3c, boom))) +
  geom_bar(aes(y = boom, fill = "2003-2013"), 
           stat = "identity") +
  geom_bar(aes(y = post_boom), 
           fill = NA, colour = "black", linetype = "dashed",
           stat = "identity", ) +
  geom_point(aes(y = post_boom, 
                 fill = "2014-2019"), size = 7, shape = 18,
             colour = "darkblue") +
  scale_fill_manual(name = "Período", 
                    values = c("2003-2013" = "#00A9E0",
                               "2014-2019" = "darkblue")) +
  coord_flip() +
  theme_minimal() +
  xlab("País") + ylab("Variação absoluta (p.p)") +
  theme(text = element_text(size = 21),
        legend.position = "bottom")

gg_welfare_net_advance

ggsave("plot/gg_welfare_net_advance.jpeg", gg_welfare_net_advance, 
       width = 13, height = 7, 
       dpi = 300)

## 3.2 Commodity Boom Era ----------------------------------------------
boom_var <- ws_dataset %>% 
  filter(iso3c %in% eff_iso3c$iso3c) %>% 
  group_by(iso3c) %>% 
  reframe(cmd_var = (real_cmd_exports_pcp[which(year == 2013)] - 
                       real_cmd_exports_pcp[which(year == 2003)])/
            real_cmd_exports_pcp[which(year == 2003)]*100,
          sexp_var = (cg_pcp_sexp[which(year == 2013)] - 
                        cg_pcp_sexp[which(year == 2003)])/
            cg_pcp_sexp[which(year == 2003)]*100,
          region2 = first(region2)) 

# Commodity percentual change
boom_var %>% 
  reframe(mean(cmd_var))

boom_var %>% 
  arrange(cmd_var) %>% 
  print(n = 50)

gg_boom_export <- boom_var %>% 
  ggplot(aes(x = reorder(iso3c, cmd_var), y = cmd_var, shape = region2)) +
  geom_point(size = 3.5) +
  geom_hline(aes(yintercept = mean(cmd_var), color = "Variação média"), 
             linetype = 'dashed', linewidth = 0.5) +
  geom_text_repel(aes(y = cmd_var, label = paste0(round(cmd_var, 1))), 
                  vjust = 5, size = 7, direction = "y") +
  scale_color_manual(name = "", 
                     values = c("Variação média" = "black")) +
  xlab("País") +
  ylab("") +
  labs(shape = "Região") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1))

gg_boom_export

ggsave("plot/gg_boom_export.jpeg", gg_boom_export, 
       width = 15, height = 9, 
       dpi = 500)

# Social expenditure percentual change
boom_var %>% 
  reframe(mean(sexp_var, na.rm = T))

gg_boom_sexp <- boom_var %>% 
  filter(!is.na(sexp_var)) %>% 
  ggplot(aes(x = reorder(iso3c, sexp_var), y = sexp_var, shape = region2)) +
  geom_point(size = 3.5) +
  geom_hline(aes(yintercept = mean(sexp_var), color = "Variação média"), 
             linetype = 'dashed', linewidth = 0.5) +
  geom_text_repel(aes(y = sexp_var, label = paste0(round(sexp_var, 1))), 
                  vjust = 5, size = 7, direction = "y") +
  scale_color_manual(name = "", 
                     values = c("Variação média" = "black")) +
  xlab("País") +
  ylab("") +
  labs(shape = "Região") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1))

gg_boom_sexp

ggsave("plot/gg_boom_sexp.jpeg", gg_boom_sexp, 
       width = 15, height = 8, 
       dpi = 500)

## 3.3 Boxplots ---------------------------------------------
ws_dataset %>% 
  filter(iso3c %in% eff_iso3c$iso3c,
         year %in% c(1990:2019)) %>% 
  mutate(iso3c = fct_reorder(iso3c, real_cmd_exports_pcp, .fun='mean')) %>%
  ggplot(aes(x = reorder(iso3c, real_cmd_exports_pcp, fun = mean), 
             y = real_cmd_exports_pcp)) +
  geom_boxplot() +
  xlab("País") +
  ylab("Exportação de commodities per capita") +
  theme_minimal()

ws_dataset %>% 
  filter(iso3c %in% eff_iso3c$iso3c, !is.na(cg_pcp_sexp),
         year %in% c(1990:2019)) %>% 
  mutate(iso3c = fct_reorder(as.factor(iso3c), cg_pcp_sexp, .fun='mean')) %>%
  ggplot(aes(x = reorder(iso3c, cg_pcp_sexp, fun = mean), 
             y = cg_pcp_sexp)) +
  geom_boxplot() +
  xlab("País") +
  ylab("Gasto Social per capita") +
  theme_minimal()


## 3.4 Histograms ----------------------------------------------------------
hist_cg_pcp_sexp <- ws_dataset %>% 
  data_sample(variable = "cg_pcp_sexp", old_dataset = dataset) %>% 
  ggplot(aes(x = cg_pcp_sexp)) +
  geom_histogram(colour = "black", fill = "grey", binwidth = 100) +
  theme_minimal() +
  xlab("Gastos sociais \n Escala original") +
  ylab("") +
  theme(text = element_text(size = 24))

hist_log_cg_pcp_sexp <- ws_dataset %>% 
  data_sample(variable = "log_cg_pcp_sexp", old_dataset = dataset) %>% 
  ggplot(aes(x = log_cg_pcp_sexp)) +
  geom_histogram(colour = "black", fill = "grey", binwidth = 0.15) +
  theme_minimal() +
  xlab("Gastos sociais \n Escala logarítmica") +
  ylab("") +
  theme(text = element_text(size = 24))

hist_pcp_sexp <- ggarrange(hist_cg_pcp_sexp, hist_log_cg_pcp_sexp, ncol = 2)

hist_cmd <- ws_dataset %>% 
  data_sample(variable = "real_cmd_exports_pcp", old_dataset = dataset) %>% 
  ggplot(aes(x = real_cmd_exports_pcp)) +
  geom_histogram(colour = "black", fill = "grey", binwidth = 200) +
  theme_minimal() +
  xlab("Exportação de commodities \n Escala original") +
  ylab("") +
  theme(text = element_text(size = 24))

hist_log_cmd <- ws_dataset %>% 
  data_sample(variable = "real_cmd_exports_pcp", old_dataset = dataset) %>% 
  ggplot(aes(x = log_real_cmd_exports_pcp)) +
  geom_histogram(colour = "black", fill = "grey", binwidth = 0.2) +
  theme_minimal() +
  xlab("Exportação de commodities \n Escala logarítmica") +
  ylab("") +
  theme(text = element_text(size = 24))

hist_cmd <- ggarrange(hist_cmd, hist_log_cmd, ncol = 2)

ggsave("plot/hist_pcp_sexp.jpeg", hist_pcp_sexp, 
       width = 14, height = 8, 
       dpi = 500)

ggsave("plot/hist_cmd.jpeg", hist_cmd, 
       width = 14, height = 8, 
       dpi = 500)

## 3.5 Percentual change by subsamples -----------------------------------------

### 3.5.1 Dependent variable = Social spending per capita -------
ws_dataset_var <- ws_dataset %>% 
  data_sample(variable = "cg_pcp_sexp",
              old_dataset = dataset) %>% 
  group_by(iso3c) %>% 
  mutate(cmd_var = (real_cmd_exports_pcp - 
                      dplyr::lag(real_cmd_exports_pcp, n = 5))/
           dplyr::lag(real_cmd_exports_pcp, n = 5)*100) %>% 
  select(iso3c, year, cmd_var) %>% 
  ungroup()

# Mean 5 year change
ws_dataset_var %>% 
  filter(year %in% c(1990:2019)) %>% 
  summarise(mean = mean(cmd_var, na.rm = T)) %>% 
  arrange(mean) %>% print(n = 50)

ws_dataset_var %>% 
  group_by(iso3c) %>% summarise(meani = mean(cmd_var, na.rm = T)) %>% 
  arrange(meani) %>% print(n = 50)

# Overall change
ws_dataset %>% 
  data_sample(variable = "cg_pcp_sexp",
              old_dataset = dataset) %>% 
  group_by(iso3c) %>% 
  reframe(cmd_var = (real_cmd_exports_pcp[which(year == 2013)] - 
                       real_cmd_exports_pcp[which(year == 2003)])/
            real_cmd_exports_pcp[which(year == 2003)]*100,
          sexp_var = (cg_pcp_sexp[which(year == 2013)] - 
                        cg_pcp_sexp[which(year == 2003)])/
            cg_pcp_sexp[which(year == 2003)]*100,
          region2 = first(region2)) %>% 
  reframe(mean(cmd_var))

### 3.5.2 Dependent variable = Social spending (% total) ------------------
ws_dataset %>% 
  data_sample(variable = "cg_prop_sexp",
              old_dataset = dataset) %>% 
  reframe(mean = mean(cg_prop_sexp, na.rm = T),
          sd = sd(cg_prop_sexp, na.rm = T))

### 3.5.3 Dependent variable = Social spending (% GDP) ------------------
ws_dataset %>% 
  data_sample(variable = "cg_gdp_sexp",
              old_dataset = dataset) %>% 
  reframe(mean = mean(cg_gdp_sexp, na.rm = T),
          sd = sd(cg_gdp_sexp, na.rm = T))

# 3.6 Costa Rica vs Panama ------------------------------------------------
ws_dataset %>% 
  filter(iso3c %in% c("PAN", "CRI")) %>% 
  group_by(iso3c) %>% 
  reframe(across(c(cg_pcp_sexp, cg_gdp_sexp, cg_prop_sexp, cg_prop_def, 
                   real_cmd_exports_pcp, maj, kof_trade_df, 
                   kof_trade_dj, gdp_pcp_ppp, v2pariglef_ord, inf_eop_g, unemp, 
                   dp_ratio_old, urban_pop), ~ mean(.x, na.rm = TRUE))) %>% 
  t()

ws_dataset %>% 
  filter(iso3c %in% c("PAN", "CRI"),
         year %in% c(2003:2019)) %>% 
  group_by(iso3c) %>% 
  reframe(across(c(cg_pcp_sexp, cg_gdp_sexp, cg_prop_sexp, cg_prop_def, 
                   real_cmd_exports_pcp, maj, kof_trade_df, 
                   kof_trade_dj, gdp_pcp_ppp, v2pariglef_ord, inf_eop_g, unemp, 
                   dp_ratio_old, urban_pop, demstock_one), ~ 
                   mean(.x, na.rm = TRUE))) %>% 
  t()

ws_dataset %>% 
  filter(iso3c %in% c("PAN", "CRI"),
         year == 2019) %>% 
  select(cg_pcp_sexp, cg_gdp_sexp, cg_prop_sexp, cg_prop_def, 
         real_cmd_exports_pcp, maj, kof_trade_df, 
         kof_trade_dj, gdp_pcp_ppp, v2pariglef_ord, inf_eop_g, unemp, 
         dp_ratio_old, urban_pop, demstock_one) %>% t()

ws_dataset %>% 
  filter(iso3c %in% c("PAN", "CRI")) %>% 
  group_by(iso3c) %>% 
  reframe(cg_gdp_sexp = cg_gdp_sexp[which(year == 2013)] - 
            cg_gdp_sexp[which(year == 2003)],
          cg_prop_sexp = cg_prop_sexp[which(year == 2013)] - 
            cg_prop_sexp[which(year == 2003)],
          cg_pcp_sexp = cg_pcp_sexp[which(year == 2013)] - 
            cg_pcp_sexp[which(year == 2003)]) %>% 
  t()

# 4 COVARIANCE PLOTS ------------------------------------------------------
cov_plot <- function(data = ws_dataset, outcome, treatment,
                     outcome.label, treatment.label,
                     colour = F) {
  outcome <- rlang::ensym(outcome)
  treatment <- rlang::ensym(treatment)
  
  data %>% filter(year %in% 1990:2019,
                  iso3c %in% eff_iso3c$iso3c) %>% 
    data_sample(variable = outcome, old_dataset = dataset) %>% 
    ggplot(aes(x = !!treatment, y = !!outcome)) +
    geom_point(aes(colour = if (colour) region2 else NULL)) +
    stat_cor() +
    xlab(treatment.label) +
    ylab(outcome.label) +
    scale_color_manual(name = "Subregião",
                       labels = c("Caribe", "América Central + Méx", 
                                  "América do Sul"),
                       values = c("#FBB4AE", "#E37BA2", "black")) +
    geom_smooth(method = "lm", color = "black", linetype = "dashed",
                linewidth = 0.5, fill = "grey") +
    theme_minimal() +
    theme(legend.position = "bottom",
          text = element_text(size = 21))
}

cov_plot.wrap <- function(data = ws_dataset, outcome, treatment,
                          outcome.label, treatment.label) {
  outcome <- rlang::ensym(outcome)
  treatment <- rlang::ensym(treatment)
  
  data %>% filter(year %in% 1990:2019,
                  iso3c %in% eff_iso3c$iso3c) %>% 
    data_sample(variable = outcome, old_dataset = dataset) %>% 
    ggplot(aes(x = !!treatment, y = !!outcome)) +
    geom_point() +
    stat_cor(label.y = Inf, vjust = 1.0) +
    geom_smooth(method = "lm", color = "black", linetype = "dashed",
                linewidth = 0.5, fill = "grey") +
    facet_wrap(~region2, nrow = 2, scale = "free") +
    xlab(treatment.label) +
    ylab(outcome.label) +
    theme_minimal() +
    theme(text = element_text(size = 21))
}

### 4.1 Social spending per capita ----------------------------------------
# Commodity Exports
cov_cg_pcp_export <- 
  cov_plot(outcome = "log_cg_pcp_sexp", 
           outcome.label = "Gasto Social per capita (log)",
           treatment = "log_real_cmd_exports_pcp",
           treatment.label = "Exportação de Commodities per capita (log)")

cov_cg_pcp_export

cov_cg_pcp_export_wrap <- 
  cov_plot.wrap(outcome = "log_cg_pcp_sexp", 
                outcome.label = "Gasto Social per capita (log)",
                treatment = "log_real_cmd_exports_pcp",
                treatment.label = "Exportação de Commodities per capita (log)")

cov_cg_pcp_export_wrap

ggsave("plot/cov_cg_pcp_export.jpeg", cov_cg_pcp_export, 
       width = 12, height = 8, 
       dpi = 500)

ggsave("plot/cov_cg_pcp_export_wrap.jpeg", cov_cg_pcp_export_wrap, 
       width = 12, height = 8, 
       dpi = 500)

### 4.2 Euclidean Distance from USA ----------------------------------------
euclidean_mean <- ws_dataset %>%
  filter(year < 2020) %>% 
  group_by(iso3c) %>% 
  summarise(mean_cmd = mean(log_real_cmd_exports_pcp, na.rm = T),
            dist = first(dist),
            iso3c = first(iso3c)) %>% 
  ggplot(aes(x = dist, y = mean_cmd)) +
  geom_smooth(method = "lm", color = "black", linetype = "dashed",
              linewidth = 0.5, fill = "grey") +
  geom_text_repel(aes(y = mean_cmd, x = dist, label = iso3c),
                  size = 5.5) +
  xlab("Distância euclidiana em relação aos Estados Unidos") +
  ylab("Média de exportação de commodities") +
  stat_cor(label.y = Inf, vjust = 1.0) +
  geom_point() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 21))

euclidean_mean

euclidean_2019 <- ws_dataset %>%
  filter(year == 2019) %>% 
  group_by(iso3c) %>% 
  ggplot(aes(x = dist, y = log_real_cmd_exports_pcp)) +
  geom_smooth(method = "lm", color = "black", linetype = "dashed",
              linewidth = 0.5, fill = "grey") +
  geom_text_repel(aes(y = log_real_cmd_exports_pcp, x = dist, label = iso3c),
                  size = 5.5) +
  xlab("Distância euclidiana em relação aos Estados Unidos") +
  ylab("Exportação de commodities (2019)") +
  stat_cor(label.y = Inf, vjust = 1.0) +
  geom_point() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 21))

euclidean_2019

euclidean_welfare_2019 <- ws_dataset %>%
  filter(year == 2019) %>% 
  group_by(iso3c) %>% 
  ggplot(aes(x = dist, y = log_cg_pcp_sexp)) +
  geom_smooth(method = "lm", color = "black", linetype = "dashed",
              linewidth = 0.5, fill = "grey") +
  geom_text_repel(aes(y = log_cg_pcp_sexp, x = dist, label = iso3c),
                  size = 5.5) +
  xlab("Distância euclidiana em relação aos Estados Unidos") +
  ylab("Gastos sociais (2019)") +
  stat_cor(label.y = Inf, vjust = 1.0) +
  geom_point() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 21))

euclidean_welfare_2019

ggsave("plot/euclidean_mean.jpeg", euclidean_mean,
       width = 12, height = 8,
       dpi = 500)

ggsave("plot/euclidean_2019.jpeg", euclidean_2019,
       width = 12, height = 8,
       dpi = 500)

ggsave("plot/euclidean_welfare_2019.jpeg", euclidean_welfare_2019,
       width = 12, height = 8,
       dpi = 500)

# 5. STATIONARITY TESTS -----------------------------------------------------
ips_test <- function(dataset = ws_dataset, variable, lag, crit = "AIC",
                     method) {
  df <- dataset %>% 
    data_sample(variable = variable, old_dataset = dataset) %>% 
    filter(!is.na(.data[[variable]])) %>% 
    pdata.frame(index = c("country", "year"))
  
  result <- purtest(df[[variable]], data = df, lags = crit, 
                    exo = "trend", test = method, pmax = lag)
  
  return(list(print(variable), result))
  
}

ws_dataset %>% filter(year %in% c(1990:2019)) %>% 
  ips_test(variable = "cg_pcp_sexp", lag = 5, method = "ips",
           crit = "AIC")

ws_dataset %>% filter(year %in% c(1990:2019)) %>% 
ips_test(variable = "log_cg_pcp_sexp", lag = 4, method = "ips",
         crit = "AIC")

# 6. MODEL SELECTION ------------------------------------------------------

## 6.2 VIF Test ------------------------------------------------------------
vif_test <- function(model, data, vd) {
  df <- data %>% 
    data_sample(variable = vd, old_dataset = dataset)
  ols <- lm(model, data = df)
  vif.r <- vif(ols)
  return(list(vd, vif.r))
}

vif_test(data = ws_dataset, model = plm.model, 
         vd = "log_cg_pcp_sexp")

vif_test(data = ws_dataset, model = prop.budget.model, 
         vd = "cg_prop_sexp")

vif_test(data = ws_dataset, model = prop.gdp.model, 
         vd = "cg_gdp_sexp")

## 6.3 F Test for Individual Effects -----------------------------------------
f_test <- function(model, data, vd) {

  df <- data %>% data_sample(variable = vd, old_dataset = dataset)
  panel_df <- df %>% pdata.frame(index = c("iso3c", "year"))
  ols <- plm(formula = model, data = panel_df, model = "pooling")
  fixed <- plm(formula = model, data = panel_df, model = "within", 
               effect = "individual")
  
  result <- pFtest(fixed, ols) %>% 
     glance() #%>% 
     # mutate(outcome = vd,
     #        explanatory = explanatory)
  
   return(result)
}

f_test(plm.model, ws_dataset, "log_cg_pcp_sexp") %>% 
  select(df1, df2, statistic, p.value)

## 6.4 Hausman Test ----------------------------------------------------
hausman_test <- function(model, data, vd) {
  
  df <- data %>% data_sample(variable = vd, old_dataset = dataset)
  panel_df <- df %>% pdata.frame(index = c("iso3c", "year"))
  
  fixed_model <- plm(formula = model, data = panel_df, model = "within")
  random_model <- plm(formula = model, data = panel_df, model = "random", 
               effect = "individual")
  
  result <- phtest(fixed_model, random_model, vcov = vcovHC) %>% 
    glance()
  
  return(result)
}

hausman_test(plm.model, ws_dataset, "log_cg_pcp_sexp") %>% 
  select(-method, -alternative)

## 6.5 Breusch-Pagan Test ----------------------------------------------------
breusch_pagan_test <- function(model, data, vd) {
  
  df <- data %>% data_sample(variable = vd, old_dataset = dataset)
  panel_df <- df %>% pdata.frame(index = c("iso3c", "year"))
  
  result <- bptest(model, data = panel_df) %>% 
    glance() #%>% 
    # mutate(outcome = vd,
    #        explanatory = explanatory)
    
  return(result)
}

breusch_pagan_test(as.formula(plm.model), ws_dataset, "log_cg_pcp_sexp") %>% 
  select(-method)

## 6.6 Auto-correlation Test --------------------------------------------------
auto_corr_test <- function(model, data, vd) {
  
  df <- data %>% data_sample(variable = vd, old_dataset = dataset)
  panel_df <- df %>% pdata.frame(index = c("iso3c", "year"))
  
  result1 <- pwtest(model, data = panel_df) %>% 
   glance()
    
  return(result1)
}

auto_corr_test(as.formula(plm.model), ws_dataset, "log_cg_pcp_sexp") %>% 
  arrange(method)

# 7. REGRESSION ANALYSIS --------------------------------------------------
p_sign <- function(df){
  df %>% 
  mutate(
  significance = case_when(
    p.value < 0.01 ~ "***",
    p.value < 0.05 ~ "**",
    p.value < 0.1  ~ "*",
    TRUE           ~ "NS"
  ))
}

## 7.1 STANDARD PANEL DATA -------------------------------------------------
plm_test <- function(model, data, vd, direction = "individual") {
  
  # Sampling
  df <- data %>% data_sample(variable = vd, 
                             old_dataset = dataset)
  
  panel_df <- pdata.frame(df, index = c("iso3c", "year"))
  
  # Panel data models
  models <- list(
    pooled = plm(model, data = panel_df, model = "pooling"),
    fe     = plm(model, data = panel_df, model = "within", 
                 effect = direction),
    re     = plm(model, data = panel_df, model = "random",
                 effect = direction),
    fd     = plm(model, data = panel_df, model = "fd")
  )
  
  # VCOV standard errors
  models_vcov <- lapply(models, function(model) {
    coeftest(model, vcov = vcovHC(model, type = "HC3", cluster = "group"))
  })
  
  # Extraction function for each model
  extract_results <- function(model, vcov_model, model_name) {
    
    # Parameters, p-value, robust std errors, confidence intervals
    tidy_model <- tidy(vcov_model, conf.int = TRUE)
    
    # Number of observations
    nobs <- nobs(vcov_model) %>% tibble()
    
    # Adj (dfcor) R Squared
    rsquared <- r.squared(model, dfcor = TRUE) %>% tibble()
    
    # Tidying up everything
    tibble(
      outcome      = vd,
      model        = model_name,
      term         = tidy_model$term,
      estimate     = tidy_model$estimate,
      std.error    = tidy_model$std.error,
      p.value      = tidy_model$p.value,
      conf.low     = tidy_model$conf.low,
      conf.high    = tidy_model$conf.high,
      nobs         = nobs,
      adj.rsquared     = rsquared) %>% 
      mutate(
        significance = case_when(
          p.value < 0.01 ~ "***",
          p.value < 0.05 ~ "**",
          p.value < 0.1  ~ "*",
          TRUE           ~ "NS"
        )
      )
  }
  
  # Applying function to each element of the list
  results <- purrr::pmap_dfr(
    list(models, models_vcov, names(models)),
    extract_results
  )
  
  return(results)
  
}

# Preferred models
linear_reg_results <- plm_test(as.formula(plm.model), 
                                     ws_dataset, "log_cg_pcp_sexp")

# Counter-argument
linear_reg_results.b <- plm_test(as.formula(plm.model.b), 
                               ws_dataset, "log_cg_pcp_sexp")

# Dummy fixed effects
dummy_df <- ws_dataset %>% data_sample(variable = "log_cg_pcp_sexp", 
                           old_dataset = dataset) %>% 
  pdata.frame(index = c("iso3c", "year"))

dummy.reg <- plm(plm.model.dummy, data = dummy_df, model = "pooling")

r.squared(dummy.reg, dfcor = TRUE)

dummy.result <- dummy.reg %>% 
  coeftest(., vcov = vcovHC(., type = "HC3", cluster = "group")) %>% 
  tidy() %>% 
  mutate(
    significance = case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.1  ~ "*",
      TRUE           ~ "NS"
    ),
    avg = (exp(estimate) - 1) * 100
  ) # Argentine is the base dummy

## 7.2 FRACTIONAL PROBIT MODEL ---------------------------------------------
return.fracset <- function(data, variable){
  complete.data <- data %>% 
    data_sample(variable = variable,
                old_dataset = dataset) %>% 
    filter(complete.cases(select(., !!sym(variable), log_real_cmd_exports_pcp, 
                                 maj,
                                 kof_trade_df, dp_ratio_old, v2pariglef_ord, 
                                 def_prop_gdp)))
  
  fracset <- dummy_cols(complete.data,
                        select_columns = c("year"),
                        remove_first_dummy = F) %>% 
    group_by(iso3c) %>%
    mutate(across(
      .cols = starts_with("year_"),
      .fns = ~ mean(.x, na.rm = TRUE),
      .names = "yb_{.col}"
    )) %>%
    mutate(across(
      .cols = c(log_real_cmd_exports_pcp, maj, kof_trade_df, dp_ratio_old,
                v2pariglef_ord, def_prop_gdp),
      .fns = ~ mean(.x, na.rm = TRUE),
      .names = "b_{.col}"
    )) %>% 
    ungroup() %>% 
    mutate(!!variable := !!sym(variable) / 100)
  
  return(fracset)
}

glm_test <- function(data, variable, model) {
  dataset <- return.fracset(data = data, variable = variable)
  
  nobs <- nrow(dataset)
  
  cre <- glue(model, 
              " + b_log_real_cmd_exports_pcp + b_maj + b_kof_trade_df + 
                           b_dp_ratio_old + b_v2pariglef_ord + b_def_prop_gdp")
  
  creu <- glue(cre, " + {paste0('yb_year_', 1995:2019, collapse = ' + ')}")
  
  models <- list(
    pooled = glm(model %>% as.formula(), 
                 family = quasibinomial(link = probit), data = dataset),
    cre    = glm(cre %>% as.formula(), 
                  family = quasibinomial(link = probit), data = dataset),
    creu   = glm(creu %>% as.formula(), 
                   family = quasibinomial(link = probit), data = dataset)
  )
  
  # VCOV standard errors
  models_vcov <- lapply(models, function(model) {
    coeftest(model, vcov = vcovHC(model, type = "HC3", cluster = "group"))
  })
  
  # Average marginal effects
  model_slopes <- lapply(models, function(model) {
    avg_slopes(model, vcov = vcovHC(model, type = "HC3", cluster = "group"))
  })
  
  # Extraction function for each model
  extract_results <- function(model, true_model, model_name) {
    
    # Parameters, p-value, robust std errors, confidence intervals
    tidy_model <- tidy(true_model, conf.int = TRUE)
    
    # Tidying up everything
    tibble(
      model        = model_name,
      term         = tidy_model$term,
      estimate     = tidy_model$estimate,
      std.error    = tidy_model$std.error,
      p.value      = tidy_model$p.value,
      conf.low     = tidy_model$conf.low,
      conf.high    = tidy_model$conf.high,
      nobs = nobs) %>% 
      mutate(
        significance = case_when(
          p.value < 0.01 ~ "***",
          p.value < 0.05 ~ "**",
          p.value < 0.1  ~ "*",
          TRUE           ~ "NS"
        )
      )
  }
  
  # Applying function to each element of the list
  std_results <- purrr::pmap_dfr(
    list(models, models_vcov, names(models)),
    extract_results
  )
  
  marginal_results <- purrr::pmap_dfr(
    list(models, model_slopes, names(models)),
    extract_results
  ) %>% 
    mutate(model = case_when(
      model == "pooled" ~ "ape_pooled",
      model == "cre" ~ "ape_cre",
      model == "creu" ~ "ape_creu",
    ))
  
  return(rbind(std_results, marginal_results))
  
}

options(scipen=999)

prop.budget.results <- glm_test(ws_dataset, "cg_prop_sexp", prop.budget.model)

prop.gdp.results <- glm_test(ws_dataset, "cg_gdp_sexp", prop.gdp.model.b)

prop.gdp.results %>% 
  filter(!grepl(x = term, pattern = "b_")) %>%
  mutate(estimate = paste0(round(estimate, digits = 3),
                           " ", significance,
                           " (",
                           round(std.error, digits = 3), ")")) %>%
  mutate(term = fct_relevel(term, "(Intercept)", "log_real_cmd_exports_pcp",
                            "maj", "kof_trade_df", "dp_ratio_old",
                            "v2pariglef_ord", "def_prop_gdp")) %>%
  arrange(term) %>%
  select(-c(std.error:significance)) %>%
  pivot_wider(names_from = model, values_from = estimate) %>%
  distinct(term, .keep_all = T) %>%
  select(pooled, ape_pooled, cre, ape_cre,
         creu, ape_creu)

## 7.3 BETA REGRESSION WITH CORRELATED RANDOM EFFECTS -------------------------
# Setup
beta.years <- prop.budget.results %>% filter(model == "ape_creu", 
                                             !is.na(std.error)) %>% 
  pull(term)

beta.yb <- glue("b_log_real_cmd_exports_pcp + b_maj + b_kof_trade_df + 
b_dp_ratio_old + b_v2pariglef_ord + b_def_prop_gdp + 
                    {paste0(beta.years, collapse = ' + ')}")

# Budget prop
prop.budget.dataset <- return.fracset(ws_dataset, "cg_prop_sexp") 

budget.beta.fit <- betareg(paste0("cg_prop_sexp ~ ", beta.yb),
                         data = prop.budget.dataset)

budget.beta.fit %>% tidy() %>% 
  p_sign()

budget.beta.slopes <- budget.beta.fit %>% 
  avg_slopes(vcov = TRUE) %>% tidy() %>% 
  p_sign()
  
# GDP prop
beta.gdp.years <- prop.gdp.results %>% filter(model == "ape_creu", 
                                             !is.na(std.error)) %>% 
  pull(term)

beta.gdp.yb <- glue("b_log_real_cmd_exports_pcp + b_maj + b_kof_trade_df + 
b_dp_ratio_old + b_v2pariglef_ord + b_def_prop_gdp + 
                    {paste0(beta.gdp.years, collapse = ' + ')}")

prop.gdp.dataset <- return.fracset(ws_dataset, "cg_gdp_sexp") 

gdp.beta.fit <- betareg(paste0("cg_gdp_sexp ~ ", beta.gdp.yb),
                           data = prop.gdp.dataset)

gdp.beta.fit %>% tidy() %>% 
  p_sign()
  
gdp.beta.fit %>% 
  avg_slopes(vcov = TRUE) %>% tidy() %>% 
  p_sign() %>% 
  filter(!grepl(x = term, pattern = "b_")) %>%
  mutate(estimate = paste0(round(estimate, digits = 3),
                           " ", significance,
                           " (",
                           round(std.error, digits = 3), ")")) %>%
  mutate(term = fct_relevel(term, "log_real_cmd_exports_pcp",
                            "maj", "kof_trade_df", "dp_ratio_old",
                            "v2pariglef_ord", "def_prop_gdp")) %>%
  arrange(term) %>% 
  select(estimate)

# APPENDIX A -----------------------------------------------------------------
ws_visualizer <- function(data, var, varlabel, scale = "free_y") {
  var <- ensym(var)
  data %>% 
    filter(year %in% 1990:2019) %>% 
    filter(n() != sum(is.na(!!var)), .by = country) %>% 
    ggplot(aes(x=year, y=!!var, group = 1)) +
    facet_wrap(
      ~factor(country), ncol = 5, scales = scale) +
    xlab("Ano") + ylab(varlabel) +
    theme_minimal() +
    geom_line(linewidth = 0.8) +
    theme(text = element_text(size = 18),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
}

# Plotting time series indicating when there is interpolation
ws_visualizer_approx <- function(data, var, var_int, varlabel) {
  var_int <- ensym(var_int)
  var <- ensym(var)
  
  data %>% 
    filter(year %in% 1990:2019) %>% 
    filter(n() != sum(is.na(!!var)), .by = country) %>% 
    ggplot(aes(x = year, group = 1)) +
    geom_line(aes(y = !!var, colour = varlabel),
              linewidth = 0.8) +
    geom_line(aes(y = !!var_int, colour = "Missings interpolados"),
              linewidth = 1.0) +
    geom_point(aes(y = !!var, colour = varlabel),
               size = 1) +
    geom_point(aes(y = !!var_int, colour = "Missings interpolados"),
               size = 1) +
    scale_color_manual(values = setNames(c("black", "red"), 
                                         c(varlabel, "Missings interpolados")),
                       name = "Legenda") +
    facet_wrap(~factor(country), ncol = 5, scales = "free_y") +
    xlab("Ano") + ylab(varlabel) +
    theme_minimal() +
    theme(text = element_text(size = 18),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position="bottom")
}

sexp_pcp_ts <- ws_dataset %>% 
  data_sample(variable = "cg_pcp_sexp", old_dataset = dataset) %>% 
  ws_visualizer_approx(varlabel = "Gasto social per capita", 
                       var = cg_pcp_sexp,
                       var_int = cg_pcp_sexp_missing_tl)

sexp_pcp_ts

ggsave("plot/sexp_pcp_ts.jpeg", sexp_pcp_ts, width = 12, height = 8, 
       dpi = 500)

sexp_prop_ts <- ws_dataset %>% 
  data_sample(variable = "cg_prop_sexp", old_dataset = dataset) %>% 
  ws_visualizer_approx(varlabel = "Gasto social (% Gasto Total)", 
                       var = cg_prop_sexp,
                       var_int = cg_prop_sexp_missing_tl)

sexp_prop_ts

ggsave("plot/sexp_prop_ts.jpeg", sexp_prop_ts, width = 12, height = 8, 
       dpi = 500)

sexp_gdp_ts <- ws_dataset %>% 
  data_sample(variable = "cg_gdp_sexp", old_dataset = dataset) %>% 
  ws_visualizer_approx(varlabel = "Gasto social (% PIB)", 
                       var = cg_gdp_sexp,
                       var_int = cg_gdp_sexp_missing_tl)

sexp_gdp_ts

ggsave("plot/sexp_gdp_ts.jpeg", sexp_gdp_ts, width = 12, height = 8, 
       dpi = 500)

def_prop_gdp_ts <- ws_dataset %>% 
  # I'm not filtering by defense expenditure since it is not a VD
  filter(iso3c %in% eff_iso3c$iso3c,
         !iso3c %in% c("CRI", "PAN")) %>% 
  ws_visualizer_approx(varlabel = "Gasto militar (% PIB)", 
                       var = def_prop_gdp,
                       var_int = def_prop_gdp_missing_tl)

def_prop_gdp_ts

ggsave("plot/def_prop_gdp_ts.jpeg", def_prop_gdp_ts, width = 12, 
       height = 8, 
       dpi = 500)

ws_dataset %>% 
  ws_visualizer(real_cmd_exports_pcp, 
                "Receita de exportação de commodities per capita")

# LEFTOVER ----------------------------------------------------------------
# formula <- glue("cg_gdp_sexp ~ log_real_cmd_exports_pcp + maj +
#                 kof_trade_df + dp_ratio_old + v2pariglef_ord + def_prop_gdp +
#                 demstock_one + log_cmd_poly +
#                 b_log_real_cmd_exports_pcp + b_maj + b_kof_trade_df +
#                 b_dp_ratio_old + b_v2pariglef_ord + b_def_prop_gdp +
#                 b_demstock_one + b_log_cmd_poly +
#                 {paste0('yb_year_', c(1995:1997,1999:2001), collapse = ' + ')}") %>%
#   as.formula()
# 
# complete.data.gdp <- ws_dataset %>%
#   data_sample(variable = "cg_gdp_sexp") %>%
#   filter(complete.cases(select(., cg_gdp_sexp,
#                                log_real_cmd_exports_pcp, maj,
#                                kof_trade_df, dp_ratio_old,
#                                v2pariglef_ord, def_prop_gdp,
#                                demstock_one, log_cmd_poly)))
# 
# fracset.gdp <- dummy_cols(complete.data.gdp,
#                       select_columns = c("year"),
#                       remove_first_dummy = F) %>%
#   group_by(iso3c) %>%
#   mutate(across(
#     .cols = starts_with("year_"),
#     .fns = ~ mean(.x, na.rm = TRUE),
#     .names = "yb_{.col}"
#   )) %>%
#   mutate(across(
#     .cols = c(log_real_cmd_exports_pcp, maj, kof_trade_df, dp_ratio_old,
#               v2pariglef_ord, def_prop_gdp, demstock_one,
#               log_cmd_poly),
#     .fns = ~ mean(.x, na.rm = TRUE),
#     .names = "b_{.col}"
#   )) %>%
#   ungroup() %>%
#   mutate(cg_gdp_sexp = cg_gdp_sexp / 100)
# 
# mod <- glm(formula,
#     family = quasibinomial(link = probit), data = fracset.gdp)
# 
# gdp.result <- mod %>%
#   avg_slopes(., vcov = vcovHC(., type = "HC3", cluster = "group")) %>%
#   tidy()
# 
# slopes(mod, variables = "log_real_cmd_exports_pcp",
#        newdata = datagrid(demstock_one = fivenum),
#        vcov = vcovHC(mod, type = "HC3", cluster = "group"))
# 
# plot_slopes(mod, variables = "log_real_cmd_exports_pcp",
#             condition = "demstock_one",
#             vcov = vcovHC(mod, type = "HC3", cluster = "group")) +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   theme_minimal()
# 
# ###
# ### Per capita
# ###
# formula.log <- glue("log_cg_pcp_sexp ~ log_real_cmd_exports_pcp * demstock_one +
# maj + kof_trade_df + dp_ratio_old + v2pariglef_ord + def_prop_gdp") %>%
#   as.formula()
# 
# df.plm <- ws_dataset %>% data_sample(variable = "log_cg_pcp_sexp",
#                            old_dataset = dataset) %>% 
#   pdata.frame(index = c("iso3c", "year"))
# 
# mod.plm.log <- plm(formula.log, data = df.plm, model = "random",
#                effect = "individual")
# 
# mod.plm.log %>%
#   coeftest(., vcov = vcovHC(., type = "HC3", cluster = "group")) %>%
#   tidy()
# 
# mod.plm.log %>% nobs()
# 
# mod.plm.log %>% r.squared(dfcor = T)
# 
# slopes(mod.plm.log, variables = "log_real_cmd_exports_pcp",
#        newdata = datagrid(demstock_one = fivenum),
#        by = F, type = "link",
#        vcov = vcovHC(mod.plm.log, type = "HC3", cluster = "group"))
# 
# plot_slopes(mod.plm.log, variables = "log_real_cmd_exports_pcp",
#             condition = "demstock_one",
#             vcov = vcovHC(mod.plm.log, type = "HC3", cluster = "group")) +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   theme_minimal()
# 
# 
# 
# #
# 
# formula <- glue("cg_pcp_sexp ~ log_real_cmd_exports_pcp * demstock_one +
# maj + kof_trade_df + dp_ratio_old + v2pariglef_ord + def_prop_gdp") %>%
#   as.formula()
# 
# df.plm <- ws_dataset %>% data_sample(variable = "cg_pcp_sexp",
#                                      old_dataset = dataset) %>% 
#   pdata.frame(index = c("iso3c", "year"))
# 
# mod.plm <- plm(formula, data = df.plm, model = "random",
#                effect = "individual")
# 
# mod.plm %>%
#   coeftest(., vcov = vcovHC(., type = "HC3", cluster = "group")) %>%
#   tidy() %>% 
#   p_sign() %>% 
#   mutate(estimate = paste0(round(estimate, digits = 3),
#                            " ", significance,
#                            " (",
#                            round(std.error, digits = 3), ")")) %>%
#   mutate(term = fct_relevel(term, "(Intercept)", "log_real_cmd_exports_pcp",
#                             "demstock_one", 
#                             "log_real_cmd_exports_pcp:demstock_one", "maj", 
#                             "kof_trade_df", "dp_ratio_old",
#                             "v2pariglef_ord", "def_prop_gdp")) %>%
#   arrange(term) %>% 
#   select(estimate)
# 
# mod.plm %>% nobs()
# 
# mod.plm %>% r.squared(dfcor = T)
# 
# slopes(mod.plm, variables = "log_real_cmd_exports_pcp",
#        newdata = datagrid(demstock_one = fivenum),
#        by = F, type = "link",
#        vcov = vcovHC(mod.plm, type = "HC3", cluster = "group"))
# 
# gg.het.plot <- plot_slopes(mod.plm, variables = "log_real_cmd_exports_pcp",
#             condition = "demstock_one",
#             vcov = vcovHC(mod.plm, type = "HC3", cluster = "group")) +
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   theme_minimal() +
#   ylab("Coeficiente") +
#   xlab("Estoque Democrático") +
#   theme(text = element_text(size = 24))
# 
# gg.het.plot
# 
# ggsave("plot/gg.het.plot.jpeg", gg.het.plot,
#        width = 20, height = 12,
#        dpi = 500)

# DISTANCE AS IV
library(ivreg)

ws_dataset %>%
  filter(year < 2020) %>% 
  group_by(iso3c) %>% 
  ggplot(aes(x = dist, y = log_cg_pcp_sexp)) +
  geom_smooth(method = "lm", color = "black", linetype = "dashed",
              linewidth = 0.5, fill = "grey") +
  xlab("Distância euclidiana em relação aos Estados Unidos") +
  ylab("") +
  stat_cor(label.y = Inf, vjust = 1.0) +
  geom_point() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 21))

ws_dataset %>%
  filter(year < 2020 & year > 2018) %>% 
  group_by(iso3c) %>% 
  ggplot(aes(x = dist, y = log_real_cmd_exports_pcp)) +
  geom_smooth(method = "lm", color = "black", linetype = "dashed",
              linewidth = 0.5, fill = "grey") +
  xlab("Distância euclidiana em relação aos Estados Unidos") +
  ylab("") +
  stat_cor(label.y = Inf, vjust = 1.0) +
  geom_point() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 21))

data_iv <- ws_dataset %>% filter(year < 2020)

iv1 <- ivreg(log_cg_pcp_sexp ~ log_real_cmd_exports_pcp +
               maj + kof_trade_df +
               dp_ratio_old + v2pariglef_ord + def_prop_gdp +
               as.character(year) |
               dist + maj + kof_trade_df +
               dp_ratio_old + v2pariglef_ord + def_prop_gdp + as.character(year),
             data = data_iv)

iv1 %>% summary()

coeftest(iv1, vcov = vcovHC, type = "HC1")