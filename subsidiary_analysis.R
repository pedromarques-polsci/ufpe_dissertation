# Packages -----------------------------------------------------------
library(countrycode)
library(ggrepel)
library(janitor)
library(readxl)
library(tidyverse)
library(zoo)

# SOCX ANALYSIS -----------------------------------------------------------
oecd <- read_csv2("raw_data/oecd_membership.csv") %>% 
  mutate(iso3c = countrycode(sourcevar = country,
                             origin = "country.name",
                             destination = "iso3c"),
         membership = 1,
         year_proxy = ifelse(year < 1980, 1980, year))

socx <- read_csv("raw_data/socx.csv") %>% 
  clean_names() %>% 
  select(ref_area, reference_area, unit_measure, time_period, obs_value) %>% 
  mutate(unit_measure = case_when(unit_measure == "PT_B1GQ" ~ "gdp_pct",
                                  unit_measure == "USD_PPP_PS" ~ "usd_ppp")) %>% 
  pivot_wider(names_from = unit_measure,
              values_from = obs_value) %>% 
  clean_names() %>% 
  arrange(reference_area, time_period) %>% 
  mutate(
    region23 = countrycode(
    sourcevar = ref_area,
    origin = "iso3c",
    destination = "region23"),
    
    region = case_when(ref_area %in% c("CHL", "COL", "CRI", "MEX") ~ "América Latina",
                       grepl("Asia", region23) ~ "Asia",
                       ref_area %in% c("USA", "CAN", "AUS", "NZL") ~ "Anglopacífico",
                       .default = region23),
    region = case_when(region == "Eastern Europe" ~ "Leste Europeu",
                       region == "Northern Europe" ~ "Europa Setentrional",
                       region == "Southern Europe" ~ "Europa Meridional",
                       region == "Western Europe" ~ "Europa Ocidental",
                       region == "Asia" ~ "Ásia",
                       .default = region)
  ) %>% 
  mutate(decade = paste0(substr(time_period, 1, 3), 0),
         year_last_digit = substr(time_period, 4, 4))

socx_df <- socx %>% 
  left_join(oecd %>% select(iso3c, year_proxy, membership), 
            join_by(ref_area == iso3c, time_period == year_proxy)) %>% 
  arrange(ref_area, time_period) %>% 
  group_by(ref_area) %>% 
  fill(membership, .direction = "down") %>% 
  mutate(membership = ifelse(is.na(membership), 0, membership),
         status_change = membership - dplyr::lag(membership, 1),
         asc = ifelse(status_change == 1, ref_area, NA))

socx_asc_txt <- socx_df %>% 
  filter(!is.na(asc)) %>% 
  group_by(time_period) %>% 
  reframe(asc_time = toString(asc))

socx_ts_old <- socx_df %>% 
  group_by(ref_area) %>% 
  mutate(new = mean(membership)) %>% 
  ungroup() %>% 
  filter(time_period < 2020, new == 1) %>% 
  group_by(time_period) %>% 
  reframe(gdp_pct_old = mean(gdp_pct, na.rm = T))

socx_ts <- socx_df %>% 
  filter(time_period < 2020) %>% 
  group_by(time_period) %>% 
  mutate(gdp_pct = ifelse(membership == 0, NA, gdp_pct)) %>% 
  reframe(public_gdp_pct = mean(gdp_pct, na.rm = T)) %>% 
  left_join(socx_asc_txt) %>% 
  left_join(socx_ts_old) %>% 
  pivot_longer(cols = c(gdp_pct_old, public_gdp_pct),
               names_to = "sample",
               values_to = "value")

shade <- data.frame(x1=c(2007, 1990), x2=c(2009, 1993), 
                    y1=c(15.5, 15.5), y2=c(23.2, 23.2))

gg_socx_gdp <- socx_ts %>% 
  ggplot(aes(x = time_period, y = value, 
             linetype = sample, group = sample)) +
  geom_rect(data = shade,
            mapping=aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
            inherit.aes = FALSE,
            color=NA, alpha=0.5, fill = "grey") +
  geom_line(linewidth = 1) +
  geom_text_repel(aes(label = ifelse(sample == "public_gdp_pct", 
                                     asc_time, NA)),
                  vjust = 3, size = 6.5) +
  geom_point() +
  # geom_smooth(method = "lm", color = "black", linetype = "dashed",
  #             linewidth = 0.5, fill = "lightblue") +
  scale_linetype_manual(labels = c("Membros de 1980", "Todos os membros"),
                        values = c("twodash", "solid")) +
  labs(x = "Ano", y = "Média de gasto social (% PIB)", linetype = "Amostra") +
  theme_minimal() +
  theme(text = element_text(size = 30),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  scale_x_continuous(breaks =  seq(1980, 2019, by = 2))

gg_socx_gdp

socx_decade <- socx_df %>% 
  group_by(decade, ref_area) %>% 
  reframe(public_gdp_pct = gdp_pct[which(year_last_digit==9)] -
            gdp_pct[which(year_last_digit==0)],
          region = first(region))

gg_socx_xx <- socx_decade %>%
  pivot_wider(names_from = decade, values_from = public_gdp_pct, 
              names_prefix = "d") %>% 
  filter(grepl("Europ", region),
         !is.na(d1980), !is.na(d1990)) %>% 
  ggplot(aes(x = reorder(ref_area, d1980))) +
  geom_bar(aes(y = d1980, fill = "1980"), 
           stat = "identity") +
  geom_bar(aes(y = d1990), 
           fill = NA, colour = "black", linetype = "dashed",
           stat = "identity", ) +
  geom_point(aes(y = d1990, shape = "1990"), size = 7,
            colour = "darkblue") +
  scale_fill_manual(name = "Década", 
                    values = c("1980" = "#00A9E0")) +
    scale_shape_manual(name = "", 
                       values = c("1990" = 18)) +
  coord_flip() +
  theme_minimal() +
  xlab("País") + ylab("Variação absoluta (p.p)") +
  theme(text = element_text(size = 21),
        legend.position = "bottom")

gg_socx_xx

socx_long_term <- socx_df %>% 
  group_by(ref_area) %>% 
  reframe(public_gdp_pct = gdp_pct[which(time_period==2019)] -
            gdp_pct[which(time_period==2000)],
          region = first(region)) %>% 
  ungroup()

gg_long_term <- socx_long_term %>% 
  filter(grepl("Europ", region)) %>% 
  ggplot(aes(x = reorder(ref_area, public_gdp_pct), y = public_gdp_pct)) +
  geom_bar(stat = "identity", fill = "#0088CC") +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
  geom_text(aes(label = paste0(round(public_gdp_pct, 1))),
            hjust = 1.3, size = 6.5) +
  theme_minimal() +
  xlab("País") + ylab("Variação absoluta (p.p)") +
  theme(text = element_text(size = 21)) +
  coord_flip()

gg_long_term

gg_socx_xxi <- socx_decade %>%
  filter(decade == 2010) %>% 
  group_by(region) %>% 
  reframe(public_gdp_pct = mean(public_gdp_pct, na.rm = T)) %>% 
  ggplot(aes(x = reorder(region, public_gdp_pct), y = public_gdp_pct)) +
  geom_bar(stat = "identity", fill = "#0088CC") +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
  theme_minimal() +
  xlab("Região") + ylab("Média de variação absoluta") +
  theme(text = element_text(size = 21))

gg_socx_xxi

ggsave("plot/gg_socx_gdp.jpeg", gg_socx_gdp, 
       width = 18, height = 10, 
       dpi = 500)

ggsave("plot/gg_socx_xx.jpeg", gg_socx_xx, 
       width = 15, height = 8, 
       dpi = 500)

ggsave("plot/gg_socx_xxi.jpeg", gg_socx_xxi, 
       width = 18, height = 10, 
       dpi = 500)

ggsave("plot/gg_long_term.jpeg", gg_long_term, 
       width = 15, height = 8, 
       dpi = 500)

# CMD PRICES --------------------------------------------------------------
cmd_prices <- read_xlsx("raw_data/cmd_prices.xlsx") %>% 
  clean_names() %>% 
  slice(-1,-2,-3) %>% 
  rename(date = commodity) %>% 
  mutate(date = as.yearmon(date, "%YM%m"),
         date = as.Date(date),
         year = year(date),
         across(pallfnf:papple, ~as.numeric(.x)))

cmd_prices %>% 
  filter(year > 2002) %>% 
  ggplot(aes(x = date, y = pallfnf)) +
  geom_line()

shade_boom <- data.frame(x1=c(as.Date("2003-01-01")), 
                         x2=c(as.Date("2013-12-01")), 
                         y1=c(30), 
                         y2=c(310))

gg_cmd_prices <- cmd_prices %>% 
  select(date, year, pfandb, prawm, pallmeta, pnrg) %>% 
  filter(year < 2020) %>% 
  pivot_longer(cols = pfandb:pnrg, names_to = "commodity",
               values_to = "price") %>%
  mutate(commodity = case_when(commodity == "pfandb" ~ 
                                 "Alimentos e Bebidas",
                               commodity == "prawm" ~ 
                                 "Materiais Agrícolas Brutos",
                               commodity == "pallmeta" ~
                                 "Metais",
                               commodity == "pnrg" ~
                                 "Combustíveis"
  )) %>% 
  ggplot(aes(x = date, y = price, group = commodity)) +
  geom_rect(data = shade_boom,
            mapping=aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
            inherit.aes = FALSE,
            color=NA, alpha=0.45, fill = "grey80") +
  geom_line(aes()) +
  theme_minimal() +
  facet_wrap(~commodity) +
  xlab("Mês") + ylab("Índice de Preços (2016 = 100)") +
  scale_x_date(date_labels = "%Y", breaks = "2 year") +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1))

gg_cmd_prices

ggsave("plot/gg_cmd_prices.jpeg", gg_cmd_prices, 
       width = 15, height = 8, 
       dpi = 300)
