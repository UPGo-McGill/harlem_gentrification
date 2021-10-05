#### HARLEM GENTRIFICATION SCRIPT ##############################################

# Attach packages ---------------------------------------------------------

library(tidyverse)
library(sf)
library(tidycensus)
library(tigris)
library(ggmap)
library(ggspatial)


# Import census data ------------------------------------------------------

CT_2019 <- 
  get_acs(
    geography = "tract", 
    variables = c(med_income = "B19013_001",
                  pop_white = "B03002_003",
                  pop_hisp = "B03002_012",
                  pop_black = "B03002_004",
                  foreign_born = "B05002_013",
                  bachelor = "B16010_041"),
    year = 2019, 
    state = "36",
    county = c("New York County",
               "Kings County",
               "Queens County",
               "Bronx County",
               "Richmond County"),
    summary_var = "B01003_001",
    geometry = TRUE) %>% 
  as_tibble() %>%
  st_as_sf() %>% 
  st_transform(32618) %>% 
  rename(pop_total = summary_est) %>% 
  select(-moe, -summary_moe) %>% 
  spread(key = variable, value = estimate) %>% 
  na.omit() %>% 
  mutate(across(c(pop_black, pop_hisp, pop_white), 
                ~{. / pop_total}, .names = "{.col}_pct"),
         .after = pop_white) %>%
  select(-geometry, everything(), geometry)

CT_2009 <- 
  get_acs(
    geography = "tract", 
    variables = c(med_income = "B19013_001",
                  pop_white = "B03002_003",
                  pop_hisp = "B03002_012",
                  pop_black = "B03002_004"),
    year = 2009, 
    state = "36",
    county = c("New York County",
               "Kings County",
               "Queens County",
               "Bronx County",
               "Richmond County"),
    summary_var = "B01003_001",
    geometry = TRUE) %>% 
  as_tibble() %>%
  st_as_sf() %>% 
  st_transform(32618) %>% 
  rename(pop_total = summary_est) %>% 
  select(-moe, -summary_moe) %>% 
  spread(key = variable, value = estimate) %>% 
  na.omit() %>% 
  mutate(across(c(pop_black, pop_hisp, pop_white), 
                ~{. / pop_total}, .names = "{.col}_pct"),
         .after = pop_white) %>%
  select(-geometry, everything(), geometry)


# Get water ---------------------------------------------------------------

water <- 
  bind_rows(area_water("NY", "New York", class = "sf"),
            area_water("NY", "Queens", class = "sf"),
            area_water("NY", "Bronx", class = "sf")) %>% 
  st_transform(32618) %>% 
  st_union()


# Get East Harlem PUMA ----------------------------------------------------

harlem <- 
  pumas(36, class = "sf") %>% 
  st_transform(32618) %>%
  as_tibble() %>% 
  st_as_sf() %>%
  mutate(PUMA_name = NAMELSAD10) %>% 
  select(-GEOID10, -NAMELSAD10, -STATEFP10, -MTFCC10, -FUNCSTAT10, -ALAND10,
         -AWATER10, -INTPTLAT10, -INTPTLON10) %>%
  filter(str_detect(PUMA_name, "East Harlem")) %>%
  st_difference(water) |> 
  st_cast("POLYGON") |> 
  filter(st_area(geometry) == max(st_area(geometry)))


# Get NYC and Harlem parcels ----------------------------------------------

parcel <- 
  # Data available for download at: 
  # https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_21v2_arc_csv.zip
  read_sf("data/nyc_mappluto_21v2_shp/MapPLUTO.shp") |>
  st_transform(32618) |> 
  transmute(id = paste(Borough, Block, Lot, sep = "_"))

parcel_CT_2009 <- 
  parcel |> 
  st_centroid(of_largest_polygon = TRUE) |> 
  st_join(CT_2009) |> 
  st_drop_geometry() |> 
  filter(!is.na(GEOID))

parcel_2009 <- 
  parcel |> 
  inner_join(parcel_CT_2009) |> 
  mutate(year = 2009)

parcel_CT_2019 <- 
  parcel |> 
  st_centroid(of_largest_polygon = TRUE) |> 
  st_join(CT_2019) |> 
  st_drop_geometry() |> 
  filter(!is.na(GEOID))

parcel_2019 <- 
  parcel |> 
  inner_join(parcel_CT_2019) |> 
  mutate(year = 2019)

parcel_census <- bind_rows(parcel_2009, parcel_2019)
  
harlem_parcel <- 
  parcel_census |> 
  st_filter(harlem)

harlem_parcel_id <- 
  harlem_parcel |> 
  st_centroid() |> 
  st_filter(harlem) |> 
  st_drop_geometry() 

harlem_parcel <- 
  harlem_parcel |> 
  semi_join(harlem_parcel_id)

# Get parcels near Harlem for map
parcels_for_map <- 
  parcel_census |> 
  st_filter(st_buffer(harlem, 2000))


# Pivot data --------------------------------------------------------------

parcel_pivot <- 
  harlem_parcel |> 
  st_drop_geometry() |> 
  select(-c(pop_total:pop_white)) |> 
  pivot_longer(c(pop_hisp_pct, pop_black_pct, pop_white_pct), names_to = "race",
               values_to = "value") |> 
  select(id, year, race, value) |> 
  mutate(race = str_remove_all(race, "pop_|_pct"))

harlem_parcel_to_join <- 
  harlem_parcel |>
  distinct(id, .keep_all = TRUE) |> 
  select(id, geometry)
  
parcel_final <- 
  parcel_pivot |> 
  left_join(harlem_parcel_to_join) |> 
  st_as_sf()


# Make maps ---------------------------------------------------------------

basemap <- 
  harlem |> 
  st_transform(4326) |> 
  st_bbox() |> 
  set_names(c("left", "bottom", "right", "top")) |> 
  {\(x) x * c(1.0008, 0.9988, 0.9996, 1.0007)}() |> 
  ggmap::get_map(zoom = 13, source = "google")

figure_1 <-
  basemap |> 
  ggmap() +
  geom_sf(data = st_transform(harlem, 4326), fill = "transparent", lwd = 1.2,
          colour = "red", inherit.aes = FALSE) +
  annotation_scale() +
  theme_void()

ggsave("output/figure_1.png", figure_1, dpi = 300)

figure_2 <- 
  parcel_final |> 
  mutate(race = case_when(
    race == "white" ~ "Non-hispanic white",
    race == "black" ~ "Non-hispanic Black",
    race == "hisp" ~ "Hispanic"
  )) |> 
  ggplot() +
  geom_sf(data = parcels_for_map, colour = "transparent", fill = "grey70") +
  geom_sf(aes(fill = value), colour = "transparent") +
  geom_sf(data = water, colour = "transparent", fill = "black") +
  scale_fill_viridis_b(name = "Population\nshare (%)",
                       labels = scales::percent, n.breaks = 6,
                       ) +
  facet_grid(rows = vars(year), cols = vars(race)) +
  coord_sf(xlim = st_bbox(harlem)[c(1, 3)], ylim = st_bbox(harlem)[c(2, 4)]) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", colour = "transparent"),
    panel.background = element_rect(fill = "white", colour = "transparent"),
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = "transparent"),
    axis.text = element_blank()
  ) +
  theme(legend.position = "right")

ggsave("output/figure_2.pdf", figure_2, width = 9, height = 9.23)


# Paper calculations ------------------------------------------------------

# Get Harlem CTs for calculations
harlem_CT <- 
  harlem_parcel |> 
  pull(GEOID) |> 
  unique()

#' According to the five-year 2019 American Community Survey (which covers the 
#' years 2015-2019), just under half of East Harlem’s residents (45.4%) were 
#' Hispanic, 31.4 percent were non-Hispanic Black and 13.8 percent were 
#' non-Hispanic white. 

CT_2019 |> 
  filter(GEOID %in% harlem_CT) |> 
  st_drop_geometry() |> 
  summarize(across(c(pop_hisp, pop_black, pop_white), 
                   ~{sum(.x) / sum(pop_total)}))

#' This is a substantially higher share of Hispanic residents than New York City 
#' as a whole (29.1%), and a substantially lower share of non-Hispanic white 
#' residents (32.1%). 

CT_2019 |> 
  st_drop_geometry() |> 
  summarize(across(c(pop_hisp, pop_black, pop_white), 
                   ~{sum(.x) / sum(pop_total)}))

#' 22.5 percent of residents in East Harlem were foreign born (compared to 36.9 
#' percent for New York City), and 22.2 percent of East Harlem residents 25 and 
#' older had a bachelor’s degree or higher (compared to 26.9 percent for New 
#' York City).
#' 
CT_2019 |> 
  filter(GEOID %in% harlem_CT) |> 
  st_drop_geometry() |> 
  summarize(across(c(foreign_born, bachelor), 
                   ~{sum(.x) / sum(pop_total)}))

CT_2019 |> 
  st_drop_geometry() |> 
  summarize(across(c(foreign_born, bachelor), 
                   ~{sum(.x) / sum(pop_total)}))

#' The percentage of Hispanic residents in East Harlem (shown in the left panels 
#' of Figure 2) saw the most dramatic decrease from 2009 to 2019, from 49.7 
#' percent to 45.4 percent.... The center panels of Figure 2 shows the 
#' percentage of Black residents, which overall remained stable, although with a 
#' modest redistribution away from the north-western edge of East Harlem toward 
#' the southern border with the Upper West Side. The white population (the right 
#' panels of Figure 2) increased from 12.3 percent to 13.8 percent across the 
#' entire neighborhood. 

CT_2009 |> 
  filter(GEOID %in% harlem_CT) |> 
  st_drop_geometry() |> 
  summarize(across(c(pop_hisp, pop_black, pop_white), 
                   ~{sum(.x) / sum(pop_total)}))

#' Shifts in median household income match the shifts in the white resident 
#' population, with incomes increasing overall (from $31,842 in 2009 to $37,464 
#' in 2019) with the exception of a few areas in central East Harlem. 

CT_2019 |> 
  filter(GEOID %in% harlem_CT) |> 
  st_drop_geometry() |> 
  summarize(med_income = sum(med_income * pop_total) / sum(pop_total))

CT_2009 |> 
  filter(GEOID %in% harlem_CT) |> 
  st_drop_geometry() |> 
  summarize(med_income = sum(med_income * pop_total) / sum(pop_total))

#' In 2015 the number of units within the rent cap range (between $2,500 and 
#' $2,999) comprised 2.15% of all rental units in East Harlem according to 2015 
#' ACS data (which includes data from the years 2011-2015). In 2019, units 
#' within that rent range had increased by 50% to 3.30% of all rental units 
#' according to 2019 ACS data (which includes data from the years 2015-2019).

# Get rent data
rent_vars <- c(rent_0_100 = "B25056_003", rent_100_149 = "B25056_004",
               rent_150_199 = "B25056_005", rent_200_249 = "B25056_006",
               rent_250_299 = "B25056_007", rent_300_349 = "B25056_008",
               rent_350_399 = "B25056_009", rent_400_449 = "B25056_010",
               rent_450_499 = "B25056_011", rent_500_549 = "B25056_012",
               rent_550_599 = "B25056_013", rent_600_649 = "B25056_014",
               rent_650_699 = "B25056_015", rent_700_749 = "B25056_016",
               rent_750_799 = "B25056_017", rent_800_899 = "B25056_018",
               rent_900_999 = "B25056_019", rent_1000_1249 = "B25056_020",
               rent_1250_1499 = "B25056_021", rent_1500_1999 = "B25056_022",
               rent_2000_2499 = "B25056_023", rent_2500_2999 = "B25056_024",
               rent_3000_3499 = "B25056_025", rent_3500_up = "B25056_026",
               rent_total_units = "B25056_002")

rents_2019 <- 
  get_acs(
    geography = "tract", 
    variables = rent_vars,
    year = 2019, 
    state = "36",
    county = c("New York County",
               "Kings County",
               "Queens County",
               "Bronx County",
               "Richmond County"),
    summary_var = "B25063_002",
    geometry = FALSE) %>% 
  as_tibble() %>%
  rename(units_total = summary_est) %>% 
  select(-moe, -summary_moe, -units_total) %>% 
  spread(key = variable, value = estimate) %>% 
  mutate(rent_under_2500 = 
           rent_0_100 + rent_100_149 + rent_150_199 + rent_200_249 + 
           rent_250_299 + rent_300_349 + rent_350_399 + rent_400_449 +
           rent_450_499 + rent_500_549 + rent_550_599 + rent_600_649 +
           rent_650_699 + rent_700_749 + rent_750_799 + rent_800_899 +
           rent_900_999 + rent_1000_1249 + rent_1250_1499 + rent_1500_1999 +
           rent_2000_2499,
         rent_over_3000 = rent_3000_3499 + rent_3500_up,
         pct_under_2500 = rent_under_2500 / rent_total_units,
         pct_over_3000 = rent_over_3000 / rent_total_units,
         pct_2500_2999 = rent_2500_2999 / rent_total_units) %>% 
  na.omit() %>%
  filter(GEOID %in% harlem_CT)

rents_2015 <- 
  get_acs(
    geography = "tract", 
    variables = rent_vars,
    year = 2015, 
    state = "36",
    county = c("New York County",
               "Kings County",
               "Queens County",
               "Bronx County",
               "Richmond County"),
    summary_var = "B25063_002",
    geometry = FALSE) %>% 
  as_tibble() %>%
  rename(units_total = summary_est) %>% 
  select(-moe, -summary_moe, -units_total) %>% 
  spread(key = variable, value = estimate) %>% 
  mutate(rent_under_2500 = 
           rent_0_100 + rent_100_149 + rent_150_199 + rent_200_249 + 
           rent_250_299 + rent_300_349 + rent_350_399 + rent_400_449 +
           rent_450_499 + rent_500_549 + rent_550_599 + rent_600_649 +
           rent_650_699 + rent_700_749 + rent_750_799 + rent_800_899 +
           rent_900_999 + rent_1000_1249 + rent_1250_1499 + rent_1500_1999 +
           rent_2000_2499,
         rent_over_3000 = rent_3000_3499 + rent_3500_up,
         pct_under_2500 = rent_under_2500 / rent_total_units,
         pct_over_3000 = rent_over_3000 / rent_total_units,
         pct_2500_2999 = rent_2500_2999 / rent_total_units) %>% 
  na.omit() %>%
  filter(GEOID %in% harlem_CT)

sum(rents_2015$rent_2500_2999) / sum(rents_2015$rent_total_units) # 2.15%
sum(rents_2019$rent_2500_2999) / sum(rents_2019$rent_total_units) # 3.30%
