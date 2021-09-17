#### FINAL MAPPING SCRIPT ######################################################


# Attach packages ---------------------------------------------------------

library(tidyverse)
library(sf)
library(tidycensus)
library(tigris)


# Import census data ------------------------------------------------------

CT_2019 <- 
  get_acs(
    geography = "tract", 
    variables = c(med_income = "B19013_001",
                  pop_white = "B03002_003",
                  pop_hisp = "B03002_012",
                  pop_black = "B03002_004",
                  immigrant = "B05001_006"),
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
  mutate(pop_density = pop_total/st_area(geometry)) %>% 
  # filter(pop_total > 100) %>%
  na.omit() %>% 
  select(-geometry, everything(), geometry) %>% 
  mutate_at(c("pop_white", "immigrant", "pop_black", "pop_hisp"),
            list(pct = ~. / pop_total))

CT_2009 <- 
  get_acs(
    geography = "tract", 
    variables = c(med_income = "B19013_001",
                  pop_white = "B03002_003",
                  pop_hisp = "B03002_012",
                  pop_black = "B03002_004",
                  immigrant = "B05001_006"),
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
  mutate(pop_density = pop_total/st_area(geometry)) %>% 
  # filter(pop_total > 100) %>%
  na.omit() %>% 
  select(-geometry, everything(), geometry) %>% 
  mutate_at(c("pop_white", "immigrant", "pop_black", "pop_hisp"),
            list(pct = ~. / pop_total))


# Get water ---------------------------------------------------------------

water <- rbind(
  area_water("NY", "New York", class = "sf"),
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


# Get NYC parcels ---------------------------------------------------------

parcel <- 
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


# Get parcels near Harlem -------------------------------------------------

parcels_for_map <- 
  parcel_census |> 
  st_filter(st_buffer(harlem, 2000))


# Pivot data --------------------------------------------------------------

parcel_pivot <- 
  harlem_parcel |> 
  st_drop_geometry() |> 
  select(-c(pop_total:pop_density)) |> 
  pivot_longer(c(pop_white_pct, pop_black_pct, pop_hisp_pct), names_to = "race",
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

# Race
parcel_final |> 
  ggplot() +
  geom_sf(data = parcels_for_map, colour = "transparent", fill = "grey70") +
  geom_sf(aes(fill = value), colour = "transparent") +
  geom_sf(data = water, colour = "transparent", fill = "black") +
  scale_fill_viridis_b(name = "White population (%)",
                       labels = scales::percent, n.breaks = 6,
                       # limits = c(0, .4), oob = scales::squish
                       ) +
  coord_sf(xlim = st_bbox(harlem)[c(1, 3)], ylim = st_bbox(harlem)[c(2, 4)]) +
  facet_grid(rows = vars(year), cols = vars(race)) +
  theme_void() +
  theme(legend.position = "right")

# Median income
harlem_parcel |> 
  ggplot() +
  geom_sf(data = parcels_for_map, colour = "transparent", fill = "grey70") +
  geom_sf(aes(fill = med_income), colour = "transparent") +
  geom_sf(data = water, colour = "transparent", fill = "black") +
  scale_fill_viridis_b(name = "Median household income",
                       labels = scales::dollar, n.breaks = 7,
                       # limits = c(0, .4), oob = scales::squish
                       ) +
  coord_sf(xlim = st_bbox(harlem)[c(1, 3)], ylim = st_bbox(harlem)[c(2, 4)]) +
  facet_wrap(~year) +
  theme_void() +
  theme(legend.position = "right")



parcel_census |> 
  st_drop_geometry()





mapview::mapview(parcels_for_map)