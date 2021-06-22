### DATA IMPORT ################################################################

# Load libraries and helper functions -------------------------------------

source("R/01_helper_functions.R")


# Import census geographies -----------------------------------------------

# Import water
nyc_water <- rbind(
  area_water("NY", "New York", class = "sf"),
  area_water("NY", "Queens", class = "sf"),
  area_water("NY", "Bronx", class = "sf")) %>% 
  st_transform(26918) %>% 
  st_union()

# Import nyc_city and county boundaries
nyc_city <- suppressWarnings(
  counties(state = "New York", class = "sf") %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_transform(26918) %>% 
  filter(NAME %in% c("New York", "Queens", "Bronx")) %>% 
  st_erase(nyc_water) %>%
      st_transform(26918) %>% 
      st_union())


# Import census data ------------------------------------------------------

### 2018 ACS

CTs_2018 <- 
  get_acs(
  geography = "tract", 
  variables = c(med_income = "B19013_001",
                pop_white = "B03002_003",
                pop_hisp = "B03002_012",
                pop_black = "B03002_004",
                immigrant = "B05001_006"),
  year = 2018, 
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
  st_transform(26918) %>% 
  rename(pop_total = summary_est) %>% 
  select(-moe, -summary_moe) %>% 
  spread(key = variable, value = estimate) %>% 
  mutate(pop_density = pop_total/st_area(geometry)) %>% 
  # filter(pop_total > 100) %>%
  na.omit() %>% 
  select(-geometry, everything(), geometry)

### 2000 decennial

CTs_2000 <- get_decennial(
  geography = "tract", 
  variables = c(#med_income = "P053001",
                pop_white = "P007003",
                pop_hisp = "P007010",
                pop_black = "P007004",
                immigrant = "P021015"),
  year = 2000, 
  state = "36",
  county = c("New York County",
             "Kings County",
             "Queens County",
             "Bronx County",
             "Richmond County"),
  summary_var = "P001001",
  geometry = TRUE) %>% 
  st_transform(26918) %>% 
  rename(estimate = value, pop_total = summary_value) %>% 
  spread(key = variable, value = estimate) %>% 
  mutate(pop_density = pop_total/st_area(geometry)) %>% 
  # filter(pop_total > 100) %>%
  na.omit() %>% 
  select(-geometry, everything(), geometry)

# Add percentage variables

CTs_2015 <- 
  CTs_2015 %>% 
  mutate_at(c("pop_white", "immigrant", "pop_black", "pop_hisp"),
            list(pct = ~. / CTs_2015$pop_total))

CTs_2000 <- 
  CTs_2000 %>% 
  mutate_at(c("pop_white", "immigrant", "pop_black", "pop_hisp"),
            list(pct = ~. / CTs_2000$pop_total)) # %>% 
#  filter(GEOID != "36061024000")


## Clip data to water

CTs_2000 <- st_erase(CTs_2000, nyc_water)
CTs_2015 <- st_erase(CTs_2015, nyc_water)

# Get East Harlem bounding box

nyc_pumas <- 
  pumas(36, class = "sf") %>% 
  st_transform(26918) %>%
  as_tibble() %>% 
  st_as_sf() %>%
  mutate(PUMA_name = NAMELSAD10) %>% 
  select(-GEOID10, -NAMELSAD10, -STATEFP10, -MTFCC10, -FUNCSTAT10, -ALAND10,
         -AWATER10, -INTPTLAT10, -INTPTLON10) %>%
  filter(str_detect(PUMA_name, "East Harlem")) %>% 
  st_erase(nyc_water)

harlem <- nyc_pumas %>% 
  filter(str_detect(PUMA_name, "East Harlem")) %>% 
  st_erase(nyc_water) %>% 
  st_cast("POLYGON") 
 
harlem <- harlem[1,]

plot(harlem)
