### DATA IMPORT ################################################################

# Load libraries and helper functions -------------------------------------

source("R/01_helper_functions.R")
library(tidyverse)
install.packages("tidyverse")

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

tigris::w

# Import census data ------------------------------------------------------

### 2019 ACS

CTs_2019 <- 
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
  st_transform(26918) %>% 
  rename(pop_total = summary_est) %>% 
  select(-moe, -summary_moe) %>% 
  spread(key = variable, value = estimate) %>% 
  mutate(pop_density = pop_total/st_area(geometry)) %>% 
  # filter(pop_total > 100) %>%
  na.omit() %>% 
  select(-geometry, everything(), geometry)

### 2009 ACS

CTs_2009 <- 
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
  st_transform(26918) %>% 
  rename(pop_total = summary_est) %>% 
  select(-moe, -summary_moe) %>% 
  spread(key = variable, value = estimate) %>% 
  mutate(pop_density = pop_total/st_area(geometry)) %>% 
  # filter(pop_total > 100) %>%
  na.omit() %>% 
  select(-geometry, everything(), geometry)


# Add percentage variables

CTs_2009 <- 
  CTs_2009 %>% 
  mutate_at(c("pop_white", "immigrant", "pop_black", "pop_hisp"),
            list(pct = ~. / CTs_2009$pop_total))

CTs_2019 <- 
  CTs_2019 %>% 
  mutate_at(c("pop_white", "immigrant", "pop_black", "pop_hisp"),
            list(pct = ~. / CTs_2019$pop_total)) # %>% 


#  filter(GEOID != "36061024000")


## Clip data to water

# CTs_2009 <- st_erase(CTs_2009, nyc_water)
# CTs_2019 <- st_erase(CTs_2019, nyc_water)

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
 
harlem_


harlem <- harlem[1,]

plot(harlem)

# Add year to data, then combine 2000 and 2015

CTs_2019 <- 
  CTs_2019 %>% 
  mutate("Year" = 2019)

CTs_2009 <- 
  CTs_2009 %>% 
  mutate("Year" = 2009)

CTs_2009_2019 <- 
  rbind(CTs_2009, CTs_2019)

# Get East Harlem CTs

CT_harlem <- 
  CTs_2009_2019 %>% 
  st_centroid() %>% 
  st_filter(harlem) #%>% 
  st_drop_geometry() %>% 
  select(GEOID)


ggplot() + 
  # st_as_sf() %>% 
  geom_sf(data = CTs_2009_2019, fill = "blue", colour = "white", 
          lwd = 0.05) +
  facet_wrap(~ Year) +
  geom_sf(data = CT_harlem, fill = "red", color = "black")+
  theme_void() +
  coord_sf(xlim = st_bbox(harlem) [c(1,3)] * c(0.9998, 1.00039), 
           ylim = st_bbox(harlem) [c(2,4)] * c(0.999999, 1.0001))

plot(harlem)


