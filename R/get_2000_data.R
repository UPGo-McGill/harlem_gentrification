med_hh_inc <- read.csv("~/Downloads/2000 Census Data/med_hh_inc_2000.csv", header=FALSE) 

v17 <- load_variables(2000, "sf1", cache = TRUE)

Sys.setenv(CENSUS_KEY = "f10d995b6180d782ebbd4f685c2191f815b9b1f1")
Sys.getenv()

census_api_key("f10d995b6180d782ebbd4f685c2191f815b9b1f1")

## 1. Get 2000 census data

CTs_2000 <- st_read("data/DECENNIALDPSF32000.DP3_2020-11-26T101806/DECENNIALDPSF32000.DP3_data_with_overlays_2020-11-26T101603.csv") 

## Make GEOIDs match tidycensus data

CTs_GEOID <- str_remove_all(CTs_2000$GEO_ID, "1400000US")

CTs_2000 <- 
  CTs_2000 %>% 
  mutate(GEO_ID = CTs_GEOID)  
  #rename(GEOID = GEO_ID)

## Get 2000 CT geometries

CTs_shp_2000 <- tracts(
  state = "36",
  county = c("New York County",
             "Kings County",
             "Queens County",
             "Bronx County",
             "Richmond County"),
  year = 2000
  ) %>% 
  st_transform(26918) %>% 
  rename(GEOID = CTIDFP00)



white_2000 <- get_decennial(
  geography = "tract", 
  variables = (
  #med_income = "P053001",
  pop_white = "P007003"),
  #pop_hisp = "P007010"),
  #pop_black = "P007004"),
  #immigrant = "P021015"),
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
  filter(pop_total > 100) %>%
  na.omit() %>% 
  rename(pop_white = P007003) %>% 
  st_drop_geometry()
  # select(-geometry, everything(), geometry)

plot(CTs_geometry_2000$geometry)

## 2. Join to spatial CTs

CTs_2000_joined <- 
  left_join(CTs_shp_2000, CTs_2000, 
            by = c("GEOID" = "GEO_ID")) #%>% 
  
CTs_2000_joined <- 
  CTs_2000_joined %>% 
  left_join(white_2000) %>% 
  rename(MHI = DP3_C112)

CTs_2000_joined <- CTs_2000_joined %>% 
  select(GEOID, NAME, pop_total, pop_white, MHI, geometry)
  #rename(pop_white = `value`) %>% 
  # %>% 

# CTs_2000_left <- 
#   left_join(CTs_geometry_2000, CTs_2000) %>% 
#   #rename(pop_white = `value`) %>% 
#   select(GEOID, GEO_ID, NAME, pop_total, pop_white, DP3_C112, geometry) # %>% 
# 
# CTs_2000_left <- 
#   st_join(CTs_shp_2000, CTs_2000_left) %>% 

plot(CTs_2000_left$geometry)

CTs_2000_joined <- CTs_2000_joined %>% 
  select(CTIDFP00, NAME00, )

## East Harlem Bounding box

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

## 3. make maps

CTs_2000_joined %>% 
  
  ggplot() +
  geom_sf(aes(fill=MHI) +
  # scale_fill_gradientn(colors = c("#9DBF9E", "#FCB97D", "#A84268"),
  #                      na.value = "grey80") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "gray"),
    panel.grid.major = element_line(colour = "gray")) +
  gg_bbox(harlem) 


?geom_sf
## 4. make 2018 maps

## pop_white

CTs_2018 %>% 
  ggplot() +
  geom_sf(aes(fill=pop_white)) +
  #scale_fill_gradientn(colors = c("#9DBF9E", "#FCB97D", "#A84268"),
                       #na.value = "grey80") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "gray"),
    panel.grid.major = element_line(colour = "gray")) +
  gg_bbox(harlem) 

## pop_black

CTs_2018 %>% 
  ggplot() +
  geom_sf(aes(fill=pop_black)) +
  scale_fill_gradientn(colors = c("#9DBF9E", "#FCB97D", "#A84268"),
                       na.value = "grey80") +
  #scale_x_continuous(breaks = seq(0, 20000, by = 5000)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "gray"),
    panel.grid.major = element_line(colour = "gray")) +
  gg_bbox(harlem) 

## pop_hisp

CTs_2018 %>% 
  ggplot() +
  geom_sf(aes(fill=pop_hisp)) +
  scale_fill_gradientn(colors = c("#9DBF9E", "#FCB97D", "#A84268"),
                       na.value = "grey80") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "gray"),
    panel.grid.major = element_line(colour = "gray")) +
  gg_bbox(harlem) 

## med_income

CTs_2018 %>% 
  ggplot() +
  geom_sf(aes(fill= med_income)) +
  scale_fill_gradientn(colors = c("#9DBF9E", "#FCB97D", "#A84268"),
                       na.value = "grey80") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "gray"),
    panel.grid.major = element_line(colour = "gray")) +
  gg_bbox(harlem) 


