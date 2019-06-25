## Get slivers that were divided by st_erase(nyc_water) and those that remained intact

CTs_without_slivers_2000 <- 
  CTs_2000[which(CTs_2000$geometry %>% st_geometry_type() == "POLYGON"),] %>% 
  select(GEOID, geometry)

CTs_touching_water_2000 <- 
  CTs_2000[which(CTs_2000$geometry %>% st_geometry_type() == "MULTIPOLYGON"),] %>% 
  st_cast("POLYGON") %>% 
  select(GEOID, geometry)

slivers_2000 <- 
  CTs_touching_water_2000 %>% 
  mutate(area = st_area(.)) %>% 
  group_by(GEOID) %>% 
  filter(area != max(area)) %>% 
  select(-area)

CTs_water_big_2000 <- 
  CTs_touching_water_2000 %>% 
  mutate(area = st_area(.)) %>% 
  group_by(GEOID) %>% 
  filter(area == max(area)) %>% 
  select(-area)

CTs_main_2000 <- 
  rbind(CTs_without_slivers_2000, CTs_water_big_2000)

base_map +
  tm_shape(CTs_main_2000) +
  tm_fill(col = "grey")

base_map +
  tm_shape(slivers_2000) +
  tm_fill(col = "red")


## Test the st_intersection on two CTs next to each other
CTs_2000 %>% 
  filter(GEOID %in% c(36061019400, 36061019600)) %>% 
  st_intersection() %>% st_collection_extract("LINESTRING") %>% plot()


## Intersect CTs_main_2000 with slivers_2000 to get the GEOID of the sliver that shares most border with CT

intersects_2000 <- 
  st_intersection(CTs_main_2000, slivers_2000) %>% 
  select(GEOID, GEOID.1, geometry) %>% 
  filter(!(st_geometry_type(.) == "POINT")) %>% 
  mutate(length = st_length(.)) %>% 
  rename(poly_ID = GEOID, sliver_ID = GEOID.1) %>% 
  group_by(sliver_ID) %>% 
  filter(length == max(length)) %>% 
  ungroup() %>% 
  arrange(poly_ID)

## Select GEOIDs of all the CTs_2000 of where the slivers will be re-assigned

CTs_fixed_2000 <- 
  CTs_2000 %>% filter(GEOID %in% intersects_2000$poly_ID) %>% 
  arrange(GEOID)



intersects_2000 %>% 
  st_drop_geometry() %>% 
  group_by(poly_ID)

slivers_2000 %>% 
  filter(GEOID == intersects_2000[1,]$sliver_ID)

map2(CTs_fixed_2000$geometry, y, st_union)

st_union(slivers, CTs_without_slivers_2000)



base_map +
  tm_shape(slivers_2000) +
  tm_fill(col = "blue") +
  tm_shape(CTs_fixed_2000) +
  tm_fill(col = "grey")


