## Get slivers that were divided by st_erase(nyc_water) and those that remained intact

CTs_without_slivers_2000 <- 
  CTs_2000[which(CTs_2000$geometry %>% 
                   st_geometry_type() == "POLYGON"),]

CTs_touching_water_2000 <- 
  CTs_2000[which(CTs_2000$geometry %>% st_geometry_type() == "MULTIPOLYGON"),] %>% 
  st_cast("POLYGON")

slivers_2000 <-
  CTs_touching_water_2000 %>% 
  group_by(GEOID) %>% 
  filter(st_area(geometry) != max(st_area(geometry))) %>% 
  ungroup() %>%
  mutate(sliver_ID = 1:191)

CTs_water_big_2000 <-
  CTs_touching_water_2000 %>% 
  filter(!geometry %in% slivers_2000$geometry)

CTs_main_2000 <- 
  rbind(CTs_without_slivers_2000, CTs_water_big_2000)


## Intersect CTs_main_2000 with slivers_2000 to get the GEOID of the sliver that shares most border with CT

intersects_2000 <- 
  st_intersection(CTs_main_2000, slivers_2000) %>% 
  select(poly_ID = GEOID, sliver_ID, geometry) %>% 
  filter(!(st_geometry_type(.) == "POINT")) %>% 
  group_by(sliver_ID) %>% 
  filter(st_length(geometry) == max(st_length(geometry))) %>% 
  ungroup() %>% 
  arrange(poly_ID)

## Select GEOIDs of all the CTs_2000 of where the slivers will be re-assigned

intersect_polys_2000 <-
  intersects_2000 %>% 
  st_drop_geometry() %>% 
  left_join(select(slivers_2000, -GEOID)) %>% 
  st_as_sf() %>% 
  group_by(poly_ID) %>% 
  summarize()

CTs_fixed_2000 <- 
  left_join(CTs_main_2000,
          as_tibble(rename(intersect_polys_2000, geom = geometry)),
          by = c("GEOID" = "poly_ID"))

CTs_fixed_2000 <-
  CTs_fixed_2000 %>% 
  filter(st_area(geom) != set_units(0, m^2)) %>% 
  group_by(GEOID) %>% 
  mutate(geometry = st_union(geometry, geom)) %>% 
  ungroup() %>% 
  rbind(CTs_fixed_2000 %>% 
          filter(st_area(geom) == set_units(0, m^2))) %>% 
  select(-geom) %>% 
  as_tibble() %>% 
  st_as_sf()

