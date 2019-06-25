
## SEPARATE EXTRA NYC_CITY POLYGONS TO ASSIGN TO CTS

CTs_2000_union <- st_union(CTs_2000) %>% 
  st_collection_extract("POLYGON")

slivers2000 <- st_erase(nyc_city, CTs_2000_union) %>% 
  st_cast("POLYGON") %>% 
  as_tibble() %>% 
  mutate(NAME = c(1:408)) %>% 
  st_as_sf()

slivers2000 <- mutate(slivers2000, NAME = as.character(NAME)) 

## ATTACH SLIVERS TO THEIR NEW CTs

# which(CTs_2000$GEOID == 36005001700)  ## Example of how to search


which(CTs_2000$GEOID == 36061019200) # 196
which(CTs_2000$GEOID == 36061017800) # 182
which(CTs_2000$GEOID == 36061016200) # 165

CTs_2000[196,]$geometry <- st_union(CTs_2000[196,]$geometry, slivers2000[377,]$geometry)
CTs_2000[165,]$geometry <-  st_union(CTs_2000[165,]$geometry, slivers2000[384,]$geometry) %>% 
  st_union(CTs_2000[165,]$geometry, slivers2000[380,]$geometry)
CTs_2000[182,]$geometry <-  st_union(CTs_2000[182,]$geometry, slivers2000[378,]$geometry)

plot(CTs_2000[196,])

## 2015
  
CTs_2015_union <- st_union(CTs_2015) %>% 
  st_collection_extract("POLYGON")

slivers2015 <- st_erase(nyc_city, CTs_2015_union) %>% 
  st_cast("POLYGON") %>% 
  as_tibble() %>% 
  mutate(NAME = c(1:454)) %>% 
  st_as_sf()

slivers2015 <- mutate(slivers2015, NAME = as.character(NAME))

CTs_2015[4,]$geometry <-  st_union(CTs_2015[4,]$geometry, slivers2015[145,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[146,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[147,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[148,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[149,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[150,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[151,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[152,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[153,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[154,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[155,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[156,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[157,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[158,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[159,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[160,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[161,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[162,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[163,]$geometry)

## CTs_comparison

CTs_comparison_union <- st_union(CTs_comparison) %>% 
  st_collection_extract("POLYGON")

CTs_comparison_prec <- st_snap(CTs_comparison_union, CTs_comparison_union, tolerance = 1)

nyc_prec <- nyc_city %>% sf::st_set_precision(x = ., precision = 1)
CTs_comparison_prec <- CTs_comparison_union %>% sf::st_set_precision(x = ., precision = 1)

slivers_comparison <- st_erase(nyc_prec, CTs_comparison_prec) %>% 
  st_cast("POLYGON") %>% 
  as_tibble() %>% 
  mutate(NAME = c(1:4770)) %>% 
  st_as_sf()

slivers_comparison <- mutate(slivers_comparison, NAME = as.character(NAME)) 


####
###
## CT and sliver maps for testing

base_map +
  tm_shape(CTs_2000) +
  tm_fill(col = "red") +
  tm_borders(col = "white", lwd = 2) +
  tm_text("GEOID", col = "black") +
  tm_shape(slivers2000) +
  tm_fill(col = "blue") +
  tm_shape(slivers2000) +
  tm_text("NAME", col = "black") +
  tm_shape(CTs_2000) +
  tm_text("GEOID", col = "black")

base_map +
  tm_shape(CTs_2015) +
  tm_fill(col = "red") +
  tm_borders(col = "white", lwd = 2) +
  tm_text("GEOID") +
  tm_shape(slivers2015) +
  tm_fill(col = "blue") +
  tm_shape(slivers2015) +
  tm_text("NAME", col = "black")

base_map +
  tm_shape(CTs_comparison) +
  tm_fill(col = "red") +
  tm_borders(col = "white", lwd = 2) +
  tm_text("GEOID.1") +
  tm_shape(slivers2015) +
  tm_fill(col = "blue") +
  tm_shape(slivers2015) +
  tm_text("NAME", col = "black")

base_map +
  tm_shape(CTs_2015) +
  tm_fill(col = "blue") +
  tm_text("GEOID", col = "black")


tm_shape(harlem) +
  tm_borders(col = "white", lwd = 2)








## TEST WITH PRECISION

### 2000

CTs_2000_union <- st_union(CTs_2000) %>% 
  st_collection_extract("POLYGON")

nyc_city_prec <- st_snap(nyc_city, nyc_city, tolerance = 5)
#CTs_2000_prec <- st_snap(CTS_2000_union, CTs_2000_union, tolerance = 5)

nyc_prec2000 <- nyc_city %>% sf::st_set_precision(x = ., precision = 5)
CTs_2000_prec <- CTS_2000_union %>% sf::st_set_precision(x = ., precision = 5)

slivers2000 <- st_erase(nyc_city, CTs_2000_union) %>% 
  st_cast("POLYGON") %>% 
  as_tibble() %>% 
  mutate(NAME = c(1:415)) %>% 
  st_as_sf()

slivers2000 <- mutate(slivers2000, NAME = as.character(NAME)) 

CTs_2000[196,]$geometry <-  st_union(CTs_2000[196,]$geometry, slivers2000[5460,]$geometry)
CTs_2000[182,]$geometry <-  st_union(CTs_2000[182,]$geometry, slivers2000[5463,]$geometry)
CTs_2000[165,]$geometry <-  st_union(CTs_2000[165,]$geometry, slivers2000[5464,]$geometry)
CTs_2000[165,]$geometry <-  st_union(CTs_2000[165,]$geometry, slivers2000[5465,]$geometry)
CTs_2000[1728,]$geometry <-  st_union(CTs_2000[1728,]$geometry, slivers2000[1931,]$geometry) %>% 
  st_union(CTs_2000[1728,]$geometry, slivers2000[1930,]$geometry)
CTs_2000[1707,]$geometry <-  st_union(CTs_2000[1707,]$geometry, slivers2000[1929,]$geometry) %>% 
  st_union(CTs_2000[1707,]$geometry, slivers2000[1928,]$geometry)

### 2015

CTs_2015_union <- st_union(CTs_2015) %>% 
  st_collection_extract("POLYGON")

#CTs_2015_prec <- st_snap(CTS_2015_union, CTs_2015_union, tolerance = 5)
CTs_2015_prec <- CTS_2015_union %>% sf::st_set_precision(x = ., precision = 1)

nyc_prec2015 <- nyc_city %>% sf::st_set_precision(x = ., precision = 5)
CTs_2000_prec <- CTS_2000_union %>% sf::st_set_precision(x = ., precision = 5)

slivers2015 <- st_erase(nyc_city, CTs_2015_union) %>% 
  st_cast("POLYGON") %>% 
  as_tibble() %>% 
  mutate(NAME = c(1:450)) %>% 
  st_as_sf()

slivers2015 <- mutate(slivers2015, NAME = as.character(NAME)) 

rm(CTs_2000_prec, CTs_2000_snap, nyc_prec2000, nyc_prec2015)

# which(CTs_2000$GEOID == 36005001700)  ## Example of how to search

CTs_2015[4,]$geometry <-  st_union(CTs_2015[4,]$geometry, slivers2015[1745,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[1746,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[1747,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[1748,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[1749,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[1750,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[1751,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[1752,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[1753,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[1754,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[1755,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[1756,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[1757,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[1758,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[1759,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[1760,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[1761,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[1762,]$geometry) %>% 
  st_union(CTs_2015[4,]$geometry, slivers2015[1763,]$geometry)







