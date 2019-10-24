### MAP MAKING #################################################################

# bounding box function 

gg_bbox <- function(geom, x1 = 0, x2 = 1, y1 = 0, y2 = 1) {
  
  bbox <- st_bbox(geom)
  
  matrix_x <- matrix(bbox[c(1,3)], nrow = 1) %*% matrix(
    c(1 - x1, x1, 1 - x2, x2), nrow = 2)
  
  matrix_y <- matrix(bbox[c(2,4)], nrow = 1) %*% matrix(
    c(1 - y1, y1, 1- y2, y2), nrow = 2)
  
  coord_sf(xlim = as.vector(matrix_x), ylim = as.vector(matrix_y))

# Add year to data, then combine 2000 and 2015

CTs_2015 <- 
  CTs_2015 %>% 
  mutate("Year" = 2015)

CTs_fixed_2000 <- 
  CTs_fixed_2000 %>% 
  mutate("Year" = 2000)

CTs_2000_2015 <- 
  rbind(CTs_fixed_2000, CTs_2015)

# Standardize and map data

CTs_standard <- 
  CTs_2000_2015 %>%
  mutate(
    std_immigrant = scale(1 - (immigrant/pop_total)),
    std_white = scale(pop_white/pop_total),
    std_black = scale(pop_black/pop_total),
    std_hisp = scale(pop_hisp/pop_total),
    std_inc = scale(med_income)) %>% 
  pivot_longer(cols = std_immigrant:std_white:std_black:std_hisp:std_inc,
               names_to = "std_variable",
               values_to = "value") %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = as.factor(value))) +
  scale_fill_brewer(name = "std_variable", palette = "PuBuGn") + #causing long delays
  facet_grid(rows = vars(std_variable),cols = vars(Year)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "gray"),
      panel.grid.major = element_line(colour = "gray")) +
  gg_bbox(harlem)



## Tidy data for ggplot mapping

#CTs_mapping <- 
CTs_2000_2015 %>% 
  pivot_longer(cols = immigrant:med_income:pop_black:pop_hisp:pop_white,
               names_to = "variable",
               values_to = "count") %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = std_count)) +
  facet_grid(rows = vars(variable),cols = vars(Year),
             scales = "free") +
  scale_x_continuous(breaks = seq(0, 100, 10)) + theme_classic()
gg_bbox(harlem)



}


## Test for NAs 

a <- CTs_2000_2015 %>% 
  pivot_longer(cols = immigrant:med_income:pop_black:pop_hisp:pop_white,
               names_to = "variable",
               values_to = "count") 

which(is.na(a$variable))
plot(is.na(a$variable))
plot(a$variable)
hist(a$variable)
hist(as.numeric(a$variable))
hist(as.factor(a$variable))

plot(CTs_2015["geometry"])

GEOID_geometry <- 
  CTs_2000_2015 %>% 
  select(GEOID, geometry)
              




