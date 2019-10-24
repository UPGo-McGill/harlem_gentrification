### MAP MAKING #################################################################

# Add year to data, then combine 2000 and 2015

CTs_2015 <- 
  CTs_2015 %>% 
  mutate("Year" = 2015)

CTs_fixed_2000 <- 
  CTs_fixed_2000 %>% 
  mutate("Year" = 2000)

CTs_2000_2015 <- 
  rbind(CTs_fixed_2000, CTs_2015)

# Standardize data


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
  geom_sf(aes(fill = value)) +
  facet_grid(rows = vars(std_variable),cols = vars(Year)) +
  theme(
    panel.background = element_rect(fill = "gray"),
      panel.grid.major = element_line(colour = "gray")) +
  gg_bbox(harlem)

# Tidy data for ggplot mapping

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


# bounding box function 

gg_bbox <- function(geom, x1 = 0, x2 = 1, y1 = 0, y2 = 1) {
  
  bbox <- st_bbox(geom)
  
  matrix_x <- matrix(bbox[c(1,3)], nrow = 1) %*% matrix(
    c(1 - x1, x1, 1 - x2, x2), nrow = 2)
  
  matrix_y <- matrix(bbox[c(2,4)], nrow = 1) %*% matrix(
    c(1 - y1, y1, 1- y2, y2), nrow = 2)
  
  coord_sf(xlim = as.vector(matrix_x), ylim = as.vector(matrix_y))
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
              

## Initialize map list and base map  
### need separate for 2000 and 2015?

figure <- list()
base_map <- tm_shape(nyc_city, bbox = bb(st_bbox(harlem$geometry), xlim=c(-0.02, 1.2),
                                        ylim=c(0.01, 0.8), relative = TRUE),
                       unit = "mi") +
  tm_fill(col = "#f0f0f0") +
  tm_shape(nyc_city) +
  tm_fill(col = "grey80", title = "Base Map") +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50") +
  tm_layout(frame = TRUE, main.title.size = 1.5, legend.title.size = 1.2,
            legend.title.fontfamily = "Futura-CondensedExtraBold",
            legend.position = c("left", "top"),
            fontfamily = "Futura-Medium",
            title.fontfamily = "Futura-CondensedExtraBold")

## Figure 1. Population density 2000

figure[[1]] <- 
  base_map +
  tm_shape(CTs_2000) +
  tm_polygons("pop_density", border.alpha = 0, palette = "YlGn") +
  tm_shape(harlem) +
  tm_borders(col = "black", alpha = 0.6, lwd = 4) +
  tm_layout(title = "Figure 1. \nPopulation density 2000",
            legend.format = list(fun = function(x) {
              paste0(formatC(x * 100, digits = 0, format = "f"))})) +
  tm_add_legend(type = "fill", labels = "No data", col = "#e0e0e0") +
  tm_add_legend(type = "fill", labels = "East Harlem", col = "NA", lwd = 2, 
                border.col = "black", alpha = 0.6, border.lwd = 2)

## Figure 2. Population density 2015

figure[[2]] <- 
  base_map +
  tm_shape(CTs_2015) +
  tm_polygons("pop_density", border.alpha = 0, palette = "YlGn",
              title = "Population Density \n2015") +
  tm_shape(harlem) +
  tm_borders(col = "white", lwd = 4)

## Figure 3. Population density change 2000-2015 TKTK

figure[[1]] <- 
  base_map +
  tm_shape(CTs_comparison) +
  tm_polygons("density_change", border.alpha = 0, palette = "-Spectral",
              title = "Change in Population Density \n2000 to 2015",
              auto.palette.mapping = FALSE,
              midpoint = NA,
              breaks = c(-Inf, -50, -25, 0, 25, 50, Inf)) +
  tm_shape(nyc_water) +
  tm_fill(col = "lightblue1") +
  tm_shape(harlem) +
  tm_borders(col = "black", alpha = 0.6, lwd = 4)

## Figure 4. Median household income 2000
  
  base_map +
    tm_shape(CTs_2000) +
    tm_polygons("med_income", border.alpha = 0, palette = "GnBu",
                title = "Median Income \n2000", 
                breaks = c(0, 15000, 30000, 45000, 60000, 75000, Inf)) +
    tm_shape(nyc_water) +
    tm_fill(col = "lightblue1") +
    tm_shape(harlem) +
    tm_borders(col = "black", alpha = 0.6, lwd = 4)

## Figure 5. Median household income 2015
  
  base_map +
    tm_shape(CTs_2015) +
    tm_polygons("med_income", border.alpha = 0, palette = "GnBu",
                title = "Median Income \n2015", 
                breaks = c(15000, 30000, 45000, 60000, 75000, Inf)) +
    tm_shape(nyc_water) +
    tm_fill(col = "lightblue1") +
    tm_shape(harlem) +
    tm_borders(col = "black", alpha = 0.6, lwd = 4)

## Figure 6. Median household income 2000 change? 
  
  base_map +
    tm_shape(CTs_comparison) +
    tm_polygons("income_change", border.alpha = 0, palette = "-Spectral",
                title = "Change in Population Density \n2000 to 2015",
                auto.palette.mapping = FALSE,
                midpoint = NA,
                breaks = c(-Inf, -50, -25, 0, 25, 50, Inf)) +
    tm_shape(nyc_water) +
    tm_fill(col = "lightblue1") +
    tm_shape(harlem) +
    tm_borders(col = "black", alpha = 0.6, lwd = 4)

## Figure 7. Percent Black residents 2000

figure[[7]] <- 
  base_map +
  tm_shape(CTs_2000) +
  tm_polygons("pop_black_pct", border.alpha = 0, palette = "BuPu",
                title = "Percent Black Residents \n2000", 
                breaks = c(0, 10, 20, 30, 40, 50, Inf)) +
  tm_shape(nyc_water) +
  tm_fill(col = "lightblue1") +
  tm_shape(harlem) +
  tm_borders(col = "black", alpha = 0.6, lwd = 4)
 

## Figure 8. Percent Black residents 2015

figure[[8]] <- 
  base_map +
  tm_shape(CTs_2015) +
  tm_polygons("pop_black_pct", border.alpha = 0, palette = "BuPu",
              title = "Percent Black Residents \n2015", 
              breaks = c(0, 10, 20, 30, 40, 50, Inf)) +
  tm_shape(nyc_water) +
  tm_fill(col = "lightblue1") +
  tm_shape(harlem) +
  tm_borders(col = "black", alpha = 0.6, lwd = 4)

## Figure 9. Percent Black residents change 2000-2015

base_map +
  tm_shape(CTs_comparison) +
  tm_polygons("black_change", border.alpha = 0, palette = "-Spectral",
              title = "Change in Black Population \n2000 to 2015",
              auto.palette.mapping = FALSE,
              midpoint = NA,
              breaks = c(-Inf, -50, -25, 0, 25, 50, Inf)) +
  tm_shape(nyc_water) +
  tm_fill(col = "lightblue1") +
  tm_shape(harlem) +
  tm_borders(col = "black", alpha = 0.6, lwd = 4)

## Figure 10. Percent Hispanic residents 2000

base_map +
  tm_shape(CTs_2000) +
  tm_polygons("pop_hisp_pct", border.alpha = 0, palette = "Purples",
              title = "Percent Hispanic Population \n2000", 
              breaks = c(0, 10, 20, 30, 40, 50, Inf)) +
  tm_shape(nyc_water) +
  tm_fill(col = "lightblue1") +
  tm_shape(harlem) +
  tm_borders(col = "black", alpha = 0.6, lwd = 4)

## Figure 11. Percent Hispanic residents 2015

base_map +
  tm_shape(CTs_2015) +
  tm_polygons("pop_hisp_pct", border.alpha = 0, palette = "Purples",
              title = "Percent Hispanic Population \n2015", 
              breaks = c(0, 10, 20, 30, 40, 50, Inf)) +
  tm_shape(nyc_water) +
  tm_fill(col = "lightblue1") +
  tm_shape(harlem) +
  tm_borders(col = "black", alpha = 0.6, lwd = 4)

## Figure 12. Percent Hispanic residents change 2000-2015

base_map +
  tm_shape(CTs_comparison) +
  tm_polygons("hisp_change", border.alpha = 0, palette = "-Spectral",
              title = "Change in Hispanic Population \n2000 to 2015",
              auto.palette.mapping = FALSE,
              midpoint = NA,
              breaks = c(-Inf, -50, -25, 0, 25, 50, Inf)) +
  tm_shape(nyc_water) +
  tm_fill(col = "lightblue1") +
  tm_shape(harlem) +
  tm_borders(col = "black", alpha = 0.6, lwd = 4)

## Figure 13. Percent white residents 2000

base_map +
  tm_shape(CTs_2000) +
  tm_polygons("pop_white_pct", border.alpha = 0, palette = "RdPu",
              title = "Percent White Population \n2000", 
              breaks = c(0, 10, 20, 30, 40, 50, Inf)) +
  tm_shape(nyc_water) +
  tm_fill(col = "lightblue1") +
  tm_shape(harlem) +
  tm_borders(col = "black", alpha = 0.6, lwd = 4)

## Figure 14. Percent white residents 2015

base_map +
  tm_shape(CTs_2015) +
  tm_polygons("pop_white_pct", border.alpha = 0, palette = "RdPu",
              title = "Percent White Population \n2015", 
              breaks = c(0, 10, 20, 30, 40, 50, Inf)) +
  tm_shape(nyc_water) +
  tm_fill(col = "lightblue1") +
  tm_shape(harlem) +
  tm_borders(col = "black", alpha = 0.6, lwd = 4)

## Figure 15. Percent white residents change 2000-2015

base_map +
  tm_shape(CTs_comparison) +
  tm_polygons("white_change", border.alpha = 0, palette = "-Spectral",
              title = "Change in White Population \n2000 to 2015",
              auto.palette.mapping = FALSE,
              midpoint = NA,
              breaks = c(-Inf, -50, -25, 0, 25, 50, Inf)) +
  tm_shape(nyc_water) +
  tm_fill(col = "lightblue1") +
  tm_shape(harlem) +
  tm_borders(col = "black", alpha = 0.6, lwd = 4)







### Saving output

tmap_save(figure[[1]], "output/figure_1.png", width = 2400, height = 2400)


