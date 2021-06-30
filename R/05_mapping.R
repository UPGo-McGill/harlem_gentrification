### MAP MAKING #################################################################

# bounding box function 

gg_bbox <- function(geom, x1 = 0, x2 = 1, y1 = 0, y2 = 1) {
  
  bbox <- st_bbox(geom)
  
  matrix_x <- matrix(bbox[c(1,3)], nrow = 1) %*% matrix(
    c(1 - x1, x1, 1 - x2, x2), nrow = 2)
  
  matrix_y <- matrix(bbox[c(2,4)], nrow = 1) %*% matrix(
    c(1 - y1, y1, 1- y2, y2), nrow = 2)
  
  coord_sf(xlim = as.vector(matrix_x), ylim = as.vector(matrix_y))
}



# median income

MHI_map <- 
  ggplot() + 
  # st_as_sf() %>% 
  geom_sf(data = CTs_2009_2019, aes(fill = med_income), colour = "white", 
          lwd = 0.05) +
  scale_fill_viridis_b(breaks = c(10000, 25000, 40000, 55000, 70000), 
                        limits = c(0,100000), name = "Median Income", 
                       option = "E") +
  facet_wrap(~ Year) +
  geom_sf() + 
  geom_sf(data = nyc_water, colour = "white", fill = "white") +
  ggtitle("Median Household Income")+
  theme_void() +
  theme(
    panel.background = element_rect(fill = "gray"),
    panel.grid.major = element_line(colour = "gray")) +
  coord_sf(xlim = st_bbox(harlem) [c(1,3)] * c(0.9998, 1.00039), 
          ylim = st_bbox(harlem) [c(2,4)] * c(0.999999, 1.0001))

# percent Hispanic

hisp_map <- 
  ggplot() + 
  # st_as_sf() %>% 
  geom_sf(data = CTs_2009_2019, aes(fill = pop_hisp_pct), colour = "white", 
          lwd = 0.05) +
  scale_fill_viridis_b(name = "% Hispanic", option = "E") +
  facet_wrap(~ Year) +
  geom_sf() + 
  geom_sf(data = nyc_water, colour = "white", fill = "white") +
  ggtitle("Percent Hispanic population")+
  theme_void() +
  theme(
    panel.background = element_rect(fill = "gray"),
    panel.grid.major = element_line(colour = "gray")) +
  coord_sf(xlim = st_bbox(harlem) [c(1,3)] * c(0.9998, 1.00039), 
           ylim = st_bbox(harlem) [c(2,4)] * c(0.999999, 1.0001))


# percent Black

pct_black_map <- 
  ggplot() + 
  # st_as_sf() %>% 
  geom_sf(data = CTs_2009_2019, aes(fill = pop_black_pct), colour = "white", 
          lwd = 0.05) +
  scale_fill_viridis_b(name = "% Immigrant", option = "E") +
  facet_wrap(~ Year) +
  geom_sf() + 
  geom_sf(data = nyc_water, colour = "white", fill = "white") +
  ggtitle("Percent Black population")+
  theme_void() +
  theme(
    panel.background = element_rect(fill = "gray"),
    panel.grid.major = element_line(colour = "gray")) +
  coord_sf(xlim = st_bbox(harlem) [c(1,3)] * c(0.9998, 1.00039), 
           ylim = st_bbox(harlem) [c(2,4)] * c(0.999999, 1.0001))


# immigrant percent

pct_imm_map <- 
  ggplot() + 
  # st_as_sf() %>% 
  geom_sf(data = CTs_2009_2019, aes(fill = immigrant_pct), colour = "white", 
          lwd = 0.05) +
  scale_fill_viridis_b(option = "E") +
  facet_wrap(~ Year) +
  geom_sf() + 
  geom_sf(data = nyc_water, colour = "white", fill = "white") +
  ggtitle("Percent immigrant population")+
  theme_void() +
  theme(
    panel.background = element_rect(fill = "gray"),
    panel.grid.major = element_line(colour = "gray")) +
  coord_sf(xlim = st_bbox(harlem) [c(1,3)] * c(0.9998, 1.00039), 
           ylim = st_bbox(harlem) [c(2,4)] * c(0.999999, 1.0001))


# percent non Hispanic white

pct_white_map <- 
  ggplot() + 
  # st_as_sf() %>% 
  geom_sf(data = CTs_2009_2019, aes(fill = pop_white_pct), colour = "white", 
          lwd = 0.05) +
  scale_fill_viridis_b(name = "% Non-Hispanic white", breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3), 
                       limits = c(0,0.5), option = "E") +
  facet_wrap(~ Year) +
  geom_sf() + 
  geom_sf(data = nyc_water, colour = "white", fill = "white") +
  ggtitle("Percent white population")+
  theme_void() +
  theme(
    panel.background = element_rect(fill = "gray"),
    panel.grid.major = element_line(colour = "gray")) +
  coord_sf(xlim = st_bbox(harlem) [c(1,3)] * c(0.9998, 1.00039), 
           ylim = st_bbox(harlem) [c(2,4)] * c(0.999999, 1.0001))

ggsave("output/figure_1.png", plot = MHI_map)
ggsave("output/figure_2.png", plot = hisp_map)
ggsave("output/figure_3.png", plot = pct_black_map)
ggsave("output/figure_4.png", plot = pct_imm_map)
ggsave("output/figure_5.png", plot = pct_white_map)


## Mapping 2009 and 2019

CTs_2009_2019 %>% 
  ggplot() +
  geom_sf(aes(fill = med_income)) +
  # scale_fill_gradientn(limits = c(0,1000), colors = c("#9DBF9E", "#FCB97D", "#A84268"),
  #                      na.value = "grey80") +
  #scale_x_continuous(breaks = c(20000, 40000, 60000)) +
  scale_fill_continuous(breaks = c(10000, 25000, 40000, 55000, 70000), limits = c(0,100000)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "gray"),
    panel.grid.major = element_line(colour = "gray")) +
  gg_bbox(harlem) %>% 
  facet_grid("Year")

CTs_2009 %>% 
  ggplot() +
  geom_sf(aes(fill = pop_hisp_pct)) +
  # scale_fill_gradientn(limits = c(0,1000), colors = c("#9DBF9E", "#FCB97D", "#A84268"),
  #                      na.value = "grey80") +
  #scale_fill_continuous(breaks = c(20000, 35000, 50000, 65000, 80000)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "gray"),
    panel.grid.major = element_line(colour = "gray")) +
  gg_bbox(harlem) 

CTs_2019 %>% 
  ggplot() +
  geom_sf(aes(fill = pop_hisp_pct)) +
  # scale_fill_gradientn(limits = c(0,1000), colors = c("#9DBF9E", "#FCB97D", "#A84268"),
  #                      na.value = "grey80") +
  #scale_fill_continuous(breaks = c(20000, 35000, 50000, 65000, 80000)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "gray"),
    panel.grid.major = element_line(colour = "gray")) +
  gg_bbox(harlem) 

CTs_2009 %>% 
  ggplot() +
  geom_sf(aes(fill = pop_black_pct)) +
  # scale_fill_gradientn(limits = c(0,1000), colors = c("#9DBF9E", "#FCB97D", "#A84268"),
  #                      na.value = "grey80") +
  #scale_fill_continuous(breaks = c(20000, 35000, 50000, 65000, 80000)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "gray"),
    panel.grid.major = element_line(colour = "gray")) +
  gg_bbox(harlem) 

CTs_2019 %>% 
  ggplot() +
  geom_sf(aes(fill = pop_black_pct)) +
  # scale_fill_gradientn(limits = c(0,1000), colors = c("#9DBF9E", "#FCB97D", "#A84268"),
  #                      na.value = "grey80") +
  #scale_fill_continuous(breaks = c(20000, 35000, 50000, 65000, 80000)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "gray"),
    panel.grid.major = element_line(colour = "gray")) +
  gg_bbox(harlem) 

CTs_2009 %>% 
  ggplot() +
  geom_sf(aes(fill = pop_white_pct)) +
  # scale_fill_gradientn(limits = c(0,1000), colors = c("#9DBF9E", "#FCB97D", "#A84268"),
  #                      na.value = "grey80") +
  #scale_fill_continuous(breaks = c(20000, 35000, 50000, 65000, 80000)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "gray"),
    panel.grid.major = element_line(colour = "gray")) +
  gg_bbox(harlem) 

CTs_2019 %>% 
  ggplot() +
  geom_sf(aes(fill = pop_white_pct)) +
  # scale_fill_gradientn(limits = c(0,1000), colors = c("#9DBF9E", "#FCB97D", "#A84268"),
  #                      na.value = "grey80") +
  #scale_fill_continuous(breaks = c(20000, 35000, 50000, 65000, 80000)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "gray"),
    panel.grid.major = element_line(colour = "gray")) +
  gg_bbox(harlem) 

CTs_2009 %>% 
  ggplot() +
  geom_sf(aes(fill = immigrant_pct)) +
  # scale_fill_gradientn(limits = c(0,1000), colors = c("#9DBF9E", "#FCB97D", "#A84268"),
  #                      na.value = "grey80") +
  #scale_fill_continuous(breaks = c(20000, 35000, 50000, 65000, 80000)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "gray"),
    panel.grid.major = element_line(colour = "gray")) +
  gg_bbox(harlem) 

CTs_2019 %>% 
  ggplot() +
  geom_sf(aes(fill = immigrant_pct)) +
  # scale_fill_gradientn(limits = c(0,1000), colors = c("#9DBF9E", "#FCB97D", "#A84268"),
  #                      na.value = "grey80") +
  #scale_fill_continuous(breaks = c(20000, 35000, 50000, 65000, 80000)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "gray"),
    panel.grid.major = element_line(colour = "gray")) +
  gg_bbox(harlem) 




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
              




