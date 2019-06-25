### SERVICE AREA COMPARISON ####################################################

## Resolve CTs 2000 and 2015 through st_intersect_summarize

CTs_comparison <- st_intersect_summarize(
  CTs_2000,
  CTs_2015,
  group_vars = vars(GEOID.1),
  population = pop_total,
  sum_vars = vars(pop_white, pop_hisp, pop_black, immigrant),
  mean_vars = vars(med_income))

CTs_comparison <- CTs_comparison %>% 
  mutate(pop_density = pop_total/st_area(geometry)) %>% 
  st_collection_extract("POLYGON")


# cts_int <- st_intersection(CTs_2000, CTs_2015)
