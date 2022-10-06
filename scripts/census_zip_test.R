library(httr)
library(jsonlite)
readRenviron("~/.Renviron")
# You can check it with:
census_key <- Sys.getenv("CENSUS_API_KEY")

unique_zip <- chicago_zip_totals %>% pull

get_cen_bus<- function(zip, api_key = census_key){
  bus_data <- GET(str_glue("https://api.census.gov/data/2018/zbp?get=ESTAB&for=zipcode:{zip}"))
  bus_json <- content(bus_data, as = "text")
  bus_matrix <- fromJSON(bus_json)
  bus_tbl <- as_tibble(bus_matrix[2:nrow(bus_matrix), ],
                       .name_repair = "minimal")
  
  names(bus_tbl) <- bus_matrix[1, ]
  
  bus_zip <- bus_tbl %>%
    janitor::clean_names() %>%
    mutate(estab = as.numeric(estab)) %>%
    group_by(zip_code) %>%
    summarise(total_bus = sum(estab, na.rm = TRUE))
  
  return(bus_zip)
  
}

business_data <- map_dfr(unique_zip, get_cen_bus)
business_data <- business_data %>%
  distinct(zip_code, .keep_all = TRUE)

chicago_zip_totals <- chicago_zip_totals %>%
  distinct(zip, .keep_all = TRUE) %>%
  left_join(business_data, by = c("zip" = "zip_code")) %>%
  mutate(rank_chicago_bl = dense_rank(desc(num_bus)),
         rank_census_bus = dense_rank(desc(total_bus)))

write_csv(chicago_zip_totals, "data/intermediate_data/chicago_zip_totals_business.csv")
  
