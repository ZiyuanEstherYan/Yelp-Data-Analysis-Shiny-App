library(tidyverse)
library(jsonlite)
library(ggmap)
library(leaflet)
library(wordcloud)
library(stringr)
library(tidytext)
library(wordcloud)

register_google(key="")

# yelp <- stream_in(file("~/Downloads/yelp_academic_dataset_business.json"))
# yelp <- jsonlite::flatten(yelp)
# yelp <- as_tibble(yelp)
# saveRDS(yelp, "yelp.rds")

# yelp_reviews <- readLines("~/Downloads/yelp_academic_dataset_review.json")[1:500]
# yelp_reviews <- stream_in(file("~/Downloads/yelp_academic_dataset_review.json"))


##----------------##
## Data Cleaning ##
##----------------##

# Read the data, filter data for cities with lots of data and address entries
yelp <- readRDS("yelp.rds")

yelp <- yelp %>%
  group_by(toupper(city)) %>%
  filter(n() > 2000 & address != "" & `toupper(city)` != 'CALGARY')

# Group into base categories for app selection, drop na's that are in 'niche' categories
yelp <- yelp %>%
  mutate(base_category = case_when(
    grepl("Restaurants", categories) ~ "Restaurants",
    grepl("Food", categories) | grepl("Grocery", categories) ~ "Grocery/Convenience Store",
    grepl("Nighlife", categories) | grepl("Bars", categories) | grepl("Dance", categories) ~ "Nightlife",
    grepl("Active Life", categories) ~ "Active Life",
    grepl("Salons", categories) | grepl("Beauty", categories) ~ "Beauty Services",
    grepl("Entertainment", categories) | grepl("Museums", categories) ~ "Entertainment",
    grepl("Shopping", categories) ~ "Shopping",
    grepl("Services", categories) ~ "Services",
    grepl("Auto", categories) | grepl("Automotive", categories) ~ "Auto Repairs",
    grepl("Medical", categories) | grepl("Doctors", categories) | grepl("Dentists", categories) ~ "Medical"
  )) %>%
  drop_na(base_category)

# Group into sub categories for further analysis of restaurants 
yelp.restaurants <- yelp %>%
  filter(base_category == 'Restaurants') 

unique_cities <- unique(toupper(yelp$city))
unique_cities <- str_sort(unique_cities, descreasing = TRUE, numeric = FALSE)
unique_categories <- unique(yelp$base_category)
unique_categories <- str_sort(unique_categories, decreasing = TRUE, numeric = FALSE)
# Smaller dataset for plotting purposes 
yelp.plot <- yelp %>%
  select(c("name":"categories", "base_category"))

table_display <- yelp.plot %>%
  select(-city) %>%
  rename(City = `toupper(city)`,
         Business_Name = name, 
         Address = address,
         Latitude = latitude,
         Longitude = longitude,
         Star_Ratings = stars,
         Review_Count = review_count,
         Open_Status = is_open,
         Sub_Categories = categories,
         Base_Category = base_category)


##------------------------##
## Restaurant Exploration ##
##------------------------##

restaurants_full <- readRDS('restaurants_full.rds')
restaurants_reviews_full <- readRDS('restaurants_reviews_full.rds')
# restaurants_reviews_full <- restaurants_reviews_full[1:370000,]
# saveRDS(restaurants_reviews_full, "restaurants_reviews_full.rds")

restaurants_full <- restaurants_full %>%
  mutate(
    cuisine = case_when(
      grepl("american", categories, ignore.case = TRUE) == TRUE | grepl("Sandwiches", categories, ignore.case = TRUE) == TRUE | grepl("Fast Food", categories, ignore.case = TRUE) == TRUE | grepl("Burgers", categories, ignore.case = TRUE) == TRUE ~ "American",
      grepl("italian", categories, ignore.case = TRUE) == TRUE ~ "Italian",
      grepl("mexican", categories, ignore.case = TRUE) == TRUE ~ "Mexican",
      grepl("chinese", categories, ignore.case = TRUE) == TRUE ~ "Chinese",
      grepl("indian", categories, ignore.case = TRUE) == TRUE ~ "Indian",
      grepl("thai", categories, ignore.case = TRUE) == TRUE ~ "Thai",
      grepl("korean", categories, ignore.case = TRUE) == TRUE ~ "Korean",
      grepl("japanese", categories, ignore.case = TRUE) == TRUE ~ "Japanese",
      grepl("french", categories, ignore.case = TRUE) == TRUE ~ "French",
      grepl("vietnamese", categories, ignore.case = TRUE) == TRUE ~ "Vietnamese",
      grepl("mediterranean", categories, ignore.case = TRUE) == TRUE ~ "Mediterranean",
      grepl("hawaiian", categories, ignore.case = TRUE) == TRUE ~ "Hawaiian",
      grepl("american", categories, ignore.case = TRUE) == FALSE ~ "Others"
    )
  )

restaurants_reviews_full <- restaurants_reviews_full %>%
  mutate(
    cuisine = case_when(
      grepl("american", categories, ignore.case = TRUE) == TRUE | grepl("Sandwiches", categories, ignore.case = TRUE) == TRUE | grepl("Fast Food", categories, ignore.case = TRUE) == TRUE | grepl("Burgers", categories, ignore.case = TRUE) == TRUE ~ "American",
      grepl("italian", categories, ignore.case = TRUE) == TRUE ~ "Italian",
      grepl("mexican", categories, ignore.case = TRUE) == TRUE ~ "Mexican",
      grepl("chinese", categories, ignore.case = TRUE) == TRUE ~ "Chinese",
      grepl("indian", categories, ignore.case = TRUE) == TRUE ~ "Indian",
      grepl("thai", categories, ignore.case = TRUE) == TRUE ~ "Thai",
      grepl("korean", categories, ignore.case = TRUE) == TRUE ~ "Korean",
      grepl("japanese", categories, ignore.case = TRUE) == TRUE ~ "Japanese",
      grepl("french", categories, ignore.case = TRUE) == TRUE ~ "French",
      grepl("vietnamese", categories, ignore.case = TRUE) == TRUE ~ "Vietnamese",
      grepl("mediterranean", categories, ignore.case = TRUE) == TRUE ~ "Mediterranean",
      grepl("hawaiian", categories, ignore.case = TRUE) == TRUE ~ "Hawaiian",
      grepl("american", categories, ignore.case = TRUE) == FALSE ~ "Others"
    )
  )
