## Data Science Capstone 2022. Francisca Moya Jimenez with contributions from Andrea Mock. Data Merging.

getwd()
setwd('/Users/francisca/Desktop/STAT 318/Final Project')

affordable.housing <- read.csv('Affordable_Rental_Housing_Developments.csv', header = TRUE)
head(affordable.housing)
table(affordable.housing$Community.Area.Name) # Examining community areas
length(unique(affordable.housing$Community.Area.Name)) # 65 unique areas

public.health.stats <- read.csv('Public_Health_Statistics-_Selected_public_health_indicators_by_Chicago_community_area.csv', header = TRUE)
head(public.health.stats)
length(unique(public.health.stats$Community.Area.Name)) # 77 unique community areas

covid.index <- read.csv('Chicago_COVID-19_Community_Vulnerability_Index__CCVI__-_CA_Only.csv', header = TRUE)
head(covid.index)
nrow(covid.index) # 16 unique ZIP Codes

life.expectancy <- read.csv('Public_Health_Statistics-_Life_Expectancy_By_Community_Area.csv', header = TRUE)
head(life.expectancy)
length(life.expectancy$Community.Area) # 78 unique community areas

schools <- read.csv('Chicago_Public_Schools_-_School_Progress_Reports_SY1819.csv', header = TRUE)
head(schools)

# Merging data
community.area.info <- cbind(public.health.stats) # Creates copy of public.health.stats df to start

# Correct Montclaire name to Montclare, Fuller Park* to Fuller Park, and Burnside* to Burnside
community.area.info$Community.Area.Name[community.area.info$Community.Area.Name == "Montclaire"] <- "Montclare"
covid.index$Community.Area.Name[covid.index$Community.Area.Name == "Fuller Park*"] <- "Fuller Park"
covid.index$Community.Area.Name[covid.index$Community.Area.Name == "Burnside*"] <- "Burnside"

community.area.info['Life.Expectancy'] <- rep(NA, nrow(community.area.info))

for (i in 1:nrow(community.area.info)){
  area.name <- community.area.info$Community.Area.Name[i]
  try(
    community.area.info[i,'Life.Expectancy'] <- life.expectancy[life.expectancy$Community.Area == area.name, ]$X2010.Life.Expectancy)
}

# Which are NA's?
which(is.na(community.area.info$Life.Expectancy)) # 6 18 35
community.area.info$Community.Area.Name[c(6, 18, 35)] # Lake View, Montclaire, and Douglas

life.expectancy$Community.Area # Has Lakeview, Montclare with a space at the end, and Douglas with a space char at the end



library(dplyr)


# Assigning correct values
community.area.info <- community.area.info %>% rowwise() %>% mutate(Life.Expectancy =
                                                        case_when(Community.Area.Name == 'Lake View' ~ life.expectancy[life.expectancy$Community.Area == "Lakeview", ]$X2010.Life.Expectancy,
                                                                  Community.Area.Name == 'Douglas' ~ life.expectancy[life.expectancy$Community.Area == "Douglas ", ]$X2010.Life.Expectancy,
                                                                  Community.Area.Name == 'Montclare' ~ life.expectancy[life.expectancy$Community.Area == "Montclare ", ]$X2010.Life.Expectancy,
                                                                  TRUE ~ Life.Expectancy))


getUnits <- function(x){
  tryCatch(
    {affordable.housing %>% filter(Community.Area.Name == x) %>% select(Units) %>% rowwise() %>% sum()},
    error = function(cond){
      return(0)
    }
  )
}

# Building affordable housing count
community.area.info <- community.area.info %>% rowwise() %>% mutate(Number.Affordable.Buildings =
                                                                      sum(affordable.housing$Community.Area.Name == Community.Area.Name))
community.area.info<- community.area.info %>% rowwise() %>%  mutate(Number.Affordable.Units =
                                                                      getUnits(Community.Area.Name)
)

community.area.info$Number.Affordable.Units


# Adding community vulnerability index
community.area.info <- community.area.info %>% rowwise() %>% mutate(Vulnerability.Index =
                                                                      covid.index[covid.index$Community.Area.Name == Community.Area.Name, ]$CCVI.Score)

dim(community.area.info)

# Saving into a csv file
write.csv(community.area.info, "Chicago_data_by_community_cleaned.csv")








