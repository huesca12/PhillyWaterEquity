library(leaflet)
library(tidyverse)
library(tidycensus)
library(dplyr)
library(sp)
library(sf)

census_api_key("e60a226d363feffbd73c921ceb5aa86717efc58b", install = TRUE, overwrite = TRUE)

grants <- rgdal::readOGR("~/Desktop/Fall 2022/envs-1650/finalProject/analysis/stormwater_grants-2.geojson")

# Map with grant locations + toggle for no markers
leaflet(data = grants) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addCircleMarkers(lng = grants.df$coords.x1, lat = grants.df$coords.x2, 
                   popup = as.character(grants$grantamount), 
                   label = grants$projectname, group = "grants") %>%
  addLayersControl(
    overlayGroups = c("grants"),
    options = layersControlOptions(collapsed = FALSE)
  )


# Map of philly population percentiles + toggle for markers
philly_pop <- get_acs(geography = "tract", variables = "B01003_001",
                      state = "PA", county = "Philadelphia", geometry = TRUE,
                      year = 2015)

pal <- colorQuantile(palette = "viridis", domain = philly_pop$estimate, n = 10)

philly_pop %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(estimate)) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~ estimate,
            title = "population percentiles") %>%
  addCircleMarkers(lng = grants.df$coords.x1, lat = grants.df$coords.x2, 
                   popup = as.character(grants$grantamount), 
                   label = grants$projectname, group = "grants",
                   color = "cream",
                   radius = 6,
                   fillOpacity = 0.7) %>%
  addLayersControl(
    overlayGroups = c("grants"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Map of philly black race population + toggle for markers
philly_race_black <- get_acs(geography = "tract", variables = "B02001_003",
                      state = "PA", county = "Philadelphia", geometry = TRUE,
                      year = 2015)

pal <- colorQuantile(palette = "viridis", domain = philly_race_black$estimate, n = 10)

philly_race_black %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(estimate)) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~ estimate,
            title = "black population") %>%
  addCircleMarkers(lng = grants.df$coords.x1, lat = grants.df$coords.x2, 
                   popup = as.character(grants$grantamount), 
                   label = grants$projectname, group = "grants",
                   color = "cream",
                   radius = 6,
                   fillOpacity = 0.7) %>%
  addLayersControl(
    overlayGroups = c("grants"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Map of philly white race population + toggle for markers
philly_race_white <- get_acs(geography = "tract", variables = "B02001_002",
                       state = "PA", county = "Philadelphia", geometry = TRUE,
                       year = 2015)

pal <- colorQuantile(palette = "viridis", domain = philly_race_white$estimate, n = 10)

philly_race_white %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(estimate)) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~ estimate,
            title = "white population") %>%
  addCircleMarkers(lng = grants.df$coords.x1, lat = grants.df$coords.x2, 
                   popup = as.character(grants$grantamount), 
                   label = grants$projectname, group = "grants",
                   color = "cream",
                   radius = 6,
                   fillOpacity = 0.7) %>%
  addLayersControl(
    overlayGroups = c("grants"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Map of philly median home value + toggle for markers
philly_house <- get_acs(geography = "tract", variables = "B25077_001",
                       state = "PA", county = "Philadelphia", geometry = TRUE,
                       year = 2015)

pal <- colorNumeric(palette = "viridis", domain = philly_house$estimate)

philly_house %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(estimate)) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~ estimate,
            title = "median home value",
            labFormat = labelFormat(prefix = "$"),) %>%
  addCircleMarkers(lng = grants.df$coords.x1, lat = grants.df$coords.x2, 
                   popup = as.character(grants$grantamount), 
                   label = grants$projectname, group = "grants",
                   color = "cream",
                   radius = 6,
                   fillOpacity = 0.7) %>%
  addLayersControl(
    overlayGroups = c("grants"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Map of philly median income last 12 months + toggle for markers
philly_income <- get_acs(geography = "tract", variables = "B06011_001",
                        state = "PA", county = "Philadelphia", geometry = TRUE,
                        year = 2015)

pal <- colorNumeric(palette = "viridis", domain = philly_income$estimate)

philly_income %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(estimate)) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~ estimate,
            title = "median income",
            labFormat = labelFormat(prefix = "$"),) %>%
  addCircleMarkers(lng = grants.df$coords.x1, lat = grants.df$coords.x2, 
                   popup = as.character(grants$grantamount), 
                   label = grants$projectname, group = "grants",
                   color = "cream",
                   radius = 6,
                   fillOpacity = 0.7) %>%
  addLayersControl(
    overlayGroups = c("grants"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Map of philly median home value + toggle for markers
philly_house <- get_acs(geography = "tract", variables = "B25077_001",
                        state = "PA", county = "Philadelphia", geometry = TRUE,
                        year = 2020)

pal <- colorNumeric(palette = "viridis", domain = philly_house$estimate)

philly_house %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(estimate)) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~ estimate,
            title = "median home value",
            labFormat = labelFormat(prefix = "$"),) %>%
  addCircleMarkers(lng = grants.df$coords.x1, lat = grants.df$coords.x2, 
                   popup = as.character(grants$grantamount), 
                   label = grants$projectname, group = "grants",
                   color = "cream",
                   radius = 6,
                   fillOpacity = 0.7) %>%
  addLayersControl(
    overlayGroups = c("grants"),
    options = layersControlOptions(collapsed = FALSE)
  )

census <- rgdal::readOGR("~/Desktop/Fall 2022/envs-1650/finalProject/analysis/Census_Tracts_2010.geojson")

plot(census)
plot(grants, add = TRUE, col="green")

res <- over(grants, census)
table(res$GEOID10)


grantstat <- data.frame(table(res$GEOID10))

merged <- merge(grantstat, philly_house, by.x="Var1", by.y="GEOID")

sum(merged$Freq)
sum(grantstat$Freq)

plot(merged$Freq, merged$estimate, ylab="Median Home Value (2015)", xlab="Grants/Tract", col="lightblue",
     pch=19, main="Home Value and Grant Allocation", lwd=1:5)

plot(merged$Freq, merged$estimate, ylab="Median Home Value (2015)", xlab="Grants/Tract", col="lightblue",
     pch=19, main="Home Value and Grant Allocation")


results <- cor.test(merged$Freq, merged$estimate, method = "spearman")
