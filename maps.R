# INSTALL NECESSARY PACKAGES ALONG WITH DEPENDENCIES
#can't install rgeos through install.packages use CRAN instead as this is a dependency for "sf" and "ggspatial"

getwd()
#setwd() to whereever data is saved

library("ggspatial")
library("ggplot2")
library("maps")
library("viridis")
library("tools")
library("sf")
library("choroplethr")
library("rnaturalearth")
library("rnaturalearthdata")

theme_set(theme_bw())


png(filename = "countychoropleth%03d.png",
    width = 13, height = 7, units = "in", res = 72) 
data_county <- read.csv("county.csv")
county_choropleth(data_county, 
                 title  = "US Twitter Usage at the County Level between start and end", 
                 legend = "Number of Users", num_colors = 9)
dev.off()

#STATE ZOOMS IF WANTED
png(filename="statezoom%03d.png", width = 13, height = 9, units = "in", res = 72)

county_choropleth(data_county,
				  title = "US Twitter Usage at the County Level (state) between start and end",
				  legend = "Number of Users",
				  state_zoom = "state name in small letters", num_colors = 9) #for example california
dev.off()

png(filename = "statechoropleth%03d.png",
    width = 13, height = 7, units = "in", res = 72)
data_state <- read.csv("state.csv")
state_choropleth(data_state,
                 title = "US Twitter Usage at the State Level between start and end",
                 legend = "Number of Users", num_colors = 9)
dev.off()

city_dc <- read.csv("city.csv")
US <- map_data("usa")
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
states <- cbind(states, st_coordinates(st_centroid(states)))
states$ID <- toTitleCase(states$ID)
world <- ne_countries(scale = "medium", returnclass = "sf")

#Move some states for better readability in East Coast states 
states$nudge_y <-0
states$nudge_x <-0
states$nudge_x[states$ID == "Rhode Island"] <- 0.8
states$nudge_y[states$ID == "Connecticut"] <- -0.5
states$nudge_y[states$ID == "New Hampshire"] <- -0.3
states$nudge_x[states$ID == "Delaware"]<- 1.3
states$nudge_y[states$ID == "District of Columbia"] <- -1
states$nudge_x[states$ID == "District of Columbia"] <- 2

#MAKE SURE QUARTZ WINDOW IS IN FULL SCREEN (saves in the dimension of the quartz)
#with State labels
ggplot() + 
   geom_sf () + 
   geom_polygon(data = US, aes(x=long, y = lat, group = group), fill="antiquewhite1") +
   geom_point(data= city_dc, aes(x=longitude, y=latitude, size = n_tweets, colour = n_tweets, alpha = n_tweets), shape=20, stroke=FALSE) + 
   scale_size_continuous(name="Number of tweets", trans="log", range=c(1,12),breaks=seq(0,60, by = 12)) +
   scale_alpha_continuous(name="Number of tweets", trans="log", range=c(0.1, .9)) +
   scale_color_viridis(option="viridis", trans="log", name="Number of tweets") +
   theme_void() + 
   ggtitle("US Twitter Usage at the City Level", subtitle = "from 8th Februrary 2019 to 15th Februrary 2019") +     
   annotation_north_arrow(location = "bl", pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) + 
   theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue")) + 
   geom_sf(data = states, fill = NA) +     
   geom_label(data = states, aes(X,Y, label = ID), size = 2, fontface = "bold", alpha = 0.5, nudge_y = states$nudge_y, nudge_x = states$nudge_x) + 
   coord_sf(xlim = c(-125,-67),ylim = c(24.5,49.5), expand = TRUE)  + #excludes alaska to zoom in map 
   ggsave("./us_city_intensity_with_labels.png")

#without State labels
ggplot() + 
   geom_sf () + 
   geom_polygon(data = US, aes(x=long, y = lat, group = group), fill="antiquewhite1") +
   geom_point(data= city_dc, aes(x=longitude, y=latitude, size = n_tweets, colour = n_tweets, alpha = n_tweets), shape=20, stroke=FALSE) + 
   scale_size_continuous(name="Number of tweets", trans="log", range=c(1,12),breaks=seq(0,60, by = 12)) +
   scale_alpha_continuous(name="Number of tweets", trans="log", range=c(0.1, .9)) +
   scale_color_viridis(option="viridis", trans="log", name="Number of tweets") +
   theme_void() + 
   ggtitle("US Twitter Usage at the City Level", subtitle = "from 8th Februrary 2019 to 15th Februrary 2019") +     
   annotation_north_arrow(location = "bl", pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) + 
   theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue")) + 
   geom_sf(data = states, fill = NA) +  
   coord_sf(xlim = c(-125,-67),ylim = c(24.5,49.5), expand = TRUE)  + #excludes alaska and hawaii to zoom in map 
   ggsave("./us_city_intensity_wittout_labels.png")
   
#map for Alaska {if wanted}

ggplot(data = world) + 
    geom_sf () + 
    geom_sf(fill = "antiquewhite1") +
    geom_point(data= city_dc, aes(x=longitude, y=latitude, size = n_tweets, colour = n_tweets, alpha = n_tweets), shape=20, stroke=FALSE) + 
    scale_size_continuous(name="Number of tweets", trans="log", range=c(1,12),breaks=seq(0,60, by = 12)) +
    scale_alpha_continuous(name="Number of tweets", trans="log", range=c(0.1, .9)) +
    scale_color_viridis(option="viridis", trans="log", name="Number of tweets") +
    theme_void() + 
    ggtitle("US Twitter Usage at the City Level (Alaska)", subtitle = "from 8th Februrary 2019 to 15th Februrary 2019") +     
    annotation_north_arrow(location = "bl", pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) + 
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue")) +    
    coord_sf(xlim = c(-170,-141),ylim = c(51,71), expand = TRUE) +
    ggsave("./us_city_intensity_alaska.png")
    
#map for Hawaii {if wanted} [Note: Only includes large island as the smaller ones are not visible on the map]

ggplot(data = world) + 
	geom_sf () + 
	geom_sf(fill = "antiquewhite1") + 
    geom_point(data= city_dc, aes(x=longitude, y=latitude, size = n_tweets, colour = n_tweets, alpha = n_tweets), shape=20, stroke=FALSE) + 
    scale_size_continuous(name="Number of tweets", trans="log", range=c(1,12),breaks=seq(0,60, by = 12)) +
    scale_alpha_continuous(name="Number of tweets", trans="log", range=c(0.1, .9)) +
    scale_color_viridis(option="viridis", trans="log", name="Number of tweets") +
    theme_void() + 
    ggtitle("US Twitter Usage at the City Level (Hawaii)", subtitle = "from 8th Februrary 2019 to 15th Februrary 2019") +     
    annotation_north_arrow(location = "bl", pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) + 
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue")) +  
    coord_sf(xlim = c(-162,-154.35),ylim = c(18.58,23), expand = TRUE) + 
    ggsave("./us_city_intensity_hawaii.png")