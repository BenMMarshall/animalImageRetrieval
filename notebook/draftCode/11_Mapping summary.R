
library(dplyr)
library(ggplot2)

inat.fullresults <- read.csv("./Data/PRELIM_inat_images.csv", stringsAsFactors = FALSE)
flickr.fullresults <- read.csv("./Data/flickr_images_list_2020-06-28.csv", stringsAsFactors = FALSE)

# inat.fullresults %>% 
#   filter( !(photos.photo.latitude == 0 | is.na(photos.photo.latitude)),
#           !(photos.photo.longitude == 0 | is.na(photos.photo.longitude))) %>% 
#   select(photos.photo.longitude, photos.photo.latitude) %>% 
#   mutate(Source = "iNaturalist") %>% 
#   rename("lon" = photos.photo.longitude, "lat" = photos.photo.latitude)

fl.data <- flickr.fullresults %>% 
  dplyr::filter( !(photos.photo.latitude == 0 | is.na(photos.photo.latitude)),
                 !(photos.photo.longitude == 0 | is.na(photos.photo.longitude))) %>% 
  dplyr::filter(photos.photo.accuracy > 3) %>% # three is where it gets to country level
  dplyr::select(photos.photo.longitude, photos.photo.latitude) %>% 
  mutate(Source = "Flickr") %>% 
  rename("lon" = photos.photo.longitude, "lat" = photos.photo.latitude)

flickr.fullresults %>% 
  dplyr::filter(photos.photo.latitude ==  50,
                photos.photo.longitude == -15.5)

# accuracy (Optional)
# Recorded accuracy level of the location information. Current range is 1-16 :
#   World level is 1
# Country is ~3
# Region is ~6
# City is ~11
# Street is ~16

map <- map_data("world")

ggplot() +
  geom_polygon(data = map, aes(x = long, y = lat, group = group),
               fill = "grey85") +
  geom_point(data = fl.data, aes(x = lon, y = lat), pch = ".") +
  coord_quickmap() +
  theme_bw()

library(raster)
library(scico)

rast <- raster(matrix(0, 180, 90), xmx = 180, xmn = -180,
               ymx = 90, ymn = -90, crs = "+proj=longlat +datum=WGS84 +no_defs ")

count.rast <- rasterize(x = fl.data[,1:2],
                        y = rast, fun = "count")

plot(count.rast)
# count.rast[is.na(count.rast)] <- 0

count.df <- rasterToPoints(count.rast)
count.df <- data.frame(count.df)
names(count.df) <- c("lon", "lat", "count")

count.df %>% 
  arrange(desc(count))

count.df[count.df$count == max(count.df$count),]
### THERE IS SOMETHING REAL ODD OCCURING IN THE CENTRE OF MADAGASCAR
# PERHAPS THOSE WITH V LOW WOLRD ACC

(map.full <- ggplot() +
    geom_polygon(data = map, aes(x = long, y = lat, group = group),
                 fill = "grey85") +
    geom_tile(data = count.df, aes(x = lon, y = lat, fill = count),
              alpha = 0.95) +
    # geom_point(data = fl.data, aes(x = lon, y = lat), pch = ".") +
    scale_fill_scico(palette = "roma",
                     name = "Count\n(log gradient)", trans = "log",
                     breaks = c(1, 5, 50, 500, 5000)) +
    coord_quickmap() +
    labs(x = "Longitude", y = "Latitude") +
    theme_bw() +
    theme(legend.position = c(0.16, 0.1),
          legend.direction = "horizontal",
          legend.background = element_rect(fill = alpha("white", 0.45)),
          axis.title = element_text(face = 2),
          axis.title.y = element_text(angle = 0, hjust = 1),
          axis.title.x = element_text(hjust = 1)))

(dens.plot <- ggplot(count.df) +
    geom_density(aes(x = count), fill = "black") +
    scale_x_log10() +
    theme_void())

library(cowplot)

ggdraw(map.full) +
  draw_grob(ggplotGrob(dens.plot),
            x = 0.235, y = 0.33, width = 0.155, height = 0.05)

ggsave("./Figures/World map count with density.png", width = 210, height = 130,
       dpi = 300, units = "mm")


# Gridded counts ----------------------------------------------------------

library(fasterize)
library(raster)
library(sf)
citation("fasterize")
packageVersion("raster")
file <- list.files("./Data/GARD Ranges/", pattern = ".shp", full.names = TRUE)[1]

ranges <- st_read(file)
gard.spp <- ranges$Binomial
# write.csv(x = as.data.frame(gard.spp),
#           file = "./Data/GARD Ranges/GARD Spp.csv")
gard.nonerpdb.spp <- gard.spp[!gard.spp %in% repdb$Species]
write.csv(x = data.frame(gardsp = gard.nonerpdb.spp),
          file = "./Data/GARD Ranges/GARD non-repDB.csv")
ranges.rast <- raster(ranges, res = 1/6)
full.raster <- fasterize(ranges, ranges.rast, fun="sum")

plot(full.raster)

flickr.fullresults <- read.csv("./Data/flickr_images_list_2020-06-28.csv", stringsAsFactors = FALSE)

fl.data <- flickr.fullresults %>% 
  dplyr::filter( !(photos.photo.latitude == 0 | is.na(photos.photo.latitude)),
                 !(photos.photo.longitude == 0 | is.na(photos.photo.longitude))) %>% 
  dplyr::filter(photos.photo.accuracy > 3) %>% # three is where it gets to country level
  dplyr::select(photos.photo.longitude, photos.photo.latitude, search) %>% 
  mutate(Source = "Flickr") %>% 
  rename("lon" = photos.photo.longitude, "lat" = photos.photo.latitude,
         "species" = search)

fl.data
reptile.rich <- raster::aggregate(full.raster, fact = 15, fun = max)
plot(reptile.rich)
r2 <- reptile.rich
r2[] <- NA
fl.data$cell <- cellFromXY(r2, fl.data[,1:2])

cell.counts <- fl.data %>% 
  group_by(cell) %>% 
  filter(!is.na(cell)) %>% 
  summarise(spp.n = length(unique(species)))

cell.counts %>% 
  ggplot() +
  geom_density(aes(x = spp.n))

length(r2[as.numeric(cell.counts$cell)]); length(cell.counts$spp.n)
r2[cell.counts$cell] <- cell.counts$spp.n

plot(r2)

spp.df <- rasterToPoints(r2)
spp.df <- data.frame(spp.df)
spp.df$source <- "Flickr"
names(spp.df) <- c("lon", "lat", "count", "source")

base.spp.df <- rasterToPoints(reptile.rich)
base.spp.df <- data.frame(base.spp.df)
base.spp.df$source <- "Base Reptile Richness (GARD)"
names(base.spp.df) <- c("lon", "lat", "count", "source")

species.richness.df <- rbind(spp.df, base.spp.df)

map <- map_data("world")

ggplot() +
  geom_polygon(data = map, aes(x = long, y = lat, group = group),
               fill = "grey85", alpha = 0.95) +
  geom_tile(data = species.richness.df, aes(x = lon, y = lat, fill = count),
            alpha = 1) +
  scale_fill_scico(palette = "roma") +
  scale_x_continuous(breaks = seq(-180, 180, 90)) +
  scale_y_continuous(breaks = seq(-90, 90, 45)) +
  facet_wrap(source~., ncol = 1) +
  coord_quickmap() +
  theme_bw() +
  labs(x = "Longitude", y = "Latitude", fill = "Number of\nspecies") +
  theme(axis.title.x = element_text(face = 2, hjust = 1),
        axis.title.y = element_text(face = 2, hjust = 1, angle = 0),
        strip.background = element_blank(),
        strip.text = element_text(face = 4, hjust = 0))

ggsave("./Figures/World heatmap.png", width = 160, height = 160,
       dpi = 300, units = "mm")
