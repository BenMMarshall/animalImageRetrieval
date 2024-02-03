
library(dplyr)
library(ggplot2)

repdb <- read.csv("./Data/reptile_checklist_2020_04.csv", stringsAsFactors = FALSE)
ph.data <- read.csv(file = "./Data/SuppData1_Species_Photo_Count_Table_2020-08-04_no_syn.csv", 
                    stringsAsFactors = FALSE)

names(ph.data)[1] <- "species"

tot.spp <- length(unique(ph.data$species))
dim(repdb)[1] - tot.spp

long.data <- ph.data %>% 
  select(-synonym, -flag, "Reptile.Database" = RDB) %>%
  tidyr::pivot_longer(cols = c("CalPhotos", "Flickr", "HerpMapper", "iNaturalist",
                               "Reptile.Database", "Wikimedia"),
                      names_to = "source",
                      values_to = "photos") %>% 
  mutate(photos = ifelse(is.na(photos), 0, photos))

sum(long.data$photos)

# Species per source figure -----------------------------------------------

library(ggplot2)

main.data <- long.data %>% 
  group_by(source) %>% 
  summarise(n.spp = sum(photos > 0),
            tot.spp = length(unique(repdb$Species)),
            percent = n.spp / tot.spp *100)

main.data$source[main.data$source == "Reptile.Database"] <- "Reptile\nDatabase"

(main.spp.plot <- main.data %>% 
    ggplot() +
    geom_col(aes(x = reorder(source, -n.spp), y = tot.spp),
             fill = "grey90", width = 0.75) +
    geom_col(aes(x = reorder(source, -n.spp), y = n.spp), fill = "darkred",
             width = 0.75) +
    geom_text(aes(x = source, y = n.spp,
                  label = n.spp), 
              colour = "darkred", fontface = 2, hjust = 0.5, vjust = -0.2) +
    geom_text(aes(x = source, y = n.spp,
                  label = paste0("(", round(percent, digits = 0), "%)")), 
              colour = "white", fontface = 2, hjust = 0.5, vjust = 1) +
    labs(x = "Source", y = "# of species\nwith images") +
    scale_y_continuous(sec.axis = dup_axis(name = "")) +
    theme_bw() +
    theme(
      axis.title.y = element_text(angle = 0, hjust = 1, face = 2),
      axis.title.x = element_text(face = 2, hjust = 0.5),
      axis.text = element_text(face = 3),
      axis.text.y.right = element_text(hjust = 0.5, margin = margin(0,0,0,10)),
      panel.grid.major.x = element_blank()
    )
)

(tot.spp.plot <- ggplot() +
    geom_col(aes(x = "Total", y = length(repdb$Species)),
             fill = "grey90", width = 0.75) +
    geom_col(aes(x = "Total", y = tot.spp),
             fill = "darkred", width = 0.75) +
    geom_text(aes(x = "Total", y = tot.spp, label = tot.spp), 
              colour = "darkred", fontface = 2, hjust = 0.5, vjust = -0.2) +
    geom_text(aes(x = "Total", y = tot.spp,
                  label = paste0("(", round(tot.spp/length(repdb$Species)*100, digits = 0),
                                 "%)")), 
              colour = "white", fontface = 2, hjust = 0.5, vjust = 1) +
    labs(x = "", y = "# of species\nwith images") +
    theme_bw() +
    theme(
      axis.title.y = element_text(angle = 0, hjust = 1, face = 2),
      axis.title.x = element_text(face = 2, hjust = 1),
      axis.text = element_text(face = 3)
    )
)

library(ggpubr)

ggarrange(main.spp.plot +
            theme(plot.margin = unit(c(1,0,1,1), "lines")),
          tot.spp.plot +
            rremove("y.axis") +
            rremove("y.text") +
            rremove("ylab") +
            theme(plot.margin = unit(c(1,1,1,0), "lines")),
          nrow = 1, ncol = 2, widths = c(4, 1),
          align = "h")

ggsave(file = "./Figures/Source species coverage.png",
       dpi = 300, width = 210, height = 160, units = "mm")  
ggsave(file = "./Figures/Source species coverage.pdf",
       width = 210, height = 160, units = "mm")  
ggsave(file = "./Figures/Source species coverage.tiff",
       dpi = 1200, width = 210, height = 160, units = "mm")  

# Photo per species summary -----------------------------------------------

overall.mean <-
  long.data %>%
  ungroup() %>% 
  filter(photos > 0) %>% 
  summarise(mean = mean(photos),
            se = sd(photos)/sqrt(length(photos)),
            max = max(photos),
            min = min(photos),
            median = median(photos),
            source = "Total")

mean.by.source <-
  long.data %>%
  group_by(source) %>% 
  filter(photos > 0) %>% 
  summarise(mean = mean(photos),
            se = sd(photos)/sqrt(length(photos)),
            max = max(photos),
            min = min(photos),
            median = median(photos)) %>% 
  mutate(source = ifelse(source == "Reptile.Database", "Reptile\nDatabase",
                         source))

mean.by.source$se[4]

pos <- position_jitter(width = 0.15, seed = 1)

library(ggrepel)

(per.species.plot <- 
    long.data %>% 
    filter(photos > 0) %>% 
    mutate(label = ifelse(photos >= tail(sort(long.data$photos), 5)[1],
                          species, ""),
           source = ifelse(source == "Reptile.Database", "Reptile\nDatabase",
                           source)) %>% 
    ggplot() +
    geom_point(aes(x = factor(source, 
                              level = c(
                                "iNaturalist",
                                "Flickr",
                                "HerpMapper",
                                "Reptile\nDatabase",
                                "Wikimedia",
                                "CalPhotos"
                              )),
                   y = photos),
               position = pos,
               colour = "darkred"
    ) +
    geom_vline(xintercept = c(1.5,2.5,3.5,4.5,5.5),
               linetype = 2, alpha = 0.25) +
    geom_text_repel(aes(x = source, y = photos, label = label),
                    position = pos,
                    xlim = c(1.2, 2),
                    fontface = 3
    ) +
    geom_text(data = mean.by.source, aes(x = source, y = -500,
                                         label = round(mean, digits = 2)),
              fontface = 4,
              vjust = 1) +
    scale_y_continuous(limits = c(-1000, 50000))+
    labs(x = "Source", y = "# of photos\nper species") +
    theme_bw() +
    theme(
      axis.title.y = element_text(angle = 0, hjust = 1, face = 2),
      axis.title.x = element_text(face = 2, hjust = 0.5),
      axis.text = element_text(face = 3),
      legend.position = "none",
      panel.grid.major.x = element_blank()
    )
)

(tot.per.species.plot <- ggplot(overall.mean) +
    geom_segment(aes(x = source, xend = source,
                     y = max, yend = min),
                 size = 1.5, position = position_nudge(x = -0.15)) +
    geom_point(aes(x = source, y = mean), colour = "black",
               size = 7, position = position_nudge(x = -0.15)) +
    geom_point(aes(x = source, y = mean), colour = "darkred",
               size = 5, position = position_nudge(x = -0.15)) +
    geom_text(aes(x = source, y = mean,
                  label = paste0("Mean\n", round(mean, digits = 2),
                                 "\n± ", round(
                                   se,
                                   digits = 2))),
              vjust = 0.5, hjust = 0, fontface = 4,
              size = 4,
              position = position_nudge(x = 0.0, y = 1000),
              lineheight = 0.85) +
    geom_text(aes(x = source, y = max,
                  label = paste0("Max\n", max)),
              vjust = 1, hjust = 0, fontface = 4,
              size = 4,
              position = position_nudge(x = 0.0, y = 0),
              lineheight = 0.85) +
    labs(x = "Source", y = "# of photos\nper species") +
    scale_y_continuous(limits = c(-1000, 50000))+
    theme_bw() +
    theme(
      axis.title.y = element_text(angle = 0, hjust = 1, face = 2),
      axis.title.x = element_text(face = 2, hjust = 0.5),
      axis.text = element_text(face = 3),
      legend.position = "none",
      panel.grid.major.x = element_blank()
    )
)

ggarrange(per.species.plot +
            theme(plot.margin = unit(c(1,1,1,1), "lines")),
          tot.per.species.plot +
            rremove("y.axis") +
            rremove("y.text") +
            rremove("ylab") +
            rremove("y.ticks") +
            rremove("x.title") +
            theme(plot.margin = unit(c(1,1,1,0), "lines")),
          nrow = 1, ncol = 2, widths = c(4, 1),
          align = "h")

ggsave(file = "./Figures/Photos per species.png",
       dpi = 300, width = 210, height = 160, units = "mm")  
ggsave(file = "./Figures/Photos per species.pdf",
       width = 210, height = 160, units = "mm")  
ggsave(file = "./Figures/Photos per species.tiff",
       dpi = 1200, width = 210, height = 160, units = "mm")  


# Clade summary -----------------------------------------------------------

library(stringr)

repdb$clade <- str_extract(repdb$Familyetc,
                           "Amphisbaenia|Serpentes|Testudines|Sauria|Crocodylia|Rhynchocephalia")

sp.per.clade <- repdb %>% 
  rename("species" = Species) %>% 
  dplyr::select(species, clade) %>% 
  group_by(clade) %>% 
  mutate(spp.clade = n())

long.clade <- long.data %>% 
  left_join(sp.per.clade)

per.clade.summary <- long.clade %>% 
  mutate(photos = photos > 0) %>% 
  group_by(species) %>% 
  summarise(clade = clade[1],
            photos = any(photos),
            spp.clade = spp.clade[1]) %>% 
  group_by(clade) %>% 
  summarise(n.w.photos = sum(photos, na.rm = TRUE),
            percent = n.w.photos / spp.clade *100,
            spp.clade = max(spp.clade)) %>% 
  slice(n = 1) %>% 
  filter(!is.na(clade))

(per.clade.plot <- per.clade.summary %>% 
    ggplot() +
    geom_col(aes(x = reorder(clade, -n.w.photos), y = spp.clade),
             fill = "grey90", width = 0.75) +
    geom_col(aes(x = reorder(clade, -n.w.photos), y = n.w.photos), fill = "darkred",
             width = 0.75) +
    geom_text(aes(x = reorder(clade, -n.w.photos), y = n.w.photos, 
                  label = paste0(n.w.photos, " (", round(percent, digits = 0), "%)")
    ), 
    colour = "darkred", fontface = 2, hjust = 0.5, vjust = -0.2,
    size = 3) +
    labs(x = "Clade", y = "# of species\nwith images") +
    scale_y_continuous() +
    theme_bw() +
    theme(
      axis.title.y = element_text(angle = 0, hjust = 1, face = 2),
      axis.title.x = element_text(face = 2, hjust = 0.5),
      axis.text = element_text(face = 3),
      axis.text.y.right = element_text(hjust = 0.5, margin = margin(0,0,0,10)),
      panel.grid.major.x = element_blank()
    ))

ggsave(per.clade.plot, file = "./Figures/Photos per species by clade.png",
       dpi = 300, width = 180, height = 120, units = "mm")
ggsave(per.clade.plot, file = "./Figures/Photos per species by clade.pdf",
       width = 180, height = 120, units = "mm")
ggsave(per.clade.plot, file = "./Figures/Photos per species by clade.tiff",
       dpi = 1200, width = 180, height = 120, units = "mm")

# Tree plot ---------------------------------------------------------------

library(ggplot2)
library(ape)
library(stringr)
library(ggtree)
library(scico)
library(cowplot)
print(citation("ape"), bibtex = TRUE)

repdb <- read.csv("./Data/reptile_checklist_2020_04.csv", stringsAsFactors = FALSE)
tree.list <- lapply(list.files("./Data/TimeTree Data/", pattern = ".nwk",
                               full.names = TRUE), read.tree)

names(tree.list) <- c("Crocodylia", "Lepidosauria", "Testudines")
class(tree.list) <- "multiPhylo"

# make all the branches relative to that group
tree.list <- lapply(tree.list, function(x){
  x$edge.length <- x$edge.length/max(x$edge.length)
  x
})

tree.list[[2]]$tip.label[tree.list[[2]]$tip.label == "Xenodermatidae"] <- "Xenodermidae"
tree.list[[2]]$tip.label[tree.list[[2]]$tip.label == "Trogonophiidae"] <- "Trogonophidae"
tree.list[[2]]$tip.label[tree.list[[2]]$tip.label == "Pareatidae"] <- "Pareidae"
tree.list[[2]]$tip.label[tree.list[[2]]$tip.label == "Bolyeridae"] <- "Bolyeriidae"

comb.tree <- bind.tree(tree.list[[1]], tree.list[[2]])
comb.tree <- bind.tree(comb.tree, tree.list[[3]])

repdb$clade <- str_extract(repdb$Familyetc,
                           "Amphisbaenia|Serpentes|Testudines|Sauria|Crocodylia|Rhynchocephalia")
# "Atractaspididae" +
# "Psammophiidae" +
# "Pseudoxyrhophiidae" to be "Lamprophiidae"
repdb$Familyetc <- str_replace_all(repdb$Familyetc, paste0(c("Atractaspididae",
                                                             "Psammophiidae",
                                                             "Pseudoxyrhophiidae"), collapse = "|"),
                                   "Lamprophiidae")

# "Cyclocoridae" +
# "Pseudaspididae" +
# "Prosymnidae" to "Colubridae"
repdb$Familyetc <- str_replace_all(repdb$Familyetc, paste0(c("Cyclocoridae",
                                                             "Pseudaspididae",
                                                             "Prosymnidae"), collapse = "|"),
                                   "Colubridae")
# "Xenotyphlopidae" to "Typhlopidae"
repdb$Familyetc <- str_replace_all(repdb$Familyetc, "Xenotyphlopidae",
                                   "Typhlopidae")

repdb$family <- str_extract(repdb$Familyetc,
                            paste0(comb.tree$tip.label, collapse = "|"))

repdb$family[is.na(repdb$family)] <- str_extract(repdb$Familyetc[is.na(repdb$family)], "Iguania")

repdb$family[which(repdb$family == "Iguania")] <- "Iguanidae"

sort(comb.tree$tip.label)
unique(repdb$Familyetc[is.na(repdb$family)])

repdb$family[str_detect(repdb$Species, "Buhoma")] <- "Lamprophiidae"

# remaining problems
# "Alopoglossidae" problem
# Diploglossidae problem
# Xenophidiidae problem

fam.clade.df <- repdb %>% 
  rename("species" = Species) %>% 
  dplyr::select(species, clade, family)

spp.photos <- long.data %>% 
  group_by(species) %>% 
  summarise(photos = sum(photos))

fam.summary <- fam.clade.df %>% 
  group_by(family) %>% 
  mutate(n.spp = length(unique(species))) %>% 
  left_join(spp.photos, by = "species") %>% 
  ungroup() %>% 
  mutate(photos = ifelse(is.na(photos), 0, photos)) %>%
  group_by(family) %>% 
  mutate(n.spp.ph = length(unique(species[photos > 0])),
         per.spp.ph = n.spp.ph / n.spp *100) %>% 
  summarise(n.photos = sum(photos, na.rm = TRUE),
            n.spp = max(n.spp),
            n.spp.ph = max(n.spp.ph),
            per.spp.ph = max(per.spp.ph)
  ) %>% 
  left_join(fam.clade.df %>% select(clade, family)) %>% 
  rename("tip.label" = family)

tree.list[[2]] <- drop.tip(tree.list[[2]], c("Hydrophiidae",
                                             "Anniellidae",
                                             "Dipsadidae"))

make_tree <- function(tree.data, tip.data, sel.clade){
  
  # tree.data <- tree.list[[2]]
  # sel.clade <- l.clades
  
  l.df <- fam.summary %>% 
    filter(
      clade %in% sel.clade) %>%
    filter(!is.na(tip.label)) %>% 
    group_by(tip.label) %>% 
    slice(n = 1) %>% 
    ungroup()
  
  l.tree <- ggtree(tree.data) %<+% l.df
  
  plot.tree <- l.tree +
    geom_tippoint(aes(
      # colour = per.spp.ph, 
      # size = n.photos
      # colour = n.photos,
      # size = per.spp.ph
                      )) +
    geom_tiplab(aes(), offset = 0.05, size = 3) +
    scale_x_continuous(expand = c(0.4,0.4),
                       limits = c(0, 1.5)) +
    scale_y_discrete(expand =
                       if(sel.clade[1] == "Crocodylia"){
                         c(0.1, 0.1)
                       } else {
                         c(0.05, 0.05)
                       }
    ) +
    scale_colour_scico(palette = "lajolla", begin = 0.2, end = 1, 
                       direction = -1, trans = "log",
                       breaks = c(0, 100, 1000, 10000, 100000),
                       labels = c("0", "100", "1,000", "10,000", "100,000")) +
    coord_cartesian(clip = "off") +
    scale_size_continuous(
      # breaks = c(0, 1000, 10000, 100000),
                          range = c(0.95, 3.5),
                          # labels = c("0", "1,000", "10,000", "100,000"),
                          # trans = "log"
                          ) +
    labs(size = "% of species\nwith photos", colour = "Number of\nphotos") +
    theme_tree() +
    theme(legend.title = element_text(face = 2),
          legend.position = c(0.25, 0.65)
    )
  
  d <- fortify(tree.data)
  d <- subset(d, isTip)
  tip.order <- with(d, label[order(y, decreasing = TRUE)])
  
  bar.plot <- l.df %>%
    mutate(tip.label = factor(tip.label, levels = rev(tip.order))) %>%
    arrange(tip.label) %>% 
    ggplot() +
    geom_col(aes(y = tip.label, x = n.spp),
             width = 0.65, fill = "grey85") +
    geom_col(aes(y = tip.label, x = n.spp.ph),
             fill = "darkred",
             width = 0.65) +
    geom_text(aes(y = tip.label, x = n.spp, label = paste0(
      round(per.spp.ph, digits = 1), "%")),
      fontface = 2, hjust = 0, vjust = 0.5) +
    scale_x_continuous(expand = expansion(c(0,0.2), c(0,0))) +
    labs(x = "Species") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          # axis.title.x = element_text(face = 2),
          panel.background = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          plot.background = element_blank(),
          legend.title = element_text(face = 4),
          legend.position = c(0.12, 0.22),
          # axis.title = element_blank(),
          # axis.text = element_blank(),
          legend.direction = "horizontal"
          )
  
  return(
    # ggarrange(plot.tree +
    #           theme(plot.margin = unit(c(0,0,0,0), "lines")),
    #         bar.plot +
    #           theme(plot.margin = unit(c(0,0,0,0), "lines")),
    #         nrow = 1, align = "v")
         
    plot_grid(plot.tree, bar.plot, nrow = 1,
              align = "v",
              axis = "tb")
         
         )
  
}

l.clades <- unique(fam.summary$clade[!fam.summary$clade %in%
                                       c("Crocodylia", "Testudines")])

library(ggpubr)
pl.list <- list(make_tree(tree.list[[1]], fam.summary, "Crocodylia") +
                  annotate("text", x=1.5, y=3, label = "Crocodylia",
                           fontface = 2, hjust = -0.5) +
                  # theme(plot.margin = unit(c(0,0,0,2), "lines")) +
                  rremove("legend"),
                make_tree(tree.list[[2]], fam.summary, l.clades) +
                  annotate("text", x=1.45, y=53, label = "Lepidosauria",
                           fontface = 2, hjust = -0.5) +
                  # theme(plot.margin = unit(c(0,0,0,2), "lines")) +
                rremove("legend"),
                make_tree(tree.list[[3]], fam.summary, "Testudines") +
                  annotate("text", x=1.5, y=14, label = "Testudines",
                           fontface = 2, hjust = -1) +
                  # theme(plot.margin = unit(c(0,3.4,0,2), "lines")) +
                  rremove("legend")
)

pl.list[[1]]
pl.list[[2]]
pl.list[[3]]

lapply(1:3, function(x){
  y <- pl.list[[x]]
  ggsave2(y, file = paste0("./Figures/Tree summary_", x, ".png"),
          dpi = 300, width = 160, height = 220, units = "mm")  
  ggsave2(y, file = paste0("./Figures/Tree summary_", x, ".pdf"),
          width = 160, height = 220, units = "mm")  
})

# pg <- plot_grid(plotlist = pl.list, nrow = 3,
#                 rel_heights = c(0.75,8,3),
#                 axis = "r")
# plot(pg)
# ggsave2(pg, file = "./Figures/Tree summary.png",
#         dpi = 300, width = 160, height = 220, units = "mm")


# GARD map ----------------------------------------------------------------

library(fasterize)
library(raster)
library(sf)

ph.data <- read.csv(file = "./Data/SuppData1_Species_Photo_Count_Table_2020-08-04_no_syn.csv", 
                    stringsAsFactors = FALSE)
names(ph.data)[1] <- "species"

file <- list.files("./Data/GARD Ranges/", pattern = ".shp", full.names = TRUE)[1]

ranges <- st_read(file)

GARD.spp.fix <- read.csv(file = "./Data/GARD Ranges/GARD non-repDB_fixed.csv", 
                         stringsAsFactors = FALSE)
names(GARD.spp.fix)[1] <- "gardsp"
ranges$Binomial <- as.character(ranges$Binomial)

# replace the old names in GARD with the fixed repDB names
for(sp in GARD.spp.fix$gardsp){
  # sp <- "Acanthocercus annectens"
  ranges[ranges$Binomial == sp,]$Binomial <- 
    GARD.spp.fix[GARD.spp.fix$gardsp == sp,]$currnet_species
}
ranges$has_photos <- 0
ranges$has_photos <- ifelse(ranges$Binomial %in% ph.data$species,
                            1, 0)

###
ranges.rast <- raster(ranges, res = 1/6)
full.raster <- fasterize(ranges, ranges.rast, fun="sum")
photos.raster <- fasterize(ranges[ranges$has_photos == 1,], ranges.rast, fun="sum")

# plot(full.raster)
# plot(full.raster - photos.raster)

miss.spp.rast <- full.raster - photos.raster
miss.spp.df <- rasterToPoints(miss.spp.rast)
miss.spp.df <- data.frame(miss.spp.df)
names(miss.spp.df) <- c("lon", "lat", "count")

world.poly <- map_data("world")

library(scico)

highlight.label <- miss.spp.df[miss.spp.df$count == max(miss.spp.df$count),]
mean(highlight.label$lon)
mean(highlight.label$lat)
miss.spp.df %>% 
  mutate(rlat = round(lat, digits = 0),
         rlon = round(lon, digits = 0)) %>% 
  group_by(rlat, rlon) %>% 
  mutate(mcount = max(count)) %>% 
  slice(n = 1) %>% 
  arrange(desc(mcount)) %>% 
  filter(mcount > 20)
# simplfy this to a trio of hotspots
highlight.label <- data.frame(lon = c(9.85, -75.4, 125), 
                              lat = c(4.304777, -10.6, 9.44),
                              lablon = c(-15, -135, 160),
                              lablat = c(-15, 10, 20),
                              count = c(27, 21, 21)) %>% 
  mutate(label = paste0(count,"\nmissing"))

(map.plot <- miss.spp.df %>% 
    # filter(count > 0) %>% 
    ggplot() +
    geom_polygon(data = world.poly, aes(x = long, y = lat, group = group)) +
    geom_tile(aes(x = lon, y = lat, fill = count, alpha = count > 1)) +
    geom_text(data = data.frame(x = c(-178,-90,90,178), y = rep(0,4)),
              aes(x = x, y = y),
              label = c(-180,-90,90,180), vjust = 0.5, hjust = 0.5,
              size = 3, colour = "grey25") +
    geom_text(data = data.frame(y = seq(-45, 45, 45), x = rep(0,3)),
              aes(x = x, y = y),
              label = seq(-45, 45, 45), vjust = 0.5, hjust = 0.5,
              size = 3, colour = "grey25") +
    geom_curve(data = highlight.label[c(1,3),],
               aes(xend = lon, yend = lat, x = lablon, y = lablat),
               curvature = -0.4, size = 1,
               arrow = arrow(length = unit(2, "mm"),
                             type = "closed")) +
    geom_text(data = highlight.label[1,],
              aes(x = lablon, y = lablat, label = label),
              lineheight = 0.85, fontface = 2, hjust = 0.5,
              vjust = 1.2) +
    geom_curve(data = highlight.label[2,],
               aes(xend = lon, yend = lat, x = lablon, y = lablat),
               curvature = 0.4, size = 1,
               arrow = arrow(length = unit(2, "mm"),
                             type = "closed")) +
    geom_text(data = highlight.label[3:2,],
              aes(x = lablon, y = lablat, label = label),
              lineheight = 0.85, fontface = 2, hjust = 0.5,
              vjust = -0.2) +
    scale_fill_scico(palette = "lajolla", direction = -1, end = 0.8,
                     breaks = c(0, seq(10, 30, 10), 27)) +
    scale_alpha_manual(values = c(0.5, 1)) +
    labs(x = "", y = "", fill = "Number of species\nwithout photos") +
    coord_quickmap(xlim = c(-180, 180), ylim = c(-77, 80),
                   clip = "off") +
    scale_x_continuous(breaks = seq(-180, 180, 90)) +
    scale_y_continuous(breaks = seq(-90, 90, 45)) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.background = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          legend.title = element_text(face = 4),
          legend.position = c(0.12, 0.22),
          legend.direction = "horizontal"
    ) +
    guides(fill = guide_colourbar(title.position = "top",
                                  barheight = unit(2, "mm"),
                                  ticks.colour = "black"),
           alpha = guide_none()))

ggsave(map.plot, file = "./Figures/Species miss photos map.png",
       dpi = 300, width = 220, height = 150, units = "mm")



percent.spp.rast <- photos.raster / full.raster *100

percent.spp.df <- rasterToPoints(percent.spp.rast)
percent.spp.df <- data.frame(percent.spp.df)
names(percent.spp.df) <- c("lon", "lat", "count")

library(ggpubr)
library(scico)

highlight.label.per <- percent.spp.df[percent.spp.df$count == min(percent.spp.df$count),]

percent.spp.df %>% 
  mutate(rlat = round(lat, digits = 0),
         rlon = round(lon, digits = 0),
         zone = case_when(
           lon < -30 ~ "Americas",
           lon >=- 30 & lon < 55 ~ "AfricaEur", 
           lon > 55 ~ "AsiaAus")
         ) %>% 
  group_by(rlat, rlon) %>% 
  mutate(mcount = min(count)) %>% 
  slice(n = 1) %>%
  group_by(zone) %>% 
  top_n(1, desc(mcount)) %>% 
  pull(lat)
# simplfy this to a trio of hotspots
highlight.label.per <- data.frame(lon = c(146.58333, 42.75, -67.91667), 
                                  lat = c(-8.56189, 16.43811, 18.10478),
                                  lablon = c(165, 65, -45),
                                  lablat = c(15, -15, 35),
                                  count = c(33, 50, 50)) %>% 
  mutate(label = paste0(round(count, digits = 0), "%"))

(mapper.plot <- percent.spp.df %>% 
    filter(count > 0) %>% 
    ggplot() +
    geom_polygon(data = world.poly, aes(x = long, y = lat, group = group)) +
    geom_tile(aes(x = lon, y = lat, fill = count)) +
    geom_text(data = data.frame(x = c(-178,-90,90,178), y = rep(0,4)),
              aes(x = x, y = y),
              label = c(-180,-90,90,180), vjust = 0.5, hjust = 0.5,
              size = 3, colour = "grey25") +
    geom_text(data = data.frame(y = seq(-45, 45, 45), x = rep(0,3)),
              aes(x = x, y = y),
              label = seq(-45, 45, 45), vjust = 0.5, hjust = 0.5,
              size = 3, colour = "grey25") +
    geom_curve(data = highlight.label.per[c(1,3),],
               aes(xend = lon, yend = lat, x = lablon, y = lablat),
               curvature = -0.4, size = 1,
               arrow = arrow(length = unit(2, "mm"),
                             type = "closed")) +
    geom_text(data = highlight.label.per[c(1,3),],
              aes(x = lablon, y = lablat, label = label),
              lineheight = 0.85, fontface = 2, hjust = 0.5,
              vjust = -0.2) +
    geom_curve(data = highlight.label.per[2,],
               aes(xend = lon, yend = lat, x = lablon, y = lablat),
               curvature = 0.4, size = 1,
               arrow = arrow(length = unit(2, "mm"),
                             type = "closed")) +
    geom_text(data = highlight.label.per[2,],
              aes(x = lablon, y = lablat, label = label),
              lineheight = 0.85, fontface = 2, hjust = 0.5,
              vjust = 1.2) +
    scale_fill_scico(palette = "lajolla", direction = -1, end = 1,
                     begin = 0.2,
                     breaks = seq(0,100,20),
                     limits = c(30, 100)) +
    labs(x = "", y = "", fill = "% of species\nwith photos") +
    coord_quickmap(xlim = c(-180, 180), ylim = c(-77, 80),
                   clip = "off") +
    scale_x_continuous(breaks = seq(-180, 180, 90)) +
    scale_y_continuous(breaks = seq(-90, 90, 45)) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.background = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          legend.title = element_text(face = 4),
          legend.position = c(0.12, 0.22),
          legend.direction = "horizontal"
    ) +
    guides(fill = guide_colourbar(title.position = "top",
                                  barheight = unit(2, "mm"),
                                  ticks.colour = "black")))

ggsave(mapper.plot, file = "./Figures/Species percent photos map.png",
       dpi = 300, width = 220, height = 150, units = "mm")


# Country map -------------------------------------------------------------

country.data <- read.csv(file = "./Data/SuppData3_Country_species_counts.csv",
         stringsAsFactors = FALSE)

names(country.data) <- c("country", "species", "species.photo", "percent", "man.corr")

world.map <- map_data("world")

unique(country.data$country)[!unique(country.data$country) %in% unique(world.map$region)]
unique(world.map$region)[!unique(world.map$region) %in% unique(country.data$country)]

country.data <- country.data %>% 
  mutate(country = case_when(
    country == "Bosnia Herzegovina" ~ "Bosnia and Herzegovina",
    country == "Brunei Darussalam" ~ "Brunei",
    country == "Congo" ~ "Republic of Congo",
    country == "Guinea (Conakry)" ~ "Guinea",
    country == "Saint Kitts  & Nevis" ~ "Saint Kitts",
    country == "Saint Vincent & Grenadines" ~ "Grenadines",
    country == "Sao Tome  Principe" ~ "Sao Tome and Principe",
    country == "Timor Leste" ~ "Timor-Leste",
    country == "Trinidad Tobago" ~ "Tobago",
    country == "Turks Caicos Islands" ~ "Turks and Caicos Islands",
    country == "United Kingdom" ~ "UK",
    country == "British Virgin Islands" ~ "Virgin Islands",
    TRUE ~ country
  ))

mapping.data <- world.map %>% 
  rename("country" = region) %>% 
  left_join(country.data)

top3.highlight <- mapping.data %>% 
  mutate(zone = case_when(
    long < -30 ~ "Americas",
    long >=- 30 & long < 55 ~ "AfricaEur", 
    long > 55 ~ "AsiaAus")) %>% 
  group_by(country) %>% 
  slice(n = 1) %>% 
  group_by(zone) %>% 
  top_n(1, species.photo) %>% 
  ungroup() %>% 
  mutate(xlab = c(80,-145,-20),
         ylab = c(-30,15,-35),
         xarr = c(135,-105,20),
         yarr = c(-22,24,-30))

(chloro.map <- mapping.data %>% 
    ggplot() +
    geom_polygon(aes(x = long, y = lat, group = group, fill = species.photo),
                 # colour = "grey85",
                 size = 0.25) +
    geom_text(data = data.frame(x = c(-178,-90,90,178), y = rep(0,4)),
              aes(x = x, y = y),
              label = c(-180,-90,90,180), vjust = 0.5, hjust = 0.5,
              size = 3, colour = "grey25") +
    geom_text(data = data.frame(y = seq(-90, 90, 45), x = rep(0,5)),
              aes(x = x, y = y),
              label = seq(-90, 90, 45), vjust = 0.5, hjust = 0.5,
              size = 3, colour = "grey25") +
    geom_text(data = top3.highlight[c(1,3),],
              aes(x = xlab, y = ylab, label = paste0(species, "\nspecies")),
              fontface = 2, hjust = 0.5, vjust = 1.2,
              lineheight = 0.85) +
    geom_curve(data = top3.highlight[c(1,3),],
               aes(xend = xarr, yend = yarr, x = xlab, y = ylab),
               curvature = -0.4, size = 1,
               arrow = arrow(length = unit(2, "mm"),
                             type = "closed")) +
    geom_text(data = top3.highlight[2,],
              aes(x = xlab, y = ylab, label = paste0(species, "\nspecies")),
              fontface = 2, hjust = 0.5, vjust = -0.2,
              lineheight = 0.85) +
    geom_curve(data = top3.highlight[2,],
               aes(xend = xarr, yend = yarr, x = xlab, y = ylab),
               curvature = 0.4, size = 1,
               arrow = arrow(length = unit(2, "mm"),
                             type = "closed")) +
    scale_fill_scico(palette = "lajolla", na.value = "grey15",
                     begin = 0.2, direction = -1,
                     breaks = c(0,100,400,1000),
                     limits = c(0, 1200),
                     trans = "sqrt") +
    scale_x_continuous(breaks = seq(-180, 180, 90)) +
    scale_y_continuous(breaks = seq(-90, 90, 45)) +
    coord_quickmap(xlim = c(-180, 180), ylim = c(-77, 80),
                   clip = "off") +
    labs(x = "", y = "", fill = "Number of species\nwith photos") +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.background = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          legend.title = element_text(face = 4),
          legend.position = c(0.12, 0.22),
          legend.direction = "horizontal"
    ) +
    guides(fill = guide_colourbar(title.position = "top",
                                  barheight = unit(2, "mm"),
                                  ticks.colour = "black")))

ggsave(chloro.map, file = "./Figures/Species chloro spp map.png",
       dpi = 300, width = 220, height = 150, units = "mm")


lowlights <- mapping.data %>% 
  mutate(zone = case_when(
    long < -30 ~ "Americas",
    long >=- 30 & long <= 60 ~ "AfricaEur", 
    long > 60 ~ "AsiaAus")) %>% 
  group_by(country) %>% 
  slice(n = 1) %>% 
  group_by(zone) %>% 
  top_n(1, desc(percent)) %>% 
  ungroup() %>% 
  mutate(xlab = c(-40,165,70),
         ylab = c(30,25,-15),
         yarr = c(mean(mapping.data[mapping.data$country == "Haiti",]$lat),
                  mean(mapping.data[mapping.data$country == "Papua New Guinea",]$lat),
                  mean(mapping.data[mapping.data$country == "Somalia",]$lat)
         ),
         xarr = c(mean(mapping.data[mapping.data$country == "Haiti",]$long),
                  mean(mapping.data[mapping.data$country == "Papua New Guinea",]$long),
                  mean(mapping.data[mapping.data$country == "Somalia",]$long) +2.5
         ))

(chloro.per.map <- mapping.data %>% 
    ggplot() +
    geom_polygon(aes(x = long, y = lat, group = group, fill = percent),
                 # colour = "grey85",
                 size = 0.25) +
    geom_text(data = data.frame(x = c(-178,-90,90,178), y = rep(0,4)),
              aes(x = x, y = y),
              label = c(-180,-90,90,180), vjust = 0.5, hjust = 0.5,
              size = 3, colour = "grey25") +
    geom_text(data = data.frame(y = seq(-90, 90, 45), x = rep(0,5)),
              aes(x = x, y = y),
              label = seq(-90, 90, 45), vjust = 0.5, hjust = 0.5,
              size = 3, colour = "grey25") +
    geom_text(data = lowlights[1:2,],
              aes(x = xlab, y = ylab, label = paste0(round(percent,
                                                           digits = 0), "%")),
              fontface = 2, hjust = 0.5, vjust = -0.2,
              lineheight = 0.85) +
    geom_curve(data = lowlights[1:2,],
               aes(xend = xarr, yend = yarr, x = xlab, y = ylab),
               curvature = -0.4, size = 1,
               arrow = arrow(length = unit(2, "mm"),
                             type = "closed")) +
    geom_text(data = lowlights[3,],
              aes(x = xlab, y = ylab, label = paste0(round(percent,
                                                           digits = 0), "%")),
              fontface = 2, hjust = 0.5, vjust = 1.2,
              lineheight = 0.85) +
    geom_curve(data = lowlights[3,],
               aes(xend = xarr, yend = yarr, x = xlab, y = ylab),
               curvature = 0.4, size = 1,
               arrow = arrow(length = unit(2, "mm"),
                             type = "closed")) +
    scale_fill_scico(palette = "lajolla", na.value = "grey15",
                     begin = 0.2, direction = -1,
                     breaks = seq(50, 100, 25),
                     limits = c(50, 100)) +
    scale_x_continuous(breaks = seq(-180, 180, 90)) +
    scale_y_continuous(breaks = seq(-90, 90, 45)) +
    coord_quickmap(xlim = c(-180, 180), ylim = c(-77, 80),
                   clip = "off") +
    labs(x = "", y = "", fill = "% of species\nwith photos") +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.background = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          legend.title = element_text(face = 4),
          legend.position = c(0.12, 0.22),
          legend.direction = "horizontal"
    ) +
    guides(fill = guide_colourbar(title.position = "top",
                                  barheight = unit(2, "mm"),
                                  ticks.colour = "black")))

ggsave(chloro.per.map, file = "./Figures/Species chloro percent map.png",
       dpi = 300, width = 220, height = 150, units = "mm")

ggarrange(map.plot +
            theme(plot.margin = unit(c(1,1,1,0), "lines")),
           mapper.plot +
            theme(plot.margin = unit(c(1,1,1,0), "lines")),
          chloro.map +
            theme(plot.margin = unit(c(1,1,1,0), "lines")),
          chloro.per.map +
            theme(plot.margin = unit(c(1,1,1,0), "lines")),
          nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"),
          align = "hv")

ggsave(file = "./Figures/Maps combined.png",
       dpi = 300, width = 320, height = 160, units = "mm")
ggsave(file = "./Figures/Maps combined.pdf",
       width = 320, height = 160, units = "mm")
ggsave(file = "./Figures/Maps combined.tiff",
       dpi = 1200, width = 320, height = 160, units = "mm")

