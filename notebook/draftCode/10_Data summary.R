
library(dplyr)
library(urltools)

repdb <- read.csv("./Data/reptile_checklist_2020_04.csv", stringsAsFactors = FALSE)

hmapper <- read.csv("./Data/herpmapper_images.csv", stringsAsFactors = FALSE)
repdb.photos <- read.csv("./Data/Reptile Database photos July 2020.csv", stringsAsFactors = FALSE)
# inat.meta <- read.csv("./Data/PRELIM_inat_meta.csv", stringsAsFactors = FALSE)
inat.meta <- read.csv("./Data/inat_reptile_photos_Uetz retrieval.csv", stringsAsFactors = FALSE)
# flickr.meta <- read.csv("./Data/PRELIM_flickr_meta.csv", stringsAsFactors = FALSE)
# flickr.meta <- readRDS("./Data/flickr_meta_2020-06-28.Rds")
# flickr.meta <- do.call(rbind, flickr.meta)
# flickr.meta$search <- as.character(flickr.meta$search)
# flickr.meta$results <- as.numeric(flickr.meta$results)

#### MAJOR SCREW UP WITH THE FLICKR META FILE, DUPLICATED VALUES WHEN NO IMAGES
##FOUND THIS IS A HIDEOUS FIX, using the raw flickr data, undermines the meta
##file as verification device, needs to be re0run with a fixed flickr function
flickr.fullresults <- read.csv("./Data/flickr_images_list_2020-06-28.csv", stringsAsFactors = FALSE)
flickr.meta <- flickr.fullresults %>% 
  count(search)
######################
flickr.spp <- flickr.meta[flickr.meta$n>0,]$search

cal.data <- read.csv("./Data/CalPhotos reptile species.csv", stringsAsFactors = FALSE)
cal.spp <- unique(cal.data$species)

wiki.data <- read.csv("./Data/wiki_images_2020-05-26_filtered.csv", stringsAsFactors = FALSE)
wiki.syn.data <- read.csv("./Data/wiki_images_synonyms_filtered_2020-06-19.csv",
                          stringsAsFactors = FALSE)
names(wiki.data)
wiki.data <- wiki.data %>% 
  mutate(acc.species = url_decode(searched))
names(wiki.syn.data)

wiki.meta <- wiki.data %>% 
  rbind(wiki.syn.data) %>% 
  group_by(acc.species) %>% 
  filter(!duplicated(image.url)) %>% 
  count()

# inat.meta <- inat.meta %>% 
#   mutate(photos = ifelse(!is.na(photos), photos, 0))

# UETZ data inat
inat.meta <- inat.meta %>% 
  rename("species" = name) 

# there are 100 names in the iNat sheet that need to be fixed
inat.fixes <- read.csv("./Data/missing spp fixes inat.csv", stringsAsFactors = FALSE)
inat.fixes$repdb.sp[inat.fixes$fixed.sp == "no valid binomial"]
inat.fixes$repdb.sp <- sub("syn ", "", inat.fixes$repdb.sp)

library(stringr)

inat.fixes <- inat.fixes %>% 
  filter(!repdb.sp == "") %>% 
  mutate(fixed.sp = ifelse(!str_detect(repdb.sp, " "),
                           paste0(repdb.sp, " ", word(org.sp, 2, 2)), repdb.sp)
  )

for(i in 1:length(inat.fixes$org.sp)){
  inat.meta$species[inat.meta$species == inat.fixes$org.sp[i]] <- inat.fixes$fixed.sp[i]
}

# set-up a dataframe for number of photos per species
names(repdb.photos) <- c("species", "n")
repdb.photos$source <- "Reptile Database"
wiki.meta.comb <- wiki.meta %>% 
  rename("species" = acc.species) %>% 
  mutate(source = "Wikimedia")
inat.meta.comb <- inat.meta %>% 
  dplyr::select(species, photos) %>% 
  filter(photos > 0) %>% 
  rename("n" = photos) %>% 
  group_by(species) %>% 
  summarise(n = sum(n)) %>% 
  mutate(source = "iNaturalist")
flickr.meta.comb <- flickr.meta %>% 
  rename("species" = search) %>% 
  mutate(source = "Flickr")
hm.meta.comb <- hmapper %>% 
  count(species) %>% 
  mutate(source = "HerpMapper")
cal.meta.comb <- cal.data %>% 
  count(species) %>% 
  mutate(source = "CalPhotos")
photos.per.species <- rbind(repdb.photos, as.data.frame(wiki.meta.comb), inat.meta.comb,
                            flickr.meta.comb, hm.meta.comb, cal.meta.comb)

# photos.per.species$source <- as.factor(photos.per.species$source)
summary.species.table <- tidyr::spread(data = photos.per.species,
                                       key = source, value = n, fill = 0)

# write.csv(x = summary.species.table, file = "./Data/Summary Species Photo Count Table_2020-07-20.csv",
#           row.names = FALSE)

# get species lists
repdb.photos.spp <- unique(repdb.photos$species)
inat.spp <- inat.meta[inat.meta$photos>0,]$species
wiki.spp <- wiki.meta$acc.species
repdb.spp <- repdb$Species
cal.spp

hm.spp <- hm.meta.comb$species
# remove the subspecies text
hm.spp <- word(hm.spp, 1, 2)
# remove the NAs only listed to genus
hm.spp <- hm.spp[!is.na(hm.spp)]
hm.spp <- unique(hm.spp)

sum(repdb.photos.spp %in% repdb.spp)
sum(inat.spp %in% repdb.spp)
sum(wiki.spp %in% repdb.spp)
sum(flickr.spp %in% repdb.spp)
sum(cal.spp %in% repdb.spp)
sum(hm.spp %in% repdb.spp)

# there are a number of HM species that aren't in repBD, some of it is
# caused by the subspecies stuff
sum(!hm.spp %in% repdb.spp)
hm.spp[!hm.spp %in% repdb.spp]
# write.csv(x = data.frame(hmsp = hm.spp[!hm.spp %in% repdb.spp]),
#           file = "./Data/hm species non-repDB.csv")

tot.spp <- sum(unique(c(repdb.photos.spp, inat.spp, wiki.spp, flickr.spp, cal.spp,
                        hm.spp)) %in% repdb.spp)

main.data <- data.frame(
  "source" = factor(c( 
    "Reptile\nDatabase",
    "iNaturalist",
    "Wikimedia",
    "CalPhotos",
    "HerpMapper",
    "Flickr"
  ), levels = c(
    "Reptile\nDatabase",
    "iNaturalist", 
    "Wikimedia", 
    "CalPhotos",
    "HerpMapper", 
    "Flickr"
  )),
  "species" = c(
    sum(repdb.photos.spp %in% repdb.spp),
    sum(inat.spp %in% repdb.spp),
    sum(wiki.spp %in% repdb.spp),
    sum(cal.spp %in% repdb.spp),
    sum(hm.spp %in% repdb.spp),
    sum(flickr.spp %in% repdb.spp)
  ))

library(ggplot2)

main.data <- main.data %>% 
  mutate(tot.spp = length(repdb.spp),
         percent = species / tot.spp *100)

(main.spp.plot <- ggplot(main.data) +
    geom_col(aes(x = source, y = length(repdb.spp)),
             fill = "grey90", width = 0.75) +
    geom_col(aes(x = source, y = species), fill = "darkred",
             width = 0.75) +
    geom_text(aes(x = source, y = species,
                  label = species), 
              colour = "darkred", fontface = 2, hjust = 0.5, vjust = -0.2) +
    geom_text(aes(x = source, y = species,
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
    geom_col(aes(x = "Total", y = length(repdb.spp)),
             fill = "grey90", width = 0.75) +
    geom_col(aes(x = "Total", y = tot.spp),
             fill = "darkred", width = 0.75) +
    geom_text(aes(x = "Total", y = tot.spp, label = tot.spp), 
              colour = "darkred", fontface = 2, hjust = 0.5, vjust = -0.2) +
    geom_text(aes(x = "Total", y = tot.spp,
                  label = paste0("(", round(tot.spp/length(repdb.spp)*100, digits = 0),
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
            theme(plot.margin = unit(c(1,1,1.55,0), "lines")),
          nrow = 1, ncol = 2, widths = c(4, 1))

ggsave(file = "./Figures/Source species coverage.png",
       dpi = 300, width = 210, height = 160, units = "mm")

# Per species plot --------------------------------------------------------

overall.mean <- photos.per.species %>% 
  summarise(mean = mean(n),
            se = sd(n)/sqrt(length(n)),
            max = max(n),
            min = min(n),
            source = "Total")

photos.per.species.toplot <- rbind(photos.per.species,
                            data.frame("species" = NA,
                                       "n" = NA,
                                       "source" = "Total"))

photos.per.species.toplot$source <- factor(photos.per.species.toplot$source,
                                           levels = c(
                                             "Reptile Database",
                                             "Wikimedia",
                                             "iNaturalist",
                                             "CalPhotos",
                                             "HerpMapper",
                                             "Flickr",
                                             "Total"))

(per.species.plot <- ggplot(photos.per.species.toplot) +
    geom_point(aes(x = source, y = n, colour = n>=10000 & source == "iNaturalist"),
               position = position_jitter(width = 0.15)
    ) +
    annotate("rect", xmin = 2.8, xmax = 3.2,
             ymin = 10000, ymax = max(photos.per.species.toplot$n, na.rm = TRUE) + 500,
             fill = "yellow", alpha = 0.2) +
    annotate("text", x = "iNaturalist", 
             y = max(photos.per.species.toplot$n, na.rm = TRUE) + 500,
             label = "Limited by max\nAPI request",
             fontface = 3, vjust = 0, hjust = 0.5,
             size = 2.5, lineheight = 0.9) +
    # geom_point(data = photos.per.species %>% 
    #              group_by(source) %>% 
    #              summarise(mean = mean(n),
    #                        se = sd(n)/sqrt(length(n))),
    #            aes(x = source, y = mean), colour = "red",
    #            position = position_nudge(x = -0.2)) +
    # geom_point(data = photos.per.species %>% 
    #              group_by(source) %>% 
    #              summarise(median = median(n)),
    #            aes(x = source, y = median), colour = "blue",
    #            position = position_nudge(x = 0.2)) +
    geom_segment(aes(x = "Total", xend = "Total",
                   y = max(n, na.rm = TRUE), yend = min(n, na.rm = TRUE)),
               size = 1.5, position = position_nudge(x = -0.15)) +
    geom_point(aes(x = "Total", y = mean(n, na.rm = TRUE)), colour = "black",
               size = 7, position = position_nudge(x = -0.15)) +
    geom_point(aes(x = "Total", y = mean(n, na.rm = TRUE)), colour = "darkred",
               size = 5, position = position_nudge(x = -0.15)) +
    geom_text(aes(x = "Total", y = mean(n, na.rm = TRUE),
                  label = paste0("Mean\n", round(mean(n, na.rm = TRUE), digits = 2),
                                 "\n± ", round(
                                   sd(n, na.rm = TRUE)/sqrt(sum(!is.na(n))),
                                   digits = 2))),
              vjust = 0.5, hjust = 0, fontface = 4,
              size = 4,
              position = position_nudge(x = 0.0, y = 1000),
              lineheight = 0.85) +
    geom_text(aes(x = "Total", y = max(n, na.rm = TRUE),
                  label = paste0("Max\n", max(n, na.rm = TRUE))),
              vjust = 1, hjust = 0, fontface = 4,
              size = 4,
              position = position_nudge(x = 0.0, y = 0),
              lineheight = 0.85) +
    geom_vline(xintercept = 6.5) +
    geom_vline(xintercept = c(1.5,2.5,3.5,4.5,5.5),
               linetype = 2) +
    scale_colour_manual(values = c("darkred", "grey15"),
                        labels = c("", "Over iNat. request limit")) +
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

ggsave(per.species.plot, file = "./Figures/Photos per species.png",
       dpi = 300, width = 180, height = 120, units = "mm")


# Clade summary -----------------------------------------------------------

library(stringr)
repdb <- read.csv("./Data/reptile_checklist_2020_04.csv", stringsAsFactors = FALSE)

repdb$Familyetc[!str_detect(repdb$Familyetc,
                            "Amphisbaenia|Serpentes|Testudines|Sauria|Crocodylia|Rhynchocephalia")]

repdb$clade <- str_extract(repdb$Familyetc,
                           "Amphisbaenia|Serpentes|Testudines|Sauria|Crocodylia|Rhynchocephalia")

clade.df <- repdb %>% 
  rename("species" = Species) %>% 
  dplyr::select(species, clade)

ALL.SPP.PHOTOS <- photos.per.species %>% 
  group_by(species) %>% 
  summarise(photos = sum(n))

### THERE ARE 100 EXTRA SPECIES IN THE UETZ INAT DATA, NEWER LIST USED????
miss.sp <- inat.spp[!inat.spp %in% repdb.spp]
write.csv(miss.sp, file = "./missing spp.csv")

clade.df <- left_join(clade.df,
                      ALL.SPP.PHOTOS)

(per.clade.plot <- clade.df %>% 
    mutate(has.photos = photos > 0) %>% 
    group_by(clade) %>% 
    summarise(n.spp = length(unique(species)),
              n.w.photos = sum(has.photos, na.rm = TRUE),
              percent = n.w.photos / n.spp *100) %>% 
    mutate(clade = factor(clade, levels = clade[order(n.spp, decreasing = TRUE)])) %>% 
    ggplot() +
    geom_col(aes(x = clade, y = n.spp),
             fill = "grey90", width = 0.75) +
    geom_col(aes(x = clade, y = n.w.photos), fill = "darkred",
             width = 0.75) +
    geom_text(aes(x = clade, y = n.w.photos, 
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


# Clade summary by family -------------------------------------------------

library(ggplot2)
library(ape)
library(stringr)
library(ggtree)
# test.tree <- read.tree(file = "./Data/TimeTree Data/reptiles_family_testudines.nwk")

tree.list <- lapply(list.files("./Data/TimeTree Data/", pattern = ".nwk",
                  full.names = TRUE), read.tree)
names(tree.list) <- c("Crocodylia", "Lepidosauria", "Testudines")
class(tree.list) <- "multiPhylo"

tree.list

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

ALL.SPP.PHOTOS <- photos.per.species %>% 
  group_by(species) %>% 
  summarise(photos = sum(n))

fam.summary <- fam.clade.df %>% 
  group_by(family) %>% 
  mutate(n.spp = length(unique(species))) %>% 
  left_join(ALL.SPP.PHOTOS, by = "species") %>% 
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
  
  l.df <- fam.summary %>% 
    filter(
      clade %in% sel.clade) %>% 
    filter(!is.na(tip.label)) %>% 
    group_by(tip.label) %>% 
    slice(n = 1) %>% 
    ungroup()
  
  l.tree <- ggtree(tree.data) %<+% l.df
  
  plot.tree <- l.tree +
    geom_tippoint(aes(colour = per.spp.ph, size = n.photos)) +
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
                       direction = 1) +
    scale_size_continuous(breaks = c(0, 100000, 20000, 30000),
                          labels = c("0", "100,000", "200,000", "300,000"),
                          trans = "log") +
    labs(colour = "% of species\nwith photos", size = "Number of\nphotos") +
    theme_tree() +
    theme(legend.title = element_text(face = 2),
          legend.position = c(0.15, 0.70)
          )
  
  return(plot.tree)
}

l.clades <- unique(fam.summary$clade[!fam.summary$clade %in% c("Crocodylia", "Testudines")])

library(ggpubr)
pl.list <- list(make_tree(tree.list[[1]], fam.summary, "Crocodylia") +
                  theme(plot.margin = unit(c(0.5,0,0.1,2), "lines")) +
                  rremove("legend"),
                make_tree(tree.list[[2]], fam.summary, l.clades) +
                  theme(plot.margin = unit(c(0.5,0,0.01,2), "lines")),
                  # rremove("legend"),
                make_tree(tree.list[[3]], fam.summary, "Testudines") +
                  theme(plot.margin = unit(c(0.5,3.4,0.01,2), "lines")) +
                  rremove("legend")
                )

# ggarrange(plotlist = pl.list,
#           nrow = 3,
#           align = "v",
#           heights = c(1,8,3))

library(cowplot)
pg <- plot_grid(plotlist = pl.list, nrow = 3,
          rel_heights = c(1,8,3),
          axis = "r"
          )

plot(pg)

ggsave2(pg, file = "./Figures/Tree summary.png",
       dpi = 300, width = 160, height = 200, units = "mm")

