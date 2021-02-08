## ----libraries, message=FALSE---------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggsn)
library(readr)
library(sf)
library(stringr)


## ----download-GADM, message=FALSE, warning=FALSE--------------------------------------------------
# Remote file information
u_remote <- "https://biogeo.ucdavis.edu/"
p_remote <- "data/gadm3.6/Rsf/"
f_name <- "gadm36_ETH_3_sf.rds"

# Local file location to save to
ethiopia_rds <- file.path(tempdir(), "gadm36_ETH_3_sf.rds")

if (toupper(Sys.info()["sysname"]) == "WINDOWS") {
  download.file(
    url = paste0(u_remote, p_remote, f_name),
    destfile = ethiopia_rds,
    method = "wininet",
    mode = "wb"
  )
} else {
  download.file(
    url = paste0(u_remote, p_remote, f_name),
    destfile = ethiopia_rds,
    method = "auto"
  )
}


## ----readRDS--------------------------------------------------------------------------------------
ethiopia_sf <- readRDS(ethiopia_rds)


## ----plot_level1----------------------------------------------------------------------------------
ethiopia_ggplot <- ggplot(ethiopia_sf) +
  geom_sf(fill = "white")

ethiopia_ggplot


## ----colnames-------------------------------------------------------------------------------------
names(ethiopia_sf)


## ----group-NAME_1, warning=FALSE, message=FALSE---------------------------------------------------
ethiopia_regions_sf <-
  ethiopia_sf %>%
  mutate(NAME_1 = gsub("Southern Nations, Nationalities and Peoples", "SNNP",
                       NAME_1)) %>%
  group_by(NAME_1) %>%
  summarise() %>%
  ungroup() %>%
  st_as_sf()


## ----plot-NAME_1, warning=FALSE, message=FALSE----------------------------------------------------
ethiopia_regions_ggplot <- 
  ggplot(ethiopia_regions_sf) +
  geom_sf(fill = "white") +
  geom_sf_text(
    aes(label = NAME_1),
    size = 2.5,
    hjust = 1
  )

ethiopia_regions_ggplot


## ----theming-labelling, warning=FALSE, message=FALSE----------------------------------------------
ethiopia_regions_ggplot <- 
  ethiopia_regions_ggplot +
  labs(x = "Longitude",
       y = "Latitude") +
  theme_bw()

ethiopia_regions_ggplot


## ----coffee-sampling, message=FALSE---------------------------------------------------------------
coffee_sampling <-
  read_csv(
    "https://raw.githubusercontent.com/emdelponte/paper-coffee-rust-Ethiopia/master/data/survey_clean.csv"
  )


## ----convert-coffee-to-sf, warning=FALSE, message=FALSE-------------------------------------------
coffee_sampling_sf <-
  st_as_sf(coffee_sampling,
           coords = c("lon", "lat"),
           crs = 4326)


## ----add-points, warning=FALSE, message=FALSE-----------------------------------------------------
ethiopia_regions_ggplot +
  geom_sf(data = coffee_sampling_sf,
          size = 2,
          color = alpha("black", 0.35))


## ----filter-ssnp-oromia, warning=FALSE, message=FALSE---------------------------------------------
# First layer, country outline
ethiopia_simple_sf <-
   ethiopia_sf %>%
   group_by(NAME_0) %>%
   summarise() %>%
   ungroup() %>%
  st_as_sf()

# Second layer, regions we're interested in
oromia_snnp_sf <- filter(
  ethiopia_regions_sf,
  NAME_1 == "Oromia" |
    NAME_1 == "SNNP"
)


## ----plot-ssnp-oromia-----------------------------------------------------------------------------
ggplot() +
  geom_sf(data = ethiopia_simple_sf,
          col = NA) +
  geom_sf(data = oromia_snnp_sf,
          aes(linetype = NAME_1,
              fill = NAME_1)) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    caption = "Data from GADM (gadm.org).",
    fill = "Region",
    linetype = "Region",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw()


## ----filter-zones, warning=FALSE, message=FALSE---------------------------------------------------
ethiopia_zones_sf <- 
  ethiopia_sf %>%
  filter(
    NAME_2 %in% c(
      "Jimma",
      "Mirab Welega",
      "Sidama",
      "Sheka",
      "Keffa",
      "Bench Maji",
      "Bale",
      "Gedeo",
      "Ilubabor"
    )
  )


## ----final-map------------------------------------------------------------------------------------
final_map <- ggplot() +
  geom_sf(data = ethiopia_simple_sf,
          col = NA,
          fill = "#D5C1AB") + # use a coffee-ish colour background for country
  geom_sf(data = oromia_snnp_sf, # add the two regions in white
          aes(linetype = NAME_1),
          fill = "white") +
  geom_sf(data = ethiopia_zones_sf, # add zones and fill by name
          aes(fill = NAME_2),
          colour = NA) + # no outline colour
  geom_sf(data = coffee_sampling_sf,
          # add sampling points
          size = 2,
          colour = alpha("black", 0.35))

final_map


## ----final-map-legend-----------------------------------------------------------------------------
final_map <- 
  final_map +
  scale_linetype( # define region outline linetype by region
    labels = function(x)
      str_wrap(x, width = 5)
  ) +
  scale_fill_brewer(palette = "Set3") # this colours the zones, https://colorbrewer2.org/

final_map


## ----final-map-labelling--------------------------------------------------------------------------
final_map <- 
  final_map +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Ethiopian Coffee Leaf Rust Survey Sites",
    caption = "Data from GADM, gadm.org, and Del Ponte & Belachew (2020)
    https://doi.org/10.17605/OSF.IO/XEJAZ",
    fill = "Zone",
    linetype = "Region"
  )

final_map


## ----final-touches--------------------------------------------------------------------------------
final_map <-
  final_map +
  north(ethiopia_simple_sf) + # from ggsn
  scalebar( # from ggsn
    ethiopia_simple_sf,
    dist = 250,
    st.size = 2.5,
    dist_unit = "km",
    transform = TRUE,
    model = "WGS84"
  ) +
theme_bw()

final_map

