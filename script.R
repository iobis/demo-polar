library(robis)
library(stringr)
library(dplyr)
library(CCAMLRGIS)
library(sf)
library(ggplot2)
library(lwgeom)
library(maptools)
library(rnaturalearth)
library(nngeo)

# fetch occurrences

df <- occurrence("Porifera", qcfields = TRUE, dropped = "include")

df_sf <- df %>%
  filter(!is.na(decimalLongitude) & !is.na(decimalLatitude)) %>%
  select(id, decimalLongitude, decimalLatitude) %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

ggplot() +
  geom_sf(data = df_sf)

# arctic

aba <- st_read("ABA-Boundaries/Arctic_Zones_complete_polygons.shp") %>%
  st_buffer(1) %>%
  st_union()

amap <- st_read("AMAP-area/amaplim_lam_poly.shp") %>%
  st_union() %>%
  st_transform(crs = st_crs(aba))

arctic <- st_union(aba, amap)

ggplot() +
  geom_sf(data = aba, fill = "skyblue3", color = "chartreuse3", alpha = 0.1) +
  geom_sf(data = amap, fill = "coral3", color = "coral3", alpha = 0.1) +
  geom_sf(data = arctic, fill = NA, color = "black")

# antarctic

asds <- load_ASDs()

antarctic <- asds %>%
  st_as_sf() %>%
  st_union() %>%
  st_remove_holes()

ggplot() +
  geom_sf(data = antarctic, fill = NA, color = "black")

# match geometries

df_sf$is_arctic <- as.logical(st_intersects(df_sf %>% st_transform(crs = st_crs(arctic)), arctic))
df_sf$is_antarctic <- as.logical(st_intersects(df_sf %>% st_transform(crs = st_crs(antarctic)), antarctic))

ggplot() +
  geom_sf(data = ne_countries(returnclass = "sf"), color = "#000000", fill = "#eeeeee") +
  geom_sf(data = df_sf %>% filter(is_arctic), color = "skyblue3") +
  geom_sf(data = df_sf %>% filter(is_antarctic), color = "coral3") +
  coord_sf(crs = 4326) +
  theme_void()

ggplot() +
  geom_sf(data = arctic, fill = "black", color = "black", alpha = 0.05) +
  geom_sf(data = df_sf %>% filter(is_arctic), color = "skyblue3") +
  coord_sf(crs = st_crs(arctic))

ggplot() +
  geom_sf(data = antarctic, fill = "black", color = "black", alpha = 0.05) +
  geom_sf(data = df_sf %>% filter(is_antarctic), color = "coral3") +
  coord_sf(crs = st_crs(antarctic))

# add to original dataframe

df_joined <- df %>%
  left_join(df_sf, by = "id")

# output

df_selection <- df_joined %>%
  select(id, dataset_id, institutionCode, collectionCode, catalogNumber, recordNumber, fieldNumber, locality, decimalLongitude, decimalLatitude, date_year, date_mid, eventDate, scientificName, originalScientificName, aphiaID, minimumDepthInMeters, maximumDepthInMeters, depth, coordinateUncertaintyInMeters, phylum, class, subclass, order, suborder, family, subfamily, genus, subgenus, species, subspecies, phylumid, classid, subclassid, orderid, suborderid, familyid, subfamilyid, genusid, subgenusid, speciesid, subspeciesid, flags, missing, invalid, shoredistance, bathymetry, is_arctic, is_antarctic)

ggplot() +
  geom_sf(data = ne_countries(returnclass = "sf"), color = "#000000", fill = "#eeeeee") +
  geom_point(data = df_selection %>% filter(is_arctic), aes(decimalLongitude, decimalLatitude), color = "skyblue3") +
  geom_point(data = df_selection %>% filter(is_antarctic), aes(decimalLongitude, decimalLatitude), color = "coral3") +
  coord_sf(crs = 4326) +
  theme_void()

write.csv(df_selection, file = paste0("porifera_", format(Sys.time(), "%Y%m%d"), ".csv"), row.names = FALSE, na = "")

