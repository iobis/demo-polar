library(robis)
library(stringr)
library(dplyr)
library(CCAMLRGIS)
library(sf)
library(ggplot2)
library(lwgeom)
library(maptools)
library(rnaturalearth)

# fetch occurrences

df <- occurrence("Porifera", qcfields = TRUE, dropped = "include")

df_sf <- df %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326, remove = FALSE)

# arctic

aba <- st_read("ABA-Boundaries/Arctic_Zones_complete_polygons.shp") %>%
  st_buffer(1) %>%
  st_union()

amap <- st_read("AMAP-area/amaplim_lam_poly.shp") %>%
  st_union() %>%
  st_transform(crs = st_crs(aba))

arctic <- st_union(aba, amap)

ggplot() +
  geom_sf(data = aba, fill = "green", color = "green", alpha = 0.1) +
  geom_sf(data = amap, fill = "red", color = "red", alpha = 0.1) +
  geom_sf(data = arctic, fill = NA, color = "black")

# antarctic

asds <- load_ASDs()

antarctic <- asds %>%
  st_as_sf() %>%
  st_union() %>%
  lapply(FUN = function(x) x[1]) %>%
  st_multipolygon() %>%
  st_sfc(crs = st_crs(arctic))
  
ggplot() +
  geom_sf(data = antarctic, fill = "green", color = "green", alpha = 0.1)

# match geometries

df_sf$is_arctic <- as.logical(st_intersects(df_sf %>% st_transform(crs = st_crs(arctic)), arctic))
df_sf$is_antarctic <- as.logical(st_intersects(df_sf %>% st_transform(crs = st_crs(antarctic)), antarctic))

# plot

ggplot() +
  geom_sf(data = ne_countries(returnclass = "sf"), color = "#000000", fill = "#eeeeee") +
  geom_sf(data = df_sf, size = 0.5) +
  geom_sf(data = df_sf %>% filter(is_arctic), color = "coral2") +
  geom_sf(data = df_sf %>% filter(is_antarctic), color = "coral3") +
  coord_sf(crs = 4326) +
  theme_void()

ggplot() +
  geom_sf(data = arctic, fill = "black", color = "black", alpha = 0.05) +
  geom_sf(data = df_sf %>% filter(is_arctic), color = "coral2") +
  coord_sf(crs = st_crs(arctic))

ggplot() +
  geom_sf(data = antarctic, fill = "black", color = "black", alpha = 0.05) +
  geom_sf(data = df_sf %>% filter(is_antarctic), color = "coral3") +
  coord_sf(crs = st_crs(antarctic))

# output

df_selection <- df_sf %>%
  as.data.frame() %>%
  select(id, dataset_id, institutionCode, collectionCode, catalogNumber, recordNumber, fieldNumber, locality, decimalLongitude, decimalLatitude, date_year, date_mid, eventDate, scientificName, originalScientificName, aphiaID, minimumDepthInMeters, maximumDepthInMeters, depth, coordinateUncertaintyInMeters, phylum, class, subclass, order, suborder, family, subfamily, genus, subgenus, species, subspecies, phylumid, classid, subclassid, orderid, suborderid, familyid, subfamilyid, genusid, subgenusid, speciesid, subspeciesid, flags, shoredistance, bathymetry, is_arctic, is_antarctic)

write.csv(df_selection, file = paste0("porifera_", format(Sys.time(), "%Y%m%d"), ".csv"), row.names = FALSE, na = "")

