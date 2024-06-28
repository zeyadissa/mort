#These are the geo-boundaries from the ONS files
LSOA_URL <- 'https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA_2011_Boundaries_Super_Generalised_Clipped_BSC_EW_V4/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson'
LAD_URL <- 'https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_December_2022_UK_BUC_V2/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson'
ICB_URL <- 'https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Integrated_Care_Boards_April_2023_EN_BSC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson'
WARDS_URL <- 'https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Wards_December_2022_Boundaries_UK_BSC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson'
CA_URL <- 'https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Combined_Authorities_December_2023_Boundaries_EN_BUC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson'

#Possible input keys for the UI
OP_TYPE <- c('Standard','Real','Pirate')
OP_PAL <- c('Continous','Categorical','String','Custom')
OP_PAL_MARKER <- c('Numeric','Custom','None')
OP_COLS <- c('LAD','LSOA','ICB','Wards')
OP_COLS_MARKERS <- c('OrganisationName','SubType','Sector','City')

#These are the controls for the zoom: they are needed to set the boundary to UK
THEME <- 'flatly'

lookup <- read.csv('const/lookup.csv') |> 
  dplyr::select(one_of(OP_COLS),ICB22NM)
sample <- read.csv('const/sample.csv')
marker_lookups <- read.csv('const/hospital_data.csv')  |> 
  dplyr::select(Longitude,Latitude,OrganisationName,SubType,Sector,City)

#Read
icb <- sf::st_read(ICB_URL) |>
  rename('ICB'='ICB23CD')
lsoa <- sf::st_read(LSOA_URL) |>
  rename('LSOA'='LSOA11CD')
lad <- sf::st_read(LAD_URL) |>
  rename('LAD'='LAD22CD')
wards <- sf::st_read(WARDS_URL) |>
  rename('Wards'='WD22CD')
