
library(tidycensus)
library(dplyr)
library(sf)
library(SpatialAcc)
library(RPostgreSQL)
library(tidyr)

# get ACS data #
acs_vars <- c(
  # total population
  "B01003_001"
)


data_tract <- get_acs(geography = "tract",
                      state = 51,
                      county = 013,
                      variables = acs_vars,
                      year = 2019,
                      survey = "acs5",
                      cache_table = TRUE,
                      output = "wide",
                      geometry = FALSE,
                      keep_geo_vars = FALSE)

acs_tract <- data_tract %>%
  transmute(tract_fips = GEOID,
            tract_name = NAME,
            total_pop = B01003_001E
  )

data_bgrp <- get_acs(geography = "block group",
                     state = 51,
                     county = 013,
                     variables = acs_vars,
                     year = 2019,
                     survey = "acs5",
                     cache_table = TRUE,
                     output = "wide",
                     geometry = FALSE,
                     keep_geo_vars = FALSE)

acs_bgrp <- data_bgrp %>%
  transmute(bgrp_fips = GEOID,
            bgrp_name = NAME,
            total_pop = B01003_001E
  )

#acs_tract$total_pop[acs_tract$total_pop == 0] <- 0.0001

## park data
# transform to utm with meter units
parks <- st_read("parks/data/arlington_parks/Park_Polygons.shp") %>%
  filter(Ownership == "Arlington County Park")

# amenities
#parks_amenities <- read.csv("~/git/dspg21mc/data/working/parks_amenities.csv")
parks_amenities <- read.csv("parks/data/parks_amenities.csv")

## distance matrices
tract_dist_mat <- read.csv("parks/data/park_to_tract_dist_mat.csv")
bgrp_dist_mat <- read.csv("parks/data/park_to_bgrp_dist_mat.csv")


### tract level ###

# total acreage
all_tract_tsfca <- ac(p = acs_tract$total_pop,
                      n = parks$Acreage,
                      D = tract_dist_mat,
                      d0 = 1609,
                      family = "2SFCA")

# access to playground
playground <- which(parks_amenities$playground == 1)

all_tract_tsfca_playground <- ac(p = acs_tract$total_pop,
                                 n = parks$Acreage[playground],
                                 D = tract_dist_mat[,playground],
                                 d0 = 1609,
                                 family = "2SFCA")

# access to parking
parking <- which(parks_amenities$free_parking == 1)

all_tract_tsfca_parking <- ac(p = acs_tract$total_pop,
                              n = parks$Acreage[parking],
                              D = tract_dist_mat[,parking],
                              d0 = 1609,
                              family = "2SFCA")

# access to basketball courts
bball <- which(parks_amenities$basketball == 1)

all_tract_tsfca_bball <- ac(p = acs_tract$total_pop,
                            n = parks$Acreage[bball],
                            D = tract_dist_mat[,bball],
                            d0 = 1609,
                            family = "2SFCA")

# access to tennis courts
tennis <- which(parks_amenities$tennis == 1)

all_tract_tsfca_tennis <- ac(p = acs_tract$total_pop,
                             n = parks$Acreage[tennis],
                             D = tract_dist_mat[,tennis],
                             d0 = 1609,
                             family = "2SFCA")

# access to charcoal grills
grill <- which(parks_amenities$charcoal_grill == 1)

all_tract_tsfca_grill <- ac(p = acs_tract$total_pop,
                            n = parks$Acreage[grill],
                            D = tract_dist_mat[,grill],
                            d0 = 1609,
                            family = "2SFCA")

# combine results
tract_fca_res <- data.frame(tract_fips = acs_tract$tract_fips,
                            total_pop = acs_tract$total_pop,
                            all_park_tsfca = all_tract_tsfca,
                            playground_tsfca = all_tract_tsfca_playground,
                            parking_tsfca = all_tract_tsfca_parking,
                            basketball_tsfca = all_tract_tsfca_bball,
                            tennis_tsfca = all_tract_tsfca_tennis,
                            grill_tsfca = all_tract_tsfca_grill)


### block group level ###

# total acreage
all_bgrp_tsfca <- ac(p = acs_bgrp$total_pop,
                     n = parks$Acreage,
                     D = bgrp_dist_mat,
                     d0 = 1609,
                     family = "2SFCA")

# access to playground
all_bgrp_tsfca_playground <- ac(p = acs_bgrp$total_pop,
                                n = parks$Acreage[playground],
                                D = bgrp_dist_mat[,playground],
                                d0 = 1609,
                                family = "2SFCA")

# access to parking
all_bgrp_tsfca_parking <- ac(p = acs_bgrp$total_pop,
                             n = parks$Acreage[parking],
                             D = bgrp_dist_mat[,parking],
                             d0 = 1609,
                             family = "2SFCA")

# access to basketball court
all_bgrp_tsfca_bball <- ac(p = acs_bgrp$total_pop,
                           n = parks$Acreage[bball],
                           D = bgrp_dist_mat[,bball],
                           d0 = 1609,
                           family = "2SFCA")

# access to tennis court
all_bgrp_tsfca_tennis <- ac(p = acs_bgrp$total_pop,
                            n = parks$Acreage[tennis],
                            D = bgrp_dist_mat[,tennis],
                            d0 = 1609,
                            family = "2SFCA")

# access to grill
all_bgrp_tsfca_grill <- ac(p = acs_bgrp$total_pop,
                           n = parks$Acreage[grill],
                           D = bgrp_dist_mat[,grill],
                           d0 = 1609,
                           family = "2SFCA")

# combine results
bgrp_fca_res <- data.frame(bgrp_fips = acs_bgrp$bgrp_fips,
                           total_pop = acs_bgrp$total_pop,
                           all_park_tsfca = all_bgrp_tsfca,
                           playground_tsfca = all_bgrp_tsfca_playground,
                           parking_tsfca = all_bgrp_tsfca_parking,
                           basketball_tsfca = all_bgrp_tsfca_bball,
                           tennis_tsfca = all_bgrp_tsfca_tennis,
                           grill_tsfca = all_bgrp_tsfca_grill)

# put together parks data for database #

tract_fca_res <- tract_fca_res %>%
  select(-total_pop) %>%
  pivot_longer(all_park_tsfca:grill_tsfca, names_to = "measure", values_to = "value") %>%
  mutate(region_type = "tract",
         year = 2021,
         measure_type = "index") %>%
  left_join(acs_tract[,-3], by = "tract_fips") %>%
  select(geoid = tract_fips,
         region_type,
         region_name = tract_name,
         year,
         measure,
         value,
         measure_type) %>%
  as.data.frame()



bgrp_fca_res <- bgrp_fca_res %>%
  select(-total_pop) %>%
  pivot_longer(all_park_tsfca:grill_tsfca, names_to = "measure", values_to = "value") %>%
  mutate(region_type = "block group",
         year = 2021,
         measure_type = "index") %>%
  left_join(acs_bgrp[,-3], by = "bgrp_fips") %>%
  select(geoid = bgrp_fips,
         region_type,
         region_name = bgrp_name,
         year,
         measure,
         value,
         measure_type) %>%
  as.data.frame()


parks_amenities_db <- parks_amenities %>%
  select(ParkName = ParkName.x,
         Acreage,
         AddressNum,
         Street,
         16:62)

parks_db <- parks %>%
  select(ParkName, geometry)

all_parks_db <- merge(parks_db, parks_amenities_db)



# write data to db
conn <- dbConnect(drv = PostgreSQL(),
                  dbname = "sdad",
                  host = "10.250.124.195",
                  port = 5432,
                  user = Sys.getenv(x = "db_userid"),
                  password = Sys.getenv(x = "db_pwd")
)

# write to data commons database, auto-detect if writing geom or not, change owner to data_commons
dc_dbWriteTable <-
  function(
    db_conn,
    schema_name,
    table_name,
    table_data,
    table_owner = "data_commons"
  ) {
    # check for geometry/geography columns
    tf <- sapply(table_data, {function(x) inherits(x, 'sfc')})
    # if TRUE, use sf
    if (TRUE %in% tf) {
      sf_write_result <- sf::st_write(obj = table_data, dsn = db_conn, layer = c(schema_name, table_name), row.names = FALSE)
      print(sf_write_result)
      # if FALSE, use DBI
    } else {
      write_result <- DBI::dbWriteTable(conn = db_conn, name = c(schema_name, table_name), value = table_data, row.names = FALSE, overwrite = TRUE)
      print(write_result)
    }
    # change table owner
    chgown_result <- DBI::dbSendQuery(conn = db_conn, statement = paste0("ALTER TABLE ", schema_name, ".", table_name, " OWNER TO ", table_owner))
    print(chgown_result)
  }

dc_dbWriteTable(conn,
                "dc_environment_land_use",
                "va013_tr_arlopendata_2021_parks_catchment_scores",
                tract_fca_res,
                "data_commons")

dc_dbWriteTable(conn,
                "dc_environment_land_use",
                "va013_bg_arlopendata_2021_parks_catchment_scores",
                bgrp_fca_res,
                "data_commons")

dc_dbWriteTable(conn,
                "dc_environment_land_use",
                "va_pl_arlington_2021_parks_amenities",
                all_parks_db,
                "data_commons")

dbDisconnect(conn)
