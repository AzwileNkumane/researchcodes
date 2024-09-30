# GWR
error1 <- a$median_house_value -gwr.model$lm$fitted.values

library(spdep)
df <- data.frame(id=1:1000,x=a[,1], y=a[,2], voi=error1)
df_sf <- st_as_sf(df, coords = c("x", "y"), crs = 4326)
# Compute distance matrix
dists <- st_distance(df_sf)

# Create a weights matrix based on distance (e.g., threshold of 2 units)
weights <- 1/dists


# Convert the weights to a list format required by spdep
nb <- dnearneigh(st_coordinates(df_sf), 0, 2)

lw <- nb2listw(weights)

# Calculate Moran's I
moran(error1,lw, length(nb),Szero(lw))


# GWRF
error2 <- a$median_house_value- gwrf_model$LGofFit$LM_yfitPred

df2 <- data.frame(id=1:1000,x=a[,1], y=a[,2], voi=error2)
df_sf2 <- st_as_sf(df2, coords = c("x", "y"), crs = 4326)
# Compute distance matrix
dists2 <- st_distance(df_sf2)

# Create a weights matrix based on distance (e.g., threshold of 2 units)
weights2 <- 1/dists2


# Convert the weights to a list format required by spdep
nb2 <- dnearneigh(st_coordinates(df_sf2), 0, 2)

lw2 <- nb2listw(nb2)

# Calculate Moran's I
moran(error2,lw2, length(nb2),Szero(lw2))


# GWANN
error3 <-p- a[idx_pred,]$median_house_value

df3 <- data.frame(id=1:210,x=a[idx_pred,]$longitude, y=a[idx_pred,]$latitude, voi=error3)
df_sf3 <- st_as_sf(df3, coords = c("x", "y"), crs = 4326)
# Compute distance matrix
dists3 <- st_distance(df_sf3)

# Create a weights matrix based on distance (e.g., threshold of 2 units)
weights3 <- 1/dists3


# Convert the weights to a list format required by spdep
nb3 <- dnearneigh(st_coordinates(df_sf3), 0, 2)

lw3 <- nb2listw(nb3)

# Calculate Moran's I
moran(error3,lw3, length(nb3),Szero(lw3))
