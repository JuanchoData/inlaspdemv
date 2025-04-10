#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE, type="binary")
#install.packages("sP")

library(terra)
library(INLA)
library(raster)
library(sp)
library(sf)
library(dplyr)
setwd('C://Users//caugh//Desktop//datasc_jc//622')

# Load rasters, those raster have cleaning techniques
#mesic200408 <- rast("earth-engine-export//WRP-040029-200408.tif")
#mesic200408 <- terra::project(mesic200408, crs("EPSG:32612"))
#mesic200908 <- rast("earth-engine-export//WRP-040029-200908.tif")
#mesic200908 <- terra::project(mesic200908, crs("EPSG:32612"))
#mesic201408 <- rast("earth-engine-export//WRP-040029-201408.tif")
#mesic201408 <- terra::project(mesic201408, crs("EPSG:32612"))
#mesic201908 <- rast("earth-engine-export//WRP-040029-201908.tif")
#mesic201908 <- terra::project(mesic201908, crs("EPSG:32612"))

# Load rasters, without cleaning techniques
mesic200408 <- rast("WRP-040029-200408.tif")
mesic200408 <- terra::project(mesic200408, crs("EPSG:32612"))
mesic200908 <- rast("WRP-040029-200908.tif")
mesic200908 <- terra::project(mesic200908, crs("EPSG:32612"))
mesic201408 <- rast("WRP-040029-201408.tif")
mesic201408 <- terra::project(mesic201408, crs("EPSG:32612"))
mesic201908 <- rast("WRP-040029-201908.tif")
mesic201908 <- terra::project(mesic201908, crs("EPSG:32612"))

# Combine all rasters into a stack, to select the pixels that with data across all the years
raster_stack <- c(mesic200408, mesic200908, mesic201408, mesic201908)
valid_pixels_mask <- !is.na(raster_stack)
valid_pixel_sum <- app(valid_pixels_mask, sum)
valid_pixel_sum_values <- values(valid_pixel_sum)
# Create a mask where pixels are valid in all years 
valid_all_years_mask <- valid_pixel_sum == 4
print(valid_all_years_mask)
valid_all_years_mask <- ifel(valid_all_years_mask, 1, NA)
plot(valid_all_years_mask)
# Randomly sample 2000 pixels for each raster (keeping coordinates)
samples <- spatSample(valid_all_years_mask, size = 2000, method = "random", na.rm = TRUE, xy = TRUE)
coords <- samples[, c("x", "y")]
sampled <- terra::vect(coords,geom = c("x", "y"),crs = crs(mesic200408))

#The response variable, normalization
mesic_200408 <- values(mesic200408)
mesic_200408_norm <- (mesic_200408 - mean(mesic_200408, na.rm = TRUE)) / (2 * sd(mesic_200408, na.rm = TRUE))
mesic200408_nor <- setValues(mesic200408, mesic_200408_norm) 
mesic_200408 <- terra::extract(mesic200408_nor, sampled)

#mesic_200908 
mesic200908v <- values(mesic200908)
mesic_200908_norm <- (mesic200908v - mean(mesic200908v, na.rm = TRUE)) / (2 * sd(mesic200908v, na.rm = TRUE))
mesic200908_nor <- setValues(mesic200908, mesic_200908_norm) 
mesic_200908 <- terra::extract(mesic200908_nor, coords)

#mesic_201408 
mesic201408v <- values(mesic201408)
mesic_201408_norm <- (mesic201408v - mean(mesic201408v, na.rm = TRUE)) / (2 * sd(mesic201408v, na.rm = TRUE))
mesic201408_nor <- setValues(mesic201408, mesic_201408_norm) 
mesic_201408 <- terra::extract(mesic201408_nor, coords)

#mesic_201908 
mesic201908v <- values(mesic201908)
mesic_201908_norm <- (mesic201908v - mean(mesic201908v, na.rm = TRUE)) / (2 * sd(mesic201908v, na.rm = TRUE))
mesic201908_nor <- setValues(mesic201908, mesic_201908_norm) 
mesic_201908 <- terra::extract(mesic201908_nor, coords)

##### COVARIATES, extracting on the same position and normalization 
#LandTenure
landtenur <- rast('TenureRasterExport.tif')
landtenur <- terra::project(landtenur, crs("EPSG:32612"))
landtenure <- terra::extract(landtenur, sampled)
landtenure[is.na(landtenure)] <- 0

#Temperature and Ppt 2004
cov200408=rast('ppt_exports//029ppt_2004_08.tif')
cov200408 <- terra::project(cov200408, crs(mesic200408))
ppt200408 = cov200408[[1]]
ppt200408_values <- values(ppt200408)
ppt200408_nor <- (ppt200408_values - mean(ppt200408_values, na.rm = TRUE)) / (2 * sd(ppt200408_values, na.rm = TRUE))
ppt200408_norm <- setValues(ppt200408, ppt200408_nor)
precip_200408 <- terra::extract(ppt200408_norm, sampled)

tem200408 = cov200408[[2]]
tem200408_values <- values(tem200408)
tem200408_nor <- (tem200408_values - mean(tem200408_values, na.rm = TRUE)) / (2*sd(tem200408_values, na.rm = TRUE))
tem200408_norm <- setValues(tem200408, tem200408_nor) 
temp_200408 <- terra::extract(tem200408_norm, sampled)

df_200408 <- cbind(coords,mesic_200408, precip_200408, temp_200408,landtenure, year =2004)
colnames(df_200408)[which(colnames(df_200408) == "040029_200408_mesic")] <- "mesic"
head(df_200408)
df_200408 <- df_200408 %>% select(-ID) # to remove ID columns

# Temperature and Ppt 2009
cov200908=rast('ppt_exports//029ppt_2009_08.tif')
cov200908 <- terra::project(cov200908, crs(mesic200408))
ppt200908 = cov200908[[1]]
ppt200908_values <- values(ppt200908)
ppt200908_nor <- (ppt200908_values - mean(ppt200908_values, na.rm = TRUE)) / (2 * sd(ppt200908_values, na.rm = TRUE))
ppt200908_norm <- setValues(ppt200908, ppt200908_nor)
precip_200908 <- terra::extract(ppt200908_norm, sampled)

tem200908 = cov200908[[2]]
tem200908_values <- values(tem200908)
tem200908_nor <- (tem200908_values - mean(tem200908_values, na.rm = TRUE)) / (2*sd(tem200908_values, na.rm = TRUE))
tem200908_norm <- setValues(tem200908, tem200908_nor) 
temp_200908 <- terra::extract(tem200908_norm, sampled)

df_200908 <- cbind(coords, mesic_200908, precip_200908, temp_200908,landtenure, year =2009)
dim(coords)
colnames(df_200908)[which(colnames(df_200908) == "040029_200908_mesic")] <- "mesic"
head(df_200908)
df_200908 <- df_200908 %>% select(-ID)
head(df_200908)

# Temperature and Ppt 2014
cov201408=rast('ppt_exports//029ppt_2014_08.tif')
cov201408 <- terra::project(cov201408, crs(mesic200408))
ppt201408 = cov201408[[1]]
ppt201408_values <- values(ppt201408)
ppt201408_nor <- (ppt201408_values - mean(ppt201408_values, na.rm = TRUE)) / (2 * sd(ppt201408_values, na.rm = TRUE))
ppt201408_norm <- setValues(ppt201408, ppt201408_nor)
precip_201408 <- terra::extract(ppt201408_norm, sampled)

tem201408 = cov201408[[2]]
tem201408_values <- values(tem201408)
tem201408_nor <- (tem201408_values - mean(tem201408_values, na.rm = TRUE)) / (2*sd(tem201408_values, na.rm = TRUE))
tem201408_norm <- setValues(tem201408, tem201408_nor) 
temp_201408 <- terra::extract(tem201408_norm, sampled)

df_201408 <- cbind(coords, mesic_201408, precip_201408, temp_201408,landtenure, year =2014)
colnames(df_201408)[which(colnames(df_201408) == "040029_201408_mesic")] <- "mesic"
df_201408 <- df_201408 %>% select(-ID)
head(df_201408)

# Temperature and Ppt 2019
cov201908=rast('ppt_exports//029ppt_2019_08.tif')
cov201908 <- terra::project(cov201908, crs(mesic200408))
ppt201908 = cov201908[[1]]
ppt201908_values <- values(ppt201908)
ppt201908_nor <- (ppt201908_values - mean(ppt201908_values, na.rm = TRUE)) / (2 * sd(ppt201908_values, na.rm = TRUE))
ppt201908_norm <- setValues(ppt201908, ppt201908_nor)
precip_201908 <- terra::extract(ppt201908_norm, sampled)

tem201908 = cov201908[[2]]
tem201908_values <- values(tem201908)
tem201908_nor <- (tem201908_values - mean(tem201908_values, na.rm = TRUE)) / (2*sd(tem201908_values, na.rm = TRUE))
tem201908_norm <- setValues(tem201908, tem201908_nor) 
temp_201908 <- terra::extract(tem201908_norm, sampled)

df_201908 <- cbind(coords, mesic_201908, precip_201908, temp_201908,landtenure, year =2019)
colnames(df_201908)[which(colnames(df_201908) == "040029_201908_mesic")] <- "mesic"
df_201908 <- df_201908 %>% select(-ID)
head(df_201908)

# Combine all years into a single dataset
data <- rbind(df_200408, df_200908, df_201408, df_201908)
head(dat)
data <- na.omit(dat)  # Remove rows with NA values

#data$mesic[data$mesic<=0]<- 0.00001
#data$mesic[data$mesic>=1]<- 0.999999
#head(data)

#library(INLA)
#url <- "https://cran.r-project.org/src/contrib/Archive/rgdal/rgdal_1.6-7.tar.gz"
#install.packages(url,type="source", repos=NULL)



coords = cbind(data$x, data$y)
#Convexity: prevent boundary effects but not excessively large. #Concavity: 
non_convex_bdry <- inla.nonconvex.hull(coords, -0.06, -0.15, resolution = c(100, 100))
mesh <- inla.mesh.2d(boundary = non_convex_bdry, max.edge=c(4000,4500),
                     offset = c(0.05, 0.1), #set the triangles out of the blue zone. 
                     cutoff = 1500) ##eliminates points within a certain distance of each other, reducing complexity
plot(mesh)
lines(PRborder, col = 3)
points(coords[, 1], coords[, 2], pch = 19, cex = 0.5, col = "red")

# This uses an A matrix, which essentially translates spatial locations on the mesh into vectors in the model
A<-inla.spde.make.A(mesh=mesh,loc=as.matrix(coords));dim(A)
spde <- inla.spde2.matern(mesh, alpha=2)
spatial_index <- inla.spde.make.index(name = "spatial", spde$n.spde)

data$year_index <- (data$year - mean(data$year, na.rm = TRUE)) / (2 * sd(data$year, na.rm = TRUE))
data$first <- ifelse(data$first != 0 & data$first != 1, 
                ifelse(data$first > 0.5, 1, 0), 
                data$first)

# Stack the data for INLA
stack <- inla.stack(data = list(y = data$mesic),  
                    A = list(A, 1),  
                    effects = list(spatial_index,  
                                   data.frame(intercept = 1, 
                                              temp = data$tmean,
                                              landc = data$first,
                                              precip =data$ppt,
                                              year_index = data$year_index,
                                              year_index2 = data$year_index
                                              )),  
                    tag = "estimation")

formula <- y ~ -1 + intercept+ temp + factor(landc)+ precip + year_index:precip + year_index2 +
  f(spatial, model = spde)+ 
  f(year_index, model = "ar1") 

result <- inla(formula, family = "Gaussian",  
               data = inla.stack.data(stack),  
               control.predictor = list(A = inla.stack.A(stack), compute = TRUE),  
               control.compute = list(dic = TRUE, return.marginals.predictor = TRUE),
               #verbose = TRUE
               )

summary(result)
plot(result)
print(result)

######### Plotting the results #########
########################################

library(ggplot2)
library(dplyr)  
# Extract fixed effects and filter for precip, temp, and their interaction
fixed_effects <- data.frame(
  Term = rownames(result$summary.fixed),  
  Mean = result$summary.fixed$mean,
  Lower = result$summary.fixed$`0.025quant`,
  Upper = result$summary.fixed$`0.975quant`
) %>% 
  filter(Term %in% c("precip", "temp", "precip:year_index","year_index2" ))  

# Add a custom color palette
colors <- c("precip" = "#1f77b4", "temp" = "#ff7f0e", "precip:year_index" = "#2ca02c")

# Plot effect sizes with luxury styling
ggplot(fixed_effects, aes(x = reorder(Term, Mean), y = Mean, color = Term)) +
  geom_point(size = 5, shape = 16) +  # Large points
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, linewidth = 1) +
  coord_flip() +
  scale_color_manual(values = colors) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.text.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "none"
  ) +
  labs(title = "Effect Sizes of Precipitation, Temperature, and Interaction", 
       y = "Effect Size", x = "")  



par(mfrow=c(3,4))

# Plot for landT 1 with shaded area
marginal_landT1 <- result$marginals.fixed[[4]]
plot(marginal_landT1, ty = "l", xlab = expression(gamma[landc]), ylab = "landT 1", col = "blue")
polygon(c(marginal_landT1[,1], rev(marginal_landT1[,1])),
        c(rep(0, length(marginal_landT1[,1])), rev(marginal_landT1[,2])),
        col = rgb(0, 0, 1, alpha = 0.2), border = NA)

# Plot for landT 0 with shaded area
marginal_landT0 <- result$marginals.fixed[[3]]
plot(marginal_landT0, ty = "l", xlab = expression(gamma[landc]), ylab = "landT 0", col = "red")
polygon(c(marginal_landT0[,1], rev(marginal_landT0[,1])),
        c(rep(0, length(marginal_landT0[,1])), rev(marginal_landT0[,2])),
        col = rgb(1, 0, 0, alpha = 0.2), border = NA)

# Plot for temp with shaded area
marginal_temp <- result$marginals.fixed[[2]]
plot(marginal_temp, ty = "l", xlab = expression(gamma[temp]), ylab = "temp", col = "green")
polygon(c(marginal_temp[,1], rev(marginal_temp[,1])),
        c(rep(0, length(marginal_temp[,1])), rev(marginal_temp[,2])),
        col = rgb(0, 1, 0, alpha = 0.2), border = NA)

# Plot for Precip with shaded area
marginal_precip <- result$marginals.fixed[[5]]
plot(marginal_precip, ty = "l", xlab = expression(gamma[precip]), ylab = "Precip", col = "purple")
polygon(c(marginal_precip[,1], rev(marginal_precip[,1])),
        c(rep(0, length(marginal_precip[,1])), rev(marginal_precip[,2])),
        col = rgb(0.5, 0, 0.5, alpha = 0.2), border = NA)

# Plot for Precip&Year with shaded area
marginal_precip_year <- result$marginals.fixed[[7]]
plot(marginal_precip_year, ty = "l", xlab = expression(gamma[precip:year_index]), ylab = "Precip&Year", col = "orange")
polygon(c(marginal_precip_year[,1], rev(marginal_precip_year[,1])),
        c(rep(0, length(marginal_precip_year[,1])), rev(marginal_precip_year[,2])),
        col = rgb(1, 0.5, 0, alpha = 0.2), border = NA)

# Plot for Year with shaded area
marginal_year <- result$marginals.fixed[[6]]
plot(marginal_year, ty = "l", xlab = expression(gamma[year_index2]), ylab = "Year", col = "brown")
polygon(c(marginal_year[,1], rev(marginal_year[,1])),
        c(rep(0, length(marginal_year[,1])), rev(marginal_year[,2])),
        col = rgb(0.6, 0.3, 0, alpha = 0.2), border = NA)




print(result$summary.hyperpar)
#Plots

# Extract observed and predicted values
y_obs <- data$mesic  
print(length(y_obs))
y_pred <- result$summary.fitted.values$mean[1:length(y_obs)]
print(length(y_pred))
anyNA(y_obs)
anyNA(y_pred)
valid_indices <- !is.na(y_obs)  # Identify non-NA indices
y_obs_clean <- y_obs[valid_indices]  # Remove NA values from observed data
y_pred_clean <- y_pred[valid_indices]  # Keep only corresponding predicted values
print(length(y_pred_clean))
print(length(y_obs_clean))
# Compute MAE after cleaning
MAE <- mean(abs(y_pred_clean - y_obs_clean))
MAE
# Compute RMSE after cleaning the data
RMSE <- sqrt(mean((y_pred_clean - y_obs_clean)^2))
RMSE


# Plot the posterior distribution
ggplot(designation_df, aes(x = x, y = y)) +
  geom_line(color = "blue", size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + 
  labs(title = "Posterior Distribution of Designation Coefficient",
       x = "Coefficient Value",
       y = "Density") +
  theme_minimal()


#plots for hyper parameters
model1.res<-inla.spde2.result(result, 'spatial', spde, do.transf=TRUE) 
par(mfrow=c(1,3))
plot(model1.res$marginals.var[[1]], ty = "l", xlab = expression(sigma[randomfield]^2), ylab = "Density") 
plot(model1.res$marginals.kap[[1]], type = "l", xlab = expression(kappa), ylab = "Density")
plot(model1.res$marginals.range[[1]], type = "l", xlab = "range nominal", ylab = "Density")

###
#looking at the spatial field and what it looks like
library(lattice)
library(gridExtra)
fitted_temp <- result$summary.random$spatial$mean
fitted_tempsd <- result$summary.random$spatial$sd
gproj <- inla.mesh.projector(mesh,  dims = c(500, 500))
g.mean <- inla.mesh.project(gproj, fitted_temp)
g.sd <- inla.mesh.project(gproj, fitted_tempsd)

grid.arrange(levelplot(g.mean, scales=list(draw=F), xlab='', ylab='', main='mean',col.regions = heat.colors(16)),
             levelplot(g.sd, scal=list(draw=F), xla='', yla='', main='sd' ,col.regions = heat.colors(16)), nrow=1)


#
library(ggplot2)
fitted_values <- result$summary.fitted.values$mean[1:length(y_obs)]
fitted_values <- fitted_values[valid_indices]

# Match row count
plot_data <- data.frame(Temperature = data$tmean, Fitted_Mesic_Vegetation = fitted_values)

ggplot(plot_data, aes(x = Temperature, y = Fitted_Mesic_Vegetation)) +
  geom_point(color = "blue", alpha = 0.6) +  # Scatter plot of observed data
  geom_smooth(method = "loess", color = "red", se = TRUE, fill = "lightpink") +  # Trend line with confidence interval
  labs(title = "Influence of Temperature on Mesic Vegetation",
       x = "scaled Temperature (c)",
       y = "Fitted Mesic Vegetation") +
  theme_minimal()

########### Marginal Effect

# Now plot using this cleaned data
ggplot(plot_data, aes(x = Temperature, y = Fitted_Mesic_Vegetation)) +
  geom_smooth(method = "loess", color = "red", se = TRUE, fill = "lightpink") +  # Trend line with confidence interval
  labs(title = "Influence of Temperature on Mesic Vegetation",
       x = "scaled Temperature (c)",
       y = "Fitted Mesic Vegetation") +
  theme_minimal()

################
plot(fitted_values, data$mesic,
     xlab = "Fitted", ylab = "Observed")
abline(0, 1, col = "red")

library(ggplot2)
library(dplyr)

# Create data frame from vectors
df <- data.frame(Fitted = fitted_values, Observed = data$mesic)

# Round to create pixel bins (like raster grid cells)
df <- df %>%
  mutate(
    Fitted_bin = round(Fitted, 2),
    Observed_bin = round(Observed, 2)
  ) %>%
  count(Fitted_bin, Observed_bin)

# Plot as if it were raster pixels
ggplot(df, aes(x = Fitted_bin, y = Observed_bin, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Fitted", y = "Observed", fill = "Pixel Count") +
  theme_minimal(base_size = 8)

#####

residuals <- data$mesic - result$summary.fitted.values$mean
hist(residuals, main = "Residuals")

