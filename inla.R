#install.packages("INLA", repos=c(getOption("repos"),
#                                 INLA="https://inla.r-inla-download.org/R/stable"),
#                 dependencies=TRUE)
library(INLA)
library(raster)
library(sp)
library(sf)
library(terra)
#install.packages("sf")
library(rgdal)
library(dplyr)
install.packages("rgdal")
setwd('//cifs-prd-01//Research//JodiBrandt//Common//Juan//622project//622project')
# Load rasters
mesic200408 <- rast("earth-engine-export//WRP-040029-200408.tif")
mesic200908 <- rast("earth-engine-export//WRP-040029-200908.tif")
mesic201408 <- rast("earth-engine-export//WRP-040029-201408.tif")
mesic201908 <- rast("earth-engine-export//WRP-040029-201908.tif")
landtenur <- rast('TenureRasterExport.tif')
#Built_Up_Area_2018 <- rast("Built_Up_Area_2018.tif")
#area<- terra::project(Built_Up_Area_2018, "EPSG:4326")
# Randomly sample 500 pixels for each raster (keeping coordinates)
mesic_200408 <- spatSample(mesic200408, size = 500, method = "random", na.rm = TRUE, xy = TRUE)
# Convert to spatial points
coords <- mesic_200408[, c("x", "y")]
# Extract values from all rasters at the same locations
mesic_200908 <- extract(mesic200908, coords)
mesic_201408 <- extract(mesic201408, coords)
mesic_201908 <- extract(mesic201908, coords)
coords <- data.frame(  x = mesic_200408$x,   y = mesic_200408$y)
sampled <- vect(coords, geom = c("x", "y"), crs = crs(mesic200408))
#LandTenure
landtenure <- extract(landtenur, sampled)
landtenure[is.na(landtenure)] <- 0
#Settlement 2018
#Temperature and Ppt 2004
cov200408=rast('ppt_exports//029ppt_2004_08.tif')
cov200408 <- terra::project(cov200408, crs(mesic200408))
ppt200408 = cov200408[[1]]
ppt200408_values <- values(ppt200408)
ppt200408_nor <- (ppt200408_values - mean(ppt200408_values, na.rm = TRUE)) / 2*sd(ppt200408_values, na.rm = TRUE)
ppt200408_norm <- setValues(ppt200408, ppt200408_nor)
precip_200408 <- extract(ppt200408_norm, sampled)

tem200408 = cov200408[[2]]
tem200408_values <- values(tem200408)
tem200408_nor <- (tem200408_values - mean(tem200408_values, na.rm = TRUE)) / 2*sd(tem200408_values, na.rm = TRUE)
tem200408_norm <- setValues(tem200408, tem200408_nor) 
temp_200408 <- extract(tem200408_norm, sampled)

df_200408 <- cbind(mesic_200408, precip_200408, temp_200408,landtenure, year =2004)
colnames(df_200408)[which(colnames(df_200408) == "040029_200408_mesic")] <- "mesic"
head(df_200408)
dim(coords)
df_200408 <- df_200408 %>% select(-ID) # to remove ID columns

# Temperature and Ppt 2009
cov200908=rast('ppt_exports//029ppt_2009_08.tif')
cov200908 <- terra::project(cov200908, crs(mesic200408))
ppt200908 = cov200908[[1]]
ppt200908_values <- values(ppt200908)
ppt200908_nor <- (ppt200908_values - mean(ppt200908_values, na.rm = TRUE)) / 2*sd(ppt200908_values, na.rm = TRUE)
ppt200908_norm <- setValues(ppt200908, ppt200908_nor)
precip_200908 <- extract(ppt200908_norm, sampled)


tem200908 = cov200908[[2]]
tem200908_values <- values(tem200908)
tem200908_nor <- (tem200908_values - mean(tem200908_values, na.rm = TRUE)) / 2*sd(tem200908_values, na.rm = TRUE)
tem200908_norm <- setValues(tem200908, tem200908_nor) 
temp_200908 <- extract(tem200908_norm, sampled)


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
ppt201408_nor <- (ppt201408_values - mean(ppt201408_values, na.rm = TRUE)) / 2*sd(ppt201408_values, na.rm = TRUE)
ppt201408_norm <- setValues(ppt201408, ppt201408_nor)
precip_201408 <- extract(ppt201408_norm, sampled)



tem201408 = cov201408[[2]]
tem201408_values <- values(tem201408)
tem201408_nor <- (tem201408_values - mean(tem201408_values, na.rm = TRUE)) / 2*sd(tem201408_values, na.rm = TRUE)
tem201408_norm <- setValues(tem201408, tem201408_nor) 
temp_201408 <- extract(tem201408_norm, sampled)


df_201408 <- cbind(coords, mesic_201408, precip_201408, temp_201408,landtenure, year =2014)
colnames(df_201408)[which(colnames(df_201408) == "040029_201408_mesic")] <- "mesic"
df_201408 <- df_201408 %>% select(-ID)
head(df_201408)


# Temperature and Ppt 2019
cov201908=rast('ppt_exports//029ppt_2019_08.tif')
cov201908 <- terra::project(cov201908, crs(mesic200408))
ppt201908 = cov201908[[1]]
ppt201908_values <- values(ppt201908)
ppt201908_nor <- (ppt201908_values - mean(ppt201908_values, na.rm = TRUE)) / 2*sd(ppt201908_values, na.rm = TRUE)
ppt201908_norm <- setValues(ppt201908, ppt201908_nor)
precip_201908 <- extract(ppt201908_norm, sampled)


tem201908 = cov201908[[2]]
tem201908_values <- values(tem201908)
tem201908_nor <- (tem201908_values - mean(tem201908_values, na.rm = TRUE)) / 2*sd(tem201908_values, na.rm = TRUE)
tem201908_norm <- setValues(tem201908, tem201908_nor) 
temp_201908 <- extract(tem201908_norm, sampled)

df_201908 <- cbind(coords, mesic_201908, precip_201908, temp_201908,landtenure, year =2019)
colnames(df_201908)[which(colnames(df_201908) == "040029_201908_mesic")] <- "mesic"
df_201908 <- df_201908 %>% select(-ID)
head(df_201908)

# Combine all years into a single dataset
data <- rbind(df_200408, df_200908, df_201408, df_201908)

data$mesic[data$mesic<=0]<-0.00001
data$mesic[data$mesic>1]<-0.999999
head(data)
print(range(data$mesic))
#library(INLA)
url <- "https://cran.r-project.org/src/contrib/Archive/rgdal/rgdal_1.6-7.tar.gz"
install.packages(url,type="source", repos=NULL)
#library('rgdal')
library('INLA')# Define the boundary for the mesh using the convex hull of the data points
#library(Matrix)
coords = cbind(data$x, data$y)
non_convex_bdry <- inla.nonconvex.hull(coords, -0.03, -0.05, resolution = c(80, 80))
mesh <- inla.mesh.2d(boundary = non_convex_bdry, max.edge=c(0.04,0.5),#controls the maximum size of the triangles
#Smaller first value near the boundary ensures more detailed resolution in areas of interest.
#The second value is for the outer areas where you can afford to have larger triangles, which helps reduce the computational load.
#A max.edge = 0.1 means the maximum edge of the mesh triangles near the boundary is 0.1 degrees, which corresponds to about 11.1 km
                     offset = c(0.1, 0.4), #set the triangles out of the blue zone, helping to prevent overly tight mesh triangles and ensuring smoothness and flexibility 
                     cutoff = 0.1) ##eliminates points within a certain distance of each other, reducing complexity
plot(mesh)
lines(PRborder, col = 3)
points(coords[, 1], coords[, 2], pch = 19, cex = 0.5, col = "red")
#boundary <- inla.nonconvex.hull(as.matrix(cbind(data$x, data$y)), convex = -0.05)

# Create the mesh
#mesh <- inla.mesh.2d(loc = cbind(data$x, data$y), 
#                    boundary = boundary, 
#                     max.edge = c(0.3, 1.5), 
#                    cutoff = 0.1)
# After the mesh has been set up, we need to feed INLA a way to convert this into a model format. 
# This uses an A matrix, which essentially translates spatial locations on the mesh into vectors in the model
A<-inla.spde.make.A(mesh=mesh,loc=as.matrix(coords));dim(A)
spde <- inla.spde2.matern(mesh, alpha=2)
spatial_index <- inla.spde.make.index(name = "spatial", spde$n.spde)
#spde <- inla.spde2.matern(mesh = mesh, alpha = 2)
# Define spatial random effect
#A <- inla.spde.make.A(mesh, loc = cbind(data$x, data$y))
#spatial_index <- inla.spde.make.index("spatial", n.spde = spde$n.spde)
# Define temporal structure
data$year_index <- as.factor((data$year))
data$year_index[data$year_index == 2]<-5
data$year_index[data$year_index == 3]<-10
data$year_index[data$year_index == 4]<-15
data$ppt_year <- data$ppt * data$year

# Stack the data for INLA
stack <- inla.stack(data = list(y = data$mesic),  
                    A = list(A, 1),  
                    effects = list(spatial_index,  
                                   data.frame(intercept = 1, 
                                              precip = data$ppt, 
                                              temp = data$tmean,
                                              landc = data$first,
                                              year = data$year,
                                              #temp_year= data$temp_year,
                                              #ppt_year = data$ppt_year,
                                              year_index = data$year_index)),  
                    tag = "estimation")

formula <- y ~ -1 + intercept + precip + temp + factor(landc)+ factor(year)+
  f(spatial, model = spde) + 
  f(year_index, model = "rw1") #If the trend over time is nonlinear

result <- inla(formula, family = "beta",  
               data = inla.stack.data(stack),  
               control.predictor = list(A = inla.stack.A(stack), compute = TRUE),  
               control.compute = list(dic = TRUE, waic = TRUE),
               #verbose = TRUE
               )

summary(result)

par(mfrow=c(2,2))
plot(result$marginals.fixed[[1]], ty = "l", xlab = expression(beta[0]), ylab = "Density") 
plot(result$marginals.fixed[[2]], ty = "l", xlab = expression(beta[precip]), ylab = "precip") 
plot(result$marginals.fixed[[3]], ty = "l", xlab = expression(beta[temp]), ylab = "temp") 
plot(result$marginals.fixed[[3]], ty = "l", xlab = expression(beta[ppt_year]), ylab = "landc") 
print(result$summary.hyperpar)
#Plots

# Extract observed and predicted values
y_obs <- data$mesic  
y_pred <- result$summary.fitted.values$mean[1:length(y_obs)]  
# Compute MAE and RMSE
MAE <- mean(abs(y_obs - y_pred))
RMSE <- sqrt(mean((y_obs - y_pred)^2))
# Print results
cat("MAE:", MAE, "\nRMSE:", RMSE, "\n")


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

gproj <- inla.mesh.projector(mesh,  dims = c(500, 500))
g.mean <- inla.mesh.project(gproj, result$summary.random$spatial$mean)
g.sd <- inla.mesh.project(gproj, result$summary.random$spatial$sd)

grid.arrange(levelplot(g.mean, scales=list(draw=F), xlab='', ylab='', main='mean',col.regions = heat.colors(16)),
             levelplot(g.sd, scal=list(draw=F), xla='', yla='', main='sd' ,col.regions = heat.colors(16)), nrow=1)