
# Installing libraries ----------------------------------------------------
#If you do not have the libraries below, install them before running the code
#Uncomment the line below to install the libraries
# install.packages(c("rnaturalearth", "ncdf4", "terra", "tidyr",
#                    "dplyr", "stringr", "ggplot2"))

# Loading libraries -------------------------------------------------------
library(ncdf4)
library(terra)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)

# Checking data -----------------------------------------------------------
#We will look into the contents of our file first to understand its structure
nc_file <- nc_open("Data/sst.ltm.nc")
#Now we will check results
nc_file
#You can now see the contents of the file (monthly climatological sea surface 
#temperature, SST) for the entire globe between Jan 1991 and Dec 2020.

# Loading data ------------------------------------------------------------
ras_sst_clim <- rast("Data/sst.ltm.nc")
#Checking data
ras_sst_clim
#We can see that we have a raster with 12 layer, one for each month within a year

# Plotting data -----------------------------------------------------------
#We can plot all timesteps at once
plot(ras_sst_clim)

#Or just a single timestep - For example, the month of October
plot(ras_sst_clim[[10]])


# Subsetting data ---------------------------------------------------------
#We will extract values for the Southern Hemisphere
#We define our extent (longitude min, longitude max, latitude min, latitude max)
south_e <- ext(0, 360, -90, 0)

#Now we crop our original raster
ras_sst_clim_south <- crop(ras_sst_clim, south_e)

#Plotting the cropped data
plot(ras_sst_clim_south[[10]])


# Plotting with ggplot2 ---------------------------------------------------
#As we have been so far, plots from rasters are ok, but not great. We will
#now use ggplot2 to make nicer looking graphs

#The first step is to turn our raster into a data frame 
df_sst_clim_south <- ras_sst_clim_south |>  
  #Turn into data frame
  as.data.frame(xy = T) |> 
  #Keep columns for SST only
  select(x, y, starts_with("sst")) |>
  #At the moment, our data frame has one column for each climatological month
  #We will manipulate the data frame, so that monthly data appears in a single
  #column - We use all columns except x and y (coordinates)
  pivot_longer(-c(x, y), names_to = "month", values_to = "values") |> 
  #From the column names, we will extract the month
  mutate(month = factor(month.name[as.numeric(str_remove(month, "sst_"))], 
                        levels = month.name, ordered = T),
         #Finally, we will change longitudes from 0-360 to -180 to +180
         x = (x+180)%%360-180)

#Checking results
glimpse(df_sst_clim_south)

#Loading a map of the world
world <- rnaturalearth::ne_countries(returnclass = "sf")

#Plotting data
df_sst_clim_south |> 
  ggplot()+
  #We use tiles to plot data as raster
  geom_tile(aes(x, y, fill = values))+
  #Changing colour palette
  scale_fill_distiller(palette = "YlOrRd", direction = 1)+
  #We add the world map
  geom_sf(inherit.aes = F, data = world)+
  #Only showing the southern hemisphere
  lims(y = c(-90, 0))+
  #Put each month in a different subplot
  facet_wrap(~month)


# Calculating time series -------------------------------------------------
#Using the data frame above, we will calculate the mean for each month
#across the entire Southern Hemisphere
ts_sst_south <- df_sst_clim_south |> 
  #We will group data by month
  group_by(month) |> 
  #..and calculate the mean value - we will ignore any NA values
  summarise(month_mean_south = mean(values, na.rm = T))

#We can now plot these values
ts_sst_south |> 
  ggplot(aes(month, month_mean_south))+
  #We use group = 1, because the month names is NOT numeric data and ggplot does
  #not know how to deal with this otherwise
  geom_line(aes(group = 1), color = "blue")+
  #Removing the grey background
  theme_bw()



# Calculating anomalies ---------------------------------------------------
#Using the data frame above, we will calculate the annual mean for the Southern 
#Hemisphere, we will then substract the monthly means values we calculated above
south_mean <- df_sst_clim_south |> 
  summarise(mean_south = mean(values, na.rm = T)) |> 
  pull(mean_south)

#Calculating anomalies
anom_south_sst <- ts_sst_south |> 
  #Adding mean value from southern hemisphere
  mutate(south_mean = south_mean,
         #Calculating anomalies
         anomalies = month_mean_south-south_mean)

#Plotting results
anom_south_sst |> 
  #Month in x axis
  ggplot(aes(month, group = 1))+
  #Plotting anomalies as red line 
  geom_line(aes(y = anomalies), color = "red")+
  #Plotting zero line in black for reference
  geom_hline(yintercept = 0, linetype = "dashed")+
  #Removing grey background
  theme_bw()


#How to save a plot?
#We can assign it to a variable as shown below
myplot <- anom_south_sst |> 
  #Month in x axis
  ggplot(aes(month, group = 1))+
  #Plotting anomalies as red line 
  geom_line(aes(y = anomalies), color = "red")+
  #Plotting zero line in black for reference
  geom_hline(yintercept = 0, linetype = "dashed")+
  #Removing grey background
  theme_bw()

#This does not bring up the plot in the viewer, but if you call the variable,
#it will show it
myplot

#We use ggsave to save the graph in our Output folder
ggsave(myplot, filename = "myplot.png", path = "Output")
