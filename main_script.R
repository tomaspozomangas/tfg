library(terra)
library(ggplot2)
library(tidyverse)
library(openxlsx)
library(dplyr)
library(xlsx)
install.packages("xlsx")
read_yearly_rasters <- function(year, working_directory,dias = 21) {
  file_paths <- c()
  
  for (i in 1:dias) {
    file_paths[i] <- paste0(working_directory, "/DATA/sept_", year, "/", "sept", i, ".tiff")
  }
  
  spat_raster <- rast(file_paths)
  return(spat_raster)
}

working_directory <- getwd()
sep2019 <- read_yearly_rasters("2019", working_directory)
sep2020 <- read_yearly_rasters("2020", working_directory)
sep2021 <- read_yearly_rasters("2021", working_directory)


for (i in 1:nlyr(sep2019)) {
  
  sep2019[[i]] <- ifel(sep2019[[i]] <= 0, NA, sep2019[[i]])
}

for (i in 1:nlyr(sep2020)) {
  
  sep2020[[i]] <- ifel(sep2020[[i]] <= 0, NA, sep2020[[i]])
}

for (i in 1:nlyr(sep2021)) {
  
  sep2021[[i]] <- ifel(sep2021[[i]] <= 0, NA, sep2021[[i]])
}



# Custom function to compute summary statistics for each raster layer
calcular_estadisticas_resumen <- function(spat_raster) {
  n_capas <- nlyr(spat_raster)
  estadisticas_resumen <- data.frame(
    capa = integer(n_capas),
    promedio = numeric(n_capas),
    maximo = numeric(n_capas),
    mediana = numeric(n_capas),
    minimo = numeric(n_capas),
    desviacion_estandar = numeric(n_capas),
    conteo_mayor_umbral = integer(n_capas),
    conteo_NA = integer(n_capas),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_len(n_capas)) {
    capa <- spat_raster[[i]]
    
    # Set values less than or equal to 0 as NA
    capa <- ifel(capa <= 0, NA, capa)
    
    estadisticas_resumen$capa[i] <- i
    estadisticas_resumen$promedio[i] <- global(capa, fun=mean, na.rm=TRUE)
    estadisticas_resumen$maximo[i] <- global(capa, fun=max, na.rm=TRUE)
    estadisticas_resumen$mediana[i] <- global(capa, fun=median, na.rm=TRUE)
    estadisticas_resumen$minimo[i] <- global(capa, fun=min, na.rm=TRUE)
    estadisticas_resumen$desviacion_estandar[i] <- global(capa, fun=sd, na.rm=TRUE)
    
    # Count the number of values greater than 0.00026507
    valores_mayores <- ifel(capa > 0.00026507, 1, 0)
    estadisticas_resumen$conteo_mayor_umbral[i] <- global(valores_mayores, fun=sum, na.rm=TRUE)
    
    # Count the number of NA values
    valores_NA <- is.na(capa)
    estadisticas_resumen$conteo_NA[i] <- global(valores_NA, fun=sum, na.rm=TRUE)
  }
  
  return(estadisticas_resumen)
}


# Calculate summary statistics for the sep2019 SpatRaster
sep2019_estadisticas_resumen <- calcular_estadisticas_resumen(sep2019)
print(sep2019_estadisticas_resumen)

sep2020_estadisticas_resumen <- calcular_estadisticas_resumen(sep2020)
print(sep2020_estadisticas_resumen)

sep2021_estadisticas_resumen <- calcular_estadisticas_resumen(sep2021)
print(sep2021_estadisticas_resumen)


#Guardar stats
years_stats <- createWorkbook()

addWorksheet(years_stats, "2019")
addWorksheet(years_stats, "2020")
addWorksheet(years_stats, "2021")

writeData(years_stats, sheet = "2019", x = sep2019_estadisticas_resumen)
writeData(years_stats, sheet = "2020", x = sep2020_estadisticas_resumen)
writeData(years_stats, sheet = "2021", x = sep2021_estadisticas_resumen)

saveWorkbook(years_stats, "resultados/years_stats.xlsx")


# Function to convert SpatRaster to data frame
spatRaster_to_df <- function(spat_raster, year) {
  raster_df <- as.data.frame(spat_raster, xy = TRUE)
  colnames(raster_df) <- c("x", "y",paste0("dia", 1:(nlyr(spat_raster))) )
  raster_df$year <- year
  return(raster_df)
}

# Convert SpatRasters to data frames
sep2019_df <- spatRaster_to_df(sep2019, "2019")
sep2020_df <- spatRaster_to_df(sep2020, "2020")
sep2021_df <- spatRaster_to_df(sep2021, "2021")

# Combine the data frames
all_years_df <- rbind(sep2019_df, sep2020_df, sep2021_df)

write.xlsx(all_years_df, "resultados/all_data.xlsx")
write.csv(all_years_df, "resultados/all_data.csv")

sep_long <- all_years_df %>%
  pivot_longer(cols = dia1:dia19, names_to = "day", values_to = "SO2") %>%
  mutate(day = factor(day, levels = paste0("dia", 1:21)))

sep_long2019 <- sep2019_df %>%
  pivot_longer(cols = dia1:dia19, names_to = "day", values_to = "SO2") %>%
  mutate(day = factor(day, levels = paste0("dia", 1:21)))

sep_long2020 <- sep2020_df %>%
  pivot_longer(cols = dia1:dia19, names_to = "day", values_to = "SO2") %>%
  mutate(day = factor(day, levels = paste0("dia", 1:21)))

sep_long2021 <- sep2021_df %>%
  pivot_longer(cols = dia1:dia19, names_to = "day", values_to = "SO2") %>%
  mutate(day = factor(day, levels = paste0("dia", 1:21)))

# Create boxplot
boxplot <- ggplot(sep_long, aes(x = day, y = SO2, color = year)) +
  geom_boxplot() +
  labs(title = "Boxplot of SO2 levels for each day",
       x = "Day",
       y = "SO2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
boxplot

# Create violin plot
violinplot <- ggplot(sep_long2021, aes(x = day, y = SO2)) +
  geom_violin() +
  labs(title = "Violin plot of SO2 levels for each day (2021)",
       x = "Day",
       y = "SO2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
violinplot


#.............................................................chat gpt (tommy)....................................................................

# Load and preprocess rasters for a given year
load_preprocess_rasters <- function(year, working_directory, dias = 21) {
  file_paths <- sprintf("%s/trabajo_R/DATA/sept_%s_sabado/sept%d.tiff", working_directory, year, 1:dias)
  spat_raster <- rast(file_paths)
  
  # Replace values <= 0 with NA
  spat_raster[spat_raster <= 0] <- NA
  
  return(spat_raster)
}
#2. Summary Statistics:
#  
# The terra package provides functions to compute global statistics. You've made good use of the global function. Just ensure you're not doing repetitive operations.
##3#. Data Preparation for Visualization:
  
#  When converting SpatRasters to data frames and combining them, you're on the right track. Just ensure that the structure and naming conventions are consistent across years.
#4. Visualization:

#It's essential to check and ensure that all objects and variables used in the visualization section are defined. The use of ggplot2 for visualization is appropriate.
#Overall:
  
# Loops vs. Vectorized Operations: In R, vectorized operations are generally faster than loops. The terra package is designed to handle vectorized operations efficiently. Use them wherever possible.
#Directory Structure: Instead of hardcoding directory structures, consider passing the entire path or making the structure adaptable based on inputs. This will make the code more flexible and reusable.
#Code Modularity: It's always a good practice to break down the code into modular functions. You've done a good job with this, but always look for opportunities to further modularize, especially if you find yourself repeating certain operations.

#Remember to test the code after making these changes to ensure everything works as expected.