#' ---
#' title : "RIPARU - read microplastics"
#' author : Aubin Woehrel
#' date : 2024-09-23
#' version : 1.0
#' ---
#'
#' =============================================================================
#' 
#' MARE VIVU RIPARU - READ MICROPLASTICS FUNCTION
#' 
#' Description : 
#' This script contains the function enabling the reading of the microplastics
#' data from the excel file.
#' 
#' =============================================================================



load_microplastics <- function(file_path, sheet_names) {
  
  data <- tibble()
  
  for (sheet_name in sheet_names) {
    print(sheet_name)
    
    # Read the data from the excel file
    sheet_data <- read.xlsx(xlsxFile = file_path, fillMergedCells = TRUE, colNames = FALSE, 
                            sheet = sheet_name, rows = 1:34)
    
    # Read specific spatial coordinates data
    
    sheet_coordinates <- read.xlsx(xlsxFile = file_path, fillMergedCells = TRUE, colNames = FALSE, 
                                   sheet = sheet_name, rows = 37:40)
    
    
    # Replace all "X" or "x" by NA
    sheet_data[sheet_data == "X"] <- NA
    sheet_data[sheet_data == "x"] <- NA
    
    # Obtaining metadata to be added to the data after transformation
    metadata <- sheet_data[c(1:3, 32), 3:ncol(sheet_data)] %>%
      t() %>%
      as.data.frame()
    
    colnames(metadata) <- metadata[1, ]
    metadata <- metadata[-1, ]
    
    # Separate the sheet_coordinate second column into two columns by the , separator
    sheet_coordinates <- sheet_coordinates %>%
      separate(X2, into = c("Latitude", "Longitude"), sep = ",") %>%
      rename(Point = X1)
    
    # Round the  Latitude and longitude to 6 decimals
    sheet_coordinates <- sheet_coordinates %>%
      mutate(across(c(Latitude, Longitude), function(x) round(as.numeric(x), 6)))
    
    # Pivot larger the coordinates to obtain one line
    sheet_coordinates <- sheet_coordinates %>%
      pivot_wider(names_from = Point, 
                  values_from = c(Latitude, Longitude))
    
    # Duplicate the coordinates to match the number of rows of the metadata and add those columns to the metadata
    sheet_coordinates <- sheet_coordinates %>%
      slice(rep(1:n(), each = nrow(metadata))) 
    
    metadata <- cbind(metadata, sheet_coordinates)
    
    
    # Convert Dates to better format and ignore all missing dates
    metadata <- metadata %>%
      filter(!is.na(DATE)) %>%
      mutate(DATE = openxlsx::convertToDate(DATE)) %>%
      rename(Date = DATE)
    
    # Better names and addition of remark
    names(metadata) <- str_to_sentence(names(metadata))
    metadata$Remarque[is.na(metadata$Remarque)] <- "Aucune"
    
    
    # Transform the sheet_data into a tidy format
    sheet_data[4, 4:ncol(sheet_data)] <- sheet_data[3, 4:ncol(sheet_data)]
    sheet_data <- sheet_data[-c(1:3, 30:32), ]
    sheet_data <- sheet_data[, !is.na(sheet_data[1, ])]
    
    colnames(sheet_data) <- sheet_data[1, ]
    sheet_data <- sheet_data[-1, ]
    
    sheet_data <- sheet_data %>%
      pivot_longer(cols = -c("Catégorie", "Type", "Description"), 
                   names_to = "Date", 
                   values_to = "Value") %>%
      mutate(Date = openxlsx::convertToDate(Date),
             Site = sheet_name)
    
    sheet_data <- sheet_data %>%
      left_join(metadata, by = c("Date" = "Date"))
    
    sheet_data <- sheet_data %>%
      pivot_wider(names_from = Catégorie, 
                  values_from = Value)
    
    # Remove all empty strings at the end of the Type and Description columns
    sheet_data$Type <- str_trim(sheet_data$Type)
    sheet_data$Description <- str_trim(sheet_data$Description)
    
    colnames(sheet_data) <- str_replace_all(colnames(sheet_data), "\\s", "_")
    colnames(sheet_data) <- str_replace_all(colnames(sheet_data), "\\(", "")
    colnames(sheet_data) <- str_replace_all(colnames(sheet_data), "\\)", "")
    colnames(sheet_data) <- str_replace_all(colnames(sheet_data), "é", "e")
    names(sheet_data) <- str_to_sentence(names(sheet_data))
    
    data <- bind_rows(data, sheet_data)
  }
  
  # Transform as factor Annee, Saison, Site and specify levels as order of seasons for seasons. 
  # Then sort by Year, Season and Site.
  data <- data %>%
    mutate(across(c(Annee, Saison, Site), as.factor)) %>%
    mutate(Saison = factor(Saison, levels = c("Printemps", "Eté", "Automne", "Hiver"))) %>%
    arrange(Annee, Saison, Site)
  
  # Convert Meso, Micro and total to numeric and create normalised columns of microplastics/100m 
  # of coastal
  data <- data %>%
    mutate(across(c(Meso_5mm, Micro_1mm, Total), as.numeric)) %>%
    mutate(Meso_normalise = Meso_5mm/5 * 100,
           Micro_normalise = Micro_1mm/5 * 100, 
           Total_normalise = Total/5 * 100)
  
  return(data)
  
}
