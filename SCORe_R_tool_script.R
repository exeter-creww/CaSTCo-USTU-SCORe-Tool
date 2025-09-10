### R Script tool to locate areas with NBS potential and reduce overland flow to the combined sewer network ###

### Tool intro ###
# Print start message using the default dialog box (tkmessageBox) without loading extra libraries
# Create temporary root
temp_root <- tcltk::tktoplevel()

# Hide the root window to avoid extra visible windows
tcltk::tkwm.withdraw(temp_root)

# Display the message box and set focus to make it appear in front
tcltk::tcl("wm", "attributes", ".", "-topmost", 1)  # Force topmost attribute for RStudio
tcltk::tkmessageBox(
  title = "Tool Introduction",
  message = paste(
    "This tool will generate areas where there is potential for nature-based solutions to reduce overland flow to the combined sewer network.\n\n",
    "A series of pop-up windows will appear throughout to inform and guide users when operating the tool. For more information, refer to the manual provided."
  ),
  icon = "info",
  type = "ok"
)

# Destroy the temporary root window after the message box is closed
tcltk::tkdestroy(temp_root)
#################################################################################################################################################

##### Check and install packages and set up libraries #####

## Inform the user that packages will be installed if required ##
temp_root <- tcltk::tktoplevel()

# Hide the root window to avoid extra visible windows
tcltk::tkwm.withdraw(temp_root)

# Display the message box and set focus to make it appear in front
tcltk::tcl("wm", "attributes", ".", "-topmost", 1)  # Force topmost attribute for RStudio
tcltk::tkmessageBox(
  title = "Package Setup",
  message = paste(
    "The tool will check what packages are currently installed and install the correct packages required to run the tool."
  ),
  icon = "info",
  type = "ok"
)

# Destroy the temporary root window after the message box is closed
tcltk::tkdestroy(temp_root)

## Check the packages installed and install the relevant packages for the user ##
list.of.packages <- c("dplyr", "rstudioapi", "sf", "svDialogs", "terra", "whitebox")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
remove(list.of.packages, new.packages)

# Librarys required ##
library(dplyr)
library(rstudioapi)
library(sf)
library(svDialogs)
library(terra)
library (whitebox)

dlgMessage("The libraries have been installed successfully.", type = "ok")

# Prompt the user to select a directory for saving the Whitebox package
repeat {
  directory <- rstudioapi::selectDirectory(caption = "Select Folder to Save Whitebox Package")
  
  # Check if a valid directory was selected
  if (is.null(directory) || directory == "") {
    # If the user cancels, abort the operation
    stop("Operation aborted by the user. No directory selected.")
  } else {
    cat("Directory selected:", directory, "\n")
    break
  }
}

# Path to the Whitebox Tools executable
whitebox_exe_path <- file.path(directory, "WBT", "whitebox_tools.exe")

# Check if the Whitebox Tools executable exists
if (file.exists(whitebox_exe_path)) {
  cat("Whitebox Tools already installed in the selected directory.\n")
} else {
  # Install Whitebox Tools if not already present
  cat("Whitebox Tools not found. Installing...\n")
  whitebox::wbt_install(pkg_dir = directory)
}

# Initialize Whitebox Tools
whitebox::wbt_init(exe_path = whitebox_exe_path)
cat("Whitebox Tools initialized successfully.\n")

## Inform user whitebox has been successfully installed ##
dlgMessage("The Whitebox package has been successfully installed.", type = "ok")

## Set up folder directory and explain what a working directory is, set up prompt ##
dlgMessage("The working directory is where R will look for and save files during this session,
    You can use the default (your home directory) or specify another location.", type = "ok")

working_directory <- rstudioapi::selectDirectory(caption = "Set up the working directory (default is your home directory)")

# Check if the user provided a working directory, otherwise set the temp directory as default
if (is.null(working_directory) || working_directory == "") {
  # If the user cancels, abort the operation
  stop("Operation aborted by the user. No working directory selected.")
} else {
  rstudioapi::showDialog(
    title = "Working Directory Selected", 
    message = paste("Working Directory selected:\n", working_directory)
  )
}

setwd(working_directory)

### set up temp folder ###
temp_dir <- tempdir()

### Prompt user for key layers ###
# Prompt the user to select the DSM layer file
dlgMessage("Digital elevation model (DEM) data will need to be provided. It is recomended that a digital surface modal (DSM) is used. The data can be freely downloaded from https://environment.data.gov.uk/survey", 
           type = "ok")

#######################################Inputs added########################################################
#### DEM input ####
# Start a repeat loop to ensure a valid file is selected
repeat {
  # Open file selection popup
  user_DSM <- rstudioapi::selectFile(
    caption = "Please select the DSM layer file (e.g., C:/path/to/your/file.tif)", 
    filter = "TIFF files (*.tif;*.tiff)|*.tif;*.tiff"
  )
  
  # Check if the user cancels the dialog
  if (is.null(user_DSM) || user_DSM == "") {
    stop("Operation aborted by the user. No file selected.")
  } else if (file.exists(user_DSM)) {
    # If a valid file is selected, break the loop
    cat("Valid file selected:\n", user_DSM, "\n")
    break
  } else {
    # If the file path is invalid, prompt the user again
    dlgMessage("Invalid file selected. Please make sure the file exists and try again.", type = "ok")
  }
}

# user_DSM now contains the path to the selected file
dlgMessage(paste("Selected DSM layer file path:", user_DSM), type = "ok")

# The user is prompted for the River network layer
dlgMessage("A River network layer is a required input for this script. This data will ensure only areas draining to the sewer network are provided. If river network data is required it can be downloaded freely from https://www.data.gov.uk/dataset/dc29160b-b163-4c6e-8817-f313229bcc23/os-open-rivers1 ", 
           type = "ok")

### River Network Input ###
# Start a repeat loop to ensure a valid file is selected
repeat {
  # Open file selection popup
  user_river <- rstudioapi::selectFile(
    caption = "Please select the river network layer file (e.g., C:/path/to/your/file.shp)",
    filter = "Shapefiles (*.shp)|*.shp"
  )
  
  # Check if the user cancels the dialog
  if (is.null(user_river) || user_river == "") {
    stop("Operation aborted by the user. No file selected.")
  } else if (file.exists(user_river)) {
    # If a valid file is selected, break the loop
    cat("Valid file selected:\n", user_river, "\n")
    break
  } else {
    # If the file path is invalid, prompt the user again
    dlgMessage("Invalid file selected. Please make sure the file exists and try again.", type = "ok")
  }
}

# user_DSM now contains the path to the selected file
dlgMessage(paste("Selected River network layer file path:", user_river), type = "ok")

# The user is prompted for the drainage information layer or IAS
dlgMessage("A impermeable area survey or similar sewer drainage layer data set that provides information on drainage location must be provided. This data must be provided in a shapefile format", 
           type = "ok")

### Add Sewer Drainage Layer ###
# Start a repeat loop to ensure a valid file is selected
repeat {
  # Open file selection popup
  user_IAS <- rstudioapi::selectFile(
    caption = "Please select the Sewer Drainage Data file (e.g., C:/path/to/your/file.shp)",
    filter = "Shapefiles (*.shp)|*.shp"
  )
  
  # Check if the user cancels the dialog
  if (is.null(user_IAS) || user_IAS == "") {
    stop("Operation aborted by the user. No file selected.")
  } else if (file.exists(user_IAS)) {
    # If a valid file is selected, break the loop
    cat("Valid file selected:\n", user_IAS, "\n")
    break
  } else {
    # If the file path is invalid, prompt the user again
    dlgMessage("Invalid file selected. Please make sure the file exists and try again.\n")
  }
}

# user_DSM now contains the path to the selected file
dlgMessage(paste("Selected Sewer Drainage Data file path:", user_IAS), type = "ok")


###load layers required###
DSM_original <- terra::rast(user_DSM)
River_original <- terra::vect(user_river) #if z coordinates are ignored that is fine
IAS_original <- terra::vect(user_IAS) #if z coordinates are ignored that is fine

dlgMessage("The layers have been installed successfully.", type = "ok")

########################################################################################################################################################
### Adding River data to the DSM ###

# The user is informed the river network will be removed from the DSM
dlgMessage("The River Network data will be used to create a barrier in the DSM. This data will ensure only areas draining to the sewer network are provided.", 
           type = "ok")

# Define the new shapefile path
new_shapefile_path <- file.path(temp_dir, "copy_of_river.shp")

# Write the shapefile to the temporary directory
terra::writeVector(River_original, new_shapefile_path, overwrite = TRUE) #overwrite = TRUE allows files to be over written 

# Read the copied shapefile
copied_shapefile <- terra::vect(new_shapefile_path)

# Add a new field called 'new_field' and populate it with 10000
copied_shapefile$new_field <- 10000

# Write the updated shapefile back to the temporary directory
terra::writeVector(copied_shapefile, new_shapefile_path, overwrite = TRUE)

#read in river network with new field 
river_net1 <- terra::vect(new_shapefile_path)

# Check if CRS is the same, and transform polyline if necessary
if (terra::crs(river_net1) != terra::crs(DSM_original)) {
  river_net2 <- terra::project(river_net1, terra::crs(DSM_original))
} else {
  river_net2 <- river_net1
} 

# Use the base raster as a template for resolution and extent
rasterized_river_net <- terra::rasterize(river_net2, DSM_original, field = "new_field")  

## set up prompt for save location ##
# Default suggestion for file output location
default_output <- file.path(tempdir(), "rasterized_river_network.tif")
# Set a default output file path
default_output <- file.path(Sys.getenv("HOME"), "rasterized_river_network.tif")

# Start a repeat loop to ensure a valid save location
repeat {
  # Open a file save dialog using rstudioapi
  save_ras_river <- rstudioapi::selectFile(
    caption = paste(
      "Specify the save location and file name for the rasterized river network.",
      "\nIt must include .tif at the end of the file name.",
      "\n(Default suggestion:", default_output, ")"
    ),
    path = default_output,  # Provide a default suggestion
    label = "Save As",
    existing = FALSE,        # Allow creating a new file
    filter = "TIFF files (*.tif;*.tiff)|*.tif;*.tiff" # only allow tifs to be viable
  )
  
  # Check if the user cancels the dialog
  if (is.null(save_ras_river) || save_ras_river == "") {
    stop("Operation aborted by the user. No save location selected.")
  } else if (grepl("\\.tif$", save_ras_river, ignore.case = TRUE)) {
    # If the file name ends with '.tif', exit the loop
    rstudioapi::showDialog("File Selected", paste("File will be saved to:\n", save_ras_river))
    break
  } else {
    # If the file name does not end with '.tif', show a warning
    rstudioapi::showDialog("Invalid Input", "Invalid input. Please ensure the file name ends with '.tif'.")
  }
}

# Optionally, save the rasterized result
terra::writeRaster(rasterized_river_net, save_ras_river, overwrite = TRUE)

# Load the saved raster into DSM_minus_rivers_highways
DSM_minus_rivers <- terra::rast(save_ras_river)

#print("Rivers removed from DSM completed successfully")
dlgMessage("River removal from the DSM has been completed successfully.", type = "ok")

#########################################################################################################################################################################

### Add the highways data to the DSM ###

# The user is prompted for the River network layer
dlgMessage("The user will need to select the sewer drainage data which is not related to the combined sewer network. This data will ensure only areas draining to the combined sewer network are provided.", 
           type = "ok")

# Step 0: Prompt user to decide if they want to skip this section
repeat {
  # Prompt to ask if the user wants to skip
  skip_highways_section <- showPrompt(
    title = "Skip Section",
    message = "Do you want to skip the section for adding drainage data not draining to the combined network to the DSM?\nType 'Yes' to skip or 'No' to proceed.",
    default = "No"
  )
  
  if (tolower(skip_highways_section) == "yes") {  # User chose "Yes" (to skip)
    # Ask for confirmation
    confirm_skip <- showPrompt(
      title = "Confirm Skip",
      message = "You selected to skip the section for adding drainage data not draining to the combined network to the DSM.\nType 'Yes' to confirm skipping or 'No' to return.",
      default = "Yes"
    )
    
    if (tolower(confirm_skip) == "yes") {  # User confirmed skipping
      showDialog(
        title = "Section Skipped",
        message = "The section for adding drainage data not draining to the combined network to the DSM has been skipped. Moving to the next step."
      )
      break  # Exit the loop and skip the section
    } else {  # User does not confirm skipping
      showDialog(
        title = "Return to Skip Prompt",
        message = "Returning to the previous prompt for selection."
      )
      next  # Restart the loop to re-prompt for skipping
    }
  } else if (tolower(skip_highways_section) == "no") {  # User chose "No" (to proceed with the section)
    showDialog(
      title = "Proceeding",
      message = "You have chosen to run the section for adding drainage data not draining to the combined network to the DSM."
    )
    
    # Place the full code for the section here
    ### Add the drainage data not draining to the combined network to the DSM ###
    # Step 1: Define the new shapefile path
    new_shapefile_path2 <- file.path(tempdir(), "copy_of_IAS1.shp")
    
    # Write the shapefile to the temporary directory
    terra::writeVector(IAS_original, new_shapefile_path2, overwrite = TRUE) #overwrite = TRUE allows files to be overwritten 
    copied_IAS1 <- st_read(new_shapefile_path2)
    
    IAS_sf <- st_as_sf(copied_IAS1)
    
    input_vector <- tryCatch({
      terra::vect(IAS_sf)
    }, error = function(e) {
      stop("Failed to load the shapefile. Please ensure the file is a valid shapefile.")
    })
    
    # Step 2: Extract attribute names from the shapefile
    attribute_options <- colnames(as.data.frame(input_vector))
    
    attribute_options_with_index <- paste(1:length(attribute_options), attribute_options, sep = ": ")
    
    repeat {
      selected_index <- showPrompt(
        title = "Select an Attribute by Number", 
        message = paste("Available attributes:\n", paste(attribute_options_with_index, collapse = "\n"),
                        "\nPlease enter the number corresponding to your choice."),
        default = "1"  # Suggest the first attribute as the default
      )
      
      selected_index <- as.numeric(selected_index)
      
      if (!is.na(selected_index) && selected_index >= 1 && selected_index <= length(attribute_options)) {
        selected_attribute <- attribute_options[selected_index]  # Get the selected attribute by index
        confirm_selection <- showPrompt(
          title = "Confirm Selection",
          message = paste("You selected the attribute:\n", selected_attribute, 
                          "\nType 'Yes' to confirm, or 'No' to go back and reselect."),
          default = "Yes"
        )
        
        if (tolower(confirm_selection) == "yes") {
          showDialog(
            title = "Attribute Selected", 
            message = paste("You have confirmed the selection of the attribute:\n", selected_attribute)
          )
          break  # Exit the loop if confirmed
        } else if (tolower(confirm_selection) == "no") {
          showDialog(
            title = "Selection Cancelled", 
            message = "Please reselect an attribute."
          )
        } else {
          showDialog(
            title = "Invalid Input", 
            message = "Invalid input. Please type 'Yes' to confirm or 'No' to cancel."
          )
        }
      } else {
        showDialog(
          title = "Invalid Selection", 
          message = "The entered number is not valid. Please try again."
        )
      }
    }
    # Step 4: Extract the unique values from the selected attribute
    unique_values <- unique(as.data.frame(input_vector)[[selected_attribute]])
    
    # Step 5: Display the unique values and allow the user to select one or more values
    values_with_index <- paste(1:length(unique_values), unique_values, sep = ": ")
    repeat {
      selected_values_input <- showPrompt(
        title = "Select Values by Number", 
        message = paste("Available values for attribute", selected_attribute, ":\n", 
                        paste(values_with_index, collapse = "\n"),
                        "\nPlease enter the numbers corresponding to the values you want to select (comma-separated e.g. 1, 2, 3)."),
        default = "1"  # Suggest the first value as the default
      )
      
      selected_values_indices <- as.numeric(unlist(strsplit(selected_values_input, ",")))
      if (all(!is.na(selected_values_indices) & selected_values_indices >= 1 & selected_values_indices <= length(unique_values))) {
        selected_values <- unique_values[selected_values_indices]
        
        confirm_values <- showPrompt(
          title = "Confirm Selected Values",
          message = paste("You selected the following values:\n", paste(selected_values, collapse = ", "),
                          "\nType 'Yes' to confirm, or 'No' to go back and reselect."),
          default = "Yes"
        )
        
        if (tolower(confirm_values) == "yes") {
          showDialog(
            title = "Values Selected", 
            message = paste("You have confirmed the selection of the values:\n", paste(selected_values, collapse = ", "))
          )
          break  # Exit the loop if confirmed
        } else if (tolower(confirm_values) == "no") {
          showDialog(
            title = "Selection Cancelled", 
            message = "Please reselect values."
          )
        } else {
          showDialog(
            title = "Invalid Input", 
            message = "Invalid input. Please type 'Yes' to confirm or 'No' to cancel."
          )
        }
      } else {
        showDialog(
          title = "Invalid Selection", 
          message = "The entered numbers are not valid. Please try again."
        )
      }
    }
    
    # Step 6: Filter the shapefile based on the selected values
    # --- Safe filtering ---
    IAS_highways_sf <- copied_IAS1 %>%
      filter(as.character(.data[[selected_attribute]]) %in% as.character(selected_values))
    
    # Check for empty result
    if (nrow(IAS_highways_sf) == 0) {
      stop(paste0("No features found in sewer drainage layer for ", selected_attribute, " = ",
                  paste(selected_values, collapse = ", "),
                  ". Please select a value that exists within the DSM extent."))
    }
    
    # Convert to terra object
    IAS_highways_filtered <- terra::vect(IAS_highways_sf)
    
    # Reproject to DSM CRS if needed
    if (terra::crs(IAS_highways_filtered) != terra::crs(DSM_original)) {
      IAS_highways_filtered <- terra::project(IAS_highways_filtered, terra::crs(DSM_original))
    }
    
    # Check for spatial overlap
    if (is.null(terra::intersect(terra::ext(IAS_highways_filtered), terra::ext(DSM_original)))) {
      stop("The selected sewer drainage layer features do not overlap the DSM extent.")
    }
    
    # Auto-detect geometry type and optionally buffer
    geom_type <- tolower(unique(terra::geomtype(IAS_highways_filtered))[1])
    
    if (geom_type %in% c("polygons")) {
      dlgMessage("Polygon geometry detected — skipping buffering step.", type = "ok")
    } else if (geom_type %in% c("lines", "points")) {
      buffer_choice <- tolower(showPrompt(
        title = "Buffer Option",
        message = paste0("The sewer drainage layer contains ", geom_type, 
                         ". Do you want to apply a buffer before rasterizing? (Yes/No)"),
        default = "Yes"
      ))
      
      if (buffer_choice == "yes") {
        buffer_input <- as.numeric(dlgInput(
          paste0("Enter buffer distance in map units (default = 3):"),
          default = "3"
        )$res)
        
        if (is.na(buffer_input) || buffer_input <= 0) {
          buffer_input <- 3
        }
        
        IAS_highways_filtered <- terra::buffer(IAS_highways_filtered, width = buffer_input)
        dlgMessage(paste("Buffered", geom_type, "by", buffer_input, "map units."), type = "ok")
      } else {
        dlgMessage("No buffer applied.", type = "ok")
      }
    } else {
      dlgMessage(paste("Unknown geometry type detected:", geom_type, "- proceeding without buffer."), type = "ok")
    }
    
    
    
    # Add a new field called 'new_field' and populate it with 10000
    IAS_highways_filtered$new_field <- 10000
    
    # Rasterize to DSM grid with NA background
    DSM_extent <- terra::rast(DSM_original)
    rasterized_highways_net <- terra::rasterize(IAS_highways_filtered, DSM_extent,
                                                field = "new_field", background = NA)
    
    
    # Step 7: Write filtered shapefile
    new_shapefile_path3 <- file.path(tempdir(), "filtered_IAS_highways.shp")
    terra::writeVector(IAS_highways_filtered, new_shapefile_path3, overwrite = TRUE)
    
    # Add a new field called 'new_field' and populate it with 1
    IAS_highways_filtered$new_field <- 10000
    
    # Write the updated shapefile back to the temporary directory
    new_shapefile_path4 <- file.path(tempdir(), "highways_IAS2.shp")
    terra::writeVector(IAS_highways_filtered, new_shapefile_path4, overwrite = TRUE)
    
    # Step 8: Check if CRS is the same, and transform polygon if necessary
    if (terra::crs(IAS_highways_filtered) != terra::crs(DSM_original)) {
      IAS_highways_CRS <- terra::project(IAS_highways_filtered, terra::crs(DSM_original))
    } else {
      IAS_highways_CRS <- IAS_highways_filtered
    }
    
    # Check for spatial overlap
    if (is.null(terra::intersect(terra::ext(IAS_highways_CRS), terra::ext(DSM_original)))) {
      stop("The selected sewer drainage layer features do not overlap the DSM extent.")
    }
    

    # Step 9: Rasterize highways network and add to DSM
    DSM_extent <- terra::rast(DSM_original)
    rasterized_highways_net <- terra::rasterize(IAS_highways_CRS, DSM_extent, field = "new_field")
    
    # Combine the rasterized highway network with the existing DSM layer
    DSM_river_highways <- terra::mosaic(rasterized_river_net, rasterized_highways_net, fun = "last", overwrite = TRUE)
    DSM_River_highways_burn <- terra::mosaic(DSM_original, DSM_river_highways, fun = "last", overwrite = TRUE)
    
    # Step 10: Save final raster
    default_output <- file.path(Sys.getenv("HOME"), "rivers_highways_mosaic.tif")
    
    repeat {
      save_rivers_highways <- rstudioapi::selectFile(
        caption = "Specify the save location and file name for the rasterized drainage data not draining to the combined network layer (must end with .tif).",
        path = default_output,
        label = "Save As",
        existing = FALSE,
        filter = "TIFF files (*.tif;*.tiff)|*.tif;*.tiff"
      )
      
      if (is.null(save_rivers_highways) || save_rivers_highways == "") {
        stop("Operation aborted by the user. No save location selected.")
      } else if (grepl("\\.tif$", save_rivers_highways, ignore.case = TRUE)) {
        break
      } else {
        showDialog("Invalid Input", "Please ensure the file name ends with '.tif'.")
      }
    }
    
    terra::writeRaster(DSM_River_highways_burn, save_rivers_highways, overwrite = TRUE)
    DSM_minus_rivers_highways <- terra::rast(save_rivers_highways)
    dlgMessage("The removal of the drainage data not draining to the combined network from the DSM has been completed successfully.", type = "ok")
    
    break  # Exit the loop after completing the section
  }
}

###############################################################################################################################################################################
### Create Combined Sewer Pour Point ###
## Inform user what the next step is ##
dlgMessage("The next step will allow the user to create a pour point layer from the Sewer Drainage data. The pour point layer will be created using the combined sewer data", type = "ok")

# Define the new shapefile path
new_shapefile_path5 <- file.path(tempdir(), "copy_of_IAS.shp")

# Write the shapefile to the temporary directory
terra::writeVector(IAS_original, new_shapefile_path5, overwrite = TRUE)  # overwrite = TRUE allows files to be overwritten
copied_IAS <- st_read(new_shapefile_path5)

IAS_sf <- st_as_sf(copied_IAS)

# Step 1: Prompt the user to select an attribute from the shapefile
attribute_options <- colnames(as.data.frame(IAS_sf))

# Add index numbers to each attribute option
attribute_options_with_index <- paste(1:length(attribute_options), attribute_options, sep = ": ")

repeat {
  selected_index <- showPrompt(
    title = "Select an Attribute by Number", 
    message = paste("Available attributes:\n", paste(attribute_options_with_index, collapse = "\n"),
                    "\nPlease enter the number corresponding to your choice."),
    default = "1"  # Suggest the first attribute as the default
  )
  
  selected_index <- as.numeric(selected_index)
  
  if (!is.na(selected_index) && selected_index >= 1 && selected_index <= length(attribute_options)) {
    selected_attribute <- attribute_options[selected_index]  # Get the selected attribute by index
    confirm_selection <- showPrompt(
      title = "Confirm Selection",
      message = paste("You selected the attribute:\n", selected_attribute, 
                      "\nType 'Yes' to confirm, or 'No' to go back and reselect."),
      default = "Yes"
    )
    
    if (tolower(confirm_selection) == "yes") {
      showDialog(
        title = "Attribute Selected", 
        message = paste("You have confirmed the selection of the attribute:\n", selected_attribute)
      )
      break  # Exit the loop if confirmed
    } else if (tolower(confirm_selection) == "no") {
      showDialog(
        title = "Selection Cancelled", 
        message = "Please reselect an attribute."
      )
    } else {
      showDialog(
        title = "Invalid Input", 
        message = "Invalid input. Please type 'Yes' to confirm or 'No' to cancel."
      )
    }
  } else {
    showDialog(
      title = "Invalid Selection", 
      message = "The entered number is not valid. Please try again."
    )
  }
}

# Step 2: Extract the unique values from the selected attribute
unique_values <- unique(as.data.frame(copied_IAS)[[selected_attribute]])

# Step 3: Display the unique values and allow the user to select one or more values
values_with_index <- paste(1:length(unique_values), unique_values, sep = ": ")
repeat {
  selected_values_input <- showPrompt(
    title = "Select Values by Number", 
    message = paste("Available values for attribute", selected_attribute, ":\n", 
                    paste(values_with_index, collapse = "\n"),
                    "\nPlease enter the numbers corresponding to the values you want to select (comma-separated e.g. 1, 2, 3)."),
    default = "1"  # Suggest the first value as the default
  )
  
  selected_values_indices <- as.numeric(unlist(strsplit(selected_values_input, ",")))
  if (all(!is.na(selected_values_indices) & selected_values_indices >= 1 & selected_values_indices <= length(unique_values))) {
    selected_values <- unique_values[selected_values_indices]
    
    confirm_values <- showPrompt(
      title = "Confirm Selected Values",
      message = paste("You selected the following values:\n", paste(selected_values, collapse = ", "),
                      "\nType 'Yes' to confirm, or 'No' to go back and reselect."),
      default = "Yes"
    )
    
    if (tolower(confirm_values) == "yes") {
      showDialog(
        title = "Values Selected", 
        message = paste("You have confirmed the selection of the values:\n", paste(selected_values, collapse = ", "))
      )
      break  # Exit the loop if confirmed
    } else if (tolower(confirm_values) == "no") {
      showDialog(
        title = "Selection Cancelled", 
        message = "Please reselect values."
      )
    } else {
      showDialog(
        title = "Invalid Input", 
        message = "Invalid input. Please type 'Yes' to confirm or 'No' to cancel."
      )
    }
  } else {
    showDialog(
      title = "Invalid Selection", 
      message = "The entered numbers are not valid. Please try again."
    )
  }
}

# Step 4: Filter the sewer drainage layer based on the selected values
IAS_sewer_sf <- copied_IAS %>%
  filter(as.character(.data[[selected_attribute]]) %in% as.character(selected_values))

# Check for empty result
if (nrow(IAS_sewer_sf) == 0) {
  stop(paste0("No features found in Sewer Drainage data for ", selected_attribute, " = ",
              paste(selected_values, collapse = ", "),
              ". Please select a value that exists within the DSM extent."))
}

# Convert to terra object
IAS_sewer_filtered <- terra::vect(IAS_sewer_sf)

# Reproject to DSM CRS if needed
if (terra::crs(IAS_sewer_filtered) != terra::crs(DSM_original)) {
  IAS_sewer_filtered <- terra::project(IAS_sewer_filtered, terra::crs(DSM_original))
}

# Check for spatial overlap
if (is.null(terra::intersect(terra::ext(IAS_sewer_filtered), terra::ext(DSM_original)))) {
  stop("The selected Sewer Drainage data features do not overlap the DSM extent.")
}


# Add a new field called 'new_field' and populate it with 1
IAS_sewer_filtered$new_field <- 1

# Rasterize to DSM grid with NA background
DSM_extent <- terra::rast(DSM_original)
rasterized_sewer_net <- terra::rasterize(IAS_sewer_filtered, DSM_extent,
                                         field = "new_field", background = NA)

# Step 5: Convert to terra object
IAS_sewer_filtered <- terra::vect(IAS_sewer_sf)

# Step 6: Write filtered shapefile
new_shapefile_path6 <- file.path(tempdir(), "filtered_IAS_sewer.shp")
terra::writeVector(IAS_sewer_filtered, new_shapefile_path6, overwrite = TRUE)

# Step 7: Check if CRS is the same, and transform polygon if necessary
if (terra::crs(IAS_sewer_filtered) != terra::crs(DSM_original)) {
  IAS_sewer_CRS <- terra::project(IAS_sewer_filtered, terra::crs(DSM_original))
} else {
  IAS_sewer_CRS <- IAS_sewer_filtered
}

# Check for spatial overlap
if (is.null(terra::intersect(terra::ext(IAS_sewer_CRS), terra::ext(DSM_original)))) {
  stop("The selected Sewer Drainage data features do not overlap the DSM extent.")
}

# Auto-detect geometry type and optionally buffer
geom_type <- tolower(unique(terra::geomtype(IAS_sewer_CRS))[1])

if (geom_type %in% c("polygons")) {
  dlgMessage("Polygon geometry detected — skipping buffering step.", type = "ok")
} else if (geom_type %in% c("lines", "points")) {
  buffer_choice <- tolower(showPrompt(
    title = "Buffer Option",
    message = paste0("The Sewer Drainage data layer contains ", geom_type, 
                     ". Do you want to apply a buffer before rasterizing? (Yes/No)"),
    default = "Yes"
  ))
  
  if (buffer_choice == "yes") {
    buffer_input <- as.numeric(dlgInput(
      paste0("Enter buffer distance in map units (default = 3):"),
      default = "3"
    )$res)
    
    if (is.na(buffer_input) || buffer_input <= 0) {
      buffer_input <- 3
    }
    
    IAS_sewer_CRS <- terra::buffer(IAS_sewer_CRS, width = buffer_input)
    dlgMessage(paste("Buffered", geom_type, "by", buffer_input, "map units."), type = "ok")
  } else {
    dlgMessage("No buffer applied.", type = "ok")
  }
} else {
  dlgMessage(paste("Unknown geometry type detected:", geom_type, "- proceeding without buffer."), type = "ok")
}

# Add a new field called 'new_field' and populate it with 1
IAS_sewer_CRS$new_field <- 1

# Write the updated shapefile back to the temporary directory
new_shapefile_path7 <- file.path(tempdir(), "sewer_IAS2.shp")
terra::writeVector(IAS_sewer_CRS, new_shapefile_path7, overwrite = TRUE)



# Step 8: Rasterize sewer network and add to DSM
DSM_extent <- terra::rast(DSM_original)
rasterized_sewer_net <- terra::rasterize(IAS_sewer_CRS, DSM_extent, field = "new_field")

# Step 9: Set up prompt for save location
# Set a default output file path
default_output <- file.path(Sys.getenv("HOME"), "rasterised_sewer_network.tif")

# Start a repeat loop to ensure a valid save location
repeat {
  # Open a file save dialog using rstudioapi
  save_rasterised_sewer_net <- rstudioapi::selectFile(
    caption = paste(
      "Specify the location and filename for the rasterized combined sewer network.",
      "\nIt must include .tif at the end of the file name.",
      "\n(Default suggestion:", default_output, ")"
    ),
    path = default_output,  # Provide a default suggestion
    label = "Save As",
    existing = FALSE,        # Allow creating a new file
    filter = "TIFF files (*.tif;*.tiff)|*.tif;*.tiff"
  )
  
  # Check if the user cancels the dialog
  if (is.null(save_rasterised_sewer_net) || save_rasterised_sewer_net == "") {
    stop("Operation aborted by the user. No save location selected.")
  } else if (grepl("\\.tif$", save_rasterised_sewer_net, ignore.case = TRUE)) {
    # If the file name ends with '.tif', exit the loop
    rstudioapi::showDialog("File Selected", paste("File will be saved to:\n", save_rasterised_sewer_net))
    break
  } else {
    # If the file name does not end with '.tif', show a warning
    rstudioapi::showDialog("Invalid Input", "Invalid input. Please ensure the file name ends with '.tif'.")
  }
}

# Save the reclassified raster to a new file
pour_point_sewer <- terra::writeRaster(rasterized_sewer_net, save_rasterised_sewer_net, overwrite = TRUE)

# Confirmation message
dlgMessage("The pour point layer has been generated successfully.", type = "ok")

########################################################################################################################################################
############################################# Whitebox tools ##################################################################################

## Inform user what the next step is e.g. use whitebox tools to fill the DEM ##
dlgMessage("The next step will fill the DEM. Filling refers to the removal of small imprefections in a DEM by filling in sinks and peaks.", type = "ok")

### Fill DEM using Whitebox ###
## set up default output ##
## set up prompt for user to save the fill output 
default_output <- file.path(Sys.getenv("HOME"), "DEM_Fill.tif")

# Start a repeat loop to ensure a valid save location
repeat {
  # Open a file save dialog using rstudioapi
  save_fill <- rstudioapi::selectFile(
    caption = paste(
      "Save location and layer name for the filled DEM. Must include .tif at the end of the layer name.",
      "\nIt must include .tif at the end of the file name.",
      "\n(Default suggestion:", default_output, ")"
    ),
    path = default_output,  # Provide a default suggestion
    label = "Save As",
    existing = FALSE,        # Allow creating a new file
    filter = "TIFF files (*.tif;*.tiff)|*.tif;*.tiff"
  )
  
  # Check if the user cancels the dialog
  if (is.null(save_fill) || save_fill == "") {
    stop("Operation aborted by the user. No save location selected.")
  } else if (grepl("\\.tif$", save_fill, ignore.case = TRUE)) {
    # If the file name ends with '.tif', exit the loop
    rstudioapi::showDialog("File Selected", paste("File will be saved to:\n", save_fill))
    break
  } else {
    # If the file name does not end with '.tif', show a warning
    rstudioapi::showDialog("Invalid Input", "Invalid input. Please ensure the file name ends with '.tif'.")
  }
}
output_filled_DSM_rivers_highways_burn <- save_fill
## > can also use DSM_rivers_burn or DSM_highways_burn if only one is required, but best to use file pathway here 

## Set up prompt for user to set z limit 
# Prompt the user to set the z limit with a sensible default value of 0.2
set_zlimit <- as.numeric(dlgInput(
  "Set the z limit. A value less than 0.5 is recommended, and the default is 0.2.If the z limit is too high barriers to flow such as walls could be removed and this would create an in accuracy in the final results.",
  "0.2"
)$res)

# Ensure the zlimit is valid (e.g., numeric)
if (is.na(set_zlimit)) {
  cat("Invalid input. Using default z limit of 0.2.\n")
  set_zlimit <- 0.2
}

### define the DEM to be used if the section to remove highways is skipped or not skipped
# Define a default raster for when the section is skipped
default_DSM <- DSM_minus_rivers  # Replace this with the appropriate raster if necessary

# Use a conditional check to assign DSM_minus_rivers_highways
if (exists("DSM_minus_rivers_highways")) {
  final_DEM <- DSM_minus_rivers_highways
} else {
  final_DEM <- default_DSM
}

## Call the Whitebox function to fill depressions using the user-specified zlimit
wbt_fill_depressions(
  dem = final_DEM,
  output = output_filled_DSM_rivers_highways_burn,
  fix_flats = TRUE,
  max_depth = set_zlimit
)

# Read the filled DSM and print a success message
Filled_DSM <- terra::rast(output_filled_DSM_rivers_highways_burn)

#print("DSM fill completed successfully")
dlgMessage("The edited DSM has been filled successfully.", type = "ok")

############################ Optional #############################################

###### expalin to user that the next step is optional #####
dlgMessage("The next step will include using Dinf to generate catchments that drain to the combined sewernetwork. This step can be skipped, if the user decides to skip tehn the catchments will be generated using D8 flow. For more information on Dinf and D8 flow refer to the manual.", type = "ok")

## Running the Dinf tools ##
## Explain to user what the next steps are (e.g., Dinf or D8)
repeat {
  # Prompt the user to decide if they want to skip the Dinf section
  skip_Dinf_section <- tolower(showPrompt(
    title = "Skip Section",
    message = "This section uses Dinf for creating catchments. Would you like to skip this and proceed to the D8 flow section?\nType 'Yes' to skip or 'No' to proceed with Dinf.",
    default = "No"
  ))
  
  # Validate user input
  if (skip_Dinf_section %in% c("yes", "no")) {
    if (skip_Dinf_section == "yes") {  # User chose to skip
      confirm_skip <- tolower(showPrompt(
        title = "Confirm Skip",
        message = "You have chosen to skip the Dinf section. Confirm by typing 'Yes' or return by typing 'No'.",
        default = "Yes"
      ))
      
      if (confirm_skip == "yes") {  # User confirms skipping
        showDialog(
          title = "Section Skipped",
          message = "Dinf section skipped. Proceeding to the D8 flow section."
        )
        break  # Exit loop and skip the section
      } else {  # User cancels skipping
        showDialog(
          title = "Returning",
          message = "Returning to the previous prompt for selection."
        )
      }
    } else if (skip_Dinf_section == "no") {  # User chose to proceed with Dinf
      showDialog(
        title = "Proceeding",
        message = "You have chosen to proceed with the Dinf section for generating catchments."
      )
      
      ### Flow Direction ###
      # Prompt the user for the save location for flow direction output
      default_output <- file.path(Sys.getenv("HOME"), "flow_direction_Dinf.tif")
      repeat {
        save_flow_direction <- rstudioapi::selectFile(
          caption = paste(
            "Select save location for the flow direction output.",
            "\nFile name must end with .tif.",
            "\n(Default suggestion:", default_output, ")"
          ),
          path = default_output,
          label = "Save As",
          existing = FALSE,
          filter = "TIFF files (*.tif;*.tiff)|*.tif;*.tiff"
        )
        
        if (is.null(save_flow_direction) || save_flow_direction == "") {
          stop("Operation aborted by the user. No save location selected.")
        } else if (grepl("\\.tif$", save_flow_direction, ignore.case = TRUE)) {
          rstudioapi::showDialog("File Selected", paste("File will be saved to:\n", save_flow_direction))
          break  # Valid file name provided
        } else {
          rstudioapi::showDialog("Invalid Input", "File name must end with '.tif'. Please try again.")
        }
      }
      
      # Run the Whitebox tool to compute flow direction
      wbt_d_inf_pointer(
        dem = Filled_DSM,
        output = save_flow_direction
      )
      dlgMessage("Flow direction layer successfully generated.", type = "ok")
      
      ### Flow Accumulation ###
      default_output <- file.path(Sys.getenv("HOME"), "flow_accumulation_Dinf.tif")
      repeat {
        save_flow_accumulation <- rstudioapi::selectFile(
          caption = paste(
            "Select save location for the flow accumulation output.",
            "\nFile name must end with .tif.",
            "\n(Default suggestion:", default_output, ")"
          ),
          path = default_output,
          label = "Save As",
          existing = FALSE,
          filter = "TIFF files (*.tif;*.tiff)|*.tif;*.tiff"
        )
        
        if (is.null(save_flow_accumulation) || save_flow_accumulation == "") {
          stop("Operation aborted by the user. No save location selected.")
        } else if (grepl("\\.tif$", save_flow_accumulation, ignore.case = TRUE)) {
          rstudioapi::showDialog("File Selected", paste("File will be saved to:\n", save_flow_accumulation))
          break
        } else {
          rstudioapi::showDialog("Invalid Input", "File name must end with '.tif'. Please try again.")
        }
      }
      
      # Run the Whitebox tool to compute flow accumulation
      wbt_d_inf_flow_accumulation(
        input = terra::rast(save_flow_direction),
        output = save_flow_accumulation
      )
      dlgMessage("Flow accumulation layer successfully generated.", type = "ok")
      
      ### Flow Distance ###
      default_output <- file.path(Sys.getenv("HOME"), "flow_distance_Dinf.tif")
      repeat {
        save_flow_distance <- rstudioapi::selectFile(
          caption = paste(
            "Select save location for the flow distance output.",
            "\nFile name must end with .tif.",
            "\n(Default suggestion:", default_output, ")"
          ),
          path = default_output,
          label = "Save As",
          existing = FALSE,
          filter = "TIFF files (*.tif;*.tiff)|*.tif;*.tiff"
        )
        
        if (is.null(save_flow_distance) || save_flow_distance == "") {
          stop("Operation aborted by the user. No save location selected.")
        } else if (grepl("\\.tif$", save_flow_distance, ignore.case = TRUE)) {
          rstudioapi::showDialog("File Selected", paste("File will be saved to:\n", save_flow_distance))
          break
        } else {
          rstudioapi::showDialog("Invalid Input", "File name must end with '.tif'. Please try again.")
        }
      }
      
      # Compute flow distance using Whitebox
      wbt_downslope_distance_to_stream(
        dem = Filled_DSM,
        streams = pour_point_sewer,
        output = save_flow_distance,
        dinf = TRUE
      )
      dlgMessage("Flow distance layer successfully generated.", type = "ok")
      
      ### Optional Shapefile Conversion ###
      repeat {
        skip_save_polygon <- tolower(showPrompt(
          title = "Skip Polygon Save",
          message = "Do you want to skip saving the flow distance as a polygon shapefile?\nType 'Yes' to skip or 'No' to proceed.",
          default = "No"
        ))
        
        if (skip_save_polygon == "yes") {
          showDialog("Section Skipped", "Polygon shapefile save skipped.")
          break
        } else if (skip_save_polygon == "no") {
          # Convert flow distance raster to shapefile
          default_output <- file.path(Sys.getenv("HOME"), "flow_distance_polygon.shp")
          repeat {
            save_flow_distance_poly <- rstudioapi::selectFile(
              caption = paste(
                "Select save location for the flow distance polygon shapefile.",
                "\nFile name must end with .shp.",
                "\n(Default suggestion:", default_output, ")"
              ),
              path = default_output,
              label = "Save As",
              existing = FALSE,
              filter = "Shapefiles (*.shp)|*.shp"
            )
            
            if (is.null(save_flow_distance_poly) || save_flow_distance_poly == "") {
              stop("Operation aborted by the user. No save location selected.")
            } else if (grepl("\\.shp$", save_flow_distance_poly, ignore.case = TRUE)) {
              rstudioapi::showDialog("File Selected", paste("File will be saved to:\n", save_flow_distance_poly))
              break
            } else {
              rstudioapi::showDialog("Invalid Input", "File name must end with '.shp'. Please try again.")
            }
          }
          
          # Save the shapefile
          flow_dis_raster <- terra::rast(save_flow_distance)
          
          flow_dis_rast2 <- (flow_dis_raster * 0)+1
          
          flow_distance_polygon <- terra::as.polygons(flow_dis_rast2, dissolve = TRUE)
          terra::writeVector(flow_distance_polygon, save_flow_distance_poly, overwrite = TRUE)
          dlgMessage("Polygon shapefile successfully saved.", type = "ok")
          break
        } else {
          rstudioapi::showDialog("Invalid Input", "Please type 'Yes' or 'No'.")
        }
      }
      break
    }
  } else {
    showDialog("Invalid Input", "Please type 'Yes' or 'No'.")
  }
}

dlgMessage("The Dinf catchment processing has completed.", type = "ok")

########### Using D8 to generate catchments ###########
## Use the whitebox D8 tools ##
# Run the Whitebox tool to compute flow direction

###### Explain to the user that the next step is optional ######
dlgMessage(
  "The next step will include using D8 to generate catchments that drain to the combined sewer network. If the user chooses to skip this section, the tool will finish running.",
  type = "ok"
)

## Running the D8 tools ##
repeat {
  # Prompt the user to decide if they want to skip the D8 section
  skip_D8_section <- tolower(showPrompt(
    title = "Skip Section",
    message = "This section uses D8 for creating catchments. Would you like to skip this section and finish running the tool?\nType 'Yes' to skip or 'No' to proceed with D8.",
    default = "No"
  ))
  
  # Validate user input
  if (skip_D8_section %in% c("yes", "no")) {
    if (skip_D8_section == "yes") {  # User chose to skip
      confirm_skip <- tolower(showPrompt(
        title = "Confirm Skip",
        message = "You have chosen to skip the D8 section. Confirm by typing 'Yes' or return by typing 'No'.",
        default = "Yes"
      ))
      
      if (confirm_skip == "yes") {  # User confirms skipping
        showDialog(
          title = "Section Skipped",
          message = "D8 section skipped. Proceeding to finish running the tool."
        )
        break  # Exit loop and skip the section, effectively skipping all prior steps including shapefile saving
      } else {  # User cancels skipping
        showDialog(
          title = "Returning",
          message = "Returning to the previous prompt for selection."
        )
      }
    } else if (skip_D8_section == "no") {  # User chose to proceed with D8
      showDialog(
        title = "Proceeding",
        message = "You have chosen to proceed with the D8 section for generating catchments."
      )
      
      ### Flow Direction ###
      default_output <- file.path(Sys.getenv("HOME"), "flow_direction_D8.tif")
      repeat {
        save_flow_direction_D8 <- rstudioapi::selectFile(
          caption = paste(
            "Select save location for the flow direction output.",
            "\nFile name must end with .tif.",
            "\n(Default suggestion:", default_output, ")"
          ),
          path = default_output,
          label = "Save As",
          existing = FALSE,
          filter = "TIFF files (*.tif;*.tiff)|*.tif;*.tiff"
        )
        
        if (!is.null(save_flow_direction_D8) && grepl("\\.tif$", save_flow_direction_D8, ignore.case = TRUE)) {
          rstudioapi::showDialog("File Selected", paste("File will be saved to:\n", save_flow_direction_D8))
          break  # Valid file name provided
        } else {
          rstudioapi::showDialog("Invalid Input", "File name must end with '.tif'. Please try again.")
        }
      }
      
      # Run the Whitebox tool to compute flow direction
      wbt_d8_pointer(
        dem = Filled_DSM,
        output = save_flow_direction_D8
      )
      dlgMessage("Flow direction layer successfully generated.", type = "ok")
      
      ### Flow Accumulation ###
      default_output <- file.path(Sys.getenv("HOME"), "flow_accumulation_D8.tif")
      repeat {
        save_flow_accumulation_D8 <- rstudioapi::selectFile(
          caption = paste(
            "Select save location for the flow accumulation output.",
            "\nFile name must end with .tif.",
            "\n(Default suggestion:", default_output, ")"
          ),
          path = default_output,
          label = "Save As",
          existing = FALSE,
          filter = "TIFF files (*.tif;*.tiff)|*.tif;*.tiff"
        )
        
        if (!is.null(save_flow_accumulation_D8) && grepl("\\.tif$", save_flow_accumulation_D8, ignore.case = TRUE)) {
          rstudioapi::showDialog("File Selected", paste("File will be saved to:\n", save_flow_accumulation_D8))
          break
        } else {
          rstudioapi::showDialog("Invalid Input", "File name must end with '.tif'. Please try again.")
        }
      }
      
      # Run the Whitebox tool to compute flow accumulation
      wbt_d8_flow_accumulation(
        input = Filled_DSM,  # Correct argument name for DEM
        output = save_flow_accumulation_D8
      )
      dlgMessage("Flow accumulation layer successfully generated.", type = "ok")
      
      ### D8 Watershed ###
      default_output <- file.path(Sys.getenv("HOME"), "Watershed_d8.tif")
      repeat {
        save_watershed_d8 <- rstudioapi::selectFile(
          caption = paste(
            "Select save location for the watershed output.",
            "\nFile name must end with .tif.",
            "\n(Default suggestion:", default_output, ")"
          ),
          path = default_output,
          label = "Save As",
          existing = FALSE,
          filter = "TIFF files (*.tif;*.tiff)|*.tif;*.tiff"
        )
        
        if (!is.null(save_watershed_d8) && grepl("\\.tif$", save_watershed_d8, ignore.case = TRUE)) {
          rstudioapi::showDialog("File Selected", paste("File will be saved to:\n", save_watershed_d8))
          break
        } else {
          rstudioapi::showDialog("Invalid Input", "File name must end with '.tif'. Please try again.")
        }
      }
      
      # Compute watershed using Whitebox
      wbt_watershed(
        d8_pntr = save_flow_direction_D8,
        pour_pts = pour_point_sewer,
        output = save_watershed_d8
      )
      dlgMessage("Watershed layer successfully generated.", type = "ok")
      
      ### Optional Shapefile Conversion ###
      repeat {
        skip_save_polygon <- tolower(showPrompt(
          title = "Skip Polygon Save",
          message = "Do you want to skip saving the D8 watersheds as a polygon shapefile?\nType 'Yes' to skip or 'No' to proceed.",
          default = "No"
        ))
        
        if (skip_save_polygon == "yes") {
          showDialog("Section Skipped", "Polygon shapefile save skipped.")
          break
        } else if (skip_save_polygon == "no") {
          # Convert flow distance raster to shapefile
          default_output <- file.path(Sys.getenv("HOME"), "Watershed_D8_polygon.shp")
          repeat {
            save_flow_distance_poly <- rstudioapi::selectFile(
              caption = paste(
                "Select save location for the flow distance polygon shapefile.",
                "\nFile name must end with .shp.",
                "\n(Default suggestion:", default_output, ")"
              ),
              path = default_output,
              label = "Save As",
              existing = FALSE,
              filter = "Shapefiles (*.shp)|*.shp"
            )
            
            if (is.null(save_flow_distance_poly) || save_flow_distance_poly == "") {
              stop("Operation aborted by the user. No save location selected.")
            } else if (grepl("\\.shp$", save_flow_distance_poly, ignore.case = TRUE)) {
              rstudioapi::showDialog("File Selected", paste("File will be saved to:\n", save_flow_distance_poly))
              break
            } else {
              rstudioapi::showDialog("Invalid Input", "File name must end with '.shp'. Please try again.")
            }
          }
          
          # Save the shapefile
          watershed_raster <- terra::rast(save_watershed_d8)
          Watershed_polygon <- terra::as.polygons(watershed_raster, dissolve = TRUE)
          terra::writeVector(Watershed_polygon, save_flow_distance_poly, overwrite = TRUE)
          dlgMessage("Polygon shapefile successfully saved.", type = "ok")
          break
        } else {
          rstudioapi::showDialog("Invalid Input", "Please type 'Yes' or 'No'.")
        }
      }
      
      break  # Exit loop after successful execution
    }
  } else {
    # Invalid input, prompt again
    showDialog(
      title = "Invalid Input",
      message = "Please type 'Yes' to skip or 'No' to proceed."
    )
  }
}

###### End of script #############################################################################################################################

#print end message
dlgMessage("The tool has finished and all selected outputs generated.", type = "ok")