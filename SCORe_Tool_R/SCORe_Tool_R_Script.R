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

working_directory <- rstudioapi::selectDirectory(caption = "Set up the working directory")

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
default_dir <- getwd()

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
    filter = "TIFF files (*.tif;*.tiff)|*.tif;*.tiff",
    path    = getwd()
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
    filter = "Shapefiles (*.shp)|*.shp",
    path    = getwd()
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
    filter = "Shapefiles (*.shp)|*.shp",
    path    = getwd()
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

repeat {
  # Display the message box and set focus to make it appear in front
  tcltk::tcl("wm", "attributes", ".", "-topmost", 1)  # Force topmost attribute for RStudio
  skip_rivers <- tcltk::tkmessageBox(
    title = "Skip Section",
    message = "Do you want to skip using the River network to create a barrier in the DEM?",
    type = "yesno",
    icon = "question"
  )
  
  if (tolower(as.character(skip_rivers)) == "yes") {
    # Display the message box and set focus to make it appear in front
    tcltk::tcl("wm", "attributes", ".", "-topmost", 1)  # Force topmost attribute for RStudio
    confirm_skip <- tcltk::tkmessageBox(
      title = "Confirm Skip",
      message = "You selected to skip adding the River network to the DEM.\nPress OK to confirm skipping or Cancel to go back.",
      type = "okcancel",
      icon = "warning"
    )
    
    if (tolower(as.character(confirm_skip)) == "ok") {
      DSM_minus_rivers <- DSM_original
      # Display the message box and set focus to make it appear in front
      tcltk::tcl("wm", "attributes", ".", "-topmost", 1)  # Force topmost attribute for RStudio
      tcltk::tkmessageBox(
        title = "Add River to DEM Skipped",
        message = "Adding the River network to the DEM has been skipped.",
        icon = "info",
        type = "ok"
      )
      break
    } else {
      next
    }
  } else {
    # Display the message box and set focus to make it appear in front
    tcltk::tcl("wm", "attributes", ".", "-topmost", 1)  # Force topmost attribute for RStudio
    tcltk::tkmessageBox(
      title = "River network and DEM Edit",
      message = "The River Network data will be used to create a barrier in the DSM.",
      icon = "info",
      type = "ok"
    )
    
    # Prepare river shapefile
    new_shapefile_path <- file.path(temp_dir, "copy_of_river.gpkg")
    terra::writeVector(River_original, new_shapefile_path, filetype = "GPKG", overwrite = TRUE)
    copied_shapefile <- terra::vect(new_shapefile_path)
    
    copied_shapefile$new_field <- 10000
    terra::writeVector(copied_shapefile, new_shapefile_path, filetype = "GPKG", overwrite = TRUE)
    
    river_net1 <- terra::vect(new_shapefile_path)
    
    # Check CRS
    if (terra::crs(river_net1) != terra::crs(DSM_original)) {
      river_net2 <- terra::project(river_net1, terra::crs(DSM_original))
    } else {
      river_net2 <- river_net1
    }
    
    rasterized_river_net <- terra::rasterize(river_net2, DSM_original, field = "new_field")
    
    ## set up prompt for save location ##
    # Default suggestion for file output location
    #default_output <- file.path(tempdir(), "rasterized_river_network.tif")
    # Set a default output file path
    default_output <- file.path(default_dir, "rasterized_river_network.tif")
    
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
    
    
    # Burn into DSM
    DSM_minus_rivers <- terra::mosaic(DSM_original, rasterized_river_net, fun = "last")
    
    #tcltk::tkmessageBox(
     # title = "River and DEM Edit Complete",
     # message = "Editing the DSM has been completed successfully.",
     # icon = "info",
     # type = "ok"
    #)
    
    
    #print("Rivers removed from DSM completed successfully")
    dlgMessage("River removal from the DSM has been completed successfully.", type = "ok")
    break
  }
}


#########################################################################################################################################################################

##### Add the highways data to the DSM #####

# The user is prompted for the River network layer
dlgMessage("The user will need to select the sewer drainage data which is not related to the combined sewer network. This data will ensure only areas draining to the combined sewer network are provided.", 
           type = "ok")

repeat {
  tcltk::tcl("wm", "attributes", ".", "-topmost", 1)  # Force topmost attribute for RStudio
  skip_highways <- tcltk::tkmessageBox(
    title = "Skip Section",
    message = "Do you want to skip the section for adding drainage data not draining to the combined network to the DSM?",
    type = "yesno",
    icon = "question"
  )
  
  if (tolower(as.character(skip_highways)) == "yes") {
    tcltk::tcl("wm", "attributes", ".", "-topmost", 1)  # Force topmost attribute for RStudio
    confirm_skip <- tcltk::tkmessageBox(
      title = "Confirm Skip",
      message = "You selected to skip the section for adding drainage data not draining to the combined network to the DSM.\nPress OK to confirm skipping or Cancel to go back.",
      type = "okcancel",
      icon = "warning"
    )
    
    if (tolower(as.character(confirm_skip)) == "ok") {
      DSM_minus_rivers_highways <- DSM_minus_rivers
      tcltk::tcl("wm", "attributes", ".", "-topmost", 1)  # Force topmost attribute for RStudio
      tcltk::tkmessageBox(
        title = "Section Skipped",
        message = "The section for adding drainage data not draining to the combined network to the DSM has been skipped. Moving to the next step.",
        icon = "info",
        type = "ok"
      )
      break
    } else {
      next
    }
  } else {
    tcltk::tcl("wm", "attributes", ".", "-topmost", 1)  # Force topmost attribute for RStudio
    tcltk::tkmessageBox(
      title = "Run Section and select attributes",
      message = "Select the attribute and values from the sewer drainage data not related to the combined sewer network.",
      icon = "info",
      type = "ok"
    )
    
    # Copy highways shapefile
    new_shapefile_path2 <- file.path(tempdir(), "copy_of_IAS1.gpkg")
    terra::writeVector(IAS_original, new_shapefile_path2, filetype = "GPKG", overwrite = TRUE)
    copied_IAS1 <- st_read(new_shapefile_path2)
    IAS_sf <- st_as_sf(copied_IAS1)
    input_vector <- terra::vect(IAS_sf)
    
    # Attribute selection
    attribute_options <- colnames(as.data.frame(input_vector))
    selected_attribute <- tcltk::tk_select.list(attribute_options, multiple = FALSE, title = "Select Attribute")
    if (length(selected_attribute) == 0 || selected_attribute == "") stop("No attribute selected. Operation aborted.")
    
    # Value selection
    unique_values <- unique(as.data.frame(input_vector)[[selected_attribute]])
    selected_values <- tcltk::tk_select.list(unique_values, multiple = TRUE, title = paste("Select Values for", selected_attribute))
    if (length(selected_values) == 0) stop("No values selected. Operation aborted.")
    
    # Filter shapefile
    IAS_highways_sf <- copied_IAS1 %>%
      filter(as.character(.data[[selected_attribute]]) %in% as.character(selected_values))
    if (nrow(IAS_highways_sf) == 0) stop("No features found for selected attribute/values.")
    
    IAS_highways_filtered <- terra::vect(IAS_highways_sf)
    if (terra::crs(IAS_highways_filtered) != terra::crs(DSM_original)) {
      IAS_highways_filtered <- terra::project(IAS_highways_filtered, terra::crs(DSM_original))
    }
    
    # Buffer if needed
    geom_type <- tolower(unique(terra::geomtype(IAS_highways_filtered))[1])
    
    if (geom_type %in% c("lines", "points")) {
      
      buffer_choice <- tolower(svDialogs::dlgMessage(
        paste0("The Sewer Drainage data layer contains ", geom_type, 
               ". Do you want to apply a buffer before rasterizing?"),
        type = "yesno"
      )$res)
      
      if (buffer_choice == "yes") {
        
        buffer_input <- as.numeric(svDialogs::dlgInput(
          paste0("Enter buffer distance in map units (default = 3):"),
          default = "3"
        )$res)
        
        if (is.na(buffer_input) || buffer_input <= 0) buffer_input <- 3
        
        IAS_highways_filtered <- terra::buffer(IAS_highways_filtered, width = buffer_input)
        svDialogs::dlgMessage(
          paste("Buffered", geom_type, "by", buffer_input, "map units."),
          type = "ok"
        )
        
      } else {
        svDialogs::dlgMessage("No buffer applied.", type = "ok")
      }
      
    } else if (geom_type %in% c("polygons")) {
      
      svDialogs::dlgMessage(
        "Polygon geometry detected — skipping buffering step.",
        type = "ok"
      )
      
    } else {
      
      svDialogs::dlgMessage(
        paste("Unknown geometry type detected:", geom_type, "- proceeding without buffer."),
        type = "ok"
      )
      
    }
    
    # Rasterize highways
    IAS_highways_filtered$new_field <- 10000
    rasterized_highways_net <- terra::rasterize(IAS_highways_filtered, DSM_original, field = "new_field")
    
    # Burn into DSM
    DSM_minus_rivers_highways <- terra::mosaic(DSM_minus_rivers, rasterized_highways_net, fun = "last")
    
   # tcltk::tkmessageBox(
    #  title = "Highways Edit Complete",
    #  message = "Highways removal completed successfully.",
     # icon = "info",
     # type = "ok"
   # )
    
    dlgMessage("The removal of the drainage data not draining to the combined network from the DSM has been completed successfully.", type = "ok")
    
    break
  }
}


################################################################################################
### Create Combined Sewer Pour Point ###

## Inform user of next step
dlgMessage("The next step will allow the user to create a pour point layer from the Sewer Drainage data. The pour point layer will be created using the combined sewer data", type = "ok")

# Step 1: Copy sewer data into temp directory
new_shapefile_path5 <- file.path(tempdir(), "copy_of_IAS.gpkg")
terra::writeVector(IAS_original, new_shapefile_path5, filetype = "GPKG", overwrite = TRUE)
copied_IAS <- st_read(new_shapefile_path5)

# Convert to sf and terra objects
IAS_sf <- st_as_sf(copied_IAS)
input_vector <- terra::vect(IAS_sf)

# Step 2: Attribute selection
attribute_options <- colnames(as.data.frame(input_vector))
selected_attribute <- tcltk::tk_select.list(attribute_options, multiple = FALSE, title = "Select Attribute")
if (length(selected_attribute) == 0 || selected_attribute == "") stop("No attribute selected. Operation aborted.")

# Step 3: Value selection
unique_values <- unique(as.data.frame(copied_IAS)[[selected_attribute]])
selected_values <- tcltk::tk_select.list(unique_values, multiple = TRUE, title = paste("Select Values for", selected_attribute))
if (length(selected_values) == 0) stop("No values selected. Operation aborted.")

# Step 4: Filter sewer drainage layer
IAS_sewer_sf <- copied_IAS %>%
  filter(as.character(.data[[selected_attribute]]) %in% as.character(selected_values))

if (nrow(IAS_sewer_sf) == 0) {
  stop(paste0("No features found in Sewer Drainage data for ", selected_attribute, " = ",
              paste(selected_values, collapse = ", "),
              ". Please select a value that exists within the DSM extent."))
}

# Step 5: Convert to terra object & match CRS
IAS_sewer_filtered <- terra::vect(IAS_sewer_sf)

if (terra::crs(IAS_sewer_filtered) != terra::crs(DSM_original)) {
  IAS_sewer_CRS <- terra::project(IAS_sewer_filtered, terra::crs(DSM_original))
} else {
  IAS_sewer_CRS <- IAS_sewer_filtered
}

# Check for overlap
if (is.null(terra::intersect(terra::ext(IAS_sewer_CRS), terra::ext(DSM_original)))) {
  stop("The selected Sewer Drainage data features do not overlap the DSM extent.")
}

# Step 6: Add 'new_field'
IAS_sewer_CRS$new_field <- 1

# Step 7: Buffer if needed
geom_type <- tolower(unique(terra::geomtype(IAS_sewer_CRS))[1])

if (geom_type %in% c("lines", "points")) {
  buffer_choice <- tolower(svDialogs::dlgMessage(
    paste0("The Sewer Drainage data layer contains ", geom_type, 
           ". Do you want to apply a buffer before rasterizing?"),
    type = "yesno"
  )$res)
  
  if (buffer_choice == "yes") {
    buffer_input <- as.numeric(svDialogs::dlgInput(
      paste0("Enter buffer distance in map units (default = 3):"),
      default = "3"
    )$res)
    if (is.na(buffer_input) || buffer_input <= 0) buffer_input <- 3
    
    IAS_sewer_CRS <- terra::buffer(IAS_sewer_CRS, width = buffer_input)
    svDialogs::dlgMessage(paste("Buffered", geom_type, "by", buffer_input, "map units."), type = "ok")
  } else {
    svDialogs::dlgMessage("No buffer applied.", type = "ok")
  }
  
} else if (geom_type %in% c("polygons")) {
  svDialogs::dlgMessage("Polygon geometry detected — skipping buffering step.", type = "ok")
} else {
  svDialogs::dlgMessage(paste("Unknown geometry type detected:", geom_type, "- proceeding without buffer."), type = "ok")
}

# Step 8: Rasterize
DSM_extent <- terra::rast(DSM_original)
rasterized_sewer_net <- terra::rasterize(IAS_sewer_CRS, DSM_extent, field = "new_field", background = NA)

# Step 9: Save filtered shapefile
new_shapefile_path6 <- file.path(tempdir(), "filtered_IAS_sewer.gpkg")
terra::writeVector(IAS_sewer_CRS, new_shapefile_path6, filetype = "GPKG", overwrite = TRUE)

# Step 10: Prompt for save location
default_output2 <- file.path(default_dir, "rasterised_sewer_network.tif")

repeat {
  save_rasterised_sewer_net <- rstudioapi::selectFile(
    caption = paste(
      "Specify the location and filename for the rasterized combined sewer network.",
      "\nIt must include .tif at the end of the file name.",
      "\n(Default suggestion:", default_output2, ")"
    ),
    path = default_output2,
    label = "Save As",
    existing = FALSE,
    filter = "TIFF files (*.tif;*.tiff)|*.tif;*.tiff"
  )
  
  if (is.null(save_rasterised_sewer_net) || save_rasterised_sewer_net == "") {
    stop("Operation aborted by the user. No save location selected.")
  } else if (grepl("\\.tif$", save_rasterised_sewer_net, ignore.case = TRUE)) {
    rstudioapi::showDialog("File Selected", paste("File will be saved to:\n", save_rasterised_sewer_net))
    break
  } else {
    rstudioapi::showDialog("Invalid Input", "Invalid input. Please ensure the file name ends with '.tif'.")
  }
}

# Step 11: Save raster
pour_point_sewer <- terra::writeRaster(rasterized_sewer_net, save_rasterised_sewer_net, overwrite = TRUE)

# Confirmation
dlgMessage("The pour point layer has been generated successfully.", type = "ok")


########################################################################################################################################################
############################################# Whitebox tools ##################################################################################

## Inform user what the next step is e.g. use whitebox tools to fill the DEM ##
dlgMessage("The next step will fill the DEM. Filling refers to the removal of small imprefections in a DEM by filling in sinks and peaks.", type = "ok")

### Fill DEM using Whitebox ###
#default_dir <- getwd()

## set up default output ##
## set up prompt for user to save the fill output 
default_output <- file.path(default_dir, "DEM_Fill.tif")

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
final_DEM <- DSM_minus_rivers_highways

# Write final_DEM to a temp file
temp_final_DEM_path <- file.path(tempdir(), "final_DEM.tif")
terra::writeRaster(final_DEM, temp_final_DEM_path, overwrite = TRUE) 

## Call the Whitebox function to fill depressions using the user-specified zlimit
wbt_fill_depressions(
  dem = temp_final_DEM_path,
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
      default_output <- file.path(default_dir, "flow_direction_Dinf.tif")
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
      default_output <- file.path(default_dir, "flow_accumulation_Dinf.tif")
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
      default_output <- file.path(default_dir, "flow_distance_Dinf.tif")
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
          message = "Do you want to skip saving the flow distance as a polygon?\nType 'Yes' to skip or 'No' to proceed.",
          default = "No"
        ))
        
        if (skip_save_polygon == "yes") {
          showDialog("Section Skipped", "Polygon shapefile save skipped.")
          break
        } else if (skip_save_polygon == "no") {
          # Convert flow distance raster to shapefile
          default_output <- file.path(default_dir, "flow_distance_polygon.gpkg")
          repeat {
            save_flow_distance_poly <- rstudioapi::selectFile(
              caption = paste(
                "Select save location for the flow distance polygon, file name must end in .gpkg",
                "\nFile name must end with .gpkg",
                "\n(Default suggestion:", default_output, ")"
              ),
              path = default_output,
              label = "Save As",
              existing = FALSE,
              filter = "GeoPackage (*.gpkg)|*.gpkg"
            )
            
            if (is.null(save_flow_distance_poly) || save_flow_distance_poly == "") {
              stop("Operation aborted by the user. No save location selected.")
            } else if (grepl("\\.gpkg$", save_flow_distance_poly, ignore.case = TRUE)) {
              rstudioapi::showDialog("File Selected", paste("File will be saved to:\n", save_flow_distance_poly))
              break
            } else {
              rstudioapi::showDialog("Invalid Input", "File name must end with '.gpkg'. Please try again.")
            }
          }
          
          # Save the shapefile
          flow_dis_raster <- terra::rast(save_flow_distance)
          
          flow_dis_rast2 <- (flow_dis_raster * 0)+1
          
          flow_distance_polygon <- terra::as.polygons(flow_dis_rast2, dissolve = TRUE)
          terra::writeVector(flow_distance_polygon, save_flow_distance_poly, filetype = "GPKG", overwrite = TRUE)
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
      default_output <- file.path(default_dir, "flow_direction_D8.tif")
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
      default_output <- file.path(default_dir, "flow_accumulation_D8.tif")
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
      default_output <- file.path(default_dir, "Watershed_d8.tif")
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
      
      ############ Optional Shapefile Conversion #########################################################################################
      repeat {
        skip_save_polygon <- tolower(showPrompt(
          title = "Skip Polygon Save",
          message = "Do you want to skip saving the D8 watersheds as a polygon?\nType 'Yes' to skip or 'No' to proceed.",
          default = "No"
        ))
        
        if (skip_save_polygon == "yes") {
          showDialog("Section Skipped", "Polygon shapefile save skipped.")
          break
        } else if (skip_save_polygon == "no") {
          # Convert flow distance raster to shapefile
          default_output <- file.path(default_dir, "Watershed_D8_polygon.gpkg")
          repeat {
            save_flow_distance_poly <- rstudioapi::selectFile(
              caption = paste(
                "Select save location for the flow distance polygon, file name must end in .gpkg",
                "\nFile name must end with .gpkg.",
                "\n(Default suggestion:", default_output, ")"
              ),
              path = default_output,
              label = "Save As",
              existing = FALSE,
              filter = "GeoPackage (*.gpkg)|*.gpkg"
            )
            
            if (is.null(save_flow_distance_poly) || save_flow_distance_poly == "") {
              stop("Operation aborted by the user. No save location selected.")
            } else if (grepl("\\.gpkg$", save_flow_distance_poly, ignore.case = TRUE)) {
              rstudioapi::showDialog("File Selected", paste("File will be saved to:\n", save_flow_distance_poly))
              break
            } else {
              rstudioapi::showDialog("Invalid Input", "File name must end with '.gpkg'. Please try again.")
            }
          }
          
          # Save the shapefile
          watershed_raster <- terra::rast(save_watershed_d8)
          Watershed_polygon <- terra::as.polygons(watershed_raster, dissolve = TRUE)
          terra::writeVector(Watershed_polygon, save_flow_distance_poly, filetype = "GPKG", overwrite = TRUE)
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
