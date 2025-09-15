# -*- coding: utf-8 -*-

import arcpy
import os
import tempfile
from arcpy import env
from arcpy.sa import *
arcpy.CheckOutExtension("Spatial")

class Toolbox(object):
    def __init__(self):
        """Define the toolbox (the name of the toolbox is the name of the .pyt file)."""
        self.label = "Area For NbS"
        self.alias = "areafornbs"
        self.tools = [SelectImpermeableAreaByAttributesTool, AddHighwaysDataOrRiverToDEMTool, PourPointTool, AreaForNatureBasedSolutionsDinf, AreaForNatureBasedSolutionsD8, Tool]  # list of tool classes

# Tool 1: Select Impermeable Area By Attributes Tool
class SelectImpermeableAreaByAttributesTool(object):
    def __init__(self):
        """Define the tool (tool name is the name of the class)."""
        self.label = "1. Select Drainage Area by Attributes Tool"
        self.description = (
            "This tool is used to select and extract sewer type data selected by the user. "
            "The output from this tool can be used with tool 2 and 3, depending on the sewer type data selected and extracted."
        )
        self.canRunInBackground = False

    def getParameterInfo(self):
        """Define the tool parameters."""
        params = []

        input_layer = arcpy.Parameter(
            displayName="Input Layer (Drainage / Sewer Data)",
            name="input_layer",
            datatype=["Feature Layer", "GPFeatureLayer"],  
            parameterType="Required",
            direction="Input"
        )
        params.append(input_layer)

        field_name = arcpy.Parameter(
            displayName="Attribute Field to Filter By",
            name="field_name",
            datatype="GPString",
            parameterType="Required",
            direction="Input"
        )
        params.append(field_name)

        field_values = arcpy.Parameter(
            displayName="Choose Attribute Values",
            name="field_values",
            datatype="GPString",
            parameterType="Required",
            direction="Input",
            multiValue=True  # Allow selecting multiple values
        )
        params.append(field_values)

        output_folder = arcpy.Parameter(
            displayName="Output Folder (Save results to)",
            name="output_folder",
            datatype="DEWorkspace",
            parameterType="Required",
            direction="Input"
        )
        params.append(output_folder)

        shapefile_name = arcpy.Parameter(
            displayName="Name for Output Shapefile",
            name="shapefile_name",
            datatype="GPString",
            parameterType="Required",
            direction="Input"
        )
        shapefile_name.value = "Selected_drainage_area.shp"  # Default value added
        params.append(shapefile_name)

        # Configure the field_values parameter filter (should be done in updateParameters)
        params[2].filter.type = "ValueList"
        params[2].filter.list = []

        return params

    def isLicensed(self):
        """Set whether the tool is licensed to execute."""
        return True

    def updateParameters(self, parameters):
        """Modify the values and properties of parameters before internal validation."""

        # Step 1: Populate field_name dropdown based on the selected input_layer
        if parameters[0].altered and not parameters[1].altered:
            input_layer_val = parameters[0].value
            if input_layer_val:
                try:
                    # Get the field names from the input layer
                    field_names = [f.name for f in arcpy.ListFields(input_layer_val)]
                    arcpy.AddMessage(f"Available fields: {field_names}")  # Debugging message
                    parameters[1].filter.list = field_names
                except Exception as e:
                    arcpy.AddError(f"Error retrieving fields: {str(e)}")

        # Step 2: Populate field_values dropdown based on the selected field_name
        if parameters[1].value:
            with arcpy.da.SearchCursor(parameters[0].valueAsText, parameters[1].valueAsText) as rows:
                parameters[2].filter.list = sorted(list(set([row[0] for row in rows])))
        else:
            parameters[2].filter.list = []

        return

    def updateMessages(self, parameters):
        """Modify the messages created by internal validation for each tool parameter."""
        return

    def hasNullGeometries(self, feature_class):
        """Check if the feature class contains any null geometries."""
        with arcpy.da.SearchCursor(feature_class, ["SHAPE@"]) as cursor:
            for row in cursor:
                if row[0] is None:
                    return True
        return False

    def execute(self, parameters, messages):
        """The source code of the tool."""
        input_layer = parameters[0].value  # Input shapefile or layer
        field_name = parameters[1].value  # Field selected from the shapefile
        field_values = parameters[2].values  # Selected attribute(s)
        output_folder = parameters[3].valueAsText  # Output folder
        shapefile_name = parameters[4].valueAsText  # Output layer name

        # Construct the full path for the output shapefile
        if not shapefile_name.endswith(".shp"):
            shapefile_name += ".shp"

        output_fc = os.path.join(output_folder, shapefile_name)

        # Create a temporary output feature class
        temp_output = "in_memory/temp_output"

        try:
            # Check if field_values is empty or None
            if not field_values:
                raise ValueError("No field values provided.")

            # Construct the selection query for multiple values
            field_values_str = ", ".join([f"'{value}'" for value in field_values])
            selection_query = f"{field_name} IN ({field_values_str})"

            # Log the query to help troubleshoot
            arcpy.AddMessage(f"Selection query: {selection_query}")

            # Perform the selection
            arcpy.analysis.Select(input_layer, temp_output, selection_query)

            # Check for null geometries and repair if needed
            if self.hasNullGeometries(temp_output):
                arcpy.AddMessage("Null geometries found. Repairing geometry.")
                arcpy.management.RepairGeometry(temp_output, "DELETE_NULL")

            # Add a new field with value 1
            new_field_name = "NewField"

            # Check if NewField already exists
            existing_fields = [field.name for field in arcpy.ListFields(temp_output)]
            if new_field_name not in existing_fields:
                arcpy.management.AddField(temp_output, new_field_name, "SHORT")
                arcpy.AddMessage(f"Field '{new_field_name}' added.")
            else:
                arcpy.AddMessage(f"Field '{new_field_name}' already exists. Values will be updated.")

            # Overwrite values in 'NewField' instead of adding a duplicate
            with arcpy.da.UpdateCursor(temp_output, [new_field_name]) as cursor:
                for row in cursor:
                    row[0] = 1
                    cursor.updateRow(row)
            arcpy.AddMessage(f"Field '{new_field_name}' values updated.")

            # Copy the final output to the specified output shapefile
            arcpy.management.CopyFeatures(temp_output, output_fc)

            arcpy.AddMessage(f"Processing complete. Output saved to: {output_fc}")

            # Add the new shapefile to the current map
            aprx = arcpy.mp.ArcGISProject("CURRENT")
            active_map = aprx.activeMap  # This will get the currently active map or scene

            if active_map:  # Check that an active map is open
                active_map.addDataFromPath(output_fc)
                arcpy.AddMessage(f"Added {output_fc} to the active map.")
            else:
                arcpy.AddError("No active map found. Please make sure a map is open and active.")

        except Exception as e:
            arcpy.AddError(f"Error: {str(e)}")
            raise

        return

    def postExecute(self, parameters):
        """This method takes place after outputs are processed and added to the display."""
        return

		
# Tool 2: Add Highways Data Or Rivers To DEM Tool
class AddHighwaysDataOrRiverToDEMTool(object):
    def __init__(self):
        """Define the tool (tool name is the name of the class)."""
        self.label = "2. Add Networks or Barriers to DEM"
        self.description = (
            "This tool is used to remove data from the DEM. Data that could be removed includes "
            "watercourses and sewer data that is not related to the combined sewer. By removing this data "
            "the accuracy of the potential areas draining to the combined sewer will be improved."
        )
        self.canRunInBackground = False

    def getParameterInfo(self):
        """Define the tool parameters."""
        params = []

        input_layer = arcpy.Parameter(
            displayName="Input Features (Rivers or Sewer Data)",
            name="input_layer",
            datatype=["Feature Layer", "GPFeatureLayer"],
            parameterType="Required",
            direction="Input"
        )
        params.append(input_layer)

        dem_layer = arcpy.Parameter(
            displayName="Input DEM (Raster)",
            name="dem_layer",
            datatype=["Raster Dataset", "GPRasterLayer"],
            parameterType="Required",
            direction="Input"
        )
        params.append(dem_layer)

        project_shapefile = arcpy.Parameter(
            displayName="Project Input Layer to Match DEM",
            name="project_shapefile",
            datatype="GPBoolean",
            parameterType="Optional",
            direction="Input"
        )
        project_shapefile.value = False  # Default unchecked
        params.append(project_shapefile)

        output_folder = arcpy.Parameter(
            displayName="Output Folder (Save results to)",
            name="output_folder",
            datatype="DEWorkspace",
            parameterType="Required",
            direction="Input"
        )
        params.append(output_folder)

        raster_name = arcpy.Parameter(
            displayName="Name for Output DEM (Raster)",
            name="raster_name",
            datatype="GPString",
            parameterType="Required",
            direction="Input"
        )
        raster_name.value = "DEM_with_barriers.tif"  # Default output raster name
        params.append(raster_name)

        return params

    def isLicensed(self):
        """Set whether the tool is licensed to execute."""
        return True

    def updateParameters(self, parameters):
        """Modify the values and properties of parameters before internal validation."""
        return

    def updateMessages(self, parameters):
        """Modify the messages created by internal validation for each tool parameter."""
        return

    def execute(self, parameters, messages):
        """The source code of the tool."""
        input_layer = parameters[0].valueAsText  # Input shapefile (can be point, polygon, or line)
        dem_layer = parameters[1].valueAsText    # Input DEM
        project_shapefile = parameters[2].value  # Check if projection is needed
        output_folder = parameters[3].valueAsText  # Output folder
        raster_name = parameters[4].valueAsText  # Output raster name

        # Ensure output raster has a .tif extension
        if not raster_name.endswith(".tif"):
            raster_name += ".tif"

        # Define paths for temporary intermediate outputs
        temp_scratch_folder = arcpy.env.scratchFolder
        temp_copy_layer = os.path.join(temp_scratch_folder, "temp_copy_layer.shp")
        projected_layer = os.path.join(temp_scratch_folder, "projected_layer.shp")
        feature_raster = os.path.join(temp_scratch_folder, "feature_raster.tif")

        # Final output path
        final_raster = os.path.join(output_folder, raster_name)

        # Copy the input shapefile to a temporary location
        arcpy.management.CopyFeatures(input_layer, temp_copy_layer)
        messages.addMessage(f"Temporary copy of shapefile created at {temp_copy_layer}")

        # Get the DEM's spatial reference
        dem_spatial_ref = arcpy.Describe(dem_layer).spatialReference

        # Project the shapefile if the checkbox is checked
        if project_shapefile:
            arcpy.management.Project(temp_copy_layer, projected_layer, dem_spatial_ref)
            messages.addMessage(f"Shapefile projected to {dem_spatial_ref.name}")
        else:
            projected_layer = temp_copy_layer
            messages.addMessage("Shapefile projection skipped.")

        # Add a new field to the reprojected shapefile
        field_name = "NewVal"
        arcpy.management.AddField(projected_layer, field_name, "SHORT")

        # Populate the new field with a value of 1
        with arcpy.da.UpdateCursor(projected_layer, [field_name]) as cursor:
            for row in cursor:
                row[0] = 1
                cursor.updateRow(row)

        # Set the snap raster and cell size to match the DEM
        arcpy.env.snapRaster = dem_layer
        arcpy.env.cellSize = dem_layer

        # Set the environment extent to match the DEM extent
        dem_extent = arcpy.Describe(dem_layer).extent
        arcpy.env.extent = dem_extent

        # Convert the reprojected shapefile to a raster
        arcpy.conversion.FeatureToRaster(projected_layer, field_name, feature_raster)

        # Reclassify the raster: valid values = 1, NODATA = 0
        reclass_raster = arcpy.sa.Reclassify(feature_raster, "Value", "1 NODATA;NODATA 0")

        # Add the reclassified raster to the original DEM
        final_result = arcpy.sa.Raster(dem_layer) + reclass_raster

        # Save the final output raster
        final_result.save(final_raster)
        messages.addMessage(f"Process completed successfully. Final output saved at {final_raster}")

        # Add the final output raster to the current map
        aprx = arcpy.mp.ArcGISProject("CURRENT")
        m = aprx.activeMap
        m.addDataFromPath(final_raster)
        messages.addMessage(f"Final output raster added to the current map.")

        return

    def postExecute(self, parameters):
        """This method takes place after outputs are processed and added to the display."""
        return

# Tool 3: Pour Point From the IAS Tool
import arcpy
import os

class PourPointTool(object):
    def __init__(self):
        self.label = "3. Sewer Pour Point Tool"
        self.description = "This tool will provide a raster layer to be used in tool 4. The layer generated will be a pour point layer. The output from tool 1, when using combined sewer data, should be used as the input for this tool."
        self.canRunInBackground = False

    def getParameterInfo(self):
        params = []

        feature_type = arcpy.Parameter(
            displayName="Type of Pour Point Features (Polygon, Point, or Line)",
            name="feature_type",
            datatype="GPString",
            parameterType="Required",
            direction="Input"
        )
        feature_type.filter.type = "ValueList"
        feature_type.filter.list = ["Polygon", "Point", "Line"]
        feature_type.value = "Polygon"
        params.append(feature_type)

        input_shapefile = arcpy.Parameter(
            displayName="Input Pour Point Shapefile",
            name="input_shapefile",
            datatype=["Feature Layer", "GPFeatureLayer"],
            parameterType="Required",
            direction="Input"
        )
        params.append(input_shapefile)

        input_dem = arcpy.Parameter(
            displayName="Reference DEM Raster",
            name="input_dem",
            datatype=["Raster Dataset", "GPRasterLayer"],
            parameterType="Required",
            direction="Input"
        )
        params.append(input_dem)

        buffer_percentage = arcpy.Parameter(
            displayName="Buffer Size (Percentage of Polygon Width)",
            name="buffer_percentage",
            datatype="GPLong",
            parameterType="Optional",
            direction="Input"
        )
        params.append(buffer_percentage)

        buffer_distance = arcpy.Parameter(
            displayName="Buffer Distance (for Points/Lines, in meters)",
            name="buffer_distance",
            datatype="GPDouble",
            parameterType="Optional",
            direction="Input"
        )
        params.append(buffer_distance)

        output_folder = arcpy.Parameter(
            displayName="Output Folder (Save results to)",
            name="output_folder",
            datatype="DEFolder",
            parameterType="Required",
            direction="Input"
        )
        params.append(output_folder)

        output_layer_name = arcpy.Parameter(
            displayName="Name for Output Raster",
            name="output_layer_name",
            datatype="GPString",
            parameterType="Required",
            direction="Input"
        )
        output_layer_name.value = "Sewer_pour_point.tif"
        params.append(output_layer_name)

        return params

    def updateParameters(self, parameters):
        feature_type = parameters[0].value
        if feature_type == "Polygon":
            parameters[3].enabled = True
            parameters[4].enabled = False
        elif feature_type in ["Point", "Line"]:
            parameters[3].enabled = False
            parameters[4].enabled = True
        return

    def execute(self, parameters, messages):
        feature_type = parameters[0].value
        input_shapefile = parameters[1].valueAsText
        input_dem = parameters[2].valueAsText
        buffer_percentage = parameters[3].valueAsText
        buffer_distance = parameters[4].valueAsText
        output_folder = parameters[5].valueAsText
        output_layer_name = parameters[6].valueAsText

        if not output_layer_name.endswith(".tif"):
            output_layer_name += ".tif"

        arcpy.env.snapRaster = input_dem
        arcpy.env.extent = input_dem

        if not arcpy.Exists(input_shapefile):
            messages.addErrorMessage(f"Input shapefile does not exist: {input_shapefile}")
            raise Exception(f"Input shapefile does not exist: {input_shapefile}")

        desc = arcpy.Describe(input_shapefile)
        actual_geometry = desc.shapeType
        expected_geometry = {"Polygon": "Polygon", "Point": "Point", "Line": "Polyline"}[feature_type]

        if actual_geometry != expected_geometry:
            messages.addErrorMessage(f"Geometry mismatch: Expected '{expected_geometry}', but got '{actual_geometry}'.")
            raise Exception(f"Geometry mismatch: Expected '{expected_geometry}', but got '{actual_geometry}'.")

        temp_dir = r"C:\Temp"
        if not os.path.exists(temp_dir):
            os.makedirs(temp_dir)

        temp_shapefile = os.path.join(temp_dir, "input.shp")

        try:
            messages.addMessage(f"Copying shapefile to: {temp_shapefile}")
            arcpy.management.CopyFeatures(input_shapefile, temp_shapefile)
        except Exception as e:
            messages.addErrorMessage(f"Failed to copy shapefile: {e}")
            raise

        if not arcpy.Exists(temp_shapefile):
            messages.addErrorMessage(f"Copied shapefile does not exist: {temp_shapefile}")
            raise Exception(f"Copied shapefile does not exist: {temp_shapefile}")

        messages.addMessage(f"Shapefile successfully copied: {temp_shapefile}")
        messages.addMessage(f"Feature type: {feature_type}")

        # Polygon: calculate perimeter, area, width
        if feature_type == "Polygon":
            try:
                # Add fields if missing
                field_names = [f.name for f in arcpy.ListFields(temp_shapefile)]
                if "Perim" not in field_names:
                    arcpy.management.AddField(temp_shapefile, "Perim", "DOUBLE")
                if "Area_m2" not in field_names:
                    arcpy.management.AddField(temp_shapefile, "Area_m2", "DOUBLE")
                if "Width" not in field_names:
                    arcpy.management.AddField(temp_shapefile, "Width", "DOUBLE")

                # Calculate geometry
                arcpy.management.CalculateGeometryAttributes(
                    temp_shapefile,
                    [["Perim", "PERIMETER_LENGTH"], ["Area_m2", "AREA"]],
                    length_unit="METERS",
                    area_unit="SQUARE_METERS"
                )

                # Width = 2 * Area / Perimeter
                arcpy.management.CalculateField(
                    temp_shapefile,
                    "Width",
                    "2 * !Area_m2! / !Perim!",
                    "PYTHON3"
                )
                messages.addMessage("Width field calculated for polygon as 2 * Area / Perimeter.")

            except Exception as e:
                messages.addErrorMessage(f"Error calculating geometry or width for polygon: {e}")
                raise

        # Buffering
        try:
            if feature_type == "Polygon" and buffer_percentage and float(buffer_percentage) > 0:
                arcpy.management.AddField(temp_shapefile, "buff_per", "DOUBLE")
                buffer_value = float(buffer_percentage) / 100
                arcpy.management.CalculateField(temp_shapefile, "buff_per", f"!Width! * {buffer_value}", "PYTHON3")
                temp_buffer_shapefile = os.path.join(temp_dir, "buffered.shp")
                arcpy.analysis.Buffer(temp_shapefile, temp_buffer_shapefile, "buff_per")
                messages.addMessage(f"Polygon buffered using percentage: {temp_buffer_shapefile}")

            elif feature_type in ["Point", "Line"] and buffer_distance and float(buffer_distance) > 0:
                temp_buffer_shapefile = os.path.join(temp_dir, "buffered.shp")
                arcpy.analysis.Buffer(temp_shapefile, temp_buffer_shapefile, f"{buffer_distance} Meters")
                messages.addMessage(f"{feature_type} buffered by fixed distance: {temp_buffer_shapefile}")

            else:
                temp_buffer_shapefile = temp_shapefile
                messages.addMessage("No buffering applied.")
        except Exception as e:
            messages.addErrorMessage(f"Error during buffering: {e}")
            raise

        # Add +1 to the FID value
        field_name = "FID_NewVal"
        if field_name not in [f.name for f in arcpy.ListFields(temp_buffer_shapefile)]:
            arcpy.management.AddField(temp_buffer_shapefile, field_name, "LONG")

        with arcpy.da.UpdateCursor(temp_buffer_shapefile, ["OID@", field_name]) as cursor:
            for row in cursor:
                row[1] = row[0] + 1
                cursor.updateRow(row)
        arcpy.AddMessage(f"Field '{field_name}' values updated.")

        # Convert to raster
        try:
            final_output_raster = arcpy.conversion.FeatureToRaster(
                temp_buffer_shapefile, field_name,
                os.path.join(output_folder, output_layer_name),
                input_dem
            )
            messages.addMessage(f"Raster created from: {temp_buffer_shapefile}")
        except Exception as e:
            messages.addErrorMessage(f"Error converting to raster: {e}")
            raise

        # Add to map
        try:
            aprx = arcpy.mp.ArcGISProject("CURRENT")
            m = aprx.activeMap
            m.addDataFromPath(final_output_raster)
            messages.addMessage("Final raster added to current map.")
        except Exception as e:
            messages.addErrorMessage(f"Error adding raster to map: {e}")
            raise

        return


# Tool 4: Flow Distance
class AreaForNatureBasedSolutionsDinf(object):
    def __init__(self):
        """Define the tool."""
        self.label = "4. Area for Nature Based Solutions Tool (DInf Version)"
        self.description = "Tool for processing rasters including clipping, filling, flow direction, and optionally generating flow accumulation and polygon outputs."
        self.canRunInBackground = False

    def getParameterInfo(self):
        """Define the tool parameters."""
        params = []

        # Parameter to input the raster DEM
        param1 = arcpy.Parameter(
            displayName="Input Raster DEM",
            name="input_dem",
            datatype=["Raster Dataset", "GPRasterLayer"],
            parameterType="Required",
            direction="Input"
        )
        params.append(param1)

        # Check box to clip DEM
        param2 = arcpy.Parameter(
            displayName="Clip DEM to Study Area?",
            name="clip_dem",
            datatype="GPBoolean",
            parameterType="Optional",
            direction="Input"
        )
        params.append(param2)

        # Input shapefile or raster for clipping
        param3 = arcpy.Parameter(
            displayName="Clip Area (Polygon or Raster)",
            name="clip_shapefile_or_raster",
            datatype=["Feature Layer", "GPFeatureLayer"],
            parameterType="Optional",
            direction="Input",
            enabled=False
        )
        params.append(param3)

        # Z limit parameter
        param4 = arcpy.Parameter(
            displayName="Z Limit (Elevation Fill Threshold)",
            name="z_limit",
            datatype="GPDouble",
            parameterType="Required",
            direction="Input"
        )
        param4.value = 0.2  # Default value
        params.append(param4)

        # Check box to generate flow accumulation
        param5 = arcpy.Parameter(
            displayName="Generate Flow Accumulation?",
            name="generate_flow_accumulation",
            datatype="GPBoolean",
            parameterType="Optional",
            direction="Input"
        )
        params.append(param5)

        # Output location for flow accumulation
        param6 = arcpy.Parameter(
            displayName="Save Flow Accumulation Raster To",
            name="flow_accumulation_output_location",
            datatype="DEFile",
            parameterType="Optional",
            direction="Output",
            enabled=False
        )
        params.append(param6)

        # Input raster for pour point
        param7 = arcpy.Parameter(
            displayName="Input Pour Point Raster",
            name="pour_point_raster",
            datatype=["Raster Dataset", "GPRasterLayer"],
            parameterType="Optional",
            direction="Input"
        )
        params.append(param7)

        # Flow distance output layer
        param8 = arcpy.Parameter(
            displayName="Flow Distance Output Layer (Save results to)",
            name="flow_distance_output_layer",
            datatype="DEFile",
            parameterType="Required",
            direction="Output"
        )
        params.append(param8)

        # Check box for polygon output
        param9 = arcpy.Parameter(
            displayName="Generate Polygon Output?",
            name="polygon_output",
            datatype="GPBoolean",
            parameterType="Optional",
            direction="Input"
        )
        params.append(param9)

        # Output location for polygon shapefile
        param10 = arcpy.Parameter(
            displayName="Save Polygon Shapefile To",
            name="polygon_output_shapefile_location",
            datatype="DEFile",
            parameterType="Optional",
            direction="Output",
            enabled=False
        )
        params.append(param10)

        return params

    def isLicensed(self):
        """Set whether the tool is licensed to execute."""
        return True

    def updateParameters(self, parameters):
        """Modify the values and properties of parameters before internal validation is performed."""
        # Enable or disable parameters based on check box selections
        if parameters[1].value:  # If Clip DEM is checked
            parameters[2].enabled = True  # Enable clip shapefile/raster parameter
        else:
            parameters[2].enabled = False  # Disable clip shapefile/raster parameter

        if parameters[4].value:  # If Generate Flow Accumulation is checked
            parameters[5].enabled = True  # Enable flow accumulation output location
        else:
            parameters[5].enabled = False  # Disable flow accumulation output location

        if parameters[8].value:  # If Polygon Output is checked
            parameters[9].enabled = True  # Enable polygon output shapefile location
        else:
            parameters[9].enabled = False  # Disable polygon output shapefile location

        return

    def execute(self, parameters, messages):
        """The source code of the tool."""
        input_dem = parameters[0].valueAsText
        clip_dem = parameters[1].value
        clip_shapefile_or_raster = parameters[2].valueAsText
        z_limit = parameters[3].value
        generate_flow_accumulation = parameters[4].value
        flow_accumulation_output_location = parameters[5].valueAsText
        pour_point_raster = parameters[6].valueAsText
        flow_distance_output_layer = parameters[7].valueAsText
        polygon_output = parameters[8].value
        polygon_output_shapefile_location = parameters[9].valueAsText

        # Ensure flow distance output layer has a .tif extension
        if not flow_distance_output_layer.endswith(".tif"):
            flow_distance_output_layer += ".tif"

        # Ensure flow accumulation output location has a .tif extension if provided
        if generate_flow_accumulation and flow_accumulation_output_location:
            if not flow_accumulation_output_location.endswith(".tif"):
                flow_accumulation_output_location += ".tif"

        # Ensure polygon output shapefile has a .shp extension if provided
        if polygon_output and polygon_output_shapefile_location:
            if not polygon_output_shapefile_location.endswith(".shp"):
                polygon_output_shapefile_location += ".shp"

        # Create temporary folder
        temp_dir = arcpy.env.scratchFolder

        # Set the snap raster to the DEM to ensure alignment
        arcpy.env.snapRaster = input_dem
        
        # Set the environment cell size to match the DEM
        arcpy.env.cellSize = input_dem

        # Clip DEM if checkbox is checked
        if clip_dem:
            clipped_dem = os.path.join(temp_dir, "clipped_dem.tif")
            arcpy.management.Clip(input_dem, "", clipped_dem, clip_shapefile_or_raster, "0", "ClippingGeometry")
            dem_to_process = clipped_dem
        else:
            dem_to_process = input_dem

        # Fill the DEM with z limit
        filled_dem = os.path.join(temp_dir, "filled_dem.tif")
        arcpy.sa.Fill(dem_to_process, z_limit).save(filled_dem)

        # Perform flow direction with DINF
        flow_direction = os.path.join(temp_dir, "flow_direction.tif")
        arcpy.sa.FlowDirection(filled_dem, "NORMAL", None, "DINF").save(flow_direction)

        # Perform flow accumulation if checkbox is checked
        if generate_flow_accumulation and flow_accumulation_output_location:
            arcpy.sa.FlowAccumulation(flow_direction, None, "FLOAT", "DINF").save(flow_accumulation_output_location)

            # Add the flow accumulation output to the map
            aprx = arcpy.mp.ArcGISProject("CURRENT")
            map_obj = aprx.activeMap
            map_obj.addDataFromPath(flow_accumulation_output_location)

        # Output flow distance layer
        arcpy.sa.FlowDistance(pour_point_raster, filled_dem, flow_direction, "VERTICAL", "DINF", "MINIMUM").save(flow_distance_output_layer)

        # Add the flow distance output to the map
        aprx = arcpy.mp.ArcGISProject("CURRENT")
        map_obj = aprx.activeMap
        map_obj.addDataFromPath(flow_distance_output_layer)

        # Convert flow direction to polygon shapefile if checkbox is checked
        if polygon_output and polygon_output_shapefile_location:
            con_output = os.path.join(temp_dir, "con_output.tif")
            int_output = os.path.join(temp_dir, "int_output.tif")
            polygon_temp = os.path.join(temp_dir, "polygon_temp.shp")

            ((arcpy.Raster(flow_distance_output_layer) * 0) + 1).save(con_output)
            arcpy.sa.Int(con_output).save(int_output)
            arcpy.conversion.RasterToPolygon(int_output, polygon_temp, "NO_SIMPLIFY", "VALUE")
            arcpy.management.MultipartToSinglepart(in_features=polygon_temp, out_feature_class=polygon_output_shapefile_location)
            
            # Add the polygon output to the map
            map_obj.addDataFromPath(polygon_output_shapefile_location)

        return

    def postExecute(self, parameters):
        """This method takes place after outputs are processed and
        added to the display."""
        return

		
# Tool 4: Watersheds	
class AreaForNatureBasedSolutionsD8(object):
    def __init__(self):
        """Define the tool."""
        self.label = "4. Area for Nature Based Solutions Tool (D8 Version)"
        self.description = "This tool will generate catchments from the combined sewer pour point. The catchments generated are based on topography and have potential for NbS. The flow method used for this tool is Deterministic 8 (D8)."
        self.canRunInBackground = False

    def getParameterInfo(self):
        """Define the tool parameters."""
        params = []

        # Parameter to input the raster DEM
        param1 = arcpy.Parameter(
            displayName="Input DEM Raster for Analysis",
            name="input_dem",
            datatype=["Raster Dataset", "GPRasterLayer"],
            parameterType="Required",
            direction="Input"
        )
        params.append(param1)

        # Check box to clip DEM
        param2 = arcpy.Parameter(
            displayName="Clip DEM to Study Area?",
            name="clip_dem",
            datatype="GPBoolean",
            parameterType="Optional",
            direction="Input"
        )
        params.append(param2)

        # Input shapefile or raster for clipping
        param3 = arcpy.Parameter(
            displayName="Clip Area (Polygon or Raster)",
            name="clip_shapefile_or_raster",
            datatype=["Feature Layer", "GPFeatureLayer"],
            parameterType="Optional",
            direction="Input",
            enabled=False
        )
        params.append(param3)

        # Z limit parameter
        param4 = arcpy.Parameter(
            displayName="Z Limit (Elevation Fill Threshold)",
            name="z_limit",
            datatype="GPDouble",
            parameterType="Required",
            direction="Input"
        )
        param4.value = 0.2  # Default value
        params.append(param4)

        # Check box to generate flow accumulation
        param5 = arcpy.Parameter(
            displayName="Generate Flow Accumulation",
            name="generate_flow_accumulation",
            datatype="GPBoolean",
            parameterType="Optional",
            direction="Input"
        )
        params.append(param5)

        # Output location for flow accumulation
        param6 = arcpy.Parameter(
            displayName="Save Flow Accumulation Raster To",
            name="flow_accumulation_output_location",
            datatype="DEFile",
            parameterType="Optional",
            direction="Output",
            enabled=False
        )
        params.append(param6)

        # Input raster for pour point
        param7 = arcpy.Parameter(
            displayName="Input Pour Point Raster",
            name="pour_point_raster",
            datatype=["Raster Dataset", "GPRasterLayer"],
            parameterType="Optional",
            direction="Input"
        )
        params.append(param7)

        # Watershed output layer
        param8 = arcpy.Parameter(
            displayName="Watershed Output Layer (Save results to)",
            name="watershed_output_layer",
            datatype="DEFile",
            parameterType="Required",
            direction="Output"
        )
        params.append(param8)

        # Check box for polygon output
        param9 = arcpy.Parameter(
            displayName="Generate Polygon Output?",
            name="polygon_output",
            datatype="GPBoolean",
            parameterType="Optional",
            direction="Input"
        )
        params.append(param9)

        # Output location for polygon shapefile
        param10 = arcpy.Parameter(
            displayName="Polygon Shapefile Output (Save results to)",
            name="polygon_output_shapefile_location",
            datatype="DEFile",
            parameterType="Optional",
            direction="Output",
            enabled=False
        )
        params.append(param10)

        return params

    def isLicensed(self):
        """Set whether the tool is licensed to execute."""
        return True

    def updateParameters(self, parameters):
        """Modify the values and properties of parameters before internal validation is performed."""
        # Enable or disable parameters based on check box selections
        if parameters[1].value:  # If Clip DEM is checked
            parameters[2].enabled = True  # Enable clip shapefile/raster parameter
        else:
            parameters[2].enabled = False  # Disable clip shapefile/raster parameter

        if parameters[4].value:  # If Generate Flow Accumulation is checked
            parameters[5].enabled = True  # Enable flow accumulation output location
        else:
            parameters[5].enabled = False  # Disable flow accumulation output location

        if parameters[8].value:  # If Polygon Output is checked
            parameters[9].enabled = True  # Enable polygon output shapefile location
        else:
            parameters[9].enabled = False  # Disable polygon output shapefile location

        return

    def execute(self, parameters, messages):
        """The source code of the tool."""
        input_dem = parameters[0].valueAsText
        clip_dem = parameters[1].value
        clip_shapefile_or_raster = parameters[2].valueAsText
        z_limit = parameters[3].value
        generate_flow_accumulation = parameters[4].value
        flow_accumulation_output_location = parameters[5].valueAsText
        pour_point_raster = parameters[6].valueAsText
        watershed_output_layer = parameters[7].valueAsText
        polygon_output = parameters[8].value
        polygon_output_shapefile_location = parameters[9].valueAsText

        if not watershed_output_layer.endswith(".tif"):
            watershed_output_layer += ".tif"

        if generate_flow_accumulation and flow_accumulation_output_location:
            if not flow_accumulation_output_location.endswith(".tif"):
                flow_accumulation_output_location += ".tif"

        if polygon_output and polygon_output_shapefile_location:
            if not polygon_output_shapefile_location.endswith(".shp"):
                polygon_output_shapefile_location += ".shp"

        temp_dir = arcpy.env.scratchFolder
        arcpy.env.snapRaster = input_dem
        arcpy.env.cellSize = input_dem

        # Clip DEM
        if clip_dem:
            clipped_dem = os.path.join(temp_dir, "clipped_dem.tif")
            arcpy.management.Clip(input_dem, "", clipped_dem, clip_shapefile_or_raster, "0", "ClippingGeometry")
            dem_to_process = clipped_dem
        else:
            dem_to_process = input_dem

        # Fill the DEM
        filled_dem = os.path.join(temp_dir, "filled_dem.tif")
        arcpy.sa.Fill(dem_to_process, z_limit).save(filled_dem)

        # Flow Direction
        flow_direction = os.path.join(temp_dir, "flow_direction.tif")
        arcpy.sa.FlowDirection(filled_dem, "NORMAL", None, "D8").save(flow_direction)

        # Flow Accumulation
        if generate_flow_accumulation and flow_accumulation_output_location:
            arcpy.sa.FlowAccumulation(flow_direction, None, "FLOAT", "D8").save(flow_accumulation_output_location)
            # Add to Current Map
            aprx = arcpy.mp.ArcGISProject("CURRENT")
            aprx.activeMap.addDataFromPath(flow_accumulation_output_location)

        # Watershed
        # Extract the directory from the output path to use as scratchWorkspace
        output_dir = os.path.dirname(watershed_output_layer)

        # Set the scratch workspace to the output directory
        arcpy.env.scratchWorkspace = output_dir
        arcpy.env.overwriteOutput = True  # Ensure existing files are overwritten if necessary

        # Check if the output layer already exists, and if so, delete it
        if arcpy.Exists(watershed_output_layer):
            arcpy.Delete_management(watershed_output_layer)

        # Perform the watershed analysis using the Flow Direction and Pour Point raster
        out_raster = arcpy.sa.Watershed(flow_direction, pour_point_raster)

        # Save the output raster to the specified location
        out_raster.save(watershed_output_layer)

        # Optionally, you can add the output layer to the map (if needed)
        aprx = arcpy.mp.ArcGISProject("CURRENT")
        map_obj = aprx.activeMap
        map_obj.addDataFromPath(watershed_output_layer)

        # Polygon Output
        if polygon_output and polygon_output_shapefile_location:
            arcpy.conversion.RasterToPolygon(watershed_output_layer, polygon_output_shapefile_location, "NO_SIMPLIFY", "VALUE")
            # Add to Current Map
            aprx.activeMap.addDataFromPath(polygon_output_shapefile_location)

        return


    def postExecute(self, parameters):
        """This method takes place after outputs are processed and
        added to the display."""
        return
		
#Tool 6 clip data based on catchments
# -*- coding: utf-8 -*-
import arcpy
import os

class Tool(object):
    def __init__(self):
        self.label = "Sewer Network–Based DEM Splitter Tool"
        self.description = (
            "If the area of interest for running this toolbox is too large for the computer to run "
            "then this tool should be used before running all the other tools. This tool will split the DEM "
            "into smaller sections based on the sewer network data and a grid of user specified dimensions. "
            "The output from this tool can be used with the other tools in the toolbox. Multiple DEMs will be provided "
            "and the total number will be dependant on the size of the grid, this does mean multiple runs of the other tools will be required."
        )

    def getParameterInfo(self):
        """Define the tool parameters."""
        params = []

        input_feature = arcpy.Parameter(
            displayName="Input Sewer Network or Drainage Data",
            name="input_feature",
            datatype="GPFeatureLayer",
            parameterType="Required",
            direction="Input"
        )
        params.append(input_feature)

        reference_raster = arcpy.Parameter(
            displayName="DEM Input",
            name="reference_raster",
            datatype="GPRasterLayer",
            parameterType="Required",
            direction="Input"
        )
        params.append(reference_raster)

        buffer_distance = arcpy.Parameter(
            displayName="Buffer Distance (around sewer network or drainage data, in meters)",
            name="buffer_distance",
            datatype="GPDouble",
            parameterType="Required",
            direction="Input"
        )
        buffer_distance.value = 500
        params.append(buffer_distance)

        rows = arcpy.Parameter(
            displayName="Number of Rows (split vertically)",
            name="rows",
            datatype="GPLong",
            parameterType="Required",
            direction="Input"
        )
        params.append(rows)

        cols = arcpy.Parameter(
            displayName="Number of Columns (split horizontally)",
            name="cols",
            datatype="GPLong",
            parameterType="Required",
            direction="Input"
        )
        params.append(cols)

        output_folder = arcpy.Parameter(
            displayName="Output Folder (Save results to)",
            name="output_folder",
            datatype="DEFolder",
            parameterType="Required",
            direction="Input"
        )
        params.append(output_folder)

        return params

    def isLicensed(self):
        return True

    def updateParameters(self, parameters):
        return

    def updateMessages(self, parameters):
        return

    def execute(self, parameters, messages):
        arcpy.env.overwriteOutput = True

        # Parameters
        input_feature = parameters[0].valueAsText
        reference_raster = parameters[1].valueAsText
        buffer_distance_val = float(parameters[2].value)
        rows_val = int(parameters[3].value)
        cols_val = int(parameters[4].value)
        output_folder = parameters[5].valueAsText

        # Get spatial reference from raster
        target_sr = arcpy.Describe(reference_raster).spatialReference

        # Step 1: Project input to raster's spatial reference
        projected_input = os.path.join(output_folder, "projected_input.shp")
        arcpy.Project_management(input_feature, projected_input, target_sr)

        # Step 2: Buffer input with dissolve=ALL
        buffer_output = os.path.join(output_folder, "buffered_input.shp")
        arcpy.PairwiseBuffer_analysis(projected_input, buffer_output, f"{buffer_distance_val} Meters", dissolve_option="ALL")

        # Step 3: Create fishnet
        extent = arcpy.Describe(buffer_output).extent
        origin = f"{extent.XMin} {extent.YMin}"
        y_axis = f"{extent.XMin} {extent.YMin + 10}"
        opposite_corner = f"{extent.XMax} {extent.YMax}"
        cell_width = (extent.XMax - extent.XMin) / cols_val
        cell_height = (extent.YMax - extent.YMin) / rows_val
        fishnet_output = os.path.join(output_folder, "fishnet.shp")

        arcpy.CreateFishnet_management(
            out_feature_class=fishnet_output,
            origin_coord=origin,
            y_axis_coord=y_axis,
            cell_width=cell_width,
            cell_height=cell_height,
            number_rows=rows_val,
            number_columns=cols_val,
            corner_coord=opposite_corner,
            labels="NO_LABELS",
            template=buffer_output,
            geometry_type="POLYGON"
        )

        # Step 4: Add and calculate Id field
        arcpy.AddField_management(fishnet_output, "Id", "LONG")
        arcpy.CalculateField_management(fishnet_output, "Id", "!FID! + 1", "PYTHON3")

        # Step 5: Export individual fishnet cells and buffer them
        arcpy.MakeFeatureLayer_management(fishnet_output, "fishnet_layer")
        cell_buffers = []

        with arcpy.da.SearchCursor(fishnet_output, ["Id"]) as cursor:
            for row in cursor:
                id_val = row[0]
                where_clause = f"\"Id\" = {id_val}"
                single_cell = os.path.join(output_folder, f"fishnet_cell_{id_val}.shp")
                arcpy.Select_analysis("fishnet_layer", single_cell, where_clause)

                buffered_cell = os.path.join(output_folder, f"buffered_cell_{id_val}.shp")
                arcpy.PairwiseBuffer_analysis(single_cell, buffered_cell, "1000 Meters", dissolve_option="ALL")
                cell_buffers.append((id_val, single_cell, buffered_cell))

        # Step 6: Clip, buffer, then extract by mask
        for id_val, buffered_cell in cell_buffers:
            clip_output = os.path.join(output_folder, f"clipped_{id_val}.shp")
            final_buffer = os.path.join(output_folder, f"final_buffer_{id_val}.shp")
            extracted_output = os.path.join(output_folder, f"extracted_final_{id_val}.tif")

            # Clip and buffer
            arcpy.Clip_analysis(projected_input, buffered_cell, clip_output)
            arcpy.PairwiseBuffer_analysis(clip_output, final_buffer, "1000 Meters", dissolve_option="ALL")

            # Check geometry before ExtractByMask
            arcpy.MakeFeatureLayer_management(final_buffer, "final_layer_temp")
            count = int(arcpy.GetCount_management("final_layer_temp")[0])
            if count > 0:
                dem_extent = arcpy.Describe(reference_raster).extent
                poly_extent = arcpy.Describe(final_buffer).extent
                if dem_extent.overlaps(poly_extent) or dem_extent.contains(poly_extent):
                    try:
                        extracted_raster = arcpy.sa.ExtractByMask(reference_raster, final_buffer)
                        extracted_raster.save(extracted_output)
                    except Exception as e:
                        arcpy.AddWarning(f"ExtractByMask failed for Id {id_val}: {str(e)}")
                else:
                    arcpy.AddWarning(f"Skipped ExtractByMask for Id {id_val} - buffer not in DEM extent.")
            else:
                arcpy.AddWarning(f"Skipped ExtractByMask for Id {id_val} - no geometry.")

        arcpy.AddMessage("Tool execution completed successfully.")
