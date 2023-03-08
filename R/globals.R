# Need this so that package will play nice with dplyr package
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
# more technical solution here: https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html

# other technical solution here:
# https://dplyr.tidyverse.org/articles/programming.html


utils::globalVariables(
  c(
    # ss_compile_hobo_data
    "variable",
    "sensor_serial_number",
    "sensor_type",
    "depth",
    "timestamp_",
    "HOBO_dat",
    "deployment_range",
    ".data",

    # ss_compile_aquameasure_data
    "Sensor",
    "Temp(Water)",
    "Record Type",
    #  "do_percent_saturation",
    "dissolved_oxygen_percent_saturation",
    "temperature_degree_c",
    "sensor_depth_at_low_tide_m",
    "sensor_depth_measured_m",

    # ss_compile_vemco_date
    "Description",
    "Units",
    "Data",

    # ss_compile_deployment_data
    "county",
    "waterbody",
    "station",
    "lease",
    "latitude",
    "longitude",

    # ss_read_log
    "Logger_Model",
    "Serial#",
    "Sensor_Depth",
    "numeric_depth",
    "detect_hobo",
    "detect_am",
    "detect_vemco",
    "log_sensor",
    "Station_Name",
    "Waterbody",
    "string_configuration",
    "Depl_Date",

    # make_column_names
    "col_name",

    # ss_convert_depth_to_ordered_factor
    "depth",

    # ss_pivot
    "value",

    # ss_download_data
    "name",

    # ss_plot_variables
    "Date",

    # ss_create_variable_labels
    "variable_label",

    # ss_generate_depl_filepath
    "depl_date",

    # ss_read_nsdfa_metadata
    "Depl_Lon",
    "Recv_Date",

    # dissolved oxygen corrections
    "F_p",
    "F_s",
    "P_wv",
    "T_Kelvin",
    "T_s",
    "alt_correction",
    "pressure_atm",
    "salinity_psu",
    "theta",

    # ss_import_data
    "Notes",

    # reformat_old_data
    "COUNTY",
    "DEPLOYMENT_PERIOD",
    "DEPTH",
    "LATITUDE",
    "LEASE",
    "LONGITUDE",
    "MOORING",
    "SENSOR",
    "STATION",
    "TIMESTAMP",
    "UNITS",
    "VALUE",
    "VARIABLE",
    "WATERBODY",
    "sensor"

  )
)




