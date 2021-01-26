# Effort maps for three surveys

## ----Load data----------------------------------------------------------------
PW_survey_path <- here("hook_and_line_data","Washington_et_al_74-77_Puget_Sound_data.xls")
excel_sheets(PW_survey_path)
PW_survey <- read_excel(PW_survey_path, sheet = "Main Data")

# Set figure directory
fig_dir <- here("map_figures")


