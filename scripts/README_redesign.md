# Gulf of Riga oxygen indicator script redesign

This bundle keeps scripts `01`-`07` unchanged and centralizes the user-defined settings for scripts `08_0` onwards in `scripts/03_define_assessment.R`.

## Main workflow

1. Edit only the `USER-DEFINED ASSESSMENT SETTINGS` section in `scripts/03_define_assessment.R`.
2. Run scripts in order:
   - `01_header.R`
   - `03_define_assessment.R`
   - `04_data_download.R`
   - `05_make_assessment_area.R`
   - `06_make_bathymetry_layer.R`
   - `07_data_preparation.R`
   - `08_0_monthly_mean_deep_data_dynamic_bottom_limit_output_names_season_highlight_dynamic_figure_name.R`
   - `08_1_smooth_seasonal_profiles.R`
   - `08_2_model_validation.R`
   - `08_3_yearly_DO_debt_profiles_deep_values_4panel_percentiles_dynamic_month_titles.R`
   - `08_4_EQRS_with_EQRS_class_figure.R`

## What `03_define_assessment.R` now writes

`03_define_assessment.R` creates the normal assessment folders and writes:

- `Output/years_<start>_<end>/assessment_settings.rds`
- `Output/years_<start>_<end>/assessment_settings_summary.csv`

Scripts `08_0`-`08_4` read the settings RDS and use its nested `assessment$indicator` object.

## Centralized settings

The following values are now centralized in `03_define_assessment.R`:

- assessment years, topic, basin, input/output roots
- shared indicator months/season
- near-bottom bathymetry-distance rule for script `08_0`
- monthly averaging columns and monthly figure settings for script `08_0`
- smoothing settings for script `08_1`
- validation variables and figure settings for script `08_2`
- selected deep-value/deepest-profile settings for script `08_3`
- percentile baseline settings for script `08_3`
- EQRS BEST baseline, ACCDEV, final-result year, class scaling, period averages, parameters, and figure settings for script `08_4`

## Notes

- `bottom_depth_limit_m` and `deep_profile_min_depth_m` are deliberately separate.
  - `bottom_depth_limit_m` is distance from the deepest sample to bathymetry depth.
  - `deep_profile_min_depth_m` is an absolute minimum measured depth for selected profile-deepest records.
- Output names in scripts `08_0`-`08_4` are built from the central settings, especially months, depth limits, method, and baseline years.
