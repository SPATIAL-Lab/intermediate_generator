# Plant intermediate generation

Run from `intermediate_generator`:

```r
source("refresh_plant_products.R")  # after adding studies or requesting a NOAA refresh
source("plant_gen.R")
```

`plant_gen.R` uses the existing, unchanged `stomata_IntermediateTemplate.xlsx` and writes only its 63 columns. It uses base R and openxlsx. The age range at the top is inclusive, in Ma: 0–23.03 for Quaternary and Neogene. Product age revisions are applied before this filter.

For each study, the source priority is Franks then SD if Franks exists; otherwise SD, SI, then SR. These are sources of raw forward-PSM inputs, not instructions to reproduce published CO2 estimates or calibration functions. Lower-priority records fill missing inputs when sample identity, age, stratigraphic level and available measurements agree. Different measurement values remain separate. Konrad methods are outside this hierarchy.

Unspecified stomatal density is treated as abaxial, as agreed. Density and its uncertainty are multiplied by 1e6 to convert mm^-2 to m^-2, with that assumption recorded in the existing notes column. Explicit SD/SEM suffixes are separated from numeric uncertainty values; the uncertainty description is kept in N_eDab. Stomatal index alone is not converted into density. Missing forward-model measurements remain blank and are reported.

Product files are matched to studies and then to samples using IDs, taxonomy and measurements, with calibration metadata used only to distinguish otherwise ambiguous product rows. Explicitly documented age revisions and missing-value additions are used. Unexplained differences remain in the issues report. Product-only rows with fossil measurements may be added; CO2-only aggregate summaries are not added as fossil samples. Matching does not rely on row order.

The product cache contains versioned NOAA workbooks, a URL/checksum manifest and verified archive links. Generation uses that fixed cache without network requests and stops if a cached workbook changes or a new source study has not been checked. Refreshing downloads the latest version for the studies currently in data_plant and preserves changed older copies. Original archive workbooks are never edited.

Outputs in `output_plant`:

- `stomata_Intermediate_<study>.xlsx` and `stomata_Intermediate_combined.xlsx`: identical template columns.
- `plant_data_issues.csv` and per-study issues files: unresolved source-data questions only.
- `plant_processing_log.csv`: handled conversions, revisions, priority selections and duplicate removals.
- `plant_product_comparison.csv`: source/product values and the decision for each comparison.
- `plant_product_coverage.csv`: product availability for local source workbooks.
- `plant_record_metadata.csv`: source rows and their retained combined-sheet rows.
- `plant_value_sources.csv`: source cells for intermediate values.
- `plant_duplicate_removals.csv`: repeated measurements retained under their explicitly cited original study.
- `plant_age_screen.csv`: age selection decisions.

Repeated calibration estimates for the same measured sample are consolidated. Cross-study removal requires matching measurements/ages/taxonomy and an explicit reference to the retained original study. Missing IDs or conflicting measurements do not by themselves establish a duplicate. Stale intermediate workbooks are moved to `previous_outputs`.
