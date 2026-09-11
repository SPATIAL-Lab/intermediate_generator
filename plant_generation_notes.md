# Plant intermediate generation

Run `source("plant_gen.R")` from `intermediate_generator`. Requires openxlsx; all other processing uses base R.

Sources are the local product workbooks in `data_plant`. Keep one product version per method/study. There are no downloads or cache dependencies. Column C records the source product filename. Sources and templates are never edited. The inclusive age filter at the top of the plant script is 0–66 Ma; it uses the product central age before any output age-format conversion.

For each study, use Franks then SD if Franks exists; otherwise SD, SI, then SR. Konrad is used only for studies without those methods. Matching lower-priority records fill blanks when sample identity, taxonomy, age, stratigraphic level and measurements agree. Different measurements remain separate. Cross-study removal also requires an explicit reference to the original study. CO2-only summary rows are excluded.

Franks/SD/SI/SR use the unchanged 63-column stomata template (field names in row 4). Konrad-only studies use the unchanged 61-column stomataKonrad template (field names in row 3). Each family has individual study workbooks and its own combined workbook.

Unspecified stomatal density is treated as abaxial, with the assumption in notes. Density and its error are converted from mm^-2 to m^-2. SD/SEM descriptions are retained in N_eDab; ambiguous uncertainty types are reported. Stomatal index alone is not converted to density.

Konrad measurements are matched by header: density to Dab, pore length to GCLab, pore depth to GCWab, and plant carbon isotopes to d13Cp. Density is converted to m^-2, lengths to m, and explicitly labeled 2-sigma measurement errors are divided by two. Fixed values come from the supplied template guide, including s1=s2=1 for these pore measurements. Grein ages use the symmetric age field; other Konrad ages use absolute bounds, following the guide. Corroborated age offsets in products are converted to absolute bounds and explained in age notes.

The Konrad guide requests A0 and its uncertainty from TRY, a plant-trait database. Those external model inputs are not supplied locally and remain blank. Their selection is a forward-model preparation task, not a source-sheet error, so they are not included in the issues CSV. The intermediate workbooks do not by themselves supply every Konrad model input.

Only two optional compiled reports are written: `plant_data_issues.csv` for unresolved source-data questions and `plant_duplicate_candidates.csv` for duplicate pairs. Duplicate status identifies retained and removed/merged records or ambiguous pairs retained for review. No age-screen or per-study reports are written. Stale workbooks are archived in `output_plant/previous_outputs`; obsolete reports are archived under `scratch/previous_reports`.

`generator_checks.R` is required by all four generators for shared age filtering, validation and report handling. The former product cache and refresh script are retired; plants now read the supplied products directly.
