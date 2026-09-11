# Proxy intermediate sheet generators

**★ PRE-CENOZOIC AGES HAVE NOT YET BEEN UPDATED TO GTS2020; THIS WORK IS IN PROGRESS. THESE SAMPLES RETAIN SOURCE AGE ASSIGNMENTS AND REQUIRE GTS2020 HARMONIZATION BEFORE ANALYSIS. THEY ARE INCLUDED HERE TO TEST THE GENERATORS ACROSS THE FULL PHANEROZOIC COMPILATION. ★**

These scripts distill paleosol, phytoplankton, boron isotope and plant workbooks into intermediate sheets for forward proxy-system modelling. They retain the measurements and supporting metadata required by the templates, rather than every source field or published CO₂ estimate.

## Running the scripts

Open `intermediate_generator.Rproj` in RStudio, or set the working directory to this folder. Processing uses base R and `openxlsx`.

```r
install.packages("openxlsx")  # once per R installation

source("paleosol_gen.R")
source("phyto_gen.R")
source("boron_gen.R")
source("plant_gen.R")
```

Each generator runs independently and sources `generator_checks.R` for source selection, shared duplicate handling and reports. Keep that file alongside the generators. Close the output workbooks in Excel before rerunning.

## Source folders

| Generator | Archive folder | Product folder | Output folder |
| --- | --- | --- | --- |
| `paleosol_gen.R` | `archive_paleosol/` | `product_paleosol/` | `output_paleosol/` |
| `phyto_gen.R` | `archive_phyto/` | `product_phyto/` | `output_phyto/` |
| `boron_gen.R` | `archive_boron/` | `product_boron/` | `output_boron/` |
| `plant_gen.R` | `archive_plant/` | `product_plant/` | `output_plant/` |

Intermediate-sheet data are sourced from the latest available version of each study’s [product workbook](https://www.ncei.noaa.gov/pub/data/paleo/climate_forcing/trace_gases/Paleo-pCO2/product_files/). Where no product is available, the corresponding [archive workbook](https://www.ncei.noaa.gov/pub/data/paleo/climate_forcing/trace_gases/Paleo-pCO2/) is used.

The scripts discover the locally stored XLSX files on each run and select the latest product version present. Selection happens per study and, for plants, per method. There is no online lookup or download step; updated database files must first be added to the source folders.

Product filenames normally end in `_p1.0.xlsx`, `_p1.1.xlsx`, etc. The highest numeric version is selected automatically. Preserve study suffixes such as `2022a` and `2022b`, and the plant method prefix, for example `stomata-franks_` or `stomata-si_`.

Matching first uses the filename without the product version. A unique DOI and method match also identifies products whose study filename has changed. Ambiguous DOI matches are not used to discard an archive. The sample duplicate checks then provide a second level of review.

Add new files to the appropriate folders and rerun. No filename list or mapping edit is needed for studies using a supported structure. Headers are matched by name, allowing differences in case, punctuation, header-row position and the extra summary columns in products. A substantially different layout or a genuinely new measurement convention still needs review.

## What gets mapped

Column C records the local source filename. Source workbooks and templates are never edited. Product age revisions are used where supplied. Samples with corrected ages remain eligible even if the original ages were quarantined; unresolved quarantine and explicit exclusion flags are respected. A CO₂-method quality category alone is not treated as an unusable raw measurement.

### Paleosols

The generator reads the older paleosol layout, the newer expanded layout and the earlier two-row header structure. It maps sample metadata, carbonate and organic isotopes, enamel and oxygen isotopes, temperature, precipitation and notes.

Headers identify the fields instead of fixed Excel column letters. Labelled temperature and precipitation uncertainties are converted to the template's 2-sigma convention. The existing respired-carbon fallback is retained for the expanded layout when neither organic pool was measured. Missing-value dashes and Unicode minus signs are normalized; a leading ± on a stated uncertainty is read as its magnitude. Additional source-publication references are included in notes. Reversed age bounds are reordered only when they bracket the central age; other inconsistencies remain in the issues report.

### Phytoplankton

The generator maps measured organic δ¹³C, temperature, phosphate, lith size, cell radius, metadata and parent/child links. Products' revised input fields (`..._use`) take precedence where populated, with their associated uncertainties. These are the physical inputs to the model, not the calculated CO₂ columns.

Supported cleanup includes alternate SST inputs, uncertainty labels misplaced in a sample-count field and the confirmed Rae 2021 `5 to 97.5 percentile` typo, interpreted as `2.5 to 97.5 percentile`. Revised organic δ¹³C uses the measured-value field, not the separately calculated biomass isotope composition. For revised lith-size errors labeled only `normal`, an explicit published sigma definition is retained when the error magnitude is unchanged. Otherwise the error is left blank and its undefined sigma count is reported.

### Boron isotopes

The generator maps sample and age metadata, species, shell size, boron isotopes, Mg/Ca and temperature. Field names identify the measurements across archive and product layouts.

Where a usable Mg/Ca mean is absent, supported comma-separated replicates can supply the mean. Text notes are kept out of numeric fields and carried into the notes column. A missing Mg/Ca error side is filled only when the source explicitly supports the symmetric ±3% case. Other one-sided uncertainties remain review items. Boron error magnitudes are also compared with reported replicate 2SE.

### Plants

Source selection is followed by the method hierarchy within each study:

1. Franks, supplemented by SD where appropriate, if Franks is available.
2. Otherwise SD, then SI, then SR.
3. Konrad only for studies without those methods.

Franks, SD, SI and SR use the 63-column stomata template. Unspecified stomatal density is treated as abaxial, with the assumption in notes. Density and its uncertainty are converted from mm⁻² to m⁻². SI alone is not converted to density. Records from the selected methods retain all information that fits the existing templates; the developing forward model does not determine which records are retained. Rows containing only combined CO₂ estimates, without fossil measurements, are excluded. Source fixed-photosynthesis settings are retained in notes where applicable.

Konrad-only studies use the 61-column stomataKonrad template. Density maps to `Dab`, pore length to `GCLab`, pore depth to `GCWab`, and plant δ¹³C to `d13Cp`. Units are converted to m⁻² and m, and measured 2-sigma errors are divided by two. Fixed inputs follow the supplied template guide, including `s1 = s2 = 1` for these pore measurements. When source notes explicitly identify a different gas-exchange model, the source taxon and warning are retained rather than assigning the generic Angiosperm label.

The Konrad guide requests photosynthetic rate `A0` and its uncertainty from TRY, a plant-trait database. Those external model inputs are not supplied locally and remain blank. Selecting them is a model-preparation task, not an original-sheet error.

## Uncertainties

The destination template defines the uncertainty convention. Archive-based intermediate fields generally use 2-sigma errors; Konrad measured errors are written at 1 sigma. Plant SD/SEM descriptions remain attached to their values. A standard error and a standard deviation are not interchangeable descriptions.

Product age errors without a stated type are assumed to be **2 sigma**, and that assumption is listed in the issues CSV. Explicit source definitions take precedence. Unsupported uncertainty definitions or incomplete error pairs remain review items rather than being silently converted.

## Duplicate handling

All four generators use the same resolver. Each supplies identity evidence appropriate to its metadata: sample/site or depth, unique IDs and parent/child links, or study/sample/taxonomy.

- Conflicting measurement values remain separate and are not flagged solely because an ID matches.
- A clear match requires identity evidence, at least one shared measurement and no conflict in the other compared inputs. Complementary values fill blanks in the retained record; notes are combined.
- Earlier publications take precedence, with plant method priority applied within studies. A redundant row is removed from both the individual and combined outputs.
- Conflicting supporting inputs, no overlapping measurements or multiple plausible matches leave both records in place for review.

The duplicate CSV records the original source rows and their final status. A shared identifier or a parent/child link is evidence to check, not proof of duplication.

## Outputs and reports

Each generator writes individual study workbooks and a combined workbook. Plants have separate `stomata_Intermediate_...` and `stomataKonrad_Intermediate_...` families, each with its own combined workbook.

All workbooks use the unchanged `data4PSM` template worksheet. Standard templates have field names in row 4 and data beginning in row 5. Konrad has field names in row 3 and data beginning in row 4. Readers of these workbooks must account for this difference.

Only two additional compiled reports are written, when needed:

| Report | Contents |
| --- | --- |
| `<proxy>_data_issues.csv` | Unresolved source-data or mapping questions and explicitly flagged assumptions, with filename, source row, field and value |
| `<proxy>_duplicate_candidates.csv` | Candidate pairs marked retained/removed/merged or ambiguous and retained for review |

Already handled cleanup is not a separate issue. Reports are compiled across studies. Current intermediate workbooks are overwritten on each run. Stale generated workbooks and obsolete reports are removed without saving previous-output copies.

A completed run can still contain incomplete model inputs. Review the issues report before analysis. If a run stops, resolve the console error and rerun to completion before using the outputs; a partial run may contain files from different runs.

## Reproducibility and maintenance

Make source-data corrections in the source workbook and reusable mapping changes in the generator. Do not edit generated sheets: those changes disappear on the next run. After adding studies, check representative source/output rows for the important measurements, units, ages and uncertainty definitions.

Keep the scripts, shared helper, templates and exact input versions together in Git, or retain an unambiguous versioned record of inputs stored elsewhere. Record R and package versions with `sessionInfo()` when preparing a release. The generators do not download files or maintain a package lockfile.

`scratch/` is ignored by Git and is not an input to generation. Local backups and Excel lock files (`~$...`) are not needed to reproduce a run. The separate age-update and timescale scripts are not called by these generators.
