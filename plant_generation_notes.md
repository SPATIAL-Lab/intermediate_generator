# Plant mapping notes

Run `source("plant_gen.R")` from the project root. Sources are discovered in `product_plant` and `archive_plant`: use the latest product for a study/method, otherwise its archive. Column C stores the local filename. General usage, source selection, duplicate handling and reports are described in `README.md`.

Franks is supplemented by SD; without Franks, use SD, SI and SR in that order. Konrad is selected only when those methods are absent. Matching lower-priority records fill missing raw inputs; published calibration descriptions do not prevent consolidation of otherwise identical measurements.

The standard template has 63 fields with names in row 4. Konrad has 61 fields with names in row 3. Neither template is modified. Konrad pore length and depth populate GCLab and GCWab with s1=s2=1, following the supplied guide. Grein 2011 uses the symmetric age field; other Konrad records use bounds where supplied.

A0 and its uncertainty are mapped where reported in the source. Unspecified stomatal density is treated as abaxial and recorded in notes. SI alone is not converted to density.

Retain all template-compatible fields from the selected methods, independently of the developing forward model. An SI-only record is not excluded or reported as an issue solely because it lacks density. Unlabeled source-note columns and source fixed-photosynthesis settings are retained in notes.
