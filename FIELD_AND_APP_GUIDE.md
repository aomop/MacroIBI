# Wetland Macroinvertebrate Sampling and MacroIBI App Guide

This guide walks you through the macroinvertebrate sampling protocol developed by the MPCA and how to process and identify your samples with the MacroIBI R/Shiny application.

## 1. Field Planning

1. **Timing.** Sample during the index window from June through early July so larvae are mature enough to identify but wetlands still hold water. Sampling later in summer can leave ponds too dry or colonized by transient invertebrates.
2. **Habitat choice.** Prefer the emergent vegetation zone because it has the highest richness. If it is missing, sample (in order) floating‐leaf plants, submerged plants, then shallow open water. Record which zones you use on your field form.

## 2. Equipment Checklist

Required Field Equipment:
- Two D‑frame dip nets with 500µm mesh
- Hardware cloth with frame
- A plastic bin to place under the hardware cloth
- 200µm mesh sieve for sample consolidation
- At least 2 squirt bottles; one filled with water, the other alcohol
- Forceps (one per crew member)
- Wetland Invertebrate Visit Form (included in this repo)
- GPS Unit for documenting location of sampling
- Pencils and clipboards for visit form
- 100% reagent alcohol
- Chest waders
- Plastic jars for sample storage
- Permament marker for labeling sample jars

Optional Field Equipment:
- Maps of wetland and surrounding area
- Scissors; if using pre-printed labels
- Camera to document site conditions
- Cooler or crate to store samples for transport
- Any additional outerwear for comfort

Required Lab Equipment:
- Stereoscope(s) (2x - 4x magnification) or other magnification device(s)
- Petri dishes for sorting taxa
- Aquatic macroinvertebrate identification guides
- Squirt bottle(s) filled with either water or alcohol
- Dissection kit for manipuating organisms under the microscope (foreceps, needles/pointers)
- A computer with R and the macroibi package installed

## 3. Collecting Macroinvertebrate Samples

1. **Determine sampling area** Using the habitats listed in step 1.2, find the highest‑priority habitat available
    - Work within a 10–15 m radius within the identified area (this may be smaller depending on the size of the habitat)
2. **Sample** Each sample consists of two "efforts"; for each effort, make 3–5 strong sweeps through the water column and vegetation, reaching out and pulling the net back quickly.
    - Avoid scraping mud; if you do, dump out the sample and repeat the effort in a new spot.
3. **Sort** After two efforts have been completed (one in each dipnet), dump both efforts onto the hardware cloth over the plastic bin
    - Set a timer for 10 minutes. Up to four people can now sort through the sample using foreceps, separating macroinvertebrates from the vegetation (see section 4 for details).
4. **Repeat** Repeat steps 2 and 3 for the second sample. Stay in the same previously identified area, but try to sample new spots.
    - This sample can be combined with the first or kept separate depending on preference.
    - The second sample is not meant to be a duplicate, but rather sample the area more broadly. Both samples will be combined in the calculation stage regardless.

*Depending on size of the crew, having some members begin the second round of sampling while others sort can save a lot of time.*

## 4. Sorting Samples in the Field

During the 10 minute sorting window:
1. Rinse vegetation with site water so invertebrates drop into the pans. Spread plant material apart so organisms can crawl or fall into the water below.
2. Use forceps to pick organisms from vegetation into the pans; avoid focusing on a single taxon so the sample stays representative.
3. After 10 minutes, whatever vegetation is left on the hardware cloth can be returned to the sampling area. The macroinvertebrates that fell into the bin is your sample.

After separating vegetation:
1. Pour all pan contents through the 200 µm sieve, flushing stuck snails or leeches with your water squirt bottle. Combine any organisms that fall into the large pans.
2. Back‑flush the sieve with reagent alcohol into a sample jar, combining both efforts for the dip net sample. Aim for ~80% final alcohol. Split into multiple jars if the jar is more than ⅓ full of organisms.
    - You may need to use the squirt bottle to get all organisms out of the sieve; **do not use water in this step**
3. Label inside and outside the jar with site ID, date, sample number, jar number (if multiple), and crew initials.

*Keep preserved jars in a designated hazardous‑materials room. Check periodically for evaporation and top up with 80% alcohol; replace lids if needed to prevent further loss.*

## 5. Preparing for Lab Identification

1. **Workspace.** Set up a clean bench with your sample jars, dissecting microscope, petri dishes, and forceps. Work through each jar in turn.
2. **Identify taxa.** Sort organisms by taxonomic group (e.g., Ephemeroptera, Odonata, Trichoptera, snails, beetles/true bugs). Count individuals from each dip net sample separately when possible to match the app’s Dipnet 1 and Dipnet 2 fields.
3. **Data you will need in the app.** For each taxon you identify: the group, the taxon name (Genus/Family/Order level as available), and counts from Dipnet 1 and Dipnet 2. Keep your field forms handy for habitat notes and dates.

## 6. Installing the MacroIBI R Package

1. Install R (https://cran.r-project.org) if you do not have it. The app uses R and Shiny but requires no coding experience.
2. Download or clone this repository, open R, and from the project root run:
```r
install.packages("devtools")
devtools::install_local()
```
These commands install the MacroIBI package locally.

## 7. Launching the MacroIBI App

1. In R, load the package and start the app:
```r
library(macroibi)
app <- run_macroibi()
shiny::runApp(app)
```
The app bundles all required data and assets; autosave files are stored in your user data directory automatically.
2. A browser window titled **Wetland IBI Dashboard** opens with two tabs: **Data** for entry and **Results** for scores.

## 8. Entering Taxa and Counts (Data Tab)

1. Use **Select Taxon** to search the built‑in taxonomy and add taxa to the appropriate colored group section. Each section shows the taxon name and numeric inputs for **Dipnet 1 Count** and **Dipnet 2 Count**; totals update automatically.
2. Each group footer displays **Total Taxa**, **Percent of Total Sample**, and **Total Individuals**, updating as you edit counts.
3. Use **Show/Hide Taxonomic Hierarchy** to visualize relationships for the taxa you entered.
4. Click **Clear All Data** to reset everything, or use the **Upload/Download** card to import previously saved CSV data back into the tables.
5. Enable **Autosave Settings** to periodically cache your work; the app stores autosaves per user automatically.

## 9. Calculating Metrics and Viewing Results

1. Go to the **Results** tab. The app shows the **Total Individuals** and a table of metric values and scores: EOT taxa, snail taxa, all taxa richness, corixid ratio, abundance of EOT, and the overall IBI (0–50).
2. Hover over “How are these calculated?” to see the corixid metric formula and adjustment figure.
3. Metric scores update instantly as you edit counts in the Data tab.

## 10. Saving and Sharing Results

Use the buttons at the bottom of the Results tab to download:
- **CSV** of metric data, named with your site and date.
- **Table Image (PNG)** with title and sampling/calculation dates.
- **Data Summary (PDF)** and **Full Report (PDF)** that include current metrics and comparisons to other saved metric files in your autosave directory.

## 11. Practical Tips for First‑Time Users

- Enter counts from Dipnet 1 and Dipnet 2 exactly as collected so the app can compute metrics correctly.
- If you split a large catch into multiple jars, combine the counts during identification before entering them into the app (the field protocol’s “jar 1 of 2” labeling helps you track this).
- Keep your field visit form nearby when filling the site title/date fields so your exports are labeled accurately.
- Re‑enable autosave whenever you start a new identification session to avoid losing work.

Following these steps will take you from planning and collecting wetland macroinvertebrate samples through preserving, identifying, and scoring them with the MacroIBI application.
