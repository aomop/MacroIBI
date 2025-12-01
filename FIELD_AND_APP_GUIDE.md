# Wetland Macroinvertebrate Sampling and MacroIBI App Guide

This guide walks you through every step of the Minnesota Pollution Control Agency (MPCA) macroinvertebrate wetland sampling protocol and shows how to process and identify your samples with the MacroIBI R/Shiny application. It is written for newcomers with no prior field or R experience.

## 1. Field Planning and Crew Readiness

1. **Timing.** Sample during the index window from June through early July so larvae are mature enough to identify but wetlands still hold water. Sampling later in summer can leave ponds too dry or colonized by transient invertebrates.
2. **Habitat choice.** Prefer the emergent vegetation zone because it has the highest richness. If it is missing, sample (in order) floating‐leaf plants, submerged plants, then shallow open water. Record which zones you use on your field form.
3. **Crew skills.** Field leaders should be trained aquatic biologists with GPS/map skills; all personnel must be fit enough to wade or hike to sites.

## 2. Equipment Checklist (take two of anything listed as pairs)

Pack the following before leaving for the site: D‑frame dip nets with 500 µm mesh; four small plastic pans; two hardware‐cloth frames; two large pans; 200 µm sieve; two squirt bottles (water and alcohol); two soft forceps; field visit forms; maps/GPS; pencils, clipboard, scissors, permanent marker; internal/external sample labels; 100% reagent alcohol; waterproof notebook; chest waders and rain gear; camera; 16 oz wide‑mouth plastic jars; cooler/crate.

## 3. Collecting Macroinvertebrate Samples

1. **Set your radius.** Work within a 10–15 m radius in the highest‑priority habitat available.
2. **Two dipnet samples per wetland.** You will combine them later for scoring.
3. **Each sample has two efforts.** For each effort, make 3–5 strong sweeps through the water column and vegetation, reaching out and pulling the net back quickly. Avoid scraping mud; if you do, rinse and redo the effort in a new spot.
4. **Repeat.** After finishing the first dip net sample (two efforts), do the second dip net sample the same way.

## 4. Separating Organisms in the Field

1. Empty each dip net effort onto a hardware‑cloth frame sitting over paired small pans that rest inside a large pan.
2. Rinse vegetation with site water so invertebrates drop into the pans. Spread plant material apart for 10 minutes so organisms can crawl or fall into the water below.
3. Use forceps to pick organisms from vegetation into the pans; avoid focusing on a single taxon so the sample stays representative.
4. Pour all pan contents through the 200 µm sieve, flushing stuck snails or leeches with your water squirt bottle. Combine any organisms that fall into the large pans.
5. Back‑flush the sieve with reagent alcohol into a sample jar, combining both efforts for the dip net sample. Aim for ~80% final alcohol. Split into multiple jars if >⅓ full of organisms.
6. Label inside and outside the jar with site ID, date, sample number, jar number (if multiple), crew initials, and habitat notes.

## 5. Storing and Maintaining Samples

Keep preserved jars in a designated hazardous‑materials room. Check periodically for evaporation and top up with 80% alcohol; replace lids if needed to prevent further loss.

## 6. Preparing for Lab Identification

1. **Workspace.** Set up a clean bench with your sample jars, dissecting microscope, petri dishes, and forceps. Work through each jar in turn.
2. **Identify taxa.** Sort organisms by taxonomic group (e.g., Ephemeroptera, Odonata, Trichoptera, snails, beetles/true bugs). Count individuals from each dip net sample separately when possible to match the app’s Dipnet 1 and Dipnet 2 fields.
3. **Data you will need in the app.** For each taxon you identify: the group, the taxon name (Genus/Family/Order level as available), and counts from Dipnet 1 and Dipnet 2. Keep your field forms handy for habitat notes and dates.

## 7. Installing the MacroIBI R Package

1. Install R (https://cran.r-project.org) if you do not have it. The app uses R and Shiny but requires no coding experience.
2. Download or clone this repository, open R, and from the project root run:
```r
install.packages("devtools")
devtools::install_local()
```
These commands install the MacroIBI package locally.

## 8. Launching the MacroIBI App

1. In R, load the package and start the app:
```r
library(macroibi)
app <- run_macroibi()
shiny::runApp(app)
```
The app bundles all required data and assets; autosave files are stored in your user data directory automatically.
2. A browser window titled **Wetland IBI Dashboard** opens with two tabs: **Data** for entry and **Results** for scores.

## 9. Entering Taxa and Counts (Data Tab)

1. Use **Select Taxon** to search the built‑in taxonomy and add taxa to the appropriate colored group section. Each section shows the taxon name and numeric inputs for **Dipnet 1 Count** and **Dipnet 2 Count**; totals update automatically.
2. Each group footer displays **Total Taxa**, **Percent of Total Sample**, and **Total Individuals**, updating as you edit counts.
3. Use **Show/Hide Taxonomic Hierarchy** to visualize relationships for the taxa you entered.
4. Click **Clear All Data** to reset everything, or use the **Upload/Download** card to import previously saved CSV data back into the tables.
5. Enable **Autosave Settings** to periodically cache your work; the app stores autosaves per user automatically.

## 10. Calculating Metrics and Viewing Results

1. Go to the **Results** tab. The app shows the **Total Individuals** and a table of metric values and scores: EOT taxa, snail taxa, all taxa richness, corixid ratio, abundance of EOT, and the overall IBI (0–50).
2. Hover over “How are these calculated?” to see the corixid metric formula and adjustment figure.
3. Metric scores update instantly as you edit counts in the Data tab.

## 11. Saving and Sharing Results

Use the buttons at the bottom of the Results tab to download:
- **CSV** of metric data, named with your site and date.
- **Table Image (PNG)** with title and sampling/calculation dates.
- **Data Summary (PDF)** and **Full Report (PDF)** that include current metrics and comparisons to other saved metric files in your autosave directory.

## 12. Practical Tips for First‑Time Users

- Enter counts from Dipnet 1 and Dipnet 2 exactly as collected so the app can compute metrics correctly.
- If you split a large catch into multiple jars, combine the counts during identification before entering them into the app (the field protocol’s “jar 1 of 2” labeling helps you track this).
- Keep your field visit form nearby when filling the site title/date fields so your exports are labeled accurately.
- Re‑enable autosave whenever you start a new identification session to avoid losing work.

Following these steps will take you from planning and collecting wetland macroinvertebrate samples through preserving, identifying, and scoring them with the MacroIBI application.
