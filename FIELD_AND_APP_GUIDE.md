# Wetland Macroinvertebrate Sampling and MacroIBI App Guide

This guide walks you through the macroinvertebrate sampling protocol developed by the MPCA and how to process and identify your samples with the MacroIBI R/Shiny application.

## 1. Equipment Checklist

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
- Dissection kit for manipuating organisms under magnification (foreceps, needles/pointers)
- A computer with R and the macroibi package installed

## 2. Collecting Macroinvertebrate Samples

**Timing.** Sample during a window from June through early July so larvae are mature enough to identify but wetlands still hold water. 
            Sampling later in summer can leave ponds too dry or colonized by transient invertebrates.
            If more than one sampling event is planned, ensure they all take place within this window to minimize temporal effects.

**Habitat choice.** Prefer the emergent vegetation zone because it has the highest richness. 
                    If it is missing, sample (in order) floating‐leaf plants, submerged plants, then shallow open water. Record which zones you use on your field form.

1. **Determine sampling area** Using the habitats listed above, find the highest‑priority habitat available
    - Work within a 10–15 m radius within the identified area (this may be smaller depending on the size of the habitat)
2. **Sample** Each sample consists of two "efforts"; for each effort, make 3–5 strong sweeps through the water column and vegetation, reaching out and pulling the net back quickly.
    - Avoid scraping mud; if you do, dump out the sample and repeat the effort in a new spot.
3. **Sort** After two efforts have been completed (one in each dipnet), dump both efforts onto the hardware cloth over the plastic bin
    - Set a timer for 10 minutes. Up to four people can now sort through the sample using foreceps, separating macroinvertebrates from the vegetation (see section 4 for details).
4. **Repeat** Repeat steps 2 and 3 for the second sample. Stay in the same previously identified area, but try to sample new spots.
    - This sample can be combined with the first or kept separate depending on preference.
    - The second sample is not meant to be a duplicate, but rather sample the area more broadly. Both samples will be combined in the calculation stage regardless.

*Depending on size of the crew, having some members begin the second round of sampling while others sort can save a lot of time.*

## 3. Sorting Samples in the Field

**During the 10 minute sorting window:**
1. Rinse vegetation with site water so invertebrates drop into the pans. Spread plant material apart so organisms can crawl or fall into the water below.
2. Use forceps to pick organisms from vegetation into the pans; avoid focusing on a single taxon so the sample stays representative.
3. After 10 minutes, whatever vegetation is left on the hardware cloth can be returned to the sampling area. The macroinvertebrates that fell into the bin is your sample.

**After separating vegetation:**
1. Pour all pan contents through the 200 µm sieve, flushing stuck snails or leeches with your water squirt bottle. Combine any organisms that fall into the large pans.
2. Back‑flush the sieve with reagent alcohol into a sample jar, combining both efforts for the dip net sample. Aim for ~80% final alcohol. Split into multiple jars if the jar is more than ⅓ full of organisms.
    - You may need to use the squirt bottle to get all organisms out of the sieve; **do not use water in this step!**
3. Label the jar with site ID, date, sample number, jar number (if multiple), and crew initials.
4. Keep preserved jars in a designated hazardous‑materials room. Check periodically for evaporation and top up with 80% alcohol; replace lids if needed to prevent further loss.

## 4. Preparing for Lab Identification

1. **Setup the software** Ensure R and the MacroIBI package are installed on your computer. See section 6 for details.
2. **Set up the Workspace** Set up a clean bench with the required lab equipment layed out in section 2.
3. **Identify + Count** This protocol requires that every organism in the sample is identified and counted.
    - Identifiers will need magnification to make many of the IDs accurately; some taxa can be ID'd with the naked eye
    - All taxa must be identified at least to the Family level, Genus ideally, and Species if possible.
    - Designate one person to handle data entry; all other team members should relay counts to them.
    - It can be helpful to designate people to pre-sort the samples; group organisms together into visually similar groups. This allows the identifiers to quickly ID many organisms.
    - If the sample cannot be fully processed in one sitting or should otherwise remain preserved, water should not be used at any point during this process.
4. **Enter Data** Periodically relay taxa counts to the data handler so they can enter the taxa and counts into the app.
    - If samples have been combined, all counts can be entered into the "Dipnet 1" box
    - See section 7 for more details on using the app.

## 5. Installing the MacroIBI R Package

1. Install R [https://cran.r-project.org] if you do not have it.
2. Install RStudio (Optional, but highly recommended) [https://posit.co/download/rstudio-desktop/]
3. Download or clone this repository.
   If you have git bash, open a terminal and run:

   ```bash
   cd "/desired/directory/path" # Change this
   git clone https://github.com/aomop/MacroIBI.git
   ```
   
   If not; download the repository as a .zip file by clicking the green "Code" button in the top left-hand corner of the repository page.
   
5. Extract the .zip file contents to the desired file path
6. Install and run the app:

   If you do not have R studio, launch R and type:
    ```r
    setwd("PATH/TO/THE/REPOSITORY") # Change this to the path of the extracted content. It should end with "/MacroIBI-main/"
    install.packages("devtools")
    devtools::install_local()
    ```
   If you have RStudio; open the `Wetland_IBI.Rproj` file, then run:
   ```r
   install.packages("devtools")
   devtools::install_local()
   ```

## 6. Using the App

Start the dashboard by loading the package and running:

```r
library(macroibi)
run_macroibi()
```

A browser window titled **Wetland IBI Dashboard** will open. A popup will appear asking you to enter a title and date for the sample. 
Enter the sample information, then hit the green "Let's go!" button. You may also "Continue without Metadata" if you plan to upload data or reload an autosave.

## Step 1: Turn On Autosave (Data Tab)

After entering a title and date, find the **Autosave Settings** panel on the left side of the **Data** tab and enable autosaving.
The app will periodically cache your progress in a user-specific autosave directory so that long identification sessions are protected.
You can re-load these autosave files with the "Load Autosave" button.

## Step 2: Enter Taxa and Counts While Sorting (Data Tab)

Use **Select Taxon** to add taxa to the group section counts.
The app automatically sorts taxa into the correct groups.

Each row displays:
- the taxon name
- numeric inputs for **Dipnet 1 Count** and **Dipnet 2 Count**
- an automatically updating **Sum Count**

Each group footer shows:
- **Total Taxa**
- **Percent of Total Sample**
- **Total Individuals**

## Step 4: Review Calculated Metrics (Results Tab)

Switch to the **Results** tab to see the calculated metrics.
Metrics will update instantly when new data is added.

Displayed values include:
- Total Individuals
- EOT richness
- Snail richness
- All‑taxa richness
- Corixid ratio
- Abundance of EOT
- Overall IBI score (0–50)

Hover over *“How are these calculated?”* for metric equation details.

## Step 5: Export Final Outputs (Results Tab)

Available downloads:
1. **Raw Data CSV**
    - For long-term storage or re-upload into the app.
3. **Table Image (PNG)**
    - A formatted snapshot of the metric table with title and sampling dates.
5. **Data Summary (PDF)**
    - A concise overview of calculated metrics.
7. **Full Report (PDF)**
    - A detailed report including your current results and comparisons to any other saved metric files in your autosave directory.
  
*Both the data summary and the full report rely on the autosave feature being populated with other locations*
                                                                                                                                                                                                                                                                                                                                                             
## Additional Features

### Visualize the Taxonomic Hierarchy
Use **Show/Hide Taxonomic Hierarchy** to display a tree of the taxa you’ve selected.

### Reloading Old Work
Upload any previously exported raw‑data CSV to restore progress.

### Clearing Data
Use the **Clear All Data** button if you need to reset, but ***BEWARE***!! If your data is not saved this cannot be undone!

## 7. Practical Tips

- Whether counts are entered per-dipnet or as a whole will not affect the calculation
- If you split a large sample into multiple jars, combine them during identification
- Keep your field visit form nearby when filling the site title/date fields so your exports are labeled accurately.
- Re‑enable autosave whenever you start a new identification session to avoid losing work.
