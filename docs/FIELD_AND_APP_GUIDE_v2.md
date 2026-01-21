
# Wetland Macroinvertebrate Sampling & MacroIBI App Guide  
*A complete workflow for field sampling, lab sorting, identification, and IBI calculation.*

---

## Table of Contents

1. [**Overview**](https://github.com/aomop/MacroIBI/blob/main/inst/docs/FIELD_AND_APP_GUIDE_v2.md#1-overview)
2. [**Equipment Checklist**](https://github.com/aomop/MacroIBI/blob/main/inst/docs/FIELD_AND_APP_GUIDE_v2.md#2-equipment-checklist)
3. [**Collecting Macroinvertebrate Samples**](https://github.com/aomop/MacroIBI/blob/main/inst/docs/FIELD_AND_APP_GUIDE_v2.md#3-collecting-macroinvertebrate-samples)
4. [**Sorting Samples in the Field**](https://github.com/aomop/MacroIBI/blob/main/inst/docs/FIELD_AND_APP_GUIDE_v2.md#4-sorting-samples-in-the-field)
5. [**Lab Identification & Enumeration**](https://github.com/aomop/MacroIBI/blob/main/inst/docs/FIELD_AND_APP_GUIDE_v2.md#5-lab-identification--enumeration)
6. [**Installing the MacroIBI R Package**](https://github.com/aomop/MacroIBI/blob/main/inst/docs/FIELD_AND_APP_GUIDE_v2.md#6-installing-the-macroibi-r-package)
7. [**Using the MacroIBI App**](https://github.com/aomop/MacroIBI/blob/main/inst/docs/FIELD_AND_APP_GUIDE_v2.md#7-using-the-macroibi-app)
8. [**Additional Features**](https://github.com/aomop/MacroIBI/blob/main/inst/docs/FIELD_AND_APP_GUIDE_v2.md#8-additional-features)

---

## 1. Overview

This guide explains how to collect, sort, identify, and score wetland macroinvertebrates using the MPCA protocol and the MacroIBI R/Shiny application. No prior R experience is required, this document walks you through installation and use of the app step-by-step.

---

## 2. Equipment Checklist

### Required Field Equipment

- Two D-frame dip nets (500 µm mesh)  
- Hardware cloth + frame  
- Plastic bin for catching material  
- 200 µm sieve  
- Squirt bottles (1 water, 1 alcohol)  
- Forceps (one per person)  
- Wetland Invertebrate Visit Form  
- GPS unit  
- Pencils, clipboards  
- 100% reagent alcohol  
- Chest waders  
- Plastic jars for storage  
- Permanent marker  

### Optional Field Equipment

- Maps  for navigation
- Scissors for labels  
- Camera  to capture site conditions
- Cooler or crate for transporting samples  

### Required Lab Equipment

- Stereoscopes (2x-4x magnification)  
- Petri dishes  
- Identification guides  
- Squirt bottles (water or alcohol)  
- Dissection tools  
- Computer with R and the **macroibi** package  

---

## 3. Collecting Macroinvertebrate Samples

### When to Sample

- Sample **June–early July** while larvae are mature but wetlands retain water.  
- Sampling too late risks dried wetlands or transient species.  
- If multiple samples are planned, keep all events within this timeframe.

### Where to Sample

Prioritize habitat zones in this order:

1. Emergent vegetation (richest fauna)  
2. Floating-leaf vegetation  
3. Submerged vegetation  
4. Shallow open water  

Record the zone(s) used.

### Sampling Procedure

1. **Identify the sampling area.**  
   Select the highest-priority habitat available and work within a 10-15 m radius.

2. **Collect two efforts per sample.**  
   For each effort, perform **3-5 strong sweeps** through vegetation and water.  
   *Avoid scraping mud; restart the effort if so.*

3. **Sort for 10 minutes.**  
   Dump both efforts onto hardware cloth above the bin.  
   Start a 10-minute timer and begin sorting (see next section).

4. **Repeat for the second sample.**  
   Stay in the same zone but choose new micro-locations.  
   Samples may be combined or kept separate for processing; both ultimately contribute to one IBI evaluation.

> **Tip:** With larger crews, some people can begin the second sample while others finish the 10-minute sort; this saves significant time.

---

## 4. Sorting Samples in the Field

### During the 10-Minute Sorting Window

1. Rinse vegetation so organisms fall into the pans.  
2. Pick organisms with forceps into the water-filled pans.  
3. After 10 minutes, return vegetation to the wetland; all organisms in the bin become the sample.

### After Vegetation Removal

1. Pour pans through the 200 µm sieve; flush snails/leeches into the sieve.  
2. Back-flush the sieve **with alcohol only** into sample jars.  
   - Aim for ~80% final concentration.  
   - Split jars if >1/3 full.  
3. Label jars (site ID, date, sample #, jar #, crew initials).  
4. Store in hazardous-materials room; check periodically for evaporation.

---

## 5. Lab Identification & Enumeration

1. **Software Setup**  
   Ensure R + MacroIBI are installed (see next section).

2. **Prepare Workspace**  
   Set up stereoscopes, petri dishes, tools, guides, and your sample.

3. **Identify & Count Everything**  
   - Every individual in the sample must be identified and counted.  
   - Family-level ID is required; genus ideal; species when possible.  
   - One person should handle data entry; others relay counts.  
   - Pre-sort into visually similar piles to speed identification.  
   - Samples may be rinsed with water to reduce irritation during the process, BUT;
   - If samples must remain preserved after ID, do not use any water.
   

4. **Enter Data Periodically**  
   Relay taxa and counts to the data handler for entry into the app.  
   - Combined samples may all be entered into “Dipnet 1.”  
   - See Section 7 for full app workflow.

---

## 6. Installing the MacroIBI App

This section walks you through installing everything you need to run MacroIBI. **No prior experience with R or programming is required.** Follow each step in order.

### What You're Installing

You need two free programs:

| Program | What It Does |
|---------|--------------|
| **R** | The engine that powers the app. You won't interact with it directly. |
| **RStudio** | The program you'll actually use. It provides a user-friendly interface for running R commands. |

Think of R as the engine and RStudio as the dashboard of a car — you need both, but you only interact with the dashboard.

---

### Step 1: Install R

1. Go to **<https://cran.r-project.org>**
2. Click the download link for your operating system:
   - **Windows:** Click "Download R for Windows" → "base" → "Download R-4.x.x for Windows"
   - **Mac:** Click "Download R for macOS" → Choose the version for your Mac (Apple Silicon or Intel)
3. Open the downloaded file and run the installer
4. **Accept all default options** — just keep clicking "Next" or "Continue"

**✓ Success:** The installer finishes without errors. You don't need to open R directly.

---

### Step 2: Install RStudio

1. Go to **<https://posit.co/download/rstudio-desktop/>**
2. Scroll down to "All Installers" and click the download for your operating system
3. Open the downloaded file and run the installer
4. **Accept all default options**

**✓ Success:** RStudio appears in your applications/programs.

---

### Step 3: Install Rtools (Windows Only — Required)

> **Mac users:** Skip to Step 4.

**This step is required on Windows.** Rtools allows R to install packages that need compilation.

1. Go to **<https://cran.r-project.org/bin/windows/Rtools/>**
2. Download "Rtools44" (or the version matching your R version)
3. Run the installer
4. **Accept all default options**

**✓ Success:** The installer completes. You won't see Rtools anywhere — it works behind the scenes.

---

### Step 4: Open RStudio

- **Windows:** Click the Start menu and search for "RStudio", then click to open
- **Mac:** Open Finder → Applications → RStudio

When RStudio opens, you'll see a window divided into panels. Look for the **Console** — it's the panel with a `>` symbol where you can type commands. This is usually in the bottom-left area.

```
┌─────────────────────────────────────────────────────────────┐
│  [Files/Plots/etc.]           │  [Environment/History]      │
├───────────────────────────────┴─────────────────────────────┤
│  Console                                                    │
│  >  ← YOU TYPE COMMANDS HERE                                │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

---

### Step 5: Install the MacroIBI Package

Now you'll type commands in the Console. Here's how:

1. **Click in the Console** (next to the `>` symbol)
2. **Type or copy-paste** the command shown below
3. **Press Enter** to run it
4. **Wait** for the `>` to reappear before typing the next command

#### First command — Install the "remotes" helper package:

```r
install.packages("remotes")
```

You'll see text scrolling as R downloads and installs the package. **This is normal.**

**✓ Success:** You see a message containing `package 'remotes' successfully unpacked` and the `>` prompt returns.

#### Second command — Install MacroIBI:

```r
remotes::install_github("aomop/MacroIBI")
```

This downloads MacroIBI from the internet. It may take 1-2 minutes. You'll see lots of text — **this is normal.**

**✓ Success:** The `>` prompt returns without red error messages.

> **If you see red text mentioning "Rtools":** Go back to Step 3 and install Rtools, then try this command again.

---

### Step 6: Launch the App

Type these two commands (press Enter after each):

```r
library(macroibi)
run_macroibi()
```

**✓ Success:** A web browser window opens showing the "Wetland IBI Dashboard". You're ready to use the app!

---

### Quick Verification (Optional)

To confirm everything works, try the demo mode:

```r
library(macroibi)
run_macroibi(demo_mode = TRUE)
```

Click "OK" on the demo message, then click **Load Autosave** on the left sidebar. If you see demo files listed, everything is working correctly.

---

### Closing the App

When you're done:
- **Close the browser tab**, OR
- **In RStudio**, click the red **Stop** button (🛑) in the Console area, OR
- **Press the Escape key** while clicked in the Console

---

### Troubleshooting

#### "Error: package 'remotes' is not available"
Run `install.packages("remotes")` first, then try again.

#### Red text mentioning "Rtools" or "compilation" (Windows)
Install Rtools from Step 3, restart RStudio, and try again.

#### "Cannot open URL" or network errors
Check your internet connection. If you're on a work network, try from home or contact IT about firewall restrictions.

#### PDF/image export not working
The PDF export feature requires additional software. In the Console, run:
```r
webshot2::install_phantomjs()
```

#### Still stuck?
Contact Sam Swanson at sam.swanson@shakopeedakota.org — happy to help!

---

## 7. Using the MacroIBI App

Start the dashboard:

```r
library(macroibi)
run_macroibi()
```

Enter the **sample title** and **date**, then click **Let’s go!**  
You can also continue without metadata if you plan to upload data or reload an autosave.

---

### Step 1 — Enable Autosave

Before entering data, find the **Autosave Settings** on the left side of the Data tab and **Enable Auto-Save**

Autosave will:

- periodically save your progress  
- store autosaves per user  
- allow recovery after crashes  
- provide history for reports and comparisons  

You can reload autosaves using **Load Autosave**.

> **Important:** Data Summary and Full Report features rely on having autosaves available.

---

### Step 2 — Enter Taxa and Counts

Still on the data tab, use **Select Taxon** to search and add taxa.  
The app assigns each taxon to the correct group automatically.

Each row displays:

- Taxon name
- Dipnet 1 Count 
- Dipnet 2 Count
- Sum Count (auto-calculated)

Each group footer updates live:

- Total Taxa
- Percent of Total Sample 
- Total Individuals

---

### Step 3 — Review Metrics

Switch to **Results** to see real-time IBI calculations.

Displayed metrics include:

- Total Individuals  
- EOT richness  
- Snail richness  
- All-taxa richness  
- Corixid ratio  
- Abundance of EOT
- **Overall IBI score (0–50)**

Hover over *“How are these calculated?”* for metric formulas.

#### Metric definitions and scoring (offline reference)

- **Total Individuals** — Sum of all individuals entered across both dipnets and all taxonomic groups.
- **EOT richness** — Number of unique taxa in Dragonflies, Mayflies, Damselflies, and Caddisflies (EOT Orders).
- **Snail richness** — Number of unique taxa in Gastropoda.
- **All-taxa richness** — Sum of unique taxa across all groups entered in the app.
- **Corixid ratio** — \(\text{Corixidae individuals} / (\text{all true bugs} + \text{beetles})\); higher ratios indicate potential nutrient loading, so scoring decreases as the ratio rises.
- **Abundance of EOT** — \(\text{Total EOT individuals} / \text{Total individuals}\).
- **IBI Score (0–50)** — Sum of five adjusted metric scores (each 0–10). Scores are scaled between anchor percentiles observed in reference data:
  - *Decrease with disturbance:* EOT richness (1–12 taxa), snail richness (1–10 taxa), all-taxa richness (10–40 taxa), abundance of EOT (0–0.16). Values below the minimum score 0; values at/above the upper anchor score 10.
  - *Increase with disturbance:* Corixid ratio uses anchors 0 (best) to 1.0 (worst) with a 5th–95th percentile band of 0–0.82 guiding the scale; values above the maximum anchor score 0.
  - The final IBI score is the sum of the capped component scores (maximum 50).

---

### Step 4 — Export Final Outputs

Available downloads:

1. **Raw Data CSV**
   For archiving or re-uploading into the app.
   Use only CSVs exported from MacroIBI without external edits; uploading unrelated or manually prepared files can break metric calculations.

2. **Results CSV**
   A basic CSV file with the final calculated scores

3. **Table Image (PNG)**  
   A formatted metric table with titles and dates.

4. **Data Summary (PDF)**  
   A concise one-page snapshot of metrics.

5. **Full Report (PDF)**  
   Comprehensive report including:  
   - current metrics  
   - comparisons to other autosaved sessions  

> *Reports require autosave history to generate comparisons.*

---

## 8. Additional Features

### Taxonomic Hierarchy Viewer

Use **Show/Hide Taxonomic Hierarchy** to view a taxonomic tree of selected taxa; this helps verify correct grouping.

### Reloading Old Work

Upload any previously exported **Raw Data CSV** to restore tables exactly as saved.

### Clearing Data

**Clear All Data** resets everything.  
***This cannot be undone if the data isn’t saved.***

---
