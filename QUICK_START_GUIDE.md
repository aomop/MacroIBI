
# Wetland MacroIBI Quick Guide

*A two-page overview for field crews and lab staff.*

---

## 1. Field Sampling (What to Do Outside)

**When:**  
- Sample **June–early July** while wetlands hold water and larvae are identifiable.

**Where (in order of preference):**

1. Emergent vegetation  
2. Floating-leaf vegetation  
3. Submerged vegetation  
4. Shallow open water  

Record what you used.

**How:**

1. Choose a 10–15 m radius in the target habitat.
2. For each sample, collect **two efforts**:
   - Each effort = 3–5 strong sweeps with the dip net.
   - Avoid scraping mud—restart an effort if needed.
3. Dump both efforts onto hardware cloth over a bin.
4. Sort for **10 minutes**:
   - Rinse vegetation so animals fall into pans.
   - Pick organisms into pans; don’t focus on one taxon.
5. Pour pans through a **200 µm sieve**, flush snails/leeches.
6. Back-flush the sieve with alcohol into sample jars (≈80%).
7. Label jars (site, date, sample #, jar #, initials) and store safely.

> **Tip:** With multiple crew members, some can start the next sample while others finish sorting.

---

## 2. Lab Work (What to Do Inside)

1. Set up: stereoscope, dishes, tools, ID guides, and sample.  
2. Identify **every organism**:
   - Family required; genus ideal; species if possible.
3. One person handles data entry; others relay counts.
4. Pre-sort organisms into similar-looking piles to speed ID.

If the sample stays preserved, **use alcohol instead of water** during sorting.

---

## 3. Installing the MacroIBI App

1. Install **R** and (recommended) **RStudio**.  
2. Download or clone the MacroIBI repository.  
3. In R or RStudio, run:

```r
setwd("PATH/TO/MacroIBI-main/")
install.packages("devtools")
devtools::install_local()
```

---

## 4. Running the App

Start the app:

```r
library(macroibi)
run_macroibi()
```

Enter the sample **title** and **date**, then click **Let’s go!**

---

## 5. Recommended Workflow in the App

### Step 1 — Turn On Autosave

- On the left of the **Data** tab, open **Autosave Settings**.  
- Enable autosave so long sessions are protected.  
- Use **Load Autosave** to resume a previous session.

> **Important:** Data Summary and Full Report features rely on having autosaves for multiple samples.

---

### Step 2 — Enter Taxa and Counts (Data Tab)

- Use **Select Taxon** to add taxa; the app puts each taxon in the right group.  
- For each taxon, fill in:
  - **Dipnet 1 Count**
  - **Dipnet 2 Count**
- The app calculates **Sum Count** automatically.

Group footers show:

- **Total Taxa**  
- **Percent of Total Sample**  
- **Total Individuals**

You can:

- Upload a saved CSV to restore tables.  
- Use **Clear All Data** to reset (cannot be undone).

---

### Step 3 — Check Results (Results Tab)

The **Results** tab shows:

- Total Individuals  
- EOT richness  
- Snail richness  
- All-taxa richness  
- Corixid ratio  
- Abundance of EOT  
- **Overall IBI score (0–50)**  

Metrics update instantly as you change counts.  
Hover over *“How are these calculated?”* for formula details.

---

### Step 4 — Save Your Outputs

From the **Results** tab, download:

1. **Raw Data CSV** – For archiving or re-uploading into the app.  
2. **Table Image (PNG)** – A snapshot of the metric table.  
3. **Data Summary (PDF)** – A concise overview of metrics.  
4. **Full Report (PDF)** – Detailed report plus comparisons across autosaved samples.

---

## 6. Helpful Extras

- **Show/Hide Taxonomic Hierarchy**: visualize how your taxa are related.  
- Re-enable **autosave** each time you start a new ID session.  
- Whether you split counts between dipnets or put everything in Dipnet 1 does **not** change the IBI score.  
