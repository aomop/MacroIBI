# macroibi 1.0.1
### 12/09/2025
- Fix taxonomic tree layout crash for some taxa selections.
- Harden tree layout logic against incomplete taxonomic hierarchies.

---

# macroibi 1.1.1
### 02/10/2026
- **New:** Taxonomy dataset now includes common names for taxa where it is available. Users may search for taxa by the latin name, common name, or taxon level. Section tables now display common names.
- **New:** Raw data outputs now include taxon level information. 

---

# macroibi 1.1.0
### 02/03/2026
- **New:** Added `generate_reports()` exported function for batch report generation from autosave files without running the Shiny app. Supports output types: `"csv"` (metric scores), `"csv_data"` (raw taxa), `"pdf_report"` (full report), `"pdf_summary"` (data summary), or `"all"`.
- **Improvement:** Replaced free-text date input with a calendar date picker (`dateInput`), providing proper date validation while maintaining backwards compatibility with existing autosave files and uploaded CSV data containing string dates.
- **Fix:** Download filenames now use ISO date format (YYYY-MM-DD) instead of raw user input, preventing invalid filenames when dates contain slashes (e.g., `07/10/2024`). Affected downloads: PNG table image, PDF full report, PDF data summary, and CSV data export.
- **Docs:** Updated user documentation to be much more helpful.

---

# macroibi 1.0.0
### 12/08/2025
- **New:** Added full documentation and user-friendly installation instructions to support new users.
- **New:** Added practical guides and explanatory material for the field sampling protocol used to collect MacroIBI data.
- **New:** Regenerated the internal taxonomy dataset with `in_region` flags to identify taxa unlikely to occur in North America, improving data entry and potential misidentification checks.
- **New (UI):** Added visual flags and a show/hide toggle for out-of-region taxa within the species selection interface.
- **Stability:** This is the first **stable**, user-oriented release of the MacroIBI application.

---

# macroibi 0.3.0
### 12/05/2025
- **Fix:** Updated LaTeX dependencies in report templates, restoring reliable PDF report generation across environments.
- **Refactor:** Removed dependencies on `treeio`, `ggtree`, `phylo`, and `ape` packages and reimplemented tree-generation logic internally, reducing heavy dependencies and improving render stability.
- **Refactor:** Reworked demo autosave behavior to prevent writing demo-state data to the user's real autosave directory.
- **Improvement:** General cleanup and stability improvements across modules.

---

# macroibi 0.2.0
### 12/03/2025
- **New:** Added `demo_mode` argument to `run_macroibi()` allowing a safe, limited-feature demonstration version of the application.
- **New:** Included shippable demo autosave files for training and demonstration.
- **Improvement:** Disabled or restricted certain features (uploads, autosave, downloads) in demo mode to prevent user confusion.
- **Fix:** Removed `pkgload` dependency to eliminate recurring CLI installation/locking issues during deployment or installation.

---

# macroibi 0.1.0
### 11/26/2025
- **Initial package version.**
- Converted the original standalone Shiny app into an installable R package.
- Resolved early dependency issues that prevented the app from starting reliably.
- Changed autosave behavior so that files are saved to a **user-local directory** instead of inside the installed package structure.
