# macroibi 1.0.1
### 12/09/2025
- Fix taxonomic tree layout crash for some taxa selections.
- Harden tree layout logic against incomplete taxonomic hierarchies.

---

# macroibi v1.0.0
### 12/08/2025
- **New:** Added full documentation and user-friendly installation instructions to support new users.
- **New:** Added practical guides and explanatory material for the field sampling protocol used to collect MacroIBI data.
- **New:** Regenerated the internal taxonomy dataset with `in_region` flags to identify taxa unlikely to occur in North America, improving data entry and potential misidentification checks.
- **New (UI):** Added visual flags and a show/hide toggle for out-of-region taxa within the species selection interface.
- **Stability:** This is the first **stable**, user-oriented release of the MacroIBI application.

---

# macroibi v0.3.0
### 12/05/2025
- **Fix:** Updated LaTeX dependencies in report templates, restoring reliable PDF report generation across environments.
- **Refactor:** Removed dependencies on `treeio`, `ggtree`, `phylo`, and `ape` packages and reimplemented tree-generation logic internally, reducing heavy dependencies and improving render stability.
- **Refactor:** Reworked demo autosave behavior to prevent writing demo-state data to the user's real autosave directory.
- **Improvement:** General cleanup and stability improvements across modules.

---

# macroibi v0.2.0
### 12/03/2025
- **New:** Added `demo_mode` argument to `run_macroibi()` allowing a safe, limited-feature demonstration version of the application.
- **New:** Included shippable demo autosave files for training and demonstration.
- **Improvement:** Disabled or restricted certain features (uploads, autosave, downloads) in demo mode to prevent user confusion.
- **Fix:** Removed `pkgload` dependency to eliminate recurring CLI installation/locking issues during deployment or installation.

---

# macroibi v0.1.0
### 11/26/2025
- **Initial package version.**
- Converted the original standalone Shiny app into an installable R package.
- Resolved early dependency issues that prevented the app from starting reliably.
- Changed autosave behavior so that files are saved to a **user-local directory** instead of inside the installed package structure.
