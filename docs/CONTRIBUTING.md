# Contributing to MacroIBI

Thank you for your interest in contributing to MacroIBI!  
This project is primarily developed and maintained internally, but community contributions are welcome where appropriate.

## Before Contributing
Please read the [Code of Conduct](https://github.com/aomop/MacroIBI/blob/main/CODE_OF_CONDUCT.md).

## Ways to Contribute

### **1. Reporting Issues**
If you encounter a problem with the app or package:

- Check existing issues to avoid duplicates.
- Open a new issue and include:
  - A clear description of the problem
  - Steps to reproduce
  - Expected vs. actual behavior
  - Relevant error messages, logs, or screenshots
  - Your R version, operating system, and package version

---

### **2. Requesting Features**
Feature requests are welcome, especially if they improve usability or clarity.

When creating a feature request:

- Explain *why* the feature would be useful  
- Provide example workflows or UI suggestions if applicable  
- Indicate whether you are able to help implement it  

---

### **3. Submitting Code Changes (Pull Requests)**

If you would like to submit a PR:

#### **Before You Start**
- Ensure an issue exists discussing the proposed change  
- Fork the repository and create a new branch:
  ```bash
  git checkout -b feature/my-feature
  ```

#### **Coding Standards**
- Follow the existing style in the codebase.
- Use clear function names, descriptive comments, and consistent formatting.
- Keep changes focused.
- All code must clear `devtools::check()` AND `devtools::test()` before being considered for implementation.

#### **For R Code**
- Use `devtools::load_all()` for local testing.
- Run:
  ```r
  devtools::check()
  ```

#### **For Shiny Code**
- Test the app locally:
  ```r
  macroibi::run_macroibi()
  ```

#### **Pull Request Checklist**
- References an issue (e.g., Fixes #123)
- No failing `R CMD check`
- Minimal test coverage provided where possible

---

## Project Structure Overview

```
R/               # Core R functions, modules, utilities
inst/app/        # App resources (UI, templates, www/, etc.)
inst/extdata/    # Example/reference files
tools/           # Deployment scripts
data-raw/        # Internal data preparation
```

---

## Tests

The project uses **testthat**.  
Please include basic tests for any new functions.

---

## Documentation

Use roxygen2 for documentation.  
Rebuild docs with:

```r
devtools::document()
```

## Note: Working with `selected_genera`
The MacroIBI app stores all user-entered taxa and dipnet counts inside a nested reactive structure called `selected_genera`. Because this object is central to the IBI workflow, and because it is a reactive list of reactive lists, it's important to understand how to work with it safely.

selected_genera is a reactiveValues container.
Each entry corresponds to a taxon section in the UI (e.g., Beetles, Dragonflies, Gastropods, etc.).

Each section entry is itself either:
- a reactive() returning a reactiveValues object, or
- a reactiveValues object directly (depending on how that section is initialized).

Inside each section object, the primary field of interest is `$data`; a list of row-lists representing user input for that taxonomic group.

You can think of the structure like this:

```scss
selected_genera  # reactiveValues

|- "section_1" -> reactive() -> reactiveValues:
|      $data = list(
|        list(taxon="...", dipnet1=..., dipnet2=..., tsn=...,parentTsn=...),
|        list(...),
|        ...
|      )
|- "section_2" -> reactive() -> reactiveValues:
|      $data = list(...)
|- etc. (one section per taxon group)
```
Since each entry may be a function or a reactiveValues object, you should always check the type and handle both cases:

```r
section_obj <- selected_genera[[section_id]]

# If it's a reactive() / reactiveVal(), call it
if (is.function(section_obj)) {
  section_obj <- section_obj()
}

# Now section_obj is the underlying reactiveValues
rows <- section_obj$data
```

---

## Code of Conduct

- Be respectful and constructive  
- Collaborate openly

Read the full Code of Conduct [here](https://github.com/aomop/MacroIBI/blob/main/CODE_OF_CONDUCT.md).

---

## Getting Help

Open an issue on GitHub if you have questions or need clarification.

Thanks again for your interest. Even small improvements are appreciated!
