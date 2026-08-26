# Security Policy

Thank you for taking the time to help keep **MacroIBI** secure.

This document explains what potential security issues may arise, how to report
them, and what you can expect in return.

---

## Scope and threat model

MacroIBI is an R package containing a Shiny app for ecological macroinvertebrate
data analysis. It is intended to be run either locally on a researcher's machine
or on a Shiny server / shinyapps.io for internal use. It does not handle
authentication, financial information, or sensitive personal data, so the overall
security risk is low.

Important:

- MacroIBI does **not** implement user authentication, authorization, or any
  multi-tenant security model.
- It is intended for **trusted environments** — internal networks or personal
  machines.
- Do **not** expose it directly to the public internet without additional
  protections (reverse proxy, authentication, HTTPS).
- If you host the app on shinyapps.io or another platform, the security
  configuration of that platform is your responsibility.

MacroIBI does not intentionally collect or transmit sensitive personal data. Any
data loaded into the app remains under the control of the user and the hosting
environment.

---

## Potential security concerns

Several realistic concerns may still arise depending on how the app is deployed:

### 1. Accidental data exposure
Although MacroIBI typically processes non-sensitive biological and survey data,
users may upload internal reports or unpublished results they do not intend to be
publicly accessible. If the app is deployed on a public server or is
misconfigured, uploaded data could be exposed to unintended parties.

### 2. Multi-user session interference
Shiny applications can share underlying resources across sessions. If MacroIBI is
hosted centrally, simultaneous users may unintentionally interact with shared
autosave files or temporary directories, potentially overwriting or accessing
each other's session data.

### 3. File handling risks
The application reads user-supplied files and writes outputs such as reports and
autosaves. Improper handling of file paths or untrusted filenames may create
opportunities for directory traversal or for overwriting files outside the
expected scope, depending on the hosting environment.

### 4. Dependency-related vulnerabilities
MacroIBI relies on the Shiny framework and several R packages that introduce
HTML, JavaScript, LaTeX, and system-level rendering tools (for example
`webshot2`). Vulnerabilities in these dependencies could indirectly affect
MacroIBI, including risks such as HTML/JS injection or unsafe interpretation of
user-supplied content.

### 5. Misconfigured deployment environment
If MacroIBI is deployed without proper protections — a public-facing server
without authentication, firewall controls, or HTTPS — users may inadvertently
expose internal data or allow unauthorized access.

---

## Supported versions

MacroIBI is a small, research-focused project. Security fixes are generally
applied only to the latest released version.

| Version | Security updates |
| ------- | ---------------- |
| 1.x     | ✔ Active         |
| 0.x     | ✖ Not supported  |

If you are using an older version and discover a security issue, please try to
reproduce it with the latest release before reporting.

---

## Reporting a vulnerability

If you believe you have found a security vulnerability in MacroIBI, **please do
not open a public GitHub issue**.

Instead, email the maintainer directly at <sam.swanson@shakopeedakota.org> with
the subject line `[MacroIBI] Security report`.

Please include, where possible:

- A clear description of the issue
- Steps to reproduce
- Any proof-of-concept code or screenshots
- Your operating system, R version, and MacroIBI version
- Any thoughts on potential impact or severity

### What to expect

1. You will receive an acknowledgement within 5 business days.
2. The issue will be investigated and, if confirmed, a fix prepared.
3. Once a fix is released, a short security note may be added to `NEWS.md`.
4. If you would like to be credited, your name or handle can be included in the
   release notes.

Please do not publicly disclose the details of a vulnerability until a fix has
been released or we have agreed on a timeline.

---

## Dependencies

Many security-relevant issues may originate in dependencies rather than in
MacroIBI itself. If you believe a vulnerability affects a dependency — Shiny,
`webshot2`, DT, and so on — please consider reporting it upstream as well.

---

## Out of scope

The following are generally out of scope for security reports:

- Issues caused solely by a misconfigured hosting environment (unencrypted HTTP,
  open admin ports).
- Problems in forked branches or heavily modified versions of the app.
- Denial of service caused by intentionally extreme or unrealistic input sizes
  beyond typical workflows.

If you are unsure whether something is in scope, send the report anyway.

---

## Responsible use

If you are testing MacroIBI for security issues:

- Only test instances you own or have permission to test.
- Avoid actions that might disrupt work for other users.
- Do not attempt to access data without authorization.

Thank you again for your interest in MacroIBI.
