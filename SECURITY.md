# Security Policy

## Supported versions

Only the latest release of macroibi receives security fixes.

## Scope

macroibi is a local R/Shiny application intended to be run by individual users
on their own machines. It does not expose a network service, does not
authenticate users, and does not store sensitive data beyond locally saved
autosave files in the user's data directory.

If you are hosting the application via shinyapps.io or another deployment
platform, you are responsible for the security configuration of that platform.

## Reporting a vulnerability

If you discover a security issue, please **do not open a public GitHub issue**.

Instead, email the maintainer directly at <sam.swanson@shakopeedakota.org>
with:

- A description of the vulnerability
- Steps to reproduce
- Any relevant logs or screenshots

You will receive an acknowledgement within 5 business days. We will work with
you to understand the issue and coordinate a fix before any public disclosure.
