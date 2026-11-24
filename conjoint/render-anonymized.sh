#!/usr/bin/env bash
# Builds the anonymized preregistration from the main .qmd and renders it to HTML and PDF.
# Re-run after editing osf-preregistration_JUSMER_conjoint2026.qmd so both versions stay in sync.
set -euo pipefail
cd "$(dirname "$0")"
src=osf-preregistration_JUSMER_conjoint2026.qmd
out=osf-preregistration_conjoint2026_anonymized.qmd
awk '
  /^author:/ { print "author: \"Anonymized for peer review\""; skip=1; next }
  skip && /^  - / { next }
  { skip=0; print }
' "$src" > "$out"
quarto render "$out"
