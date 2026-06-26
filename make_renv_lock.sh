#!/usr/bin/env bash
#
# make_renv_lock.sh -- generate renv.lock for ttmouse INSIDE the deployment base
# image (R 4.5 + Bioconductor 3.22).
#
# Why in the image (not on a dev machine): the lock is then guaranteed to
# restore in the container, and this run doubles as proof that edgeR /
# DGEobj.utils actually build on the chosen base (the open item from the
# base-image decision). renv.lock is written back into the repo via the bind
# mount.
#
# PREREQUISITES
#   * Docker on the host (nothing is installed on the host itself).
#   * Run from the ttmouse repo root (the directory holding DESCRIPTION).
#   * bslib (and graphics) already added to DESCRIPTION Imports, so the snapshot
#     records bslib regardless of how renv scans inst/.
#
# USAGE
#   bash make_renv_lock.sh
#
set -euo pipefail

# --- Pins (these also fix the F2 base image tag) ----------------------------
R_IMAGE="rocker/r-ver:4.5.3"   # latest R 4.5 patch; 4.5.2 works equally if preferred
BIOC_VERSION="3.22"            # Bioconductor release paired with R 4.5
TT_REF="VadimVasilyev1994/TimeTeller-v2@f3cbd4492ac2f4afbfaddc16d717d34c96353527"

# Must be run from the package root.
if [[ ! -f DESCRIPTION ]]; then
  echo "ERROR: run this from the ttmouse repo root (no DESCRIPTION found here)." >&2
  exit 1
fi

# --- Generate the lock in a throwaway container -----------------------------
# Repo is bind-mounted at /work; the resulting renv.lock lands back in the repo.
# BIOC_VERSION / TT_REF are passed as env vars and read in R via Sys.getenv(),
# so the quoted here-docs below stay fully literal (no shell expansion to fight).
docker run --rm -i \
  -v "$PWD":/work -w /work \
  -e BIOC_VERSION="$BIOC_VERSION" \
  -e TT_REF="$TT_REF" \
  "$R_IMAGE" bash -s <<'CONTAINER'
set -euo pipefail
export DEBIAN_FRONTEND=noninteractive

# 1) System libraries for the dependency tree (compile + ggplot2 graphics stack).
#    P3M serves binaries for most CRAN packages, but Bioconductor/source builds
#    still need these. If a build later reports a missing -dev library, add it
#    here and re-run.
apt-get update -qq
apt-get install -y --no-install-recommends \
  git \
  libcurl4-openssl-dev libssl-dev libxml2-dev zlib1g-dev \
  libfontconfig1-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
  libharfbuzz-dev libfribidi-dev
rm -rf /var/lib/apt/lists/*

# 2) renv + BiocManager, then a controlled install + snapshot.
cat > /tmp/snapshot.R <<'RSCRIPT'
# renv and BiocManager from the rocker-configured P3M CRAN mirror.
install.packages(c("renv", "BiocManager"))

# Scaffold renv with Bioconductor active and pinned, but do NOT auto-install --
# we install dependencies explicitly below for full control over sources.
renv::init(bioconductor = Sys.getenv("BIOC_VERSION"),
           bare = TRUE, restart = FALSE)

# edgeR is a Bioconductor package: install it explicitly so its source is
# unambiguous and any build problem surfaces clearly and early.
renv::install("bioc::edgeR")

# TimeTeller from the pinned commit, so renv records that exact RemoteSha.
renv::install(Sys.getenv("TT_REF"))

# Everything else the package declares (DESCRIPTION Imports: DGEobj.utils,
# openxlsx, plotly, shiny, ggplot2, bslib, ...) resolved from the active
# CRAN/Bioc repos. Already-installed packages are skipped.
renv::install()

# Write renv.lock capturing the full resolved graph.
renv::snapshot(prompt = FALSE)
RSCRIPT

Rscript /tmp/snapshot.R
CONTAINER

# --- Host-side verification readout -----------------------------------------
echo
echo "=== renv.lock summary ==="
if [[ ! -f renv.lock ]]; then
  echo "ERROR: renv.lock was not produced -- check the container output above." >&2
  exit 1
fi
echo -n "R version (lock):  "; grep -m1 '"Version"' renv.lock | tr -d ' ,"' | sed 's/Version://'
echo -n "Bioconductor:      "; grep -A1 '"Bioconductor"' renv.lock | grep '"Version"' | tr -d ' ,"' | sed 's/Version://' || echo "(none)"
echo "TimeTeller record:"
grep -A8 '"TimeTeller"' renv.lock || echo "  MISSING"
echo "Key packages present:"
for p in edgeR DGEobj.utils openxlsx plotly shiny ggplot2 bslib; do
  if grep -q "\"$p\"" renv.lock; then echo "  ok:      $p"; else echo "  MISSING: $p"; fi
done
echo
echo "Done. renv.lock written to: $PWD/renv.lock"
echo "Commit renv.lock. The renv/ infra + .Rprofile that init created are optional"
echo "for a restore-only Docker flow -- keep them if you want local dev on renv too."
