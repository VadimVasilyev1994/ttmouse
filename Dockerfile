# ttmouse deployment image.
#
# Base R pinned to the renv.lock R version (4.5.1); Bioconductor 3.22. The same
# image serves Hugging Face Spaces (port 7860) and Cloud Run ($PORT) unchanged,
# because run_app() resolves the port from $PORT and falls back to 7860.
FROM rocker/r-ver:4.5.1

# --- System libraries -------------------------------------------------------
# Build deps for the few source packages (edgeR / limma from Bioconductor) and
# runtime shared libs for the CRAN binaries pulled from P3M. This is a sensible
# default for the dependency tree; the authoritative list for this exact distro
# is `renv::sysreqs("ubuntu:24.04", report = TRUE, collapse = TRUE)` -- if the
# build ever stops on a missing -dev library, regenerate from that and update.
RUN apt-get update && apt-get install -y --no-install-recommends \
      git cmake \
      libcurl4-openssl-dev libssl-dev libxml2-dev zlib1g-dev libuv1-dev \
      libfontconfig1-dev libfreetype6-dev libpng-dev libtiff-dev libjpeg-dev \
      libharfbuzz-dev libfribidi-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# --- Restore the locked R library -------------------------------------------
# Copy only the lockfile + restore helper first, so this expensive layer is
# cached and re-runs only when the lock changes, not on app-code edits.
# Copy packages into the library (no cache symlinks), so the site library is
# self-contained and readable after the USER switch below.
ENV RENV_CONFIG_CACHE_SYMLINKS=FALSE
RUN R -s -e "install.packages('renv')"
COPY renv.lock renv.lock
COPY docker/restore.R docker/restore.R
RUN Rscript docker/restore.R

# --- Install the ttmouse package + bake the assets --------------------------
COPY . .
RUN R CMD INSTALL .

# Assets live at /app/assets (gene_lengths.rds + one subdir per organ);
# discover_assets() reads this env var. Baked into the image (~17 MB), so there
# is no runtime download.
ENV TTMOUSE_DATA_DIR=/app/assets

# --- Run as non-root (Hugging Face Spaces runs the container as UID 1000) ----
RUN useradd --create-home --uid 1000 appuser
USER appuser

# run_app() binds 0.0.0.0 on $PORT (else 7860), with a 50 MB upload cap.
EXPOSE 7860
CMD ["R", "-s", "-e", "ttmouse::run_app()"]
