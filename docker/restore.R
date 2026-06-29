# Restore the ttmouse package library from renv.lock during the Docker build.
#
# Installs into the image's SITE library (.libPaths()[1]) rather than a project
# library, so the running app uses the normal library path -- no renv project
# activation (.Rprofile / renv/activate.R) is needed at runtime, and the repo
# only has to commit renv.lock.
#
# Repository setup:
#
#  * CRAN / RSPM -> Posit P3M Linux binaries for Ubuntu 24.04 ("noble", the base
#    image's distro), so the ~100 CRAN dependencies install as prebuilt binaries
#    (fast, no compilation = no missing-system-library failures). renv matches
#    each locked package to the repo whose NAME equals its recorded `Repository`
#    field ("CRAN", and "RSPM" for a few); both names point at the P3M binary
#    endpoint -- naming it anything else makes renv fall back to source and
#    compile everything.
#
#  * Bioconductor -> the *versioned* 3.22 release repositories, so the restore
#    stays reproducible after newer Bioconductor releases ship. (edgeR / limma /
#    BiocVersion build from source, which is why the toolchain is in the image.)

p3m_noble <- "https://p3m.dev/cran/__linux__/noble/latest"

options(
  renv.bioconductor.repos = c(
    BioCsoft      = "https://bioconductor.org/packages/3.22/bioc",
    BioCann       = "https://bioconductor.org/packages/3.22/data/annotation",
    BioCexp       = "https://bioconductor.org/packages/3.22/data/experiment",
    BioCworkflows = "https://bioconductor.org/packages/3.22/workflows"
  )
)

# --------------------------------------------------------------------------
# Pre-install locfit and statmod before restore.
#
# These two are CRAN packages (recorded in the lockfile as Source: Repository),
# but they are pulled in as dependencies of the Bioconductor packages edgeR and
# limma. During `renv::restore()` renv resolves them in the Bioconductor repo
# context and searches for them ONLY in the Bioconductor repos -- where, being
# CRAN packages, they don't exist -- so the exact locked versions 404 and the
# edgeR/limma installs fail for a missing dependency. (They also aren't the
# current versions on P3M's rolling snapshot, so renv's archive fallback can't
# rescue them.) Installing them here, explicitly and at the exact locked
# versions, puts them in the library before restore runs; renv then sees them as
# already satisfied, skips them, and edgeR/limma build against them.
#
# remotes::install_version() resolves an exact version from CRAN whether it is
# the current release or has been moved to the Archive. They are small packages
# and compile from source quickly (the gfortran/gcc toolchain is in the image).
install.packages("remotes", repos = p3m_noble)
remotes::install_version("locfit",  version = "1.5-9.12",
                         repos = "https://cran.rstudio.com", upgrade = "never")
remotes::install_version("statmod", version = "1.5.2",
                         repos = "https://cran.rstudio.com", upgrade = "never")

# --------------------------------------------------------------------------
renv::restore(
  lockfile = "renv.lock",
  library  = .libPaths()[1],   # the image's site library
  repos    = c(CRAN = p3m_noble, RSPM = p3m_noble),
  prompt   = FALSE
)
