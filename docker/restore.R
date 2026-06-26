# Restore the ttmouse package library from renv.lock during the Docker build.
#
# Installs into the image's SITE library (.libPaths()[1]) rather than a project
# library, so the running app uses the normal library path -- no renv project
# activation (.Rprofile / renv/activate.R) is needed at runtime, and the repo
# only has to commit renv.lock.
#
# Two repository overrides matter:
#
#  * CRAN -> Posit P3M Linux binaries for Ubuntu 24.04 ("noble", the base image's
#    distro). The ~100 CRAN dependencies install as prebuilt binaries (fast)
#    instead of compiling. cran.rstudio.com is listed second as a source
#    fallback for anything P3M doesn't carry.
#
#  * Bioconductor -> the *versioned* 3.22 release repositories. The lockfile
#    recorded edgeR / limma / BiocVersion from r-universe, which only serves the
#    current Bioconductor release; pinning to the versioned 3.22 repos keeps the
#    restore reproducible after newer Bioconductor releases ship.

options(
  renv.bioconductor.repos = c(
    BioCsoft      = "https://bioconductor.org/packages/3.22/bioc",
    BioCann       = "https://bioconductor.org/packages/3.22/data/annotation",
    BioCexp       = "https://bioconductor.org/packages/3.22/data/experiment",
    BioCworkflows = "https://bioconductor.org/packages/3.22/workflows"
  )
)

renv::restore(
  lockfile = "renv.lock",
  library  = .libPaths()[1],   # the image's site library
  repos    = c(
    P3M  = "https://packagemanager.posit.co/cran/__linux__/noble/latest",
    CRAN = "https://cran.rstudio.com"
  ),
  prompt   = FALSE
)
