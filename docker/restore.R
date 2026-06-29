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
#    distro), so the ~100 CRAN dependencies install as prebuilt binaries (fast,
#    and no compilation = no missing-system-library failures). renv matches each
#    locked package to the repo whose NAME equals its recorded `Repository`
#    field, which is "CRAN" (and "RSPM" for a few). Both names must therefore
#    point at the P3M binary endpoint -- naming it anything else makes renv fall
#    back to a source repo and compile everything.
#
#  * Bioconductor -> the *versioned* 3.22 release repositories. The lockfile
#    recorded edgeR / limma / BiocVersion from r-universe, which only serves the
#    current Bioconductor release; pinning to the versioned 3.22 repos keeps the
#    restore reproducible after newer Bioconductor releases ship. (These few
#    packages build from source, which is why the toolchain is in the image.)

options(
  renv.bioconductor.repos = c(
    BioCsoft      = "https://bioconductor.org/packages/3.22/bioc",
    BioCann       = "https://bioconductor.org/packages/3.22/data/annotation",
    BioCexp       = "https://bioconductor.org/packages/3.22/data/experiment",
    BioCworkflows = "https://bioconductor.org/packages/3.22/workflows"
  )
)

p3m_noble <- "https://p3m.dev/cran/__linux__/noble/latest"
renv::restore(
  lockfile = "renv.lock",
  library  = .libPaths()[1],   # the image's site library
  repos    = c(CRAN = p3m_noble, RSPM = p3m_noble),
  prompt   = FALSE
)
