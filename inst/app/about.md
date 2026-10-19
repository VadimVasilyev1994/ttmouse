## About ttmouse

**ttmouse** estimates the internal circadian time and the functional state of the molecular clock in mouse tissue from bulk RNA-seq. Upload a raw count matrix and ttmouse handles the rest: it converts counts to log2(TPM + 1) using gene lengths (longest transcript per gene) from Ensembl release 107 (GRCm39), checks that every gene the model uses is present, and projects each sample onto a pre-trained TimeTeller model. For every sample it returns an estimated circadian time and &Theta;, a measure of clock dysfunction. No R installation or preprocessing is needed.

TimeTeller is a machine-learning method that treats the circadian clock as a noisy multigene dynamical system and estimates clock function from a single transcriptome ([Vlachou et al., *PLoS Comput Biol* 2024](https://doi.org/10.1371/journal.pcbi.1011779)). Two models are available. **Intergene** normalises each sample on its own and works for single samples. **Timecourse** normalises each group across its own time series; it usually gives more accurate timing but needs metadata with sample times and at least one grouping column, and each group must span more than 12 hours.

### Input

- **Counts:** a CSV or XLSX file (up to 50 MB). Column 1 holds the gene IDs; every other column is one sample of counts, with no annotation columns. Gene IDs can be Ensembl IDs (version suffixes such as `.3` are removed) or gene symbols; the format is detected automatically, and `Bmal1`/`Arntl` are treated as the same gene. IDs must be unique. Values must be raw counts (not normalised or logged), non-negative and with no missing values; non-integer estimated counts (e.g. from salmon or RSEM) are accepted. Genes with no length in the Ensembl 107 annotation are dropped before TPM conversion; no expression filtering is applied.
- **Metadata** (required for timecourse, optional for intergene): one row per sample, **in the same order as the count columns**. Recognised columns (any case): `time`, `group_1`, `group_2`, `group_3`, `replicate`. Time can be a number of hours (`4`) or a ZT/CT label (`ZT4`, `CT12`); any other format is rejected on timecourse and ignored with a warning on intergene.

### Results

All times are in hours on a 0&ndash;24 h clock.

- **`time_1st_peak`: the estimated time.** For each sample, TimeTeller computes a likelihood curve over the day in each local projection and averages these curves; the estimated time is the highest peak of the averaged curve (the maximum-likelihood estimate). If no peak is found, the weighted mean below is used instead.
- **`weighted_mean_time_pred`: an alternative estimate.** Each local projection's own best time, averaged with weights according to how strongly that projection's likelihood peaks. A large difference from `time_1st_peak` means the projections disagree.
- **&Theta; (`Theta`, 0&ndash;1): clock dysfunction.** The proportion of the day for which the sample's normalised likelihood curve lies above a cosine-shaped envelope centred on its main peak. A well-functioning clock gives one sharp peak and a **low &Theta;**; a disrupted clock gives a flat curve or competing peaks and a **higher &Theta;**. There is no universal cut-off: compare &Theta; between samples analysed with the same model, e.g. treatment against control.
- **`Actual_Time` and `Pred_Error`:** shown when the metadata includes time. `Actual_Time` is your time converted to a 0&ndash;24 h clock (e.g. ZT28 &rarr; 4). `Pred_Error` is `time_1st_peak` minus `Actual_Time`, measured the short way round the clock (between &minus;12 and +12 h); positive means the estimated time is later than the actual time.

### Plots

The **Results plot** plots any result column against another, coloured, shaped or split into panels by group; the dashed line marks perfect timing. Because times wrap at 24 h, a sample near midnight can sit far from the dashed line even when its error is small; `Pred_Error` accounts for this. Hover over a point to see the sample, and use the camera icon to download a PNG. The 3D projection and likelihood plots show where each sample sits relative to the training clock.
