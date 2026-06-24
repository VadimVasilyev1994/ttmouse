# build_gene_lengths.R
#
# One-time build step: parse the Ensembl mouse GTF and serialise the gene-length
# table that counts_to_tpm() needs for TPM normalisation. Run this ONCE locally
# and ship the resulting .rds with the app, so the (large) GTF never has to be
# re-parsed at runtime.
#
# This reproduces, unchanged, the gene-length logic used when the TimeTeller
# mouse models were trained. The length definition (longest transcript per gene)
# and the annotation source (GRCm39, Ensembl release 107) are part of the frozen
# preprocessing contract and MUST NOT be altered, or projected data will no
# longer match the models' training scale.
#
# Output: a data.frame with columns (gene_id, gene_length, gene_name), exactly
# the three columns counts_to_tpm() expects.

library(GenomicFeatures)
library(txdbmaker)
library(rtracklayer)
library(dplyr)

# --- Inputs / outputs --------------------------------------------------------
GTF_FILE <- "Mus_musculus.GRCm39.107.gtf"        # Ensembl release 107, GRCm39
OUT_RDS  <- "gene_lengths_GRCm39_107.rds"         # shipped with the app

# --- 1. Build a TxDb and compute per-gene length (longest transcript) --------
txdb       <- txdbmaker::makeTxDbFromGFF(GTF_FILE, format = "gtf")
tx_lengths <- transcriptLengths(txdb, with.cds_len = FALSE)
gene_lengths <- tx_lengths %>%
  group_by(gene_id) %>%
  summarise(gene_length = max(tx_len), .groups = "drop")  # longest transcript

# --- 2. Ensembl gene_id -> gene_name map from the same GTF -------------------
# Import only gene-level features to keep this fast.
gtf <- rtracklayer::import(GTF_FILE, feature.type = "gene")
id_symbol <- as.data.frame(mcols(gtf)) %>%
  dplyr::select(gene_id, gene_name) %>%
  distinct()

# --- 3. Join into the (gene_id, gene_length, gene_name) table ---------------
gene_lengths <- gene_lengths %>%
  left_join(id_symbol, by = "gene_id") %>%
  base::as.data.frame()

# --- 4. Serialise ------------------------------------------------------------
saveRDS(gene_lengths, OUT_RDS)
cat(sprintf("Wrote %s: %d genes x %d columns (%s)\n",
            OUT_RDS, nrow(gene_lengths), ncol(gene_lengths),
            paste(colnames(gene_lengths), collapse = ", ")))
