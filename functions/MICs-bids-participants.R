# MICs tsv generator
# This script creates a tsv file compatible with BIDS
#
# RRC, Jan 28, 2021

# Set home and working directory
P <- function(Path){koti=Sys.getenv("KOTI"); return(paste0(koti,Path))}

# ---------------------------------------------------------------------------
# Load table of equivalence IRM-ID with MICs release
MICs <- read.csv(P("git_here/mica-sandbox/MICA-MTL/MICs_rename-equivalence.csv"))

# ---------------------------------------------------------------------------
# Loads the curated MICA-MTL database
MICA.MTL <- read.csv(P("git_here/mica-sandbox/MICA-MTL/MICA-MTL_behavior.csv"))

# Padded names for HC?? and P00?
MICA.MTL$Original <- gsub("HC", "HC0", MICA.MTL$ID)
MICA.MTL$Original <- gsub("P0", "PX0", MICA.MTL$Original)

# Merge databases to get the MICs' ID
MICs.tsv <- merge(MICs, MICA.MTL, by="Original")

# Subset of Transversal data (First 'Visit')
MICs.tsv <- MICs.tsv[MICs.tsv$Visit==1,]

# Subset for the BIDS tsv
MICs.tsv <- MICs.tsv[, c("MICs", "group", "age", "sex", "handed")]

# Rename columns and rows
colnames(MICs.tsv) <- c("participant_id", "group", "age", "sex", "handedness")
rownames(MICs.tsv) <- 1:dim(MICs.tsv)[1]

# Remove duplicated subject (row 51)
MICs.tsv <- MICs.tsv[-51,]

# Replace comment by handeness in HC022 (L)
MICs.tsv$handedness[22] <- "L"

# Save new participants.tsv file
write.table(MICs.tsv, file='/data_/mica3/BIDS_MIC/MICs_release/rawdata/participants.tsv', quote=FALSE, sep='\t', row.names = FALSE, na = 'n/a')


