# ----------------------------------------------------------------------
# MICs dataset
# func-task accuracy Group table generation
#
# MST2: mnemonic similarity/discrimination task
#
# retrieval: Episodic retrieval task
# Subjects here identify pairs of objects that were presented in encoding phase
#
# semantic: Semantic retrieval task. 
# Here subject identify the object that is most conceptually related to the prime object
#
# spatial: Spatial discrimination task
# Subjects here are presented with 3 objects. then spatial configuration of these three objects 
# are manipulated (i.e distance to each other, or rotations).
#
# Bias corrected pattern separation score (BPS): (pattern separation rate – similar bias rate).
# BPS = P(similar|similar) − P(similar|new).
#
# Bias corrected pattern completion score (BPC): (pattern completion rate –false alarm rate).
# BPC=P(old|similar)-P(old|new)
# ----------------------------------------------------------------------

# Run behavior first to update the behavioral database !!!!
source("/host/yeatman/local_raid/rcruces/git_here/mica-sandbox/MICA-MTL/MICs_behavior.R")
rm(list=ls())

# Working directory
rawdata <- "/data_/mica3/BIDS_MICs/rawdata"
setwd(rawdata)

# Functions
# -----------------------------------------------------------------------
#### Function that calculates the mean accuracy from a given task #### 
get_accu <- function(task, subid, sesid) {
  func <- paste0(rawdata,"/",subid,"/",sesid,"/func")
  tsv <- list.files(func, pattern = paste0("*task-",task,"_events.tsv"))
  if (identical(character(0), tsv)) {
    print(paste("[ERROR]... ", subid, sesid, task,"task not found"))
    accu <- NA
  } else if (file.exists(paste0(func,"/",tsv))) {
    # Read file
    data <- read.delim(paste0(func,"/",tsv), sep = "\t")
    # Calculate accuracy
    if (task == "MST2") {
      accu <- mean(as.character(data$trial_type)==as.character(data$subject.response))
    } else {
      if (is.null(data$accuracy)) {
        accu <- ""
      } else {
        accu <- mean(na.exclude(data$accuracy))
      }
    }
  }
  return(accu)
}
#### Function that calculates the Bias Corrected Pattern from MST2 task #### 
get_bcp <- function(subid, sesid, task="MST2") {
  func <- paste0(rawdata,"/",subid,"/",sesid,"/func")
  tsv <- list.files(func, pattern = paste0("*task-",task,"_events.tsv"))
  if (identical(character(0), tsv)) {
    print(paste("[ERROR]... ", subid, sesid, task,"Bias corrected pattern missing"))
    bcp <- c(NA, NA)
  } else if (file.exists(paste0(func,"/",tsv))) {
    # Read file
    data <- read.delim(paste0(func,"/",tsv), sep = "\t")
    # patter separation scores
    p.sim=which(data$trial_type=="similar")
    p.new=which(data$trial_type=="new")
    BPS <- mean(as.character(data$subject.response[p.sim])==as.character(data$trial_type[p.sim])) - 
      mean(data$subject.response[p.new]=="similar")
    # BPC=P(old|similar)-P(old|new)
    BPC <- mean(data$subject.response[p.sim]=="old") - mean(data$subject.response[p.new]=="old")
    bcp <- c(BPS, BPC)
    }
  return(bcp)
}

# Get subject's 
subs <- list.files(rawdata, pattern = "sub-*")

# Group data vector
group.data <- c()

# For each subject get the sessions
for (i in 1:length(subs)) {
  ses <- list.files(paste0(rawdata,"/",subs[i]), pattern = "ses-*")
  
  # Iterate over each session
  for (j in 1:length(ses)) {
    group.data <- rbind(group.data, c(subs[i], ses[j], 
                        get_accu("MST2", subs[i], ses[j]), get_accu("retrieval", subs[i], ses[j]),
                        get_accu("semantic", subs[i], ses[j]),get_accu("spatial", subs[i], ses[j]),
                        get_bcp(subs[i], ses[j]) ))
  }
}

# create data frame
group.data <- data.frame(group.data)

# Set the column names
colnames(group.data) <- c("sub", "ses", "task.mst2", "task.retrieval", "task.semantic", "task.spatial", "task.BPS", "task.BPC")

# Save file as csv
write.csv(group.data, file = "participants_task.csv", quote = FALSE, row.names = FALSE, )

# Merge both databases
# -----------------------------------------------------------------------
# Load the MICs_behavior.csv file
behv <- read.csv("/host/yeatman/local_raid/rcruces/git_here/mica-sandbox/MICA-MTL/MICs_behavior.csv")
task <- read.csv("/data_/mica3/BIDS_MICs/rawdata/participants_task.csv")

# Merge data by ID
task$sub <- gsub("sub-", "", task$sub)
task$ses <- gsub("ses-0", "", task$ses)
task$ID <- paste0(task$sub,"-",task$ses)
full.data <- merge(task[,c(9,3:8)], behv[c(1:2,66:67,4,5,63:65,6:62)], by = "ID", all = TRUE)
# Reorder columns
full.data <- full.data[,c(1,8:15, 2:7, 16:72)]

# HC062-2
full.data <- full.data[-which(full.data$ID=="HC062-2"),]
indx <- which(full.data$ID=="HC062-3")
full.data$ID[indx] <- "HC062-2"
full.data$ses[indx] <- 2

# Save new database
write.csv(full.data, file = "participants_behaviour.csv", quote = FALSE, row.names = FALSE, )
