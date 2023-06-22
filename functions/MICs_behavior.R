# Read the googlespreadsheet
library(googledrive)
library(googlesheets4)
P <- function(Path){koti=Sys.getenv("KOTI"); return(paste0(koti,Path))}
data.google <- read_sheet("https://docs.google.com/spreadsheets/d/120hEbtTBakcwZRksRjiO08Q0tKcjVCEMIWXecjCXApY", skip=0)

# Erase comments
data.google <- data.google[!is.na(data.google$Study_name),]
# slices Columns of interest
noms <- read.csv(P("git_here/mica-sandbox/MICA-MTL/MICs_behavior-colnames.csv"))
data.sliced <- data.frame(data.google[,noms$names])

# renames the columns
colnames(data.sliced) <- noms$new.names

# Replaces c("NULL", "n/a", "--", "pending", "na") for NA
null <- c("NULL", "n/a", "--", "pending", "na", "<NA>", "?")
N <- dim(data.sliced)
data.clean <- data.sliced
for (j in 5:N[2]) {
  for (i in 1:N[1]) {
    data.clean[i,j] <- ifelse(data.sliced[i,j] %in% null, NA, data.sliced[i,j])
  }
}
# Data frame
data.clean <- data.frame(data.clean)
for (i in 1:dim(data.clean)[2]) {data.clean[,i] <- unlist(data.clean[,i])}

# Maximum scores for Behavior
behavior.max <- data.frame(behav=noms$new.names[6:22])
behavior.max$max.score <- c(30, 5, 3, 6, 3, 2, 5, 6, 42, 42, rep(1,6), 15)

# Unlist data -  replaces '1 (' & ')' for nothing
for (i in as.character(behavior.max$behav)) {
  V <- data.clean[,i]
  V <- gsub(patter='[:):]', replacement = '', gsub(pattern = '1\\s[:(:]', replacement = '', V))
  data.clean[,i] <- sapply(strsplit(as.character(V), split='/', fixed=TRUE), function(x) (x[1]))
}

# Erases all strings containing n/a string
N <- dim(data.clean)
for (j in 6:N[2]) {
  for (i in 1:N[1]) {
    if (grepl(pattern = 'n/a', x = data.clean[i,j], fixed = TRUE)==TRUE) {
      # print(paste("Coord",i,j,data.clean[i,j]))
      data.clean[i,j] <- NA
    }}}

# Find duplicated labels HC10 and HC04 HC03
data.clean$ID <-paste0(data.clean$sub,"-",data.clean$ses)
dup<-names(which(table(data.clean$ID)>1))
Inx <- which(data.clean$ID %in% dup==TRUE)
for (j in 6:N[2]) {
  for (i in Inx) {
    if (grepl(pattern = 'see', x = data.clean[i,j], fixed = TRUE)==TRUE) {
      #print(paste("Coord",i,j,data.clean[i,j]))
      data.clean[i,j] <- NA
    }}}

# Calculates the percentage of response
per <- grep(pattern = '.per', colnames(data.clean))
data.clean[,per] <- apply(data.clean[,per],1:2, function (x) {
    x<-strsplit(as.character(x), " ")[[1]][1]
    as.numeric(strsplit(as.character(x), "/")[[1]][1])*100/as.numeric(strsplit(as.character(x), "/")[[1]][2])
  })

# Remove subjects with bad encoding "HC003-2" "HC004-3" "HC010-1B" "HC062-2"
#Inx <- match(c("HC003-2", "HC004-3", "HC010-1B", "HC062-2"), data.clean$ID)
Inx <- match(c("HC004-3", "HC010-1B", "HC062-2"), data.clean$ID)
data.clean <- data.clean[-Inx,]
 
# Only Right or Left
# data.clean$handed <- substr(data.clean$handed,0,1)

# # Calculate age from Scan date and Age of bird
# char2date <- function(VecChar, Sep='-'){
#   Format <- paste0(c('%d','%m','%Y'), collapse = Sep)
#   Char <- gsub("\\.",Sep,VecChar)
#   Date <- as.Date(Char, format=Format)
#   return(Date)
# }
# data.clean$Age <- round(as.numeric(difftime(char2date(data.clean$scan.DMY), char2date(data.clean$birth.DMY), units = "weeks"))/52.25,0)

# # Discretize variable with 5 year interval
# NO AGE ANYMORE
# data.clean$age.interval <- cut(data.clean$Age, breaks = seq(0,100,5))
# data.clean$age.interval <- gsub("\\,","-", data.clean$age)
# data.clean$age.interval <- gsub("\\(|\\]", "", data.clean$age)

# Create the group Column
data.clean$group <- ifelse(grepl("HC", data.clean$ID)==TRUE, 'Healthy', 'Patient')

# Language
data.clean$Language <- ifelse(data.clean$Language=="English", "en",data.clean$Language)
data.clean$Language <- ifelse(data.clean$Language=="French", "fr",data.clean$Language)

# Sort by IDmrinfo 
data.clean <- data.clean[order(data.clean$ID),]

# --------------------------------------------------------------------------------- #
# Write curated database
write.csv(data.clean,P("git_here/mica-sandbox/MICA-MTL/MICs_behavior.csv"),quote = TRUE, row.names = FALSE)


