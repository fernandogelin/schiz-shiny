library(data.table)
# Open connection, read files and store in a varible.
TablefileName="table_list.txt"
con=file(TablefileName, open="r")
table_list=readLines(con)
close(con)

IDfileName="table_list_ids.txt"
con=file(IDfileName, open="r")
table_list_ids=readLines(con)
close(con)

famIDfileName="familyID.txt"
con=file(famIDfileName, open="r")
family_IDs=readLines(con)
close(con)

varIDfileName="variantID.txt"
con=file(varIDfileName, open="r")
variant_IDs=readLines(con)
close(con)

# Read tables from disk.
for (i in 1:length(table_list)) {
    fileName=paste("data", as.character(table_list[i]), sep="/")
    print(paste("Reading table",fileName,sep=" "))
    assign(table_list_ids[i], read.table(fileName, header=TRUE), envir=.GlobalEnv)
}


# Create a list with the variant tag to be used as argument of the by.family() function.
var_id_list <- paste("variant",c(1:19), sep="")
var_id_list[1] <- "variant01"

# Function that takes a variant tag as argument and returns 
by.family <- function(variantID) {
  variant <- grep(variantID,table_list_ids, fixed=TRUE)
  variant_file_names <- c()
  for (i in 1:length(variant)) {
    variant_file_names <- c(variant_file_names, table_list_ids[variant[i]])
  }
  return(variant_file_names)
}


# Subset the tables to contain only those variants exclusive for each family. Argument is a list of individual table ids. This function gives the variants exclusive for each family. Variants that occur in other families are filtered out.
# cat.family.var <- function(table_id) {
#   sub <- sub("(\\d*)(\\w)(\\d*).", "", table_id)
#   print(paste("Subsetting table",table_id,sep=" "))
#  if (sub == "variant01") {
#    filter <- get(table_id)$set == "variant"
#  }
#  else {
#    filter <- get(table_id)$set == sub
#  }
#  assign(paste("unique.var.",table_id, sep=""), subset(get(table_id), filter), envir=.GlobalEnv)
#  print(paste("Finished subsetting table",table_id,sep=" "))
#  }  


# Apply the cat.family.var function to the samples in the table_list_ids list.
# lapply(table_list_ids, cat.family.var)

# Creates a list with unique.var file names.
# unique_var_list <- paste("unique.var.", table_list_ids, sep="")

# Subset tables based on given filters.
filter.reduce.tables <- function(fam_id_list) {
  fam_list <- unlist(lapply(unique(fam_id_list), get))
  print(fam_list)
  # Filter
  reduced_fam_list <- c()
  for (i in 1:length(fam_list)) {
    print(paste("Reducing table",fam_list[i],sep=" "))
    exclude_gene_function <- c("ncRNA_exonic", "downstream", "intronic", "UTR3", "upstream", "UTR5", " ncRNA_intronic", " ncRNA_UTR3", " ncRNA_UTR5", "intergenic", " ncRNA_splicing")
    exclude_exonic_function <- c("0", "synonymous_SNV", "unknown", "nonframeshift_insertion", "nonframeshift_deletion", "NA")
    # Filter snp132 by NA, 1000g and esp6500 frequencies, gene and exonic functions.
    filter1 <- is.na(get(fam_list[i])$snp132)
    # filter1b <- is.na(get(fam_list[i])$snp138)
    filter2 <- get(fam_list[i])$esp6500si_all < 0.01
    filter3 <- get(fam_list[i])$X1000g2014aug_all < 0.01
    filter4 <- !(get(fam_list[i])$Func.refGene %in% exclude_gene_function)
    filter5 <- !(get(fam_list[i])$ExonicFunc.refGene %in% exclude_exonic_function)
    filter6 <- as.character(get(fam_list[i])[,83]) != "./."
    assign(fam_list[i], subset(get(fam_list[i]), filter1 & filter2 & filter3 & filter4 & filter5 & filter6))
    sub <- sub("(\\d*)(\\w)(\\d*).", "", fam_list[i])
    gt <- strsplit(as.character(get(fam_list[i])[,83]), "/")
    filter7 <- c()
    for (j in 1:length(gt)) {
      if (sub == "variant2") {
        filter7 <- c(filter7, unlist(gt[j])[1] == unlist(gt[j])[2])
      }
      else {
        filter7 <- c(filter7, unlist(gt[j])[1] != unlist(gt[j])[2])
      }
    }
    assign(paste("reduced.",fam_list[i], sep=""), subset(get(fam_list[i]), filter7), envir=.GlobalEnv)
    print(paste(i, "Done reducing table",fam_list[i],sep=" "))
    reduced_fam_list <- c(reduced_fam_list, paste("reduced.",fam_list[i], sep=""))
  }
  # Creates lists with start position and a start_list with ids.
  start_list <- c()
  for (i in 1:length(reduced_fam_list)) {
    assign(paste("start",reduced_fam_list[i], sep="."), get(reduced_fam_list[i])$POS, envir=.GlobalEnv)
    start_list <- c(start_list, paste("start",reduced_fam_list[i], sep="."))
  }
}

# Assign "sampleID.varianID" to id_family
for (i in 1:length(family_IDs)) {
  assign(family_IDs[i],by.family(variant_IDs[i]))
}
# Double lapply. Get the family list and apply filter.reduce.tables to each individual from the family
lapply(family_IDs, filter.reduce.tables)

# Assign "start.reduced.sampleID.varianID" to id_family
famIDfileName="familyID.txt"
con=file(famIDfileName, open="r")
family_IDs=readLines(con)
close(con)
for (i in 1:length(family_IDs)) {
  assign(family_IDs[i],paste("start.reduced",by.family(variant_IDs[i]), sep="."))
}

# Find the shared variants
family <- c()
for (i in 1:length(family_IDs)) {
  family <- lapply(family_IDs, get)
  assign(paste("shared.var",family_IDs[i], sep="."), Reduce(intersect, lapply(unlist(family[i]), get)), envir=.GlobalEnv)
}

# Subset tables to contain only shared variants.
  # Filter
  for (i in 1:length(table_list_ids)) {
    filter <- get(paste("reduced.", table_list_ids[i], sep=""))$POS %in% get(paste("shared.var.", family_IDs[i], sep=""))
    file_name <- paste("shared.reduced.",table_list_ids[i], sep="")
    table <- data.table(get(paste("reduced.",table_list_ids[i], sep="")))
    shared_reduced_table <- subset(table, filter)
    assign(file_name, shared_reduced_table, envir=.GlobalEnv)
  }

# Add columns with excluded genes logical list.
fileName="pseudogenes.txt"
conn=file(fileName,open="r")
linn=readLines(conn)
pseudogenes <- vector()
for (i in 1:length(linn)){
  pseudogenes <- c(pseudogenes, linn[i])
}
close(conn)

fileName="table7.txt"
conn=file(fileName,open="r")
linn=readLines(conn)
table7 <- vector()
for (i in 1:length(linn)){
  table7 <- c(table7, linn[i])
}
close(conn)

for (i in 1:length(table_list_ids)) {
  file_name <- paste("shared.reduced.added.col.",table_list_ids[i], sep="")
  table <- data.table(get(paste("shared.reduced.",table_list_ids[i], sep="")))
  table_add_on <- cbind(table, pseudogenes=as.character(table$GL) %in% pseudogenes, table7=as.character(table$GL) %in% table7)
  assign(file_name, table_add_on, envir=.GlobalEnv)
}

save(shared.reduced.added.col.00C01655.variant19,shared.reduced.added.col.02C10767.variant9,shared.reduced.added.col.03C17802.variant7,shared.reduced.added.col.90C00691.variant16,shared.reduced.added.col.90C01334.variant14,shared.reduced.added.col.90C03009.variant18,shared.reduced.added.col.00C01656.variant19,shared.reduced.added.col.02C11048.variant13,shared.reduced.added.col.03C19764.variant10,shared.reduced.added.col.90C00789.variant16,shared.reduced.added.col.90C01798.variant11,shared.reduced.added.col.90C03067.variant17,shared.reduced.added.col.00C01822.variant19,shared.reduced.added.col.02C12825.variant13,shared.reduced.added.col.03C20094.variant10,shared.reduced.added.col.90C00866.variant16,shared.reduced.added.col.90C01801.variant11,shared.reduced.added.col.90C03142.variant18,shared.reduced.added.col.01C08396.variant4,shared.reduced.added.col.02C12826.variant13,shared.reduced.added.col.03C21012.variant10,shared.reduced.added.col.90C00990.variant16,shared.reduced.added.col.90C01805.variant11,shared.reduced.added.col.90C03310.variant6,shared.reduced.added.col.01C08398.variant4,shared.reduced.added.col.03C13990.variant7,shared.reduced.added.col.03C21155.variant10,shared.reduced.added.col.90C01063.variant12,shared.reduced.added.col.90C01840.variant11,shared.reduced.added.col.90C03313.variant8,shared.reduced.added.col.01C08760.variant4,shared.reduced.added.col.03C14076.variant9,shared.reduced.added.col.04C30815.variant5,shared.reduced.added.col.90C01235.variant14,shared.reduced.added.col.90C01890.variant14,shared.reduced.added.col.90C03332.variant6,shared.reduced.added.col.01C08761.variant4,shared.reduced.added.col.03C14816.variant01,shared.reduced.added.col.04C30822.variant5,shared.reduced.added.col.90C01238.variant12,shared.reduced.added.col.90C01891.variant14,shared.reduced.added.col.90C03520.variant17,shared.reduced.added.col.01C08766.variant4,shared.reduced.added.col.03C14821.variant01,shared.reduced.added.col.04C30824.variant5,shared.reduced.added.col.90C01297.variant2,shared.reduced.added.col.90C02936.variant8,shared.reduced.added.col.90C03522.variant17,shared.reduced.added.col.01C09383.variant15,shared.reduced.added.col.03C14829.variant01,shared.reduced.added.col.04C30825.variant5,shared.reduced.added.col.90C02963.variant8,shared.reduced.added.col.90C03540.variant6,shared.reduced.added.col.01C09388.variant15,shared.reduced.added.col.03C14865.variant01,shared.reduced.added.col.04C37691.variant3,shared.reduced.added.col.90C01304.variant2,shared.reduced.added.col.90C02970.variant18,shared.reduced.added.col.90C04224.variant14,shared.reduced.added.col.02C09539.variant15,shared.reduced.added.col.03C14989.variant01,shared.reduced.added.col.04C37692.variant3,shared.reduced.added.col.90C01308.variant2,shared.reduced.added.col.90C02971.variant18,shared.reduced.added.col.02C10243.variant9,shared.reduced.added.col.03C15391.variant7,shared.reduced.added.col.05C50396.variant3,shared.reduced.added.col.90C01319.variant2,shared.reduced.added.col.90C02992.variant17, file="env.RData")
