library(gplots)
# Open connection, read files and store in a varible.
TablefileName="data/table_list.txt"
con=file(TablefileName, open="r")
table_list=readLines(con)
close(con)

IDfileName="data/table_list_ids.txt"
con=file(IDfileName, open="r")
table_list_ids=readLines(con)
close(con)

famIDfileName="data/familyID.txt"
con=file(famIDfileName, open="r")
family_IDs=readLines(con)
close(con)

varIDfileName="data/variantID.txt"
con=file(varIDfileName, open="r")
variant_IDs=readLines(con)
close(con)

by.family <- function(variantID) {
  variant <- grep(variantID,table_list_ids, fixed=TRUE)
  variant_file_names <- c()
  for (i in 1:length(variant)) {
    variant_file_names <- c(variant_file_names, table_list_ids[variant[i]])
  }
  return(variant_file_names)
}

for (i in 1:length(family_IDs)) {
  assign(family_IDs[i],paste("start.reduced", by.family(variant_IDs[i]), sep="."))
}

fam_list <- unique(family_IDs)

for(i in 1:length(unique(family_IDs))) {
  sample_list <- get(fam_list[i])
  if (length(sample_list) == 2) {
    assign(paste("venn", fam_list[i], sep="."), venn(list(a=get(sample_list[1]), b=get(sample_list[2]))))
  }
  else if (length(sample_list) == 3) {
    assign(paste("venn", fam_list[i], sep="."), venn(list(a=get(sample_list[1]), b=get(sample_list[2]), c=get(sample_list[3]))))
  }
  else if (length(sample_list) == 4) {
    assign(paste("venn", fam_list[i], sep="."), venn(list(a=get(sample_list[1]), b=get(sample_list[2]), c=get(sample_list[3]), d=get(sample_list[4]))))
  }
  else if (length(sample_list) == 5) {
    assign(paste("venn", fam_list[i], sep="."), venn(list(a=get(sample_list[1]), b=get(sample_list[2]), c=get(sample_list[3]), d=get(sample_list[4]), e=get(sample_list[5]))))
  }
  else {break("Error: more than 5 individuals")}
}

