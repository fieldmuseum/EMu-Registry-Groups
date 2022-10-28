# EMu Registry Summary-scripts
# to summarize group permissions

library(tidyr)
library(readr)
library(stringr)
# library(d3heatmap)
library(heatmaply)
library(RColorBrewer)

if (nchar(Sys.getenv("REGISTRY_IN")) > 0) {
  
  input_filepath <- paste0(Sys.getenv("REGISTRY_IN"),
                           "table_security")
  
} else {
  
  input_filepath <- "EMuRegistry/sample_data/table_security/"
  
}


# point to your csv files
mm_sec_bu <- read_csv(file=paste0(input_filepath,"eregistr.csv"))

# remove 'Display' Security settings
mm_sec <- mm_sec_bu[mm_sec_bu$Key6!="Insert",]


# split out the non-SecDept settings
mm_sec$IntranetYesOnly <- NA
mm_sec$IntranetYesOnly[grepl("AdmPublishWebPassword\\=Yes", mm_sec$Value)] <- "Intranet_Yes"

mm_sec$RecordStatusActiveOnly <- NA
mm_sec$RecordStatusActiveOnly[grepl("SecRecordStatus\\=Active", mm_sec$Value)] <- "Status_Active"


mm_sec$SecDept <- 
  gsub("\\s*SecDepartment_tab\\=|\\s*AdmPublishWebPassword\\=Yes(;)*|\\s*SecRecordStatus\\=Active(;)*",
       "", mm_sec$Value)

max_depts <- 2

for (i in 1:NROW(mm_sec)) {
  
  depts_count <- str_count(mm_sec$SecDept[i], ";") + 1
  if (depts_count > max_depts) {
    max_depts <- depts_count
  }
  
}

mm_sec <- separate(mm_sec, SecDept, 
                   into=c(paste0("Dept_",1:max_depts)),
                   sep = ";")

drop_cols <- c("eregistry_key", "irn", "Key1", "Key3", "Key5",
               "Key7","Key8","Key9","Key10","Value",
               "AdmDateInserted", "AdmDateModified")

# gather columns to re-order/re-shape
mm_sec2 <- mm_sec[,!colnames(mm_sec) %in% drop_cols]

mm_sec3 <- pivot_longer(mm_sec2, cols = IntranetYesOnly:colnames(mm_sec2)[NCOL(mm_sec2)], # Dept_1:...
                        names_to = "Perm_order",
                        values_to = "Perm", values_drop_na = TRUE)

# spread display/edit/delete permission
mm_sec3 <- mm_sec3[order(mm_sec3$Key4, mm_sec3$Key2, mm_sec3$Key6),  # ]
                   !colnames(mm_sec3) %in% c("Perm_order")]

mm_sec4 <- pivot_wider(mm_sec3, id_cols = c(Key4,Key2,Perm),
                       names_from = Key6, values_from = Key6)

mm_sec4$Permission <- NA
mm_sec4$Permission[is.na(mm_sec4$Display)==F] <- 1
mm_sec4$Permission[is.na(mm_sec4$Edit)==F] <- 2
mm_sec4$Permission[is.na(mm_sec4$Delete)==F] <- 3

drop_cols2 <- c("Display", "Edit", "Delete")

mm_sec4 <- mm_sec4[nchar(mm_sec4$Perm) > 0,]

mm_sec5 <- pivot_wider(mm_sec4[,!colnames(mm_sec4) %in% drop_cols2], id_cols=Key4:Key2,
                       names_from=Perm, values_from=Permission)

mm_sec5 <- unite(mm_sec5, col = "table_group", Key4:Key2)
mm_sec5[is.na(mm_sec5)] <- 0

heatmaply(as.matrix(mm_sec5[,2:NCOL(mm_sec5)]),
          labRow = mm_sec5$table_group,
          labCol = colnames(mm_sec5)[2:NCOL(mm_sec5)],
          colors = brewer.pal (3, "Blues" ), # heat.colors(3),
          k_row = 1, k_col = 1,
          dendrogram = c("none"),
          show_dendrogram = c(FALSE, FALSE),
          show_grid = FALSE,
          # scale = "column",
          colorbar_yanchor = "top")
