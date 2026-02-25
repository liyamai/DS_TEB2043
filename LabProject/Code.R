# ============================================================
# uncleaned_ara.R
# Data Cleaning — tailored to Unclean_Dataset.csv
# ============================================================

library(dplyr)
library(stringr)

# ── 1. Load raw file ──────────────────────────────────────────
raw <- read.csv("Unclean Dataset.csv",
                fileEncoding = "ISO-8859-1",
                stringsAsFactors = FALSE,
                na.strings = c("", "NA", "N/A", "nan"))

names(raw) <- str_trim(names(raw))

col_names <- c("Student_ID", "First_Name", "Last_Name", "Age",
               "Gender", "Course", "Enrollment_Date", "Total_Payments")

# ── 2. Parse mixed-format rows into a clean matrix ───────────
# Some rows pack all data into Student_ID as pipe-delimited text.
# Others use proper separate columns.

parsed_matrix <- matrix(NA_character_, nrow = nrow(raw), ncol = 8)

for (i in seq_len(nrow(raw))) {
  sid <- str_trim(as.character(raw[i, "Student_ID"]))

  if (!is.na(sid) && grepl("\\|", sid)) {
    # Pipe-delimited row: split and pad/truncate to 8 fields
    parts <- str_trim(unlist(strsplit(sid, "\\|")))
    length(parts) <- 8
    parsed_matrix[i, ] <- parts
  } else {
    # Normal CSV row: read each column directly
    parsed_matrix[i, ] <- str_trim(as.character(
      c(raw[i, "Student_ID"], raw[i, "First_Name"], raw[i, "Last_Name"],
        raw[i, "Age"],        raw[i, "Gender"],     raw[i, "Course"],
        raw[i, "Enrollment_Date"], raw[i, "Total_Payments"])
    ))
  }
}

df <- as.data.frame(parsed_matrix, stringsAsFactors = FALSE)
names(df) <- col_names

# Replace "NA" / "NULL" strings and "N/A" with true NA
df[df == "NA" | df == "NULL" | df == "N/A" | df == "nan" | df == ""] <- NA

# ── 3. Initial inspection ─────────────────────────────────────
cat("=== Initial inspection ===\n")
cat("Rows:", nrow(df), "| Cols:", ncol(df), "\n")
cat("\nMissing values per column:\n");  print(colSums(is.na(df)))
cat("\nData types:\n");                 print(sapply(df, class))
cat("\nHead:\n");                       print(head(df))

# ── 4. Remove duplicates ──────────────────────────────────────
cat("\nDuplikat sebelum:", sum(duplicated(df)), "\n")
df <- distinct(df)
cat("Penghapusan duplikasi berhasil!\n")

# ── 5. Clean Age (may contain "M 22" or "F 24") ──────────────
df$Age <- as.numeric(str_extract(df$Age, "\\d+"))

# ── 6. Clean Gender — keep only M / F ────────────────────────
df$Gender <- str_extract(str_trim(df$Gender), "[MF]")

# ── 7. Clean Total_Payments — strip currency symbols & commas ─
df$Total_Payments <- as.numeric(gsub("[^0-9.]", "", df$Total_Payments))

# ── 8. Handle missing values ──────────────────────────────────
mode_val <- function(x) {
  ux <- unique(x[!is.na(x)])
  if (length(ux) == 0) return(NA)
  ux[which.max(tabulate(match(x, ux)))]
}

for (col in names(df)) {
  if (is.numeric(df[[col]])) {
    df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
  } else {
    df[[col]][is.na(df[[col]])] <- mode_val(df[[col]])
  }
}
cat("Menangani missing values berhasil\n")

# ── 9. Convert data types ─────────────────────────────────────
df$Age        <- as.integer(df$Age)
df$Student_ID <- suppressWarnings(as.integer(df$Student_ID))

# Parse dates — handle multiple formats found in dataset
parse_date_safe <- function(x) {
  formats <- c("%Y-%m-%d", "%d-%m-%y", "%d-%m-%Y",
               "%d/%m/%Y", "%m/%d/%Y", "%d-%b-%y", "%d-%b-%Y")
  result <- as.Date(rep(NA, length(x)))
  for (fmt in formats) {
    na_idx <- is.na(result)
    if (!any(na_idx)) break
    parsed <- suppressWarnings(as.Date(x[na_idx], format = fmt))
    result[na_idx] <- parsed
  }
  result
}
df$Enrollment_Date <- parse_date_safe(df$Enrollment_Date)
cat("Mengonversi tipe data berhasil\n")

# ── 10. Detect & cap outliers (IQR method) ────────────────────
num_cols <- names(df)[sapply(df, is.numeric)]
for (col in num_cols) {
  q1  <- quantile(df[[col]], 0.25, na.rm = TRUE)
  q3  <- quantile(df[[col]], 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  df[[col]] <- ifelse(df[[col]] < lower | df[[col]] > upper,
                      median(df[[col]], na.rm = TRUE), df[[col]])
}
cat("Menangani outlier berhasil\n")

# ── 11. Final check ───────────────────────────────────────────
cat("\nMissing values setelah pembersihan:\n"); print(colSums(is.na(df)))
cat("\nTipe data akhir:\n");                    print(sapply(df, class))
cat("\nSummary akhir:\n");                      print(summary(df))
print(head(df))

# ── 12. Save ──────────────────────────────────────────────────
write.csv(df, "Cleaned Dataset.csv", row.names = FALSE)
cat("File disimpan: Cleaned Dataset.csv\n")
