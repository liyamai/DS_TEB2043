library(dplyr)
library(stringr)

# ═══════════════════════════════════════════════════════════
# STEP 1 — Load raw file
# ═══════════════════════════════════════════════════════════
raw <- read.csv("Unclean Dataset.csv",
                fileEncoding = "ISO-8859-1",
                stringsAsFactors = FALSE,
                na.strings = c("", "NA", "N/A", "nan"))
names(raw) <- str_trim(names(raw))

col_names <- c("Student_ID", "First_Name", "Last_Name", "Age",
               "Gender", "Course", "Enrollment_Date", "Total_Payments")

# ═══════════════════════════════════════════════════════════
# STEP 2 — Parse mixed-format rows into a clean matrix
# ═══════════════════════════════════════════════════════════
parsed_matrix <- matrix(NA_character_, nrow = nrow(raw), ncol = 8)

for (i in seq_len(nrow(raw))) {
  sid <- str_trim(as.character(raw[i, "Student_ID"]))
  if (!is.na(sid) && grepl("\\|", sid)) {
    parts <- str_trim(unlist(strsplit(sid, "\\|")))
    length(parts) <- 8
    parsed_matrix[i, ] <- parts
  } else {
    parsed_matrix[i, ] <- str_trim(as.character(
      c(raw[i, "Student_ID"],      raw[i, "First_Name"],
        raw[i, "Last_Name"],       raw[i, "Age"],
        raw[i, "Gender"],          raw[i, "Course"],
        raw[i, "Enrollment_Date"], raw[i, "Total_Payments"])
    ))
  }
}

df <- as.data.frame(parsed_matrix, stringsAsFactors = FALSE)
names(df) <- col_names
df[df == "NA" | df == "NULL" | df == "N/A" | df == "nan" | df == ""] <- NA

cat("=== Initial inspection ===\n")
cat("Rows:", nrow(df), "| Cols:", ncol(df), "\n")
cat("\nMissing values per column:\n"); print(colSums(is.na(df)))
cat("\nHead:\n"); print(head(df))

# ═══════════════════════════════════════════════════════════
# STEP 3 — Remove duplicates
# ═══════════════════════════════════════════════════════════
cat("\nDuplikat sebelum:", sum(duplicated(df)), "\n")
df <- distinct(df)
cat("Penghapusan duplikasi berhasil!\n")

# ═══════════════════════════════════════════════════════════
# STEP 4 — Clean GENDER
# Handles: "M 25", "F 24" (age mixed in), lowercase "f"
# ═══════════════════════════════════════════════════════════
df$Gender <- str_extract(toupper(str_trim(df$Gender)), "[MF]")
cat("Gender dibersihkan\n")

# ═══════════════════════════════════════════════════════════
# STEP 5 — Clean AGE
# Extracts digits only, then sets impossible values (< 16 or > 60) to NA
# Catches: "78*", "4", "F", "M" (gender-only cells)
# ═══════════════════════════════════════════════════════════
df$Age <- as.numeric(str_extract(df$Age, "\\d+"))
df$Age[!is.na(df$Age) & (df$Age < 16 | df$Age > 60)] <- NA
cat("Age dibersihkan (nilai di luar 16-60 dihapus)\n")

# ═══════════════════════════════════════════════════════════
# STEP 6 — Clean COURSE
# Corrects typos, removes invalid values like "4"
# ═══════════════════════════════════════════════════════════
valid_courses <- c("Data Science", "Machine Learning", "Web Development",
                   "Data Analysis", "Cyber Security")

course_map <- c(
  "Web Develpment"  = "Web Development",
  "Web Developmet"  = "Web Development",
  "Web Developmen"  = "Web Development",
  "Machine Learnin" = "Machine Learning",
  "Data Analytics"  = "Data Analysis"
)

df$Course <- str_trim(df$Course)
df$Course <- ifelse(!is.na(df$Course) & df$Course %in% names(course_map),
                    course_map[df$Course], df$Course)
df$Course[!is.na(df$Course) & !(df$Course %in% valid_courses)] <- NA
cat("Course dibersihkan (typo diperbaiki, nilai tidak valid → NA)\n")

# ═══════════════════════════════════════════════════════════
# STEP 7 — Clean TOTAL_PAYMENTS
# Strips all currency symbols ($, £, ?) and commas
# ═══════════════════════════════════════════════════════════
df$Total_Payments <- as.numeric(gsub("[^0-9.]", "", df$Total_Payments))
cat("Total_Payments dibersihkan\n")

# ═══════════════════════════════════════════════════════════
# STEP 8 — Parse & standardise ENROLLMENT_DATE → yyyy-mm-dd

parse_date_safe <- function(x) {
  formats <- c(
    "%Y-%m-%d",   # 2022-05-15  (ISO — try first to avoid misparse)
    "%d-%b-%y",   # 01-Jul-22, 01-Aug-05, 16-Jul-16
    "%d-%b-%Y",   # 01-Jul-2022
    "%d-%m-%y",   # 05-09-23, 08-01-24  (dd-mm-yy)
    "%d-%m-%Y",   # 05-09-2023
    "%d/%m/%Y"    # fallback slash format
  )
  result <- as.Date(rep(NA, length(x)))
  for (fmt in formats) {
    na_idx <- is.na(result)
    if (!any(na_idx)) break
    result[na_idx] <- suppressWarnings(as.Date(x[na_idx], format = fmt))
  }

  # Reject years before 2000 or beyond current year (catches "02-05-99" → 1999)
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  parsed_year  <- as.integer(format(result, "%Y"))
  result[!is.na(result) & (parsed_year < 2000 | parsed_year > current_year)] <- NA

  result
}

dates_parsed <- parse_date_safe(df$Enrollment_Date)
df$Enrollment_Date <- format(dates_parsed, "%Y-%m-%d")   # → "yyyy-mm-dd" string
df$Enrollment_Date[df$Enrollment_Date == "NA"] <- NA

# Report any dates that couldn't be parsed
n_date_na <- sum(is.na(df$Enrollment_Date))
cat(sprintf("Enrollment_Date: %d nilai valid, %d menjadi NA (format tidak dikenal / tahun tidak valid)\n",
            nrow(df) - n_date_na, n_date_na))

# ═══════════════════════════════════════════════════════════
# STEP 9 — Impute missing values
# Numeric  → median | Character → mode
# ═══════════════════════════════════════════════════════════
mode_val <- function(x) {
  ux <- unique(x[!is.na(x)])
  if (length(ux) == 0) return(NA)
  ux[which.max(tabulate(match(x, ux)))]
}

df$Age            <- as.numeric(df$Age)
df$Total_Payments <- as.numeric(df$Total_Payments)

for (col in setdiff(names(df), "Student_ID")) {
  if (is.numeric(df[[col]])) {
    df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
  } else {
    df[[col]][is.na(df[[col]])] <- mode_val(df[[col]])
  }
}
cat("Missing values diimputasi\n")

# ═══════════════════════════════════════════════════════════
# STEP 10 — Convert types & assign surrogate IDs
# ═══════════════════════════════════════════════════════════
df$Age        <- as.integer(df$Age)
df$Student_ID <- suppressWarnings(as.integer(df$Student_ID))

missing_idx <- which(is.na(df$Student_ID))
if (length(missing_idx) > 0) {
  max_id <- max(df$Student_ID, na.rm = TRUE)
  df$Student_ID[missing_idx] <- seq(max_id + 1, max_id + length(missing_idx))
  cat(sprintf("Surrogate IDs diberikan: %d baris (ID %d sampai %d)\n",
              length(missing_idx), max_id + 1, max_id + length(missing_idx)))
}
df$Student_ID <- as.integer(df$Student_ID)

# ═══════════════════════════════════════════════════════════
# STEP 11 — Cap outliers using IQR
# Handles the massively inflated Total_Payments in rows 72–117
# ═══════════════════════════════════════════════════════════
num_cols <- names(df)[sapply(df, is.numeric)]
for (col in num_cols) {
  q1    <- quantile(df[[col]], 0.25, na.rm = TRUE)
  q3    <- quantile(df[[col]], 0.75, na.rm = TRUE)
  iqr   <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  n_capped <- sum(df[[col]] < lower | df[[col]] > upper, na.rm = TRUE)
  df[[col]] <- ifelse(df[[col]] < lower | df[[col]] > upper,
                      median(df[[col]], na.rm = TRUE), df[[col]])
  if (n_capped > 0)
    cat(sprintf("Outlier di '%s': %d nilai di-cap\n", col, n_capped))
}
cat("Penanganan outlier selesai\n")

# ═══════════════════════════════════════════════════════════
# STEP 12 — Final check
# ═══════════════════════════════════════════════════════════
cat("\n=== Final check ===\n")
cat("Rows:", nrow(df), "\n")
cat("\nMissing values setelah pembersihan:\n"); print(colSums(is.na(df)))
cat("\nTipe data akhir:\n");                    print(sapply(df, class))
cat("\nCourse values:\n");                      print(table(df$Course))
cat("\nGender values:\n");                      print(table(df$Gender))
cat("\nAge range:", min(df$Age), "-", max(df$Age), "\n")
cat("\nTotal_Payments range:", min(df$Total_Payments), "-", max(df$Total_Payments), "\n")
cat("\nSample Enrollment_Date values:\n");      print(head(unique(df$Enrollment_Date), 15))
cat("\nHead:\n"); print(head(df, 10))

# ═══════════════════════════════════════════════════════════
# STEP 13 — Save
# ═══════════════════════════════════════════════════════════
write.csv(df, "Cleaned_Unclean_Dataset.csv", row.names = FALSE)
cat("\nFile disimpan: Cleaned_Unclean_Dataset.csv\n")
