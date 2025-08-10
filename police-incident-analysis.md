# PURPOSE: Create a new GitHub Pages blog post Markdown file with front matter,
#          body content, heatmap image, and link to the full report.
# HOW TO USE:
# 1) Update `repo_dir` below to the local path of your GitHub Pages repo:
#      irasharenow100.github.io
# 2) Run this whole script in RStudio.
# 3) Use the git commands shown after this code block to commit and push.

# --- USER SETUP: point to your local GitHub Pages repo directory -------------
repo_dir <- "D:/path/to/irasharenow100.github.io"  # <-- CHANGE THIS to your actual path

# --- Output filename ----------------------------------------------------------
post_filename <- "police-incident-analysis.md"
post_path <- file.path(repo_dir, post_filename)

# --- Construct Markdown with YAML front matter + content ----------------------
# Note: The date is set to today's date in YYYY-MM-DD format.
post_lines <- c(
  "---",
  "layout: post",
  'title: "El Cerrito Police Incident Analysis (2019–2025)"',
  paste0("date: ", format(Sys.Date(), "%Y-%m-%d")),
  "permalink: /police-incident-analysis/",
  "---",
  "",
  "I have completed a detailed analysis of police incident patterns in El Cerrito using data from 2019 through March 2025.  ",
  "The report examines the number, type, and location of incidents, with geographic visualizations showing where incidents are most concentrated.",
  "",
  "One of the most notable findings is the **high concentration of incidents along San Pablo Avenue**, including the proposed site for the new library, ",
  "which stands out as a significant hotspot. Seasonal variations are relatively small, while incident counts dropped during the COVID years (2020–2022).",
  "",
  "---",
  "",
  "### Citywide Incident Heatmap",
  "*Darker areas indicate higher concentrations of incidents.*",
  "",
  "![El Cerrito Incident Heatmap](https://github.com/IraSharenow100/el-cerrito-police-report/raw/main/incident-heatmap.png)",
  "",
  "---",
  "",
  "📄 **Full Report:**  ",
  "[Download the Word document](https://github.com/IraSharenow100/el-cerrito-police-report/raw/main/el-cerrito-police-report.docx)",
  "",
  "📧 **Contact:** irasharenow100@gmail.com"
)

# --- Write the file (UTF-8) ---------------------------------------------------
if (!dir.exists(repo_dir)) {
  stop("The repo_dir does not exist. Update `repo_dir` to your local path.")
}
writeLines(enc2utf8(post_lines), post_path, useBytes = TRUE)

message("Wrote post to: ", post_path)
