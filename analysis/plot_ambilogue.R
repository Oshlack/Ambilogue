#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Plots for the Ambilogue methods (what the methods are and how they are
# distributed), drawn from Ambilogue.csv.
#
# The citation plots (OpenAlex data) are drawn by plot_citations.R.
#
# Input : Ambilogue.csv
# Output: figures in --out (.png)
#
# Usage:
#   Rscript plot_ambilogue.R
#   Rscript plot_ambilogue.R --csv=path/to/Ambilogue.csv --out=path/to/figures --format=png,svg
#
# Defaults: --csv = ../Ambilogue.csv relative to this script (or to the working
#           directory when sourced in RStudio), --out = <script folder>/plots,
#           --format = png.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(ggplot2)
  library(svglite)
  library(dplyr)
  library(ggtext)
  library(tidyr)
})

# ARGUMENTS --------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[1]) else default
}
script_dir <- local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(f)) dirname(normalizePath(f[1])) else getwd()
})
csv_path <- get_arg("csv", file.path(script_dir, "..", "Ambilogue.csv"))
if (!file.exists(csv_path)) stop("Missing ", csv_path, call. = FALSE)
csv_path <- normalizePath(csv_path)
out_dir  <- get_arg("out", file.path(script_dir, "plots"))
formats  <- strsplit(get_arg("format", "png"), ",")[[1]]
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# DATA -------------------------------------------------------------------------

PLOT_DATE <- Sys.Date()   # written to methods_last_updated.txt (shown on plots.html), not drawn on the plots
PLOT_YEAR <- as.integer(format(PLOT_DATE, "%Y"))   # the current year, as a number

# fileEncoding = "UTF-8-BOM" strip byte-order mark so the first column is called "Method".
ambilogue <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE,
                 fileEncoding = "UTF-8-BOM")

# Publication_Date is day/month/year (e.g. 22/3/2019).
ambilogue$Publication_Date <- as.Date(ambilogue$Publication_Date, format = "%d/%m/%Y")
if (anyNA(ambilogue$Publication_Date)) {
  stop("Unparseable Publication_Date for: ",
       paste(ambilogue$Method[is.na(ambilogue$Publication_Date)], collapse = ", "),
       call. = FALSE)
}
ambilogue$Publication_Year <- as.integer(format(ambilogue$Publication_Date, "%Y"))

# Publication_Date is day/month/year (e.g. 22/3/2019).
ambilogue$Preprint_Date <- as.Date(ambilogue$Preprint_Date, format = "%d/%m/%Y")
ambilogue$Preprint_Year <- as.integer(format(ambilogue$Preprint_Date, "%Y"))

# Methods in publication order (oldest first).
ambilogue <- ambilogue[order(ambilogue$Publication_Date), ]
ambilogue$Method <- factor(ambilogue$Method, levels = ambilogue$Method)

# Publication_Status ("Preprint" or "Published") has its own column
ambilogue$Citations <- as.numeric(ambilogue$Citations)

# Several columns hold ';'-separated lists (Ecosystem, Availability,
# Technology_Suitability, Ambient_Basis, Method_Category). This returns one row
# per method x value, e.g. split_multi(ambilogue, "Technology_Suitability").
split_multi <- function(data, cols, empty = NA_character_) {
  for (col in cols) {
    vals <- lapply(strsplit(as.character(data[[col]]), ";"), function(v) {
      v <- trimws(v)
      v <- v[nzchar(v)]
      if (length(v) == 0) empty else v
    })
    data <- data[rep(seq_len(nrow(data)), lengths(vals)), , drop = FALSE]
    data[[col]] <- unlist(vals)
  }
  rownames(data) <- NULL
  data
}

n_methods <- nrow(ambilogue)
message("Read ", n_methods, " methods from ", csv_path)

# PLOT STYLING -----------------------------------------------------------------

METHOD_CATEGORY_COLOURS <- c("Empty droplet-like" = "#93c5fd",
                             "Expression profiling-like" = "#c4b5fd",
                             "Demultiplexing-like" = "#f9a8d4")

ECOSYSTEM_COLOURS <- c("R" = "#6ee7b7",
                       "Python" = "#fde047",
                       "Command Line" = "#c4b5fd",
                       "Cell Ranger" = "#cbd5e1")

TECHNOLOGY_SUITABILITY_COLOURS <- c("Droplet-based scRNA-seq" = "#93c5fd",
                                    "Droplet-based snRNA-seq" = "#c4b5fd",
                                    "Droplet-based CITE-seq" = "#fdba74",
                                    "Droplet-based scATAC-seq" = "#f9a8d4",
                                    "Droplet-based scCRISPR-seq" = "#fca5a5",
                                    "Droplet-based scRNA+ATAC multiome" = "#5eead4",
                                    "Droplet-based single-cell omics" = "#cbd5e1",
                                    "Spatial transcriptomics" = "#fcd34d",
                                    "Well-plate scRNA-seq" = "#bef264")

FEATURE_LABELS <- c("Can_Filter_Empties" = "Filter empty droplets",
                    "Can_Ambient_Estimate_Droplets" = "Estimate ambient contamination in droplets",
                    "Can_Ambient_Estimate_Genes" = "Identify genes driving ambient contamination",
                    "Can_Ambient_Correct" = "Ambient correction of counts matrix",
                    "Can_Ambient_QC_Plot" = "Generate ambient QC plots",
                    "Can_Ambient_Simulate" = "Simulate ambient-contaminated datasets")

FEATURE_DETAILED_COLOURS <- c("Per-Droplet" = "#87CEFA",
                              "Global" = "#DAB1DA",
                              "Has this feature" = "#86efac",
                              "All genes" = "#FFC067",
                              "Selected genes only" = "#FF746C")

PUBLICATION_COLOURS <- c("Preprint" = "#fdba74",
                         "Published" = "#93c5fd",
                         "Unpublished" = "#fca5a5")

# Rename the values in a column
# Values not in `labels` are left as they are. Levels follow the order of `labels`.
relabel <- function(x, labels) {
  x <- as.character(x)
  new <- ifelse(x %in% names(labels), labels[x], x)
  factor(new, levels = unique(c(unname(labels), new)))
}

theme_ambilogue_bar <- function(base_size = 11) {
  theme_linedraw(base_size = base_size) +
  theme(plot.title = element_markdown(face = "bold", size = 18, family = "arial"),
        plot.subtitle = element_markdown(face = "italic", size = 12, family = "arial"),
        legend.title = element_text(size = 15, family = "arial", face = "bold"),
        legend.text = element_text(size = 13, family = "arial"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
        axis.title = element_text(size = 18, family = "arial", face = "bold"),
        axis.text = element_text(size = 16, family = "arial"),
        panel.border          = element_blank(),
        panel.background      = element_blank(),
        panel.grid.major.x    = element_blank(),
        panel.grid.minor      = element_blank(),
        axis.line             = element_line(),
        legend.position = "none")
}

theme_ambilogue_pie <- function(base_size = 11) {
  theme_void(base_size = base_size) +
    theme(plot.title = element_markdown(face = "bold", size = 18, family = "arial", hjust = 0.5),
          plot.subtitle = element_markdown(face = "italic", size = 10, family = "arial", hjust = 0.5),
          legend.title = element_text(size = 15, family = "arial", face = "bold"),
          legend.text = element_text(size = 13, family = "arial"),
          legend.position = "bottom")
}

save_plot <- function(p, name, width, height) {
  for (fmt in formats) {
    f <- file.path(out_dir, paste0(name, ".", fmt))
    ggsave(f, p, width = width, height = height, dpi = 200, bg = "white")
    message("Wrote ", f)
  }
}

# PLOTS ------------------------------------------------------------------------

# PUBLICATION STATUS
plot_df <- ambilogue
plot_df <- plot_df %>% group_by(Publication_Status) %>% summarise(count = n())
plot_df$percent <- plot_df$count / sum(plot_df$count) * 100
plot_df$percent <- paste0(round(plot_df$percent, 1), "% (", plot_df$count, ")")

ggplot(plot_df, aes(x = "", y = count, fill = Publication_Status)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(aes(label = percent), position = position_stack(vjust = 0.5),
            size = 6, fontface = "bold") +
  coord_polar(theta = "y", direction = -1, clip = "off") +
  scale_fill_manual(values = PUBLICATION_COLOURS) +
  labs(fill = "Publication Status") + theme_ambilogue_pie() -> publication_status_pie

save_plot(publication_status_pie, "publication_status_pie", width=8, height=8)

# METHOD CATEGORY
plot_df <- split_multi(ambilogue, "Method_Category")
plot_df <- plot_df %>% group_by(Method_Category) %>% summarise(count = n())
plot_df$percent <- plot_df$count / sum(plot_df$count) * 100
plot_df$percent <- paste0(round(plot_df$percent, 1), "% (", plot_df$count, ")")

ggplot(plot_df, aes(x = "", y = count, fill = Method_Category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(aes(label = percent), position = position_stack(vjust = 0.5),
            size = 6, fontface = "bold") +
  coord_polar(theta = "y", direction = -1, clip = "off") +
  scale_fill_manual(values = METHOD_CATEGORY_COLOURS) +
  labs(fill = "Method Categories") + theme_ambilogue_pie() -> method_categories_pie

save_plot(method_categories_pie, "method_categories_pie", width=8, height=8)

# SOFTWARE ECOSYSTEM
plot_df <- split_multi(ambilogue, "Ecosystem")

# Show Cell Ranger without the version number
plot_df$Ecosystem <- sub("^Cell Ranger.*$", "Cell Ranger", plot_df$Ecosystem)
plot_df <- plot_df %>% group_by(Ecosystem) %>% summarise(count = n())
plot_df$percent <- plot_df$count / sum(plot_df$count) * 100
plot_df$percent <- paste0(round(plot_df$percent, 1), "% (", plot_df$count, ")")

ggplot(plot_df, aes(x = "", y = count, fill = Ecosystem)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(aes(label = percent), position = position_stack(vjust = 0.5),
            size = 6, fontface = "bold") +
  coord_polar(theta = "y", direction = -1, clip = "off") +
  scale_fill_manual(values = ECOSYSTEM_COLOURS) +
  labs(fill = "Ecosystem") + theme_ambilogue_pie()-> ecosystem_pie

save_plot(ecosystem_pie, "ecosystem_pie", width=8, height=8)

# DATA TYPE
plot_df <- split_multi(ambilogue, "Technology_Suitability")
plot_df$Technology_Suitability <- factor(plot_df$Technology_Suitability,
                                         levels = names(sort(table(plot_df$Technology_Suitability), decreasing = TRUE)))

ggplot(plot_df, aes(x = Technology_Suitability, fill = Technology_Suitability)) +
  geom_bar() + xlab("Data Type (Officially Supported)") + ylab("Number of methods") +
  geom_text(stat = "count", y = 0, aes(label = after_stat(count)), size = 5,
            vjust = -0.5, fontface = "bold") +
  scale_fill_manual(values = TECHNOLOGY_SUITABILITY_COLOURS) +
  theme_ambilogue_bar() +
  # extra left margin so the long, angled first label is not cut off
  theme(plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 70)) -> technology_suitability_bar

save_plot(technology_suitability_bar, "technology_suitability_bar", width=12, height=8)

# AMBIENT FEATURES
plot_df <- ambilogue
plot_df <- plot_df %>% pivot_longer(cols = starts_with("Can_"), names_to = "Feature")
plot_df <- split_multi(plot_df, "value")
plot_df <- plot_df %>% filter(value != "No")
plot_df[which(plot_df$Feature == "Can_Ambient_Correct" & plot_df$value == "Yes"), "value"] <- "All genes"
plot_df$value <- ifelse(plot_df$value == "Yes", "Has this feature", plot_df$value)
plot_df$Feature <- relabel(plot_df$Feature, FEATURE_LABELS)
plot_df$value <- factor(plot_df$value, levels = c("Has this feature", "Per-Droplet", "Global",
                                                  "All genes", "Selected genes only"))

ggplot(plot_df, aes(x = Feature, fill = value)) +
  geom_bar() + xlab("Feature") + ylab("Number of methods") +
  geom_text(stat = "count", aes(label = after_stat(count)), size = 5,
            position = position_stack(vjust = 0), vjust = -0.5, fontface = "bold") +
  scale_fill_manual(values = FEATURE_DETAILED_COLOURS) +
  labs(fill = "Details") + theme_ambilogue_bar() +
  # same extra left margin as the data types plot so the two look alike
  theme(legend.position = "bottom",
        plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 70)) -> ambient_feature_bar

save_plot(ambient_feature_bar, "ambient_feature_bar", width=12, height=8)

# METHODS PER YEAR
plot_df <- ambilogue
plot_df$Preprint_Year <- replace_na(plot_df$Preprint_Year, +.Machine$integer.max)
plot_df$First_Version_Year <- ifelse(plot_df$Preprint_Year <= plot_df$Publication_Year,
                                   plot_df$Preprint_Year, plot_df$Publication_Year)
plot_df <- plot_df %>% group_by(First_Version_Year) %>% summarise(YearMethods = n()) %>% 
                mutate(CumSumMethods = cumsum(YearMethods))
plot_df$Current_Year <- ifelse(plot_df$First_Version_Year == PLOT_YEAR, TRUE, FALSE)

if (!any(plot_df$Current_Year)) {
  cumsum_max <- max(plot_df$CumSumMethods)
  plot_df <- plot_df %>% add_row(First_Version_Year = PLOT_YEAR,
                                 YearMethods = 0,
                                 CumSumMethods = cumsum_max,
                                 Current_Year = TRUE)
}

# Cumulative line: solid up to the last full year, dashed from there into the current (year to date) point.
last_full_year <- max(plot_df$First_Version_Year[!plot_df$Current_Year])
line_solid  <- plot_df[!plot_df$Current_Year, ]
line_dashed <- plot_df[plot_df$Current_Year | plot_df$First_Version_Year == last_full_year, ]

CURRENT_YEAR_COLOURS <- c("TRUE" = "#9FBEE0", "FALSE" = "#2A6FBB")
ggplot(plot_df, aes(x = First_Version_Year, fill = Current_Year)) +
  geom_bar(stat = "identity", aes(y = YearMethods)) +
  geom_line(data = line_solid, aes(y = CumSumMethods, group = 1, linetype = "Full year")) +
  geom_line(data = line_dashed, aes(y = CumSumMethods, group = 1, linetype = "Year-to-date")) +
  geom_point(data = line_solid, aes(y = CumSumMethods), size = 4, show.legend = FALSE) +
  geom_point(data = plot_df[plot_df$Current_Year, ], aes(y = CumSumMethods),
             shape = 21, fill = "white", size = 4, stroke = 0.8, show.legend = FALSE) +   # open circle for the year-to-date point
  ylab("Number of methods") +
  xlab("Year") +
  geom_text(stat = "identity", aes(label = YearMethods, y = YearMethods), size = 5,
            position = position_stack(vjust = 0), vjust = -0.5, fontface = "bold") +
  scale_fill_manual(values = CURRENT_YEAR_COLOURS,
                    breaks = c("FALSE", "TRUE"),
                    labels = c("Full year", "Year-to-date"),
                    name = "New methods published") +
  scale_linetype_manual(values = c("Full year" = "solid", "Year-to-date" = "dashed"),
                        name = "Cumulative number of methods") +
  guides(fill = guide_legend(order = 1), linetype = guide_legend(order = 2)) +
  scale_x_continuous(breaks = min(plot_df$First_Version_Year):max(plot_df$First_Version_Year)) +
  theme_ambilogue_bar() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5)) -> method_year_bar

save_plot(method_year_bar, "method_year_bar", width=12, height=8)

# Date the method plots were last drawn; plots.html reads this file.
writeLines(format(PLOT_DATE, "%Y-%m-%d"), file.path(out_dir, "methods_last_updated.txt"))

message("Done: figures are in ", normalizePath(out_dir))
