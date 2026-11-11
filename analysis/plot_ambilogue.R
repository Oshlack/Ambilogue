#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Plots for the Ambilogue tools (what the tools are and how they are
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

PLOT_DATE <- Sys.Date()   # written to tools_last_updated.txt (shown on plots.html), not drawn on the plots
PLOT_YEAR <- as.integer(format(PLOT_DATE, "%Y"))   # the current year, as a number

# fileEncoding = "UTF-8-BOM" strip byte-order mark so the first column is called "Tool".
ambilogue <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE,
                 fileEncoding = "UTF-8-BOM")

# Publication_Date is day/month/year (e.g. 22/3/2019).
ambilogue$Publication_Date <- as.Date(ambilogue$Publication_Date, format = "%d/%m/%Y")
if (anyNA(ambilogue$Publication_Date)) {
  stop("Unparseable Publication_Date for: ",
       paste(ambilogue$Tool[is.na(ambilogue$Publication_Date)], collapse = ", "),
       call. = FALSE)
}
ambilogue$Publication_Year <- as.integer(format(ambilogue$Publication_Date, "%Y"))

# Publication_Date is day/month/year (e.g. 22/3/2019).
ambilogue$Preprint_Date <- as.Date(ambilogue$Preprint_Date, format = "%d/%m/%Y")
ambilogue$Preprint_Year <- as.integer(format(ambilogue$Preprint_Date, "%Y"))

# Tools in publication order (oldest first).
ambilogue <- ambilogue[order(ambilogue$Publication_Date), ]
ambilogue$Tool <- factor(ambilogue$Tool, levels = ambilogue$Tool)

# Publication_Status ("Preprint" or "Published") has its own column
ambilogue$Citations <- as.numeric(ambilogue$Citations)

# Several columns hold ';'-separated lists (Ecosystem, Availability,
# Technology_Suitability, Ambient_Basis, Methodological_Category). This returns one row
# per tool x value, e.g. split_multi(ambilogue, "Technology_Suitability").
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

n_tools <- nrow(ambilogue)
message("Read ", n_tools, " tools from ", csv_path)

# PLOT STYLING -----------------------------------------------------------------
# Styling follows the paper figures (analysis/ambilogue_paper/ambilogue_paper.R): pastel fills with
# black outlines, a black border around the plot area, the standard sans serif font and legends on
# the right of the pies. No titles are drawn on the plots (plots.html gives each one a title).

OUTLINE_WIDTH <- 1.2        # black outline on bars and pies (ggplot default is 0.5)
PANEL_BORDER_WIDTH <- 3     # black border around the plot area on the bar-type plots
PIE_SMALL_SLICE <- 0.10     # pie slices smaller than this share get their label outside the pie
LEGEND_WRAP <- 40           # long legend labels wrap after this many characters, keeping the legend inside the plot area
PAIR_FIG_WIDTH <- 18        # figure size (inches) of the timeline and overall citations plots, so they match
PAIR_FIG_HEIGHT <- 13
PAIR_PANEL_WIDTH <- 14      # size (inches) of the plot area inside each of those two figures
PAIR_PANEL_HEIGHT <- 7.5
PIE_SIZE <- 5.5             # width and height (inches) of each pie's plot area, so both pies match
FONT_FAMILY <- "Open Sans"  # the font used on the website (site.css); install it on your computer, or R falls back to the default sans serif
FONT_SCALE <- 1.5           # multiplies every font size below (1 = original website sizes)

# Text is measured on a Cairo device, the same one ggsave() uses for the PNGs, so legend widths
# are worked out with the real fonts (Open Sans) rather than the PDF device's built-in ones.
if (capabilities("cairo")) grDevices::png(tempfile(fileext = ".png"), type = "cairo")

# Bold text (axis titles, legend titles, the numbers on the plots) needs the bold face of the font.
# If only the regular face is installed, R silently draws everything in regular weight, so warn.
tryCatch(local({   # the check itself must never stop the script
  plain <- systemfonts::match_fonts(FONT_FAMILY, weight = "normal")$path
  bold  <- systemfonts::match_fonts(FONT_FAMILY, weight = "bold")$path
  if (identical(plain, bold))
    warning("No bold face of '", FONT_FAMILY, "' found, so bold text will be drawn in regular weight. ",
            "Install the static 'Open Sans Bold' (e.g. from the 'static' folder of the Google Fonts download, ",
            "not only the variable font), restart R and run the script again.", call. = FALSE)
}), error = function(e) NULL)

# Text drawn with geom_text (the numbers on bars and pies) uses the same font
update_geom_defaults("text", list(family = FONT_FAMILY))

save_plot <- function(p, name, width, height) {
  for (fmt in formats) {
    f <- file.path(out_dir, paste0(name, ".", fmt))
    ggsave(f, p, width = width, height = height, dpi = 200, bg = "white")
    message("Wrote ", f)
  }
}

# Fixes the plot area (panel) to width x height inches (height defaults to width). Used so plots
# come out the same size however wide their legends are. Returns a gtable (draw it with grid.draw).
set_panel_size <- function(p, width, height = width) {
  g <- ggplotGrob(p)
  panel <- g$layout[g$layout$name == "panel", ]
  g$widths[panel$l]  <- unit(width, "in")
  g$heights[panel$t] <- unit(height, "in")
  g
}

# Wrap text at `width` characters (used for legend titles and labels, which now sit on the right)
wrap_text <- function(x, width) {
  vapply(x, function(v) paste(strwrap(v, width), collapse = "\n"), character(1), USE.NAMES = FALSE)
}

# Width (points) of a legend title, measured with the plot font.
title_width <- function(x, size) {
  tg <- grid::textGrob(x, gp = grid::gpar(fontfamily = FONT_FAMILY, fontsize = size, fontface = "bold"))
  grid::convertWidth(grid::grobWidth(tg), "pt", valueOnly = TRUE)
}

# Pads `title` on the right with non-breaking spaces until it is as wide as `target`, so the
# entries of two stacked legends start at the same left edge. `size` is the title font size (pt).
pad_title <- function(title, target, size) {
  for (i in 1:60) {
    if (title_width(title, size) >= title_width(target, size)) break
    title <- paste0(title, "\u00a0")
  }
  title
}


# Preprint / Published pie colours
PUBLICATION_COLOURS <- c("Preprint" = "#F4C7A1",
                         "Published" = "#C1DAFB",
                         "Unpublished" = "#E5B9B8")

# Full year (FALSE) and year-to-date (TRUE) bars
CURRENT_YEAR_COLOURS <- c("TRUE" = "#E4F0FC",
                          "FALSE" = "#C1DAFB")

METHODOLOGICAL_CATEGORY_COLOURS <- c("Empty droplet-based" = "#B697F1",
                             "Expression-based" = "#FEF5A8",
                             "Variant-based" = "#F2CAE0",
                             "Empty droplet-based (with alternative expression-based mode)" = "#F4C7A1")

ECOSYSTEM_COLOURS <- c("R" = "#BEE9C1",
                       "Python" = "#A6D3D1",
                       "Command Line" = "#B1BAD1",
                       "Cell Ranger" = "#E3E3E3")

TECHNOLOGY_SUITABILITY_COLOURS <- c("Droplet-based scRNA-seq" = "#a3c6ed",
                                    "Droplet-based snRNA-seq" = "#cac0f2",
                                    "Droplet-based CITE-seq" = "#e8ba89",
                                    "Droplet-based scATAC-seq" = "#edb4d3",
                                    "Droplet-based scCRISPR-seq" = "#efb2b2",
                                    "Droplet-based scRNA+ATAC multiome" = "#73d5c6",
                                    "Droplet-based single-cell omics" = "#ced5de",
                                    "Spatial transcriptomics" = "#e2c567",
                                    "Well-plate scRNA-seq" = "#b8dd79")

FEATURE_LABELS <- c("Can_Filter_Empties" = "Filter empty droplets",
                    "Can_Ambient_Estimate_Droplets" = "Estimate ambient contamination in droplets",
                    "Can_Ambient_Estimate_Genes" = "Identify genes driving ambient contamination",
                    "Can_Ambient_Correct" = "Ambient correction of counts matrix",
                    "Can_Ambient_QC_Plot" = "Generate ambient QC plots",
                    "Can_Ambient_Simulate" = "Simulate ambient-contaminated datasets")

FEATURE_DETAILED_COLOURS <- c("Per-Droplet" = "#C1DAFB",
                              "Global" = "#F9A3CB",
                              "Both per-droplet and global" = "#B697F1",
                              "Has this feature" = "#BEE9C1",
                              "All genes" = "#FEF5A8",
                              "Selected genes only" = "#E5B9B8")

SUPPORT_COLOURS <- c("Officially supported" = "#C1DAFB",
                     "Community supported" = "#E5B9B8")

# Rename the values in a column
# Values not in `labels` are left as they are. Levels follow the order of `labels`.
relabel <- function(x, labels) {
  x <- as.character(x)
  new <- ifelse(x %in% names(labels), labels[x], x)
  factor(new, levels = unique(c(unname(labels), new)))
}

theme_ambilogue_bar <- function(base_size = 11) {
  theme_linedraw(base_size = base_size) +
  theme(text = element_text(family = FONT_FAMILY),
        plot.background = element_rect(fill = NA, colour = NA),   # transparent background
        legend.background = element_rect(fill = NA, colour = NA),
        legend.key = element_rect(fill = NA, colour = NA),
        legend.box.background = element_rect(fill = NA, colour = NA),
        plot.title = element_markdown(face = "bold", size = 18 * FONT_SCALE, family = FONT_FAMILY, hjust = 0.5),
        plot.subtitle = element_markdown(face = "italic", size = 12 * FONT_SCALE, family = FONT_FAMILY),
        legend.title = element_text(size = 15 * FONT_SCALE, family = FONT_FAMILY, face = "bold"),
        legend.text = element_text(size = 13 * FONT_SCALE, family = FONT_FAMILY),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16 * FONT_SCALE),
        axis.title = element_text(size = 18 * FONT_SCALE, family = FONT_FAMILY, face = "bold"),
        axis.text = element_text(size = 16 * FONT_SCALE, family = FONT_FAMILY),
        panel.border          = element_rect(colour = "black", fill = NA, linewidth = PANEL_BORDER_WIDTH),   # solid black border around the plot area (raise linewidth for a bolder line)
        panel.background      = element_blank(),
        panel.grid.major.x    = element_blank(),
        panel.grid.minor      = element_blank(),
        axis.line             = element_blank(),   # the panel border already draws these edges
        plot.title.position = "panel",   # centre the title over the plot area, not the legend
        legend.justification = "left",   # legend sits at the left edge of the plot area
        legend.box.just = "left",        # stacked legends line up on the left
        legend.title.position = "left",  # titles beside their entries
        legend.position = "none")
}

theme_ambilogue_pie <- function(base_size = 11) {
  theme_void(base_size = base_size) +
    theme(text = element_text(family = FONT_FAMILY),
          plot.background = element_rect(fill = NA, colour = NA),   # transparent background
          legend.background = element_rect(fill = NA, colour = NA),
          legend.key = element_rect(fill = NA, colour = NA),
          legend.box.background = element_rect(fill = NA, colour = NA),
        plot.title = element_markdown(face = "bold", size = 18 * FONT_SCALE, family = FONT_FAMILY, hjust = 0.5),
          plot.subtitle = element_markdown(face = "plain", size = 11 * FONT_SCALE, family = FONT_FAMILY, hjust = 0.5),
          legend.title = element_text(size = 15 * FONT_SCALE, family = FONT_FAMILY, face = "bold"),
          legend.text = element_text(size = 13 * FONT_SCALE, family = FONT_FAMILY),
          legend.position = "right")
}

# PLOTS ------------------------------------------------------------------------

## Publication status ----------------------------------------------------------

plot_df <- ambilogue
plot_df <- plot_df %>% group_by(Publication_Status) %>% summarise(count = n())
plot_df$percent <- plot_df$count / sum(plot_df$count) * 100
plot_df$share <- plot_df$percent / 100
plot_df$percent <- paste0(round(plot_df$percent, 1), "%\n(", plot_df$count, ")")
plot_df$label_inside  <- ifelse(plot_df$share < PIE_SMALL_SLICE, "", plot_df$percent)
plot_df$label_outside <- ifelse(plot_df$share < PIE_SMALL_SLICE, plot_df$percent, "")

ggplot(plot_df, aes(x = "", y = count, fill = Publication_Status)) +
  geom_bar(stat = "identity", width = 1, color = "black", linewidth = OUTLINE_WIDTH) +
  # Labels sit part-way out from the centre (x = 1.2), clear of the slice edges. Small slices
  # are labelled outside the pie instead (x = 1.75).
  geom_text(aes(x = 1.2, label = label_inside), position = position_stack(vjust = 0.5),
            size = 5 * FONT_SCALE, fontface = "bold", lineheight = 0.8) +
  geom_text(aes(x = 1.75, label = label_outside), position = position_stack(vjust = 0.5),
            size = 5 * FONT_SCALE, fontface = "bold", lineheight = 0.8) +
  coord_polar(theta = "y", direction = -1, clip = "off") +
  scale_fill_manual(values = PUBLICATION_COLOURS) +
  labs(fill = "Publication Status") + theme_ambilogue_pie() -> publication_status_pie

publication_status_pie_grob <- set_panel_size(publication_status_pie, PIE_SIZE)
save_plot(publication_status_pie_grob, "publication_status_pie", width = 11, height = 7)

## Methodological category -------------------------------------------------------------

# Tools with two categories get their own combined slice ("Empty droplet-based (with alternative
# expression-based mode)") instead of being counted once in each category, so every tool is counted once
plot_df <- ambilogue
plot_df$Methodological_Category <- gsub("\\s*;\\s*", " and ", plot_df$Methodological_Category)
# Name of the combined slice
plot_df$Methodological_Category[plot_df$Methodological_Category == "Empty droplet-based and Expression-based"] <-
  "Empty droplet-based (with alternative expression-based mode)"
plot_df <- plot_df %>% group_by(Methodological_Category) %>% summarise(count = n())
# Order of the slices (clockwise from the top), which is also the order of the legend: the three
# single categories, then the combined (orange) one last
plot_df$Methodological_Category <- factor(plot_df$Methodological_Category,
                                  levels = c("Empty droplet-based", "Expression-based", "Variant-based",
                                             "Empty droplet-based (with alternative expression-based mode)"))
plot_df$percent <- plot_df$count / sum(plot_df$count) * 100
plot_df$share <- plot_df$percent / 100
plot_df$percent <- paste0(round(plot_df$percent, 1), "%\n(", plot_df$count, ")")
plot_df$label_inside  <- ifelse(plot_df$share < PIE_SMALL_SLICE, "", plot_df$percent)
plot_df$label_outside <- ifelse(plot_df$share < PIE_SMALL_SLICE, plot_df$percent, "")

ggplot(plot_df, aes(x = "", y = count, fill = Methodological_Category)) +
  geom_bar(stat = "identity", width = 1, color = "black", linewidth = OUTLINE_WIDTH) +
  # Labels sit part-way out from the centre (x = 1.2), clear of the slice edges. Small slices
  # are labelled outside the pie instead (x = 1.75). Both pies have both layers, so they
  # keep the same size.
  geom_text(aes(x = 1.2, label = label_inside), position = position_stack(vjust = 0.5),
            size = 5 * FONT_SCALE, fontface = "bold", lineheight = 0.8) +
  geom_text(aes(x = 1.75, label = label_outside), position = position_stack(vjust = 0.5),
            size = 5 * FONT_SCALE, fontface = "bold", lineheight = 0.8) +
  coord_polar(theta = "y", direction = -1, clip = "off") +
  # long legend labels wrap onto two lines so the legend stays inside the cell
  scale_fill_manual(values = METHODOLOGICAL_CATEGORY_COLOURS, labels = function(x) wrap_text(x, LEGEND_WRAP)) +
  labs(fill = "Methodological Category") + theme_ambilogue_pie() -> methodological_categories_pie

methodological_categories_pie_grob <- set_panel_size(methodological_categories_pie, PIE_SIZE)
save_plot(methodological_categories_pie_grob, "methodological_categories_pie", width = 11, height = 7)

## Software ecosystem ----------------------------------------------------------

# A tool that runs in two ecosystems is counted once in each
plot_df <- split_multi(ambilogue, "Ecosystem")

# Show Cell Ranger without the version number
plot_df$Ecosystem <- sub("^Cell Ranger.*$", "Cell Ranger", plot_df$Ecosystem)
plot_df <- plot_df %>% group_by(Ecosystem) %>% summarise(count = n())
plot_df$percent <- plot_df$count / sum(plot_df$count) * 100
plot_df$share <- plot_df$percent / 100
plot_df$percent <- paste0(round(plot_df$percent, 1), "%\n(", plot_df$count, ")")
plot_df$label_inside  <- ifelse(plot_df$share < PIE_SMALL_SLICE, "", plot_df$percent)
plot_df$label_outside <- ifelse(plot_df$share < PIE_SMALL_SLICE, plot_df$percent, "")

ggplot(plot_df, aes(x = "", y = count, fill = Ecosystem)) +
  geom_bar(stat = "identity", width = 1, color = "black", linewidth = OUTLINE_WIDTH) +
  # Labels sit part-way out from the centre (x = 1.2), clear of the slice edges. Small slices
  # are labelled outside the pie instead (x = 1.75). Both pies have both layers, so they
  # keep the same size.
  geom_text(aes(x = 1.2, label = label_inside), position = position_stack(vjust = 0.5),
            size = 5 * FONT_SCALE, fontface = "bold", lineheight = 0.8) +
  geom_text(aes(x = 1.75, label = label_outside), position = position_stack(vjust = 0.5),
            size = 5 * FONT_SCALE, fontface = "bold", lineheight = 0.8) +
  coord_polar(theta = "y", direction = -1, clip = "off") +
  scale_fill_manual(values = ECOSYSTEM_COLOURS) +
  labs(fill = "Ecosystem", subtitle = "Tools can have multiple ecosystems") + theme_ambilogue_pie() -> ecosystem_pie

ecosystem_pie_grob <- set_panel_size(ecosystem_pie, PIE_SIZE)
save_plot(ecosystem_pie_grob, "ecosystem_pie", width = 11, height = 7)

## Data type -------------------------------------------------------------------

official  <- split_multi(ambilogue, "Technology_Suitability")
community <- split_multi(ambilogue, "Technology_Suitability_Community")

plot_df <- bind_rows(
  data.frame(Data_Type = official$Technology_Suitability,            Support = "Officially supported"),
  data.frame(Data_Type = community$Technology_Suitability_Community, Support = "Community supported")
) %>% filter(!is.na(Data_Type))

# x-axis order: by total (official + community), biggest first
plot_df$Data_Type <- factor(plot_df$Data_Type,
                            levels = names(sort(table(plot_df$Data_Type), decreasing = TRUE)))
# first level is stacked on top, so community goes first -> sits above official
plot_df$Support <- factor(plot_df$Support,
                          levels = c("Community supported", "Officially supported"))

ggplot(plot_df, aes(x = Data_Type, fill = Support)) +
  geom_bar(colour = "black", linewidth = OUTLINE_WIDTH) + xlab("Data Type") + ylab("Number of tools") +
  geom_text(stat = "count", aes(label = after_stat(count)), size = 5 * FONT_SCALE,
            position = position_stack(vjust = 0.5), fontface = "bold") +
  scale_fill_manual(values = SUPPORT_COLOURS) +
  guides(fill = guide_legend(reverse = TRUE)) +   # legend: official first, then community
  labs(fill = "Support") +
  theme_ambilogue_bar() +
  # extra left margin so the long, angled first label is not cut off (raise l if it still is)
  theme(legend.position = "bottom",
        plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 68 * FONT_SCALE)) -> technology_suitability_bar

save_plot(technology_suitability_bar, "technology_suitability_bar", width = 16, height = 12)

## Ambient features ------------------------------------------------------------

plot_df <- ambilogue
plot_df <- plot_df %>% pivot_longer(cols = starts_with("Can_"), names_to = "Feature")
# Tools that estimate contamination both per droplet and globally get their own category, rather
# than being counted once as per-droplet and once as global (so each tool is counted once)
plot_df$value <- ifelse(plot_df$Feature == "Can_Ambient_Estimate_Droplets" &
                          grepl("Per-Droplet", plot_df$value) & grepl("Global", plot_df$value),
                        "Both per-droplet and global", plot_df$value)
plot_df <- split_multi(plot_df, "value")
plot_df <- plot_df %>% filter(value != "No")
plot_df[which(plot_df$Feature == "Can_Ambient_Correct" & plot_df$value == "Yes"), "value"] <- "All genes"
plot_df$value <- ifelse(plot_df$value == "Yes", "Has this feature", plot_df$value)
plot_df$Feature <- relabel(plot_df$Feature, FEATURE_LABELS)
# Legend order: the three ways of estimating contamination together, then the plain "has this
# feature", then the two kinds of correction together
plot_df$value <- factor(plot_df$value, levels = c("Per-Droplet", "Global", "Both per-droplet and global",
                                                  "Has this feature",
                                                  "All genes", "Selected genes only"))

ggplot(plot_df, aes(x = Feature, fill = value)) +
  geom_bar(colour = "black", linewidth = OUTLINE_WIDTH) + xlab("Feature") + ylab("Number of tools") +
  geom_text(stat = "count", aes(label = after_stat(count)), size = 5 * FONT_SCALE,
            position = position_stack(vjust = 0), vjust = -0.25, fontface = "bold") +
  scale_fill_manual(values = FEATURE_DETAILED_COLOURS) +
  labs(fill = "Details") + theme_ambilogue_bar() +
  # same extra left margin as the data types plot so the two look alike
  theme(legend.position = "bottom",
        plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 68 * FONT_SCALE)) -> ambient_feature_bar

save_plot(ambient_feature_bar, "ambient_feature_bar", width = 16, height = 12)

## Tools per year ------------------------------------------------------------

plot_df <- ambilogue
plot_df$Preprint_Year <- replace_na(plot_df$Preprint_Year, +.Machine$integer.max)
plot_df$First_Version_Year <- ifelse(plot_df$Preprint_Year <= plot_df$Publication_Year,
                                   plot_df$Preprint_Year, plot_df$Publication_Year)
plot_df <- plot_df %>% group_by(First_Version_Year) %>% summarise(YearTools = n()) %>%
                mutate(CumSumTools = cumsum(YearTools))
plot_df$Current_Year <- ifelse(plot_df$First_Version_Year == PLOT_YEAR, TRUE, FALSE)

if (!any(plot_df$Current_Year)) {
  cumsum_max <- max(plot_df$CumSumTools)
  plot_df <- plot_df %>% add_row(First_Version_Year = PLOT_YEAR,
                                 YearTools = 0,
                                 CumSumTools = cumsum_max,
                                 Current_Year = TRUE)
}

# Cumulative line: solid up to the last full year, dashed from there into the current (year to date) point.
last_full_year <- max(plot_df$First_Version_Year[!plot_df$Current_Year])
line_solid  <- plot_df[!plot_df$Current_Year, ]
line_dashed <- plot_df[plot_df$Current_Year | plot_df$First_Version_Year == last_full_year, ]

ggplot(plot_df, aes(x = First_Version_Year, fill = Current_Year)) +
  # year-to-date bar has a dashed outline (one linetype per bar, so no extra legend)
  geom_bar(stat = "identity", aes(y = YearTools), colour = "black", linewidth = OUTLINE_WIDTH,
           linetype = ifelse(plot_df$Current_Year, "dashed", "solid")) +
  geom_line(data = line_solid, aes(y = CumSumTools, group = 1, linetype = "Full year"), linewidth = 1) +
  geom_line(data = line_dashed, aes(y = CumSumTools, group = 1, linetype = "Year-to-date"), linewidth = 1) +
  geom_point(data = line_solid, aes(y = CumSumTools), size = 4, show.legend = FALSE) +
  geom_point(data = plot_df[plot_df$Current_Year, ], aes(y = CumSumTools),
             shape = 21, fill = "white", size = 4, stroke = 0.8, show.legend = FALSE) +   # open circle for the year-to-date point
  ylab("Number of tools") +
  xlab("Year") +
  # Counts sit inside every bar, close to the bottom (vjust = -0.25; it was -0.5) so that even the
  # one-tool bars have room for the number between their top and bottom outlines.
  geom_text(stat = "identity", aes(label = YearTools, y = YearTools), size = 5 * FONT_SCALE,
            position = position_stack(vjust = 0), vjust = -0.25, fontface = "bold") +
  scale_fill_manual(values = CURRENT_YEAR_COLOURS,
                    breaks = c("FALSE", "TRUE"),
                    labels = c("Full year", "Year-to-date"),
                    name = pad_title("New tools published", "Cumulative number of tools", 15 * FONT_SCALE)) +
  scale_linetype_manual(values = c("Full year" = "solid", "Year-to-date" = "dashed"),
                        name = "Cumulative number of tools") +
  guides(fill = guide_legend(order = 1), linetype = guide_legend(order = 2)) +
  scale_x_continuous(breaks = min(plot_df$First_Version_Year):max(plot_df$First_Version_Year)) +
  theme_ambilogue_bar() +
  theme(legend.position = "bottom",
        legend.box = "vertical",   # stack the two legends so they stay inside the plot area
        axis.text.x = element_text(angle = 0, hjust = 0.5)) -> tool_year_bar

tool_year_bar_grob <- set_panel_size(tool_year_bar, PAIR_PANEL_WIDTH, PAIR_PANEL_HEIGHT)
save_plot(tool_year_bar_grob, "tool_year_bar", width = PAIR_FIG_WIDTH, height = PAIR_FIG_HEIGHT)

# Date the tool plots were last drawn; plots.html reads this file.
writeLines(format(PLOT_DATE, "%Y-%m-%d"), file.path(out_dir, "tools_last_updated.txt"))

invisible(grDevices::graphics.off())
message("Done: figures are in ", normalizePath(out_dir))
