#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Plots for the Ambilogue citations (how often the methods are cited), drawn
# from the OpenAlex data written by citations_over_time.py.
#
# The method plots are drawn by plot_ambilogue.R.
#
# Input : citations_by_year_long.csv, citations_by_year_overall.csv, ../Ambilogue.csv (Publication_Status)
# Output: figures in --out (.png)
#
# Usage:
#   Rscript plot_citations.R
#   Rscript plot_citations.R --dir=path/to/csvs --out=path/to/figures --format=png,svg --csv=path/to/Ambilogue.csv
#
# Defaults: --dir = the folder containing this script, --out = <script folder>/plots,
#           --format = png.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(ggplot2)
  library(svglite)
  library(dplyr)
  library(ggtext)
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
in_dir   <- normalizePath(get_arg("dir", script_dir), mustWork = TRUE)
out_dir  <- get_arg("out", file.path(script_dir, "plots"))
formats  <- strsplit(get_arg("format", "png"), ",")[[1]]
csv_path <- get_arg("csv", file.path(script_dir, "..", "Ambilogue.csv"))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# DATA -------------------------------------------------------------------------

read_data <- function(name) {
  path <- file.path(in_dir, name)
  if (!file.exists(path)) stop("Missing ", path, " - run citations_over_time.py first.", call. = FALSE)
  data <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  data$Year_to_date <- as.logical(data$Year_to_date)
  data
}

# One row per method and year
long <- read_data("citations_by_year_long.csv")

# Preprint or Published for each method
ambilogue <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE,
                      fileEncoding = "UTF-8-BOM")

# One row per year, all methods together
overall <- read_data("citations_by_year_overall.csv")

# Written to citations_last_updated.txt (shown on plots.html), not drawn on the plots
RETRIEVED_DATE <- unique(long$Retrieved)[1]

all_years <- sort(unique(long$Year))
facet_years <- seq(min(all_years), max(all_years))

# The dashed lines start from the last full year
last_full_year <- if (any(long$Year_to_date)) max(long$Year[long$Year_to_date]) - 1 else Inf

# Methods in publication order (oldest first)
methods_df <- long %>%
  group_by(Method, Publication_Date, Publication_Year, Preprint_Date) %>%
  summarise(Total = sum(Citations), .groups = "drop") %>%
  arrange(as.Date(Publication_Date))

# Year of the preprint (if there was one) and the year the method first appeared
methods_df$Preprint_Year <- as.integer(format(as.Date(methods_df$Preprint_Date), "%Y"))
methods_df$First_Year <- pmin(methods_df$Preprint_Year, methods_df$Publication_Year, na.rm = TRUE)
methods_df$Still_preprint <- ambilogue$Publication_Status[match(methods_df$Method, ambilogue$Method)] == "Preprint"
methods_df$Still_preprint[is.na(methods_df$Still_preprint)] <- FALSE
# Preprint-only methods stay orange to the end of the plot
methods_df$Preprint_End <- ifelse(methods_df$Still_preprint, Inf, methods_df$Publication_Year - 0.5)
methods_df$Panel <- sprintf("%s\n(total=%s)", methods_df$Method,
                            format(methods_df$Total, big.mark = ",", trim = TRUE))
methods_df$Panel <- factor(methods_df$Panel, levels = methods_df$Panel)
long$Panel <- methods_df$Panel[match(long$Method, methods_df$Method)]

n_methods <- nrow(methods_df)
message("Read ", n_methods, " methods from ", in_dir)

# PLOT STYLING -----------------------------------------------------------------

CURRENT_YEAR_COLOURS <- c("TRUE" = "#9FBEE0", "FALSE" = "#2A6FBB")

UNIQUE_COLOUR <- "black"
SUM_COLOUR <- "#E57373"
METHOD_LINE_COLOUR <- "#222222"

UNIQUE_LABEL <- "Cumulative number of unique papers citing at least one methods"
SUM_LABEL <- "Cumulative total of method citations (double-count papers citing more than one tool)"

METHOD_LINE_LABEL <- "Full year"
BEFORE_PUBLICATION_LABEL <- "Before preprint or publication"
PREPRINT_LABEL <- "Preprint only"
YEAR_TO_DATE_LABEL <- "Year-to-date"

PREPRINT_COLOUR <- "#fdba74"

# Number of methods across in the plot of each method
METHOD_COLUMNS <- 4

theme_ambilogue_bar <- function(base_size = 11) {
  theme_linedraw(base_size = base_size) +
  theme(plot.title = element_markdown(face = "bold", size = 18, family = "arial"),
        plot.subtitle = element_markdown(face = "italic", size = 12, family = "arial"),
        legend.title = element_text(size = 15, family = "arial", face = "bold"),
        legend.text = element_text(size = 13, family = "arial"),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 16),
        axis.title = element_text(size = 18, family = "arial", face = "bold"),
        axis.text = element_text(size = 16, family = "arial"),
        panel.border          = element_blank(),
        panel.background      = element_blank(),
        panel.grid.major.x    = element_blank(),
        panel.grid.minor      = element_blank(),
        axis.line             = element_line(),
        legend.position = "bottom")
}

# Large text so the plot stays readable when shown at page width
theme_ambilogue_facet <- function(base_size = 11) {
  theme_ambilogue_bar(base_size) +
  theme(axis.text = element_text(size = 18, family = "arial"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 16),
        axis.title = element_text(size = 21, family = "arial", face = "bold"),
        strip.background = element_blank(),
        strip.text = element_text(size = 20, face = "bold", hjust = 0, family = "arial", colour = "black"),
        legend.title = element_text(size = 19, family = "arial", face = "bold"),
        legend.text = element_text(size = 19, family = "arial"),
        legend.key.size = unit(1.3, "lines"),
        legend.box = "vertical",
        panel.spacing = unit(1.4, "lines"))
}

# Only whole numbers on the y axis
whole_number_breaks <- function(limits) {
  breaks <- pretty(limits, n = 4)
  breaks[breaks == floor(breaks)]
}

format_number <- function(x) format(x, big.mark = ",")

save_plot <- function(p, name, width, height) {
  for (fmt in formats) {
    f <- file.path(out_dir, paste0(name, ".", fmt))
    ggsave(f, p, width = width, height = height, dpi = 200, bg = "white")
    message("Wrote ", f)
  }
}

# Cumulative line: solid up to the last full year, dashed from there into the
# year to date point (open circle). With a label the line gets a legend entry,
# with all_points every full year gets a dot too, and with one_legend the solid
# and dashed lines share a single legend (needs matching colour and linetype scales).
cumulative_layers <- function(data, column, colour, linewidth = 1, point_size = 2.5,
                              label = NULL, all_points = FALSE, one_legend = FALSE) {
  full_years <- data[!data$Year_to_date, ]
  if (is.null(label)) {
    solid_line <- geom_line(data = full_years, aes(y = .data[[column]]),
                            colour = colour, linewidth = linewidth)
  } else if (one_legend) {
    solid_line <- geom_line(data = full_years,
                            aes(y = .data[[column]], colour = label, linetype = label),
                            linewidth = linewidth)
  } else {
    solid_line <- geom_line(data = full_years, aes(y = .data[[column]], colour = label),
                            linewidth = linewidth)
  }
  year_to_date_data <- data[data$Year >= last_full_year, ]
  if (one_legend) {
    dashed_line <- geom_line(data = year_to_date_data,
                             aes(y = .data[[column]], colour = YEAR_TO_DATE_LABEL,
                                 linetype = YEAR_TO_DATE_LABEL),
                             linewidth = linewidth)
  } else {
    dashed_line <- geom_line(data = year_to_date_data,
                             aes(y = .data[[column]], linetype = YEAR_TO_DATE_LABEL),
                             colour = colour, linewidth = linewidth)
  }
  layers <- list(solid_line,
                 dashed_line,
                 geom_point(data = data[data$Year_to_date, ], aes(y = .data[[column]]),
                            shape = 21, fill = "white", colour = colour, size = point_size, stroke = 0.8))
  if (all_points) {
    layers <- c(layers, list(geom_point(data = full_years, aes(y = .data[[column]]),
                                        colour = colour, size = point_size)))
  }
  layers
}

# PLOTS ------------------------------------------------------------------------

# CITATIONS BY METHOD
plot_df <- long
plot_df$First_Year <- methods_df$First_Year[match(plot_df$Method, methods_df$Method)]

# Makes each y axis start at zero
axis_df <- aggregate(Cumulative_citations ~ Panel, plot_df, function(x) max(x, 1))

ggplot(plot_df, aes(x = Year)) +
  # Grey out the years before the preprint (or the publication if there was no preprint)
  geom_rect(data = methods_df, aes(xmin = -Inf, xmax = First_Year - 0.5, ymin = -Inf, ymax = Inf,
                                   fill = BEFORE_PUBLICATION_LABEL),
            alpha = 0.4, inherit.aes = FALSE) +
  # Pale orange for the years between the preprint and the publication (or to the end if not published)
  geom_rect(data = methods_df[methods_df$Still_preprint | methods_df$First_Year < methods_df$Publication_Year, ],
            aes(xmin = First_Year - 0.5, xmax = Preprint_End, ymin = -Inf, ymax = Inf,
                fill = PREPRINT_LABEL),
            alpha = 0.4, inherit.aes = FALSE) +
  # White underlay so the shading never shows through the bars
  geom_col(aes(y = Citations), fill = "white") +
  # Year-to-date bars are the same blue as in plot_ambilogue.R (#9FBEE0), drawn with alpha so
  # the shading can have its own legend; over the white underlay they look solid
  geom_col(aes(y = Citations, alpha = Year_to_date), fill = CURRENT_YEAR_COLOURS[["FALSE"]]) +
  # The cumulative line starts at the first year the method was available
  cumulative_layers(plot_df[which(plot_df$Year >= plot_df$First_Year), ],
                    "Cumulative_citations", METHOD_LINE_COLOUR, linewidth = 1.3,
                    point_size = 4, label = METHOD_LINE_LABEL, all_points = TRUE,
                    one_legend = TRUE) +
  geom_blank(data = axis_df, aes(y = Cumulative_citations), inherit.aes = FALSE) +
  scale_alpha_manual(values = c("FALSE" = 1, "TRUE" = 0.45),
                     labels = c("Full year", "Year-to-date"),
                     name = "New citations per year") +
  scale_fill_manual(values = setNames(c("grey70", PREPRINT_COLOUR),
                                      c(BEFORE_PUBLICATION_LABEL, PREPRINT_LABEL)),
                    name = "Publication status") +
  scale_colour_manual(values = setNames(rep(METHOD_LINE_COLOUR, 2), c(METHOD_LINE_LABEL, YEAR_TO_DATE_LABEL)),
                      name = "Cumulative number of citations") +
  scale_linetype_manual(values = setNames(c("solid", "22"), c(METHOD_LINE_LABEL, YEAR_TO_DATE_LABEL)),
                        name = "Cumulative number of citations") +
  guides(alpha = guide_legend(order = 1),
         fill = guide_legend(order = 2),
         colour = guide_legend(order = 3),
         linetype = guide_legend(order = 3)) +
  scale_x_continuous(breaks = facet_years) +
  scale_y_continuous(breaks = whole_number_breaks, expand = expansion(mult = c(0, 0.08))) +
  facet_wrap(~Panel, ncol = METHOD_COLUMNS, scales = "free_y", axes = "all_x") +   # x axis on every panel
  labs(x = "Year", y = "Number of citing papers") +
  theme_ambilogue_facet() -> citations_by_method

save_plot(citations_by_method, "citations_by_method", width=24,
          height=3.6 * ceiling(n_methods / METHOD_COLUMNS) + 3)

# CITATIONS OVERALL
plot_df <- overall
last_year_df <- plot_df[which.max(plot_df$Year), ]

ggplot(plot_df, aes(x = Year)) +
  geom_col(aes(y = Unique_citing_works, fill = Year_to_date)) +
  cumulative_layers(plot_df, "Cumulative_sum", SUM_COLOUR, point_size = 4,
                    label = SUM_LABEL, all_points = TRUE) +
  cumulative_layers(plot_df, "Cumulative_unique", UNIQUE_COLOUR, point_size = 4,
                    label = UNIQUE_LABEL, all_points = TRUE) +
  geom_text(data = last_year_df, aes(y = Cumulative_unique, label = format_number(Cumulative_unique)),
            colour = UNIQUE_COLOUR, size = 5, vjust = -1.2, fontface = "bold") +
  geom_text(data = last_year_df, aes(y = Cumulative_sum, label = format_number(Cumulative_sum)),
            colour = SUM_COLOUR, size = 5, vjust = -1.2, fontface = "bold") +
  ylab("Number of citing papers") +
  xlab("Year") +
  scale_fill_manual(values = CURRENT_YEAR_COLOURS,
                    breaks = c("FALSE", "TRUE"),
                    labels = c("Full year", "Year-to-date"),
                    name = "New unique citing papers") +
  scale_colour_manual(values = setNames(c(UNIQUE_COLOUR, SUM_COLOUR), c(UNIQUE_LABEL, SUM_LABEL)),
                      breaks = c(UNIQUE_LABEL, SUM_LABEL), name = "Cumulative number of citations") +
  scale_linetype_manual(values = setNames("22", YEAR_TO_DATE_LABEL), name = NULL) +
  guides(fill = guide_legend(order = 1),
         colour = guide_legend(order = 2, nrow = 2),
         linetype = guide_legend(order = 3, override.aes = list(colour = "grey40"))) +
  scale_x_continuous(breaks = all_years) +
  scale_y_continuous(labels = format_number, expand = expansion(mult = c(0, 0.14))) +
  theme_ambilogue_bar() +
  theme(legend.box = "vertical") -> citations_overall

save_plot(citations_overall, "citations_overall", width=12, height=8)

# Date the OpenAlex data was retrieved; plots.html reads this file.
writeLines(as.character(RETRIEVED_DATE), file.path(out_dir, "citations_last_updated.txt"))

message("Done: figures are in ", normalizePath(out_dir))
