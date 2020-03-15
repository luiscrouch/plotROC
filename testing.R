
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(plotROC)

# Generate example --------------------------------------------------------

D.ex <- rbinom(50, 1, .5)
rocdata <- data.frame(D = rep(D.ex, 4), 
                      M = c(
                        rnorm(50, mean = D.ex, sd = .4),
                        rnorm(50, mean = D.ex, sd = 1),
                        -rnorm(50, mean = D.ex, sd = .4),
                        -rnorm(50, mean = D.ex, sd = 1)
                        ),
                      noise_level = c(
                        rep("clean",25),
                        rep("noisy",25),
                        rep("clean",25),
                        rep("noisy",25)
                        ),
                      flipped = c(
                        rep("no", 100),
                        rep("yes", 100)
                        )
                      )

rocplot <- ggplot(rocdata, aes(m = M, d = D,col = noise_level, lty = flipped)) + geom_roc()

my_auc <- calc_auc(rocplot)

l1 <- ggplot_build(rocplot)$data[[1]]
l2 <- ggplot_build(rocplot)

aesthetics_used <- names(l1)[1:(which(names(l1) == "x") - 1)]

aesthetic_names <- rep(NA, length(aesthetics_used))

for (i in 1:length(aesthetics_used)) {
  current_name <- aesthetics_used[i]
  aesthetic_names[i] <- as.character(rlang::quo_get_expr(l2$plot$mapping[current_name][[1]]))
}

group_labels <- unique(l2$plot$data[, aesthetic_names])

group_labels <- group_labels[do.call(order, group_labels), ]

group_mapping <- cbind("group" = unique(l1$group),  group_labels)

merge(my_auc, group_mapping)

# Example fix -------------------------------------------------------------

calc_auc <- function(ggroc)
{
  lays <- sapply(ggroc$layers, function(g) class(g$geom)[1])
  stopifnot("GeomRoc" %in% lays)
  l1 <- ggplot_build(ggroc)$data[[1]]
  l2 <- ggplot_build(ggroc)
  comp_auc <- function(df) {
    auc <- 0
    for (i in 2:length(df$x)) {
      auc <- auc + 0.5 * (df$x[i] - df$x[i - 1]) * (df$y[i] + df$y[i - 1])
    }
    return(data.frame(AUC = auc))
  }
  auc_output <- plyr::ddply(l1, ~PANEL + group, comp_auc)
  
  # get panel (facet) names
  roc_layout <- l2$layout$layout
  drop_names <- c("ROW", "COL", "SCALE_X", "SCALE_Y")
  panel_names <- names(roc_layout)[!(names(roc_layout) %in% c("PANEL", drop_names))]
  auc_output_panels <- merge(
    auc_output,
    roc_layout[, !(names(roc_layout) %in% drop_names)]
    )
  
  # get group (aesthetic) names
  if (which(names(l1) == "x") == 1) {
    auc_output_panels_groups <- auc_output_panels
    
    # format final output
    formatted_output <- auc_output_panels_groups[
      order(auc_output_panels_groups$PANEL, auc_output_panels_groups$group),
      c("PANEL",panel_names,"group","AUC")
      ]
    formatted_output
  } else {
    aesthetics_used <- names(l1)[1:(which(names(l1) == "x") - 1)]
    aesthetic_names <- rep(NA, length(aesthetics_used))
    for (i in 1:length(aesthetics_used)) {
      current_name <- aesthetics_used[i]
      aesthetic_names[i] <- as.character(rlang::quo_get_expr(l2$plot$mapping[current_name][[1]]))
    }
    group_df <- data.frame(l2$plot$data[, aesthetic_names])
    group_labels <- unique(group_df)
    group_labels <- data.frame(group_labels[do.call(order, group_labels), ])
    names(group_labels) <- aesthetic_names
    group_mapping <- cbind("group" = unique(l1$group),  group_labels)
    auc_output_panels_groups <- merge(auc_output_panels, group_mapping)
    
    # format final output
    formatted_output <- auc_output_panels_groups[
      order(auc_output_panels_groups$PANEL, auc_output_panels_groups$group),
      c("PANEL",panel_names,"group",aesthetic_names,"AUC")
      ]
    formatted_output
  }
  
}


# Test 1 ------------------------------------------------------------------

D.ex <- rbinom(50, 1, .5)
rocdata <- data.frame(D = c(D.ex, D.ex), 
                      M = c(rnorm(50, mean = D.ex, sd = .4), rnorm(50, mean = D.ex, sd = 1)),
                      Y = rep(paste0("Y",1:4),25),
                      Z = c(rep("A", 50), rep("B", 50)))

rocplot <- ggplot(rocdata, aes(m = M, d = D, col = Y)) + geom_roc() + facet_wrap(~ Z)

rocplot
calc_auc(rocplot)

# Test 2 ------------------------------------------------------------------

D.ex <- rbinom(50, 1, .5)
rocdata <- data.frame(D = rep(D.ex, 4), 
                      M = c(
                        rnorm(50, mean = D.ex, sd = .4),
                        rnorm(50, mean = D.ex, sd = 1),
                        -rnorm(50, mean = D.ex, sd = .4),
                        -rnorm(50, mean = D.ex, sd = 1)
                      ),
                      noise_level = c(
                        rep("clean",50),
                        rep("noisy",50),
                        rep("clean",50),
                        rep("noisy",50)
                      ),
                      flipped = c(
                        rep("no", 100),
                        rep("yes", 100)
                      )
)

rocplot <- ggplot(rocdata, aes(m = M, d = D,col = noise_level, lty = flipped)) + geom_roc()

rocplot
calc_auc(rocplot)

# Test 3 ------------------------------------------------------------------

D.ex <- rbinom(50, 1, .5)
rocdata_factor <- data.frame(D = rep(D.ex, 4), 
                      M = c(
                        rnorm(50, mean = D.ex, sd = .4),
                        rnorm(50, mean = D.ex, sd = 1),
                        -rnorm(50, mean = D.ex, sd = .4),
                        -rnorm(50, mean = D.ex, sd = 1)
                      ),
                      noise_level = factor(c(
                        rep("clean",50),
                        rep("noisy",50),
                        rep("clean",50),
                        rep("noisy",50)
                      ), levels = c("noisy","clean")),
                      flipped = c(
                        rep("no", 100),
                        rep("yes", 100)
                      )
)

rocplot_factor <- ggplot(rocdata_factor, aes(m = M, d = D,col = noise_level, lty = flipped)) + geom_roc()

rocplot_factor
calc_auc(rocplot_factor)

# Test 4 ------------------------------------------------------------------

D.ex <- rbinom(50, 1, .5)
rocdata <- data.frame(D = rep(D.ex, 4), 
                      M = rnorm(50, mean = D.ex, sd = 1)
)

rocplot <- ggplot(rocdata, aes(m = M, d = D)) + geom_roc()

rocplot
calc_auc(rocplot)
