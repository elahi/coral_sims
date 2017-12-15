################################################################################
##' @title Summarise simulated time series data
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2017-03-17
##' 
##' @log Add a log here
################################################################################

#rm(list=ls(all=TRUE))

source("2_summarise_data.R")
library(RColorBrewer)
library(gridExtra)
library(grid)
theme_set(theme_bw(base_size = 12))

plot_dat <- grand_means

## Get colors for scenarios
scenarios <- unique(plot_dat$scenario)
scenarios
cb_pal_scenario <- c("#D55E00", "#E69F00", "#56B4E9", "#009E73")

barplot(1:4, col = cb_pal_scenario)

names(cb_pal_scenario) <- scenarios
col_scale_scenario <- scale_colour_manual(name = "Model Scenario", values = cb_pal_scenario)
fill_scale_scenario <- scale_fill_manual(name = "Model Scenario", values = cb_pal_scenario)

##### DISTRIBUTION OF Z-SCORES #####

names(plot_dat)

plot_dat %>% 
  ggplot(aes(med_z)) + 
  geom_density(alpha = 0.5, fill = "black") + 
  labs(x = "Median z-score of coral cover", 
       y = "Probability density") + 
  theme(legend.position = c(1,1), legend.justification = c(1.25, 1.25)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_blank()) 
ggsave("figs_sims/density_all_sims.pdf", height = 3.5, width = 3.5)

plot_dat %>% 
  filter(scenario != "Stable") %>% 
  ggplot(aes(med_z)) + 
  geom_density(alpha = 0.5, fill = "black") + 
  labs(x = "Median z-score of coral cover", 
       y = "Probability density") + 
  theme(legend.position = c(1,1), legend.justification = c(1.25, 1.25)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_blank()) 
ggsave("figs_sims/density_all_sims_no_stable.pdf", height = 3.5, width = 3.5)

plot_dat %>% 
  ggplot(aes(med_z, fill = scenario)) + 
  geom_density(alpha = 0.5) + 
  labs(x = "Median z-score of coral cover", 
       y = "Probability density") + 
  scale_fill_manual(name = "Model Scenario", values = cb_pal_scenario) + 
  #theme(legend.position = c(1,1), legend.justification = c(1, 1)) + 
  theme(legend.position = "none") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        strip.background = element_blank()) + 
  facet_wrap(~ scenario)
ggsave("figs_sims/density_scenario.pdf", height = 7, width = 7)

##### OASIS Z PLOT #####

plot_dat %>% 
  ggplot(aes(med_z, cv, fill = scenario, shape = scenario)) + 
  xlab("Median z-score of coral cover") + 
  ylab("Temporal variability in coral cover (CV)") + 
  annotate("rect", xmin = -2, xmax = -1, ymin = 100, ymax = 150,
           alpha = 0, color = "gray", linetype = "solid") +
  annotate("rect", xmin = -2, xmax = -1, ymin = 50, ymax = 100,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = -2, xmax = -1, ymin = 0, ymax = 50,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = -1, xmax = -0, ymin = 50, ymax = 100,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = 0, xmax = 1, ymin = 50, ymax = 100,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = 1, xmax = 2, ymin = 0, ymax = 50,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 50,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = -1, xmax = 0, ymin = 0, ymax = 50,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = -1, xmax = -2, ymin = 0, ymax = 50,
           alpha = 0, color = "gray") +
  geom_point(alpha = 0.75, size = 5) + 
  theme(legend.position = c(1,1), legend.justification = c(1.25, 1.25)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_fill_manual(name = "Model Scenario", values = cb_pal_scenario) + 
  scale_shape_manual(name = "Model Scenario", values = c(21,22,23,25))

ggsave("figs_sims/oasis_z_plot_big.pdf", height = 5, width = 6)

##### PICK REPRESENTATIVE SITES #####

##' (stable positive)
##' (phase shift)
##' (stable negative)
##' (non-linear decline)

plot_dat %>% 
  ggplot(aes(med_z, cv, color = scenario)) + 
  xlab("Median z-score of coral cover") + 
  ylab("Temporal variability in coral cover (CV)") + 
  annotate("rect", xmin = -2, xmax = -1, ymin = 100, ymax = 150,
           alpha = 0, color = "gray", linetype = "solid") +
  annotate("rect", xmin = -2, xmax = -1, ymin = 50, ymax = 100,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = -2, xmax = -1, ymin = 0, ymax = 50,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = -1, xmax = -0, ymin = 50, ymax = 100,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = 0, xmax = 1, ymin = 50, ymax = 100,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = 1, xmax = 2, ymin = 0, ymax = 50,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 50,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = -1, xmax = 0, ymin = 0, ymax = 50,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = -1, xmax = -2, ymin = 0, ymax = 50,
           alpha = 0, color = "gray") +
  geom_point(alpha = 0.75) + 
  theme(legend.position = c(1,1), legend.justification = c(1.25, 1.25)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_color_manual(name = "Model Scenario", values = cb_pal_scenario) + 
  geom_text(data = plot_dat,  
            aes(label = sim_total), size = 2.5, check_overlap = FALSE, 
            vjust = 0, nudge_y = 3)

## Manually select time-series for each box
## (choosing one haphazardly for each scenario in the middle of boxes)
box1 = c(229, 121, 395)
box2 = c(226, 135, 384)
box3 = c(113, 373)
box4 = c(124, 302)
box5 = c(5, 246, 176, 326)
box6 = c(50, 201, 116, 342)
box7 = c(78, 237, 195, 324)
box8 = c(92, 216, 137, 314)

## Get needed items for the loop
box_list <- list(box1, box2, box3, box4, 
                 box5, box6, box7, box8)

box_names <- c("box1", "box2", "box3", "box4", 
               "box5", "box6", "box7", "box8")

box_names_length <- length(box_list)
box_results <- vector("list", box_names_length)

for(i in 1:box_names_length){
  
  box_i <- box_list[[i]]
  box_name_i <- box_names[i]
  box_i_df <- sim_df[sim_df$sim_total %in% box_i, ]
  box_i_df$box <- box_name_i
  
  plot_box <- box_i_df %>% 
    ggplot(aes(year, y)) + 
    ylab("coral cover (%)") + xlab("year") + 
    scale_y_continuous(limits = c(0, 60)) + 
    theme(legend.position = "none") + 
    geom_line(aes(year, y_mean), alpha = 1, size = 1, 
              data = mean_cover_df, color = "black", linetype = "dashed") + 
    geom_line(alpha = 0.8, aes(group = sim2, color = scenario), 
              size = 1.25) + 
    col_scale_scenario + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  box_results[[i]] <- plot_box
}

##### USE GRID ARRANGE TO CREATE PANEL B #####

hlay <- rbind(c(1,NA,NA,NA),
              c(2,3,4,NA),
              c(5,6,7,8))

pdf("figs_sims/box_time_series_panels.pdf", 
    height = 5, width = 7)
grid.arrange(grobs = box_results, layout_matrix = hlay)
dev.off()


##### ILLUSTRATING THE METHOD #####

## Relevel scenarios 
sim_df2 <- sim_df %>%
  mutate(scenario = factor(scenario,
                           levels = c("Stable", "Linear",
                                      "Phase_shift", "Oscillations")))

scenarios
text_df <- data.frame(scenario = scenarios, 
                      cc = c("b ~ N(40, 5) | U(40, 5)", 
                             "b ~ N(40, 5) | U(40, 5); bnew ~ U(0.5b, 1)", 
                             "b ~ N(40, 5) | U(40, 5)", 
                             "b ~ N(30, 5) | U(30, 5)"), 
                      cc_sd = c("cc_sd ~ N(0.1b, 0.5)"), 
                      w = c(as.character(expression(paste(w[i], " ~ N(0, cc_sd)")))), 
                                #"w ~ N(0.1 b, 0.5); wnew ~ N(w, 0.1)", 
                                #"w ~ N(0.1 b, 0.5)", 
                                #"w ~ N(0.1 b, 0.5)"), 
                      equation = c(as.character(expression(paste(y[i], " = b + ", w[i]))), 
                                   "y = b + w; ynew = bnew + wnew", 
                                   "y = mx + b + w", 
                                   as.character(expression(paste(y[i], " = b + ", w[i])))), 
                      extras = c("", 
                                 "",
                                 "m ~ N(-0.5, 0.25)", 
                                 paste("c ~ N(0, 0.25)", "a ~ N(15, 7.5)", "p ~ N(40, 5)", sep = ";") 
                                 ))

text_df

sim_df %>% 
  #filter(sim < 2) %>% 
  ggplot(aes(year, y, color = scenario)) + 
  theme_bw(base_size = 16) + 
  ylab("Coral cover (%)") + 
  xlab("Year") + 
  scale_y_continuous(limits = c(0, 80)) + 
  theme(legend.position = "none") + 
  geom_line(alpha = 0.6, aes(group = sim, color = scenario), 
            size = 0.5) + 
  facet_wrap(~ scenario) + 
  col_scale_scenario + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(strip.background = element_blank()) 
  # geom_text(data = text_df, aes(x = 1, y = 99, label = equation), hjust = 0, parse = T) + 
  # geom_text(data = text_df, aes(x = 1, y = 89, label = cc), hjust = 0, parse = F) + 
  # geom_text(data = text_df, aes(x = 1, y = 79, label = cc_sd), hjust = 0, parse = F) + 
  # geom_text(data = text_df, aes(x = 1, y = 69, label = extras), hjust = 0)

ggsave("figs_sims/scenario_examples.pdf", height = 7, width = 7)
