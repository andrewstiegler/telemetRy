# Calculate AUC ------

pct_baseline <- function(data, time){
    data_baseline <- data %>% filter(InjTimeH < 0 &
                                         InjTimeH > (-1 * time)) %>%
        select(!any_of(c("InjTimeH", "InjTime", "mean", "se"))) %>%
        colMeans(na.rm = TRUE)
    
    data_only <- data %>% select(!any_of(c("InjTimeH", "InjTime", "mean", "se")))
    n_cols <- length(data_only)
    for (i in 1:length(data_only)){
        data_only[i] <- data_only[i] / data_baseline[[i]]
        i <- i + 1
    }
    data_pct_baseline <- cbind(data_only, 
          data %>% select(any_of(c("InjTimeH", "InjTime"))))
    data_n_col <- n_cols - (data_pct_baseline %>% 
                                select(!any_of(c("InjTimeH", "InjTime"))) %>% 
                                is.na() %>% rowSums())
    data_row_means <- data_pct_baseline %>% select(!any_of(c("InjTimeH", "InjTime"))) %>% rowMeans(na.rm = TRUE)
    data_row_se <- data_pct_baseline %>% select(!any_of(c("InjTimeH", "InjTime"))) %>% 
        as.matrix() %>% rowSds(na.rm = TRUE) / sqrt(data_n_col)
    data_pct_baseline$mean <- data_row_means
    data_pct_baseline$se <- data_row_se
    data_pct_baseline$n <- data_n_col
    return(data_pct_baseline)
}

ChAT_1mgkg_map_pct <- pct_baseline(ChAT_1mgkg_map, time = 0.5)

pct_plot <- function(data, time, xmin, xmax) {
    pct_data <- pct_baseline(data, time)
    
    pct_plot <- ggplot(pct_data)+
    geom_line(aes(x = InjTimeH, y = mean))+
    scale_x_continuous(limits = (c(xmin, xmax)))+
    geom_hline(yintercept = 1)+
        theme_bw(base_size = 14)
    return(pct_plot)
}

pct_plot(ChAT_1mgkg_map, time = 0.5, xmin = -0.5, xmax = 3)
pct_plot(V_map, time = 0.5, xmin = -0.5, xmax = 3)
pct_plot(PEG_1mgkg_map, time = 0.5, xmin = -0.5, xmax = 3)
pct_plot(LNAME_map, time = 0.5, xmin = -0.5, xmax = 3)
pct_plot(VPRE_map, time = 0.5, xmin = -0.5, xmax = 3)

auc_calc <- function(data, baseline, xmin = 0, xmax, label) {
    data_pct <- pct_baseline(data, baseline)
    
    data_avg <- filter(data_pct, InjTimeH > xmin &
                           InjTimeH < xmax) %>%
        dplyr::select(!any_of(c("InjTime", "InjTimeH", "mean", "se", "n")))
    data_avg <- data_avg - 1
    data_avg <- data_avg %>% colSums(na.rm = TRUE) %>% stack()
    data_avg$dose <- label
    data_avg$values <- -10 * data_avg$values
    return(data_avg)
}

ChAT_1mgkg_map_auc <- auc_calc(ChAT_1mgkg_map, baseline = 0.5, xmax = 3,
                               label = "ChAT 1 mg/kg")
ChAT_0p1_map_auc <- auc_calc(ChAT_0p1mgkg_map, baseline = 0.5, xmax = 3,
                               label = "ChAT 0.1 mg/kg")
V_map_auc <- auc_calc(V_map, baseline = 0.5, xmax = 3,
                               label = "Vehicle")
PEG_1mgkg_map_auc <- auc_calc(PEG_1mgkg_map, baseline = 0.5, xmax = 3,
                               label = "PEG-ChAT 1 mg/kg")
PEG_0p1_map_auc <- auc_calc(PEG_0p1mgkg_map, baseline = 0.5, xmax = 3,
                             label = "PEG-ChAT 0.1 mg/kg")
LNAME_map_auc <- auc_calc(LNAME_map, baseline = 0.1, xmax = 0.5,
                      label = "L-NAME+ChAT")
VPRE_map_auc <- auc_calc(VPRE_map, baseline = 0.1, xmax = 0.5,
                         label = "Vehicle+ChAT")


all_nlight_auc <- rbind(ChAT_1mgkg_map_auc, ChAT_0p1_map_auc, V_map_auc,
                      PEG_1mgkg_map_auc, PEG_0p1_map_auc,
                      LNAME_map_auc, VPRE_map_auc)
all_nlight_auc$dose <- factor(all_nlight_auc$dose, levels = 
                                c("Vehicle", "ChAT 0.1 mg/kg",
                                  "ChAT 1 mg/kg", "PEG-ChAT 0.1 mg/kg",
                                  "PEG-ChAT 1 mg/kg",
                                  "L-NAME+ChAT", "Vehicle+ChAT"))

ggplot(all_nlight_auc, aes(x = dose, y = values, color = dose))+
    stat_summary(size = 1.1)+
    geom_hline(yintercept = 0)+
    geom_sina(maxwidth = 0.25, alpha = 0.6)+
    xlab("")+
    scale_y_continuous(bquote(Delta~"MAP (AUC, % x s)"))+
    theme_classic(base_size = 14)


ggplot(filter(all_nlight_auc, dose == "Vehicle" |
                  dose == "ChAT 1 mg/kg" |
                  dose == "ChAT 0.1 mg/kg" |
                  dose == "PEG-ChAT 0.1 mg/kg" |
                  dose == "PEG-ChAT 1 mg/kg"),
       aes(x = dose, y = values, color = dose))+
    stat_summary(size = 1.1)+
    geom_hline(yintercept = 0)+
    geom_sina(maxwidth = 0.25, alpha = 0.6)+
    xlab("")+
    scale_y_continuous(bquote(Delta~"MAP (AUC, % x s)"))+
    theme_classic(base_size = 14)

ggplot(filter(all_nlight_auc, dose == "Vehicle+ChAT" |
                  dose == "L-NAME+ChAT"),
       aes(x = dose, y = values, color = dose))+
    stat_summary(size = 1.1)+
    geom_hline(yintercept = 0)+
    geom_sina(maxwidth = 0.25, alpha = 0.6)+
    xlab("")+
    scale_y_continuous(bquote(Delta~"MAP (AUC, % x s)"))+
    theme_classic(base_size = 14)

write.xlsx(all_nlight_auc, "auc_ChAT_injection.xlsx")

ChAT_rlight_1mgkg_map_auc <- auc_calc(ChAT_all_1mgkg_map, baseline = 0.5, xmax = 3,
                               label = "ChAT 1 mg/kg")
ChAT_rlight_0p1_map_auc <- auc_calc(ChAT_all_0p1_map, baseline = 0.5, xmax = 3,
                             label = "ChAT 0.1 mg/kg")
ChAT_V_rlight_map_auc <- auc_calc(ChAT_all_V_map, baseline = 0.5, xmax = 3,
                                  label = "Vehicle")
ChAT_rlight_1mgkg_map_auc <- ChAT_rlight_1mgkg_map_auc[which(ChAT_rlight_1mgkg_map_auc$values != 0),]
ChAT_rlight_0p1_map_auc <- ChAT_rlight_0p1_map_auc[which(ChAT_rlight_0p1_map_auc$values != 0),]

ChAT_V_rlight_map_auc$values <- ChAT_V_rlight_map_auc$values * 30
ChAT_rlight_1mgkg_map_auc$values <- ChAT_rlight_1mgkg_map_auc$values * 30
ChAT_rlight_0p1_map_auc$values <- ChAT_rlight_0p1_map_auc$values * 30


PEG_V_rlight_map_auc <- auc_calc(V_rlight_map, baseline = 0.5, xmax = 3,
                      label = "Vehicle")
PEG_rlight_1mgkg_map_auc <- auc_calc(PEG_1_rlight_map, baseline = 0.5, xmax = 3,
                              label = "PEG-ChAT 1 mg/kg")
PEG_rlight_1mgkg_map_auc <- PEG_rlight_1mgkg_map_auc[which(PEG_rlight_1mgkg_map_auc$values != 0),]
PEG_rlight_0p1_map_auc <- auc_calc(PEG_0p1_rlight_map, baseline = 0.5, xmax = 3,
                            label = "PEG-ChAT 0.1 mg/kg")

V_rlight_map_auc <- rbind(ChAT_V_rlight_map_auc, PEG_V_rlight_map_auc)
V_rlight_map_auc <- V_rlight_map_auc[which(V_rlight_map_auc$values != 0),]

all_rlight_auc <- rbind(V_rlight_map_auc, ChAT_rlight_0p1_map_auc,
                        ChAT_rlight_1mgkg_map_auc, PEG_rlight_0p1_map_auc,
                        PEG_rlight_1mgkg_map_auc)
all_rlight_auc$dose <- factor(all_rlight_auc$dose, levels = 
                                  c("Vehicle", "ChAT 0.1 mg/kg", "ChAT 1 mg/kg",
                                    "PEG-ChAT 0.1 mg/kg", "PEG-ChAT 1 mg/kg"))
aov(values ~ dose, all_rlight_auc) %>% glht(mcp = "Dunnett") %>% summary() %>% tidy()

ggplot(all_rlight_auc, aes(x = dose, y = values, color = dose))+
    stat_summary(size = 1.1)+
    geom_hline(yintercept = 0)+
    geom_sina(maxwidth = 0.25, alpha = 0.6)+
    xlab("")+
    scale_y_continuous(bquote(Delta~"MAP (AUC, % x s)"))+
    theme_classic(base_size = 14)

write.xlsx(all_rlight_auc, "auc_ChAT_injection_rlight.xlsx")
