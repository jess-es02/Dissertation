#7) Prioritising stations based on accessibility disparities (equity analysis)

#In this file we:
  # - Characterise inaccessible station catchments based on accessibility disparity (travel time and cumulative opportunity) and presence of in-need population
  # - Decide the 8 stations to be upgraded under the equity scenario
  # - Compare catchment characteristics to TfL's chosen stations

#And maybe to the network stations - not sure yet!

#Beforehand, ensure to run files 3-6

library(GGally)

# ---- Combine Variables and EDA ------

cluster_vars <- fastest_time_to_stations %>%
  dplyr::select(lsoa21cd, lsoa21nm, fastest_station, ratioCP, ratioSLOW)%>%
  rename("time_ratioCP" = ratioCP,
         "time_ratioSLOW" = ratioSLOW)%>%
  left_join(jobs_in_45_min%>%
              st_drop_geometry()%>%
              dplyr::select(lsoa21cd, ratioCP, ratioSLOW, step_free_benefit_indexW)%>%
              rename("job_ratioCP" = ratioCP,
                     "job_ratioSLOW" = ratioSLOW), 
            by="lsoa21cd")
st_write(cluster_vars, "data_export_vis/cluster_vars.gpkg")
#cluster_vars <- st_read("data_export_vis/cluster_vars.gpkg")

#Invert job ratios, for more intuitive analysis
#Higher values = more inequality
cluster_vars <- cluster_vars %>%
  mutate(inv_job_ratioCP=1/job_ratioCP,
         inv_job_ratioSLOW=1/job_ratioSLOW)

#Lots of infinities in the slow ratio, due to 0 values - let's manually cap at the highest ratio with a non-zero job value
ratio_cap <- jobs_in_45_min %>%
  filter(jobs_accessible_SLOW>0)%>%
  inner_join(cluster_vars%>%st_drop_geometry(), by="lsoa21cd")%>%
  summarise(max_ratio=max(inv_job_ratioSLOW))%>%
  pull(max_ratio)
cluster_vars <- cluster_vars %>%
  mutate(inv_job_ratioSLOW = ifelse(is.infinite(inv_job_ratioSLOW), ratio_cap, inv_job_ratioSLOW))
hist(cluster_vars$inv_job_ratioSLOW) #not ideal, but at least we are incorporating these values
rm(ratio_cap)

#Quick explore correlations
cor.test(cluster_vars$time_ratioCP, cluster_vars$step_free_benefit_indexW)
cor.test(cluster_vars$time_ratioSLOW, cluster_vars$step_free_benefit_indexW)
cor.test(cluster_vars$inv_job_ratioCP, cluster_vars$step_free_benefit_indexW)
cor.test(cluster_vars$inv_job_ratioSLOW, cluster_vars$step_free_benefit_indexW)
cor.test(cluster_vars$inv_job_ratioCP, cluster_vars$time_ratioCP)
cor.test(cluster_vars$inv_job_ratioSLOW, cluster_vars$time_ratioSLOW)

ggplot(cluster_vars, aes(inv_job_ratioCP, time_ratioCP)) +
  geom_point(alpha = 0.25)
ggplot(cluster_vars, aes(inv_job_ratioCP, step_free_benefit_indexW)) +
  geom_point(alpha = 0.25)
ggplot(cluster_vars, aes(time_ratioCP, step_free_benefit_indexW)) +
  geom_point(alpha = 0.25)
ggplot(cluster_vars, aes(inv_job_ratioSLOW, time_ratioSLOW)) +
  geom_point(alpha = 0.25)
ggplot(cluster_vars, aes(inv_job_ratioSLOW, step_free_benefit_indexW)) +
  geom_point(alpha = 0.25)
ggplot(cluster_vars, aes(time_ratioSLOW, step_free_benefit_indexW)) +
  geom_point(alpha = 0.25)
#Slow ratios look "better" distributed, but job ratio potentially means less because of a bias towards larger LSOAs?

#Bivariate maps of accessibility variables (others in files 5-6)

#Need to add some slight noise to the data so we can add quantiles
bivariate_data <- cluster_vars %>%
  dplyr::select(lsoa21cd, time_ratioCP, time_ratioSLOW, inv_job_ratioCP, inv_job_ratioSLOW)%>%
  mutate(inv_job_ratioCP_jitter = jitter(inv_job_ratioCP, amount = 1e-6),
         inv_job_ratioSLOW_jitter = jitter(inv_job_ratioSLOW, amount = 1e-6),
         time_ratioCP_jitter=jitter(time_ratioCP, amount = 1e-6))

bi_data <- bi_class(bivariate_data, x = inv_job_ratioCP_jitter, y = time_ratioCP_jitter, style = "quantile", dim = 4)
pal <- bi_pal("GrPink2", dim = 4, preview = FALSE)
bi_classes <- names(pal)
tmap_save(
  tm_shape(bi_data) +
    tm_polygons("bi_class",
                palette = pal,
                border.alpha = 0,
                legend.show = FALSE) +
    tm_shape(boroughs)+
    tm_polygons(lwd=1, fill=NA, alpha=0)+
    tm_compass(type = "8star",
               size = 3,
               position = c(0.9, 0.22)) +
    tm_scalebar(
      position = c(0.82, 0.08),
      text.size = 0.7,
      breaks = c(0, 5, 10)) +
    tm_title("Job Accessibility Disparity versus Time to Accessible Station Disparity")+
    tm_layout(
      title.fontfamily = "Segoe UI Semibold",
      title.size = 1.2,
      bg.color = "grey70"),
  filename = "maps/bivariate_accessibility_choropleth_CP.png",
  dpi = 300)
legend <- bi_legend(
  pal = "GrPink2",
  dim = 4,
  xlab = "Higher Job Disparity",
  ylab = "Higher Time Disparity")+
  theme(
    text = element_text(family = "Segoe UI", size = 7))
ggsave(filename = "maps/bivariate_legend_accessibility.png",
       plot = legend, dpi = 300, bg = "white")
#We can see it pulling characteristics of destinations - e.g. Met Line, lower accessibility near accessible stations because destinations are inaccessible
#So redder areas are typically those negatively affected by destination characteristics

bi_data <- bi_class(bivariate_data, x = inv_job_ratioSLOW_jitter, y = time_ratioSLOW, style = "quantile", dim = 4)
pal <- bi_pal("GrPink2", dim = 4, preview = FALSE)
bi_classes <- names(pal)
tmap_save(
  tm_shape(bi_data) +
    tm_polygons("bi_class",
                palette = pal,
                border.alpha = 0,
                legend.show = FALSE) +
    tm_shape(boroughs)+
    tm_polygons(lwd=1, fill=NA, alpha=0)+
    tm_compass(type = "8star",
               size = 3,
               position = c(0.9, 0.22)) +
    tm_scalebar(
      position = c(0.82, 0.08),
      text.size = 0.7,
      breaks = c(0, 5, 10)) +
    tm_title("Job Accessibility Disparity versus Travel to Accessible Station Disparity,\nSlower Walking Speed")+
    tm_layout(
      title.fontfamily = "Segoe UI Semibold",
      title.size = 1.2,
      bg.color = "grey70"),
  filename = "maps/bivariate_accessibility_choropleth_SLOW.png",
  dpi = 300)
#Useful because blue now picks out remote areas with high time disparity, but limited job access anyway

rm(bivariate_data, bivariate_data, bi_data, pal, bi_classes, legend)

cluster_vars <- cluster_vars %>% #quick fix
  mutate(inv_job_ratioCP=if_else(inv_job_ratioCP<1, 1, inv_job_ratioCP))

# ---- Clustering -----
#https://www.datacamp.com/tutorial/hierarchical-clustering-R

#We will do hierarchal clustering as k-means/medoids assumes circular clusters
#For now, I'm going to use ratioCP, because the ratioSLOW values for job accessibility seem biased towards larger LSOAs (and thus reveal less about station impacts)

#Explore distributions
cluster_vars_df <- cluster_vars%>%st_drop_geometry()%>%rename("step_free_pop_index"=step_free_benefit_indexW,
                                                              "time_ratio" = time_ratioCP,
                                          "inverse_job_ratio"=inv_job_ratioCP)
ggpairs(cluster_vars_df[, c("time_ratio", "inverse_job_ratio", "step_free_pop_index")],
        upper = list(continuous = "points"),
        diag = list(continuous = "densityDiag"),
        lower = list(continuous = "smooth"))+
  labs(title = "Distribution of Equity-Based Variables") +
  theme_minimal(base_family = "Segoe UI")+
  theme(
    plot.title = element_text(family = "Segoe UI Semibold", size = 16, hjust=0.5))
#Index is normally distributed
#Both accessibility ratios are positively skewed

symbox(~as.numeric(inv_job_ratioCP), cluster_vars, na.rm=T, powers=seq(-3, 3, by=.5))
symbox(~as.numeric(time_ratioCP), cluster_vars, na.rm=T, powers=seq(-3, 3, by=.5))
#None are great!
#Box-Cox also not great

#Try a rank-based normal transformation (chatGPT helped here)
inverse_normal_transform <- function(x) {
  ranks <- rank(x, ties.method = "average")
  p <- ranks / (length(x) + 1)
  qnorm(p)
}
cluster_vars$inv_job_ratioCP_rank <- inverse_normal_transform(cluster_vars$inv_job_ratioCP)
cluster_vars$time_ratioCP_rank <- inverse_normal_transform(cluster_vars$time_ratioCP)
#I actually think rank-based works well because some ratios are insane; 10x is still bad, even though it's far from 1000x

cluster_vars_df <- cluster_vars%>%st_drop_geometry()%>%rename("step_free_pop_index"=step_free_benefit_indexW,
                                                              "time_ratio_rank" = time_ratioCP_rank,
                                                              "inverse_job_ratio_rank"=inv_job_ratioCP_rank)
ggpairs(cluster_vars_df[, c("time_ratio_rank", "inverse_job_ratio_rank", "step_free_pop_index")],
        upper = list(continuous = "points", alpha=0.5),
        diag = list(continuous = "densityDiag"),
        lower = list(continuous = "smooth"))+
  labs(title = "Transformed Distribution of Equity-Based Variables") +
  theme_minimal(base_family = "Segoe UI")+
  theme(
    plot.title = element_text(family = "Segoe UI Semibold", size = 16, hjust=0.5))

#Now let's standardise them so the scales are the same
cluster_vars_numeric_scaled <- cluster_vars %>%
  dplyr::select(where(is.numeric))%>%
  st_drop_geometry()%>%
  scale()
cluster_vars_scaled <- cluster_vars %>% #Reattach to ID
  dplyr::select(lsoa21cd)%>%
  st_drop_geometry()%>%
  bind_cols(as.data.frame(cluster_vars_numeric_scaled))%>%
  dplyr::select(lsoa21cd, step_free_benefit_indexW, inv_job_ratioCP_rank, time_ratioCP_rank)

ggplot(cluster_vars_scaled, aes(inv_job_ratioCP_rank, step_free_benefit_indexW)) +
  geom_point(alpha = 0.25)
ggplot(cluster_vars_scaled, aes(time_ratioCP_rank, step_free_benefit_indexW)) +
  geom_point(alpha = 0.25)
ggplot(cluster_vars_scaled, aes(time_ratioCP_rank, inv_job_ratioCP_rank)) +
  geom_point(alpha = 0.25)

#Calculate distances and cluster
set.seed(10)
dist_mat <- dist(cluster_vars_scaled %>% dplyr::select(where(is.numeric)), method = "euclidean")
hc <- hclust(dist_mat, method = 'complete') #only way to get defined clusters

#Silhouette scores
max_k <- 10
avg_sil <- numeric(max_k)

for (k in 2:max_k) {
  clusters <- cutree(hc, k)
  sil <- silhouette(clusters, dist_mat)
  avg_sil[k] <- mean(sil[, 3])}

#Plot
sil_data <- data.frame(
  k = 2:max_k,
  avg_sil = avg_sil[2:max_k])
ggplot(sil_data, aes(x = k, y = avg_sil)) +
  geom_point(color = "deeppink3", size = 2) +        
  geom_line(color = "deeppink3", size=0.5, linetype="dashed") + 
  scale_x_continuous(breaks = 2:max_k)+
  labs(
    title = "Cluster Silhouette Analysis",
    x = "Number of clusters",
    y = "Average silhouette width") +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Segoe UI Semibold", size = 16, hjust = 0.5),
    axis.title = element_text(family = "Segoe UI Semibold", size = 12),
    axis.text = element_text(family = "Segoe UI", size = 10))

tree_cut <- cutree(hc, k = 7)

#Plot dendrogram
hc_obj <- as.dendrogram(hc)
dend_plot <- color_branches(hc_obj, k=7)
plot(dend_plot)

#Add back to data
cluster_vars <- mutate(cluster_vars, cluster = tree_cut)
table(cluster_vars$cluster)

#Use Dark2 palette but change to make it clearer when mapped
cols_changed <- c(
  "1" = "#f7df28",
  "2" = "#7570b3",
  "3" = "#ff931f",
  "4" = "#f589c0",
  "5" = "#66a61e",
  "6" = "#1b9e77",
  "7" = "#6bbfed")
cluster_vars$cluster <- factor(cluster_vars$cluster)

#Map clusters
tmap_save(
  tm_shape(cluster_vars) +
    tm_polygons(
      col = "cluster",
      palette=cols_changed,
      title = "Cluster",
      textNA = "",
      alpha=0.8,
      border.alpha=0) +
    tm_shape(boroughs)+
    tm_polygons(fill=NA, alpha=0, lwd=1.5)+
    tm_basemap("Esri.OceanBasemap") +
    tm_title("LSOAs by Accessibility Need Cluster") +
    tm_compass(type = "8star",
               size = 3,
               position = c(0.9, 0.22)) +
    tm_scalebar(
      position = c(0.82, 0.08),
      text.size = 0.7,
      breaks = c(0, 5, 10)
    ) +
    tm_layout(
      legend.position = c(0.01, 0.38),
      legend.bg.color = "white",
      legend.showNA = FALSE,
      title.fontfamily = "Segoe UI Semibold",
      title.size = 1.6,
      legend.text.fontfamily = "Segoe UI",
      legend.title.fontfamily = "Segoe UI Semibold",
      legend.text.size = 0.8,
      legend.title.size = 0.9),
  filename = "maps/clusters.png",
  dpi=300)

#Plots - I am sure we could facet these, but it's probably clearer individually

#Transformed variables
ggplot(cluster_vars, aes(x=inv_job_ratioCP_rank, y=step_free_benefit_indexW, color = factor(cluster)))+
  geom_point()+
  labs(title = "Transformed Cluster Output: Population and Job Accessibility",
       x = "Rank-Based Inverse Normal Job Accessibility Ratio",
       y = "In-Need Population Index",
       color = "Cluster")+
  theme_minimal() +
  scale_color_manual(values = cols_changed) +
  theme(
    plot.title = element_text(family = "Segoe UI Semibold", size = 16, hjust=0.5),
    axis.title = element_text(family = "Segoe UI Semibold", size=10),
    axis.text = element_text(family = "Segoe UI", size=9),
    legend.title = element_text(family = "Segoe UI Semibold", size = 10),
    legend.text = element_text(family = "Segoe UI", size = 9))
ggplot(cluster_vars, aes(x=time_ratioCP_rank, y=step_free_benefit_indexW, color = factor(cluster)))+
  geom_point()+
  labs(title = "Transformed Cluster Output: Population and Station Accessibility",
       x = "Rank-Based Inverse Normal Station Travel Time Ratio",
       y = "In-Need Population Index",
       color = "Cluster")+
  theme_minimal() +
  scale_color_manual(values = cols_changed) +
  theme(
    plot.title = element_text(family = "Segoe UI Semibold", size = 16, hjust=0.5),
    axis.title = element_text(family = "Segoe UI Semibold", size=10),
    axis.text = element_text(family = "Segoe UI", size=9),
    legend.title = element_text(family = "Segoe UI Semibold", size = 10),
    legend.text = element_text(family = "Segoe UI", size = 9))
ggplot(cluster_vars, aes(x=time_ratioCP_rank, y=inv_job_ratioCP_rank, color = factor(cluster)))+
  geom_point()+
  labs(title = "Transformed Cluster Output: Accessibility Ratios",
       x = "Rank-Based Inverse Normal Station Travel Time Ratio",
       y = "Rank-Based Inverse Normal Job Accessibility Ratio",
       color = "Cluster")+
  theme_minimal() +
  scale_color_manual(values = cols_changed) +
  theme(
    plot.title = element_text(family = "Segoe UI Semibold", size = 16, hjust=0.5),
    axis.title = element_text(family = "Segoe UI Semibold", size=10),
    axis.text = element_text(family = "Segoe UI", size=9),
    legend.title = element_text(family = "Segoe UI Semibold", size = 10),
    legend.text = element_text(family = "Segoe UI", size = 9))

#Original Variables
ggplot(cluster_vars, aes(x=inv_job_ratioCP, y=step_free_benefit_indexW, color = factor(cluster)))+
  geom_point()+
  labs(title = "Untransformed Cluster Output: Population and Job Accessibility",
       x = "Job Accessibility Ratio",
       y = "In-Need Population Index",
       color = "Cluster")+
  theme_minimal() +
  scale_color_manual(values = cols_changed) +
  theme(
    plot.title = element_text(family = "Segoe UI Semibold", size = 16, hjust=0.5),
    axis.title = element_text(family = "Segoe UI Semibold", size=10),
    axis.text = element_text(family = "Segoe UI", size=9),
    legend.title = element_text(family = "Segoe UI Semibold", size = 10),
    legend.text = element_text(family = "Segoe UI", size = 9))
ggplot(cluster_vars, aes(x=time_ratioCP, y=step_free_benefit_indexW, color = factor(cluster)))+
  geom_point()+
  labs(title = "Untransformed Cluster Output: Population and Station Accessibility",
       x = "Accessible Station Travel Time Ratio",
       y = "In-Need Population Index",
       color = "Cluster")+
  theme_minimal() +
  scale_color_manual(values = cols_changed) +
  theme(
    plot.title = element_text(family = "Segoe UI Semibold", size = 16, hjust=0.5),
    axis.title = element_text(family = "Segoe UI Semibold", size=10),
    axis.text = element_text(family = "Segoe UI", size=9),
    legend.title = element_text(family = "Segoe UI Semibold", size = 10),
    legend.text = element_text(family = "Segoe UI", size = 9))
ggplot(cluster_vars, aes(x=time_ratioCP, y=inv_job_ratioCP, color = factor(cluster)))+
  geom_point()+
  labs(title = "Untransformed Cluster Output: Accessibility Ratios",
       x = "Accessible Station Travel Time Ratio",
       y = "Job Accessibility Ratio",
       color = "Cluster")+
  theme_minimal() +
  scale_color_manual(values = cols_changed) +
  theme(
    plot.title = element_text(family = "Segoe UI Semibold", size = 16, hjust=0.5),
    axis.title = element_text(family = "Segoe UI Semibold", size=10),
    axis.text = element_text(family = "Segoe UI", size=9),
    legend.title = element_text(family = "Segoe UI Semibold", size = 10),
    legend.text = element_text(family = "Segoe UI", size = 9))

#Cluster descriptions:
  #1: high for all variables -> priority 
  #2: average for all variables -> non-priority
  #3: average-high for in-need population, average-high for job, high for travel time -> second priority
  #4: above average for in-need population, average-high ratios -> probably 3rd priority
    #basically they would benefit from upgrades, but generally lower job access here anyway - less impact
  #5: high-in-need population, lower accessibility ratios
  #6: low in-need population, lower accessibility ratios -> lowest priority
  #7: low in-need population, higher accessibility ratios

rm(cluster_vars_numeric_scaled, cluster_vars_df, cluster_vars_scaled, hc, hc_obj, sil_data, clusters, dist_mat, k, max_k, sil, tree_cut, inverse_normal_transform, avg_sil, dend_plot)
#st_write(cluster_vars, "data_export_vis/clusters.gpkg")
#cluster_vars <- st_read("data_export_vis/clusters.gpkg")

#Brief summary of attributes, by cluster (or cluster group)

#Time
summary(cluster_vars$time_ratioCP[cluster_vars$cluster %in% c(1)])
summary(cluster_vars$time_ratioCP[cluster_vars$cluster %in% c(2)])
summary(cluster_vars$time_ratioCP[cluster_vars$cluster %in% c(3)])
summary(cluster_vars$time_ratioCP[cluster_vars$cluster %in% c(4)])
summary(cluster_vars$time_ratioCP[cluster_vars$cluster %in% c(5)])
summary(cluster_vars$time_ratioCP[cluster_vars$cluster %in% c(6)])
summary(cluster_vars$time_ratioCP[cluster_vars$cluster %in% c(7)])

summary(cluster_vars$time_ratioCP[cluster_vars$cluster %in% c(1, 3)])
summary(cluster_vars$time_ratioCP[cluster_vars$cluster %in% c(2, 4, 5, 6, 7)])

#Jobs
summary(cluster_vars$inv_job_ratioCP[cluster_vars$cluster %in% c(1)])
summary(cluster_vars$inv_job_ratioCP[cluster_vars$cluster %in% c(2)])
summary(cluster_vars$inv_job_ratioCP[cluster_vars$cluster %in% c(3)])
summary(cluster_vars$inv_job_ratioCP[cluster_vars$cluster %in% c(4)])
summary(cluster_vars$inv_job_ratioCP[cluster_vars$cluster %in% c(5)])
summary(cluster_vars$inv_job_ratioCP[cluster_vars$cluster %in% c(6)])
summary(cluster_vars$inv_job_ratioCP[cluster_vars$cluster %in% c(7)])

summary(cluster_vars$inv_job_ratioCP[cluster_vars$cluster %in% c(1, 3)])
summary(cluster_vars$inv_job_ratioCP[cluster_vars$cluster %in% c(2, 4, 5, 6, 7)])

#Step-free index
summary(cluster_vars$step_free_benefit_indexW[cluster_vars$cluster %in% c(1)])
summary(cluster_vars$step_free_benefit_indexW[cluster_vars$cluster %in% c(2)])
summary(cluster_vars$step_free_benefit_indexW[cluster_vars$cluster %in% c(3)])
summary(cluster_vars$step_free_benefit_indexW[cluster_vars$cluster %in% c(4)])
summary(cluster_vars$step_free_benefit_indexW[cluster_vars$cluster %in% c(5)])
summary(cluster_vars$step_free_benefit_indexW[cluster_vars$cluster %in% c(6)])
summary(cluster_vars$step_free_benefit_indexW[cluster_vars$cluster %in% c(7)])

summary(cluster_vars$step_free_benefit_indexW[cluster_vars$cluster %in% c(1, 3)])
summary(cluster_vars$step_free_benefit_indexW[cluster_vars$cluster %in% c(2, 4, 5, 6, 7)])

# ----- Station Catchment Analysis -----

#Join data
station_catchments <- fastest_time_to_stations %>%
  dplyr::select(lsoa21cd, lsoa21nm, fastest_station, ratioCP, ratioSLOW) %>%
  rename("time_ratioCP" = ratioCP, "time_ratioSLOW" = ratioSLOW)%>%
  left_join(cluster_vars %>% st_drop_geometry() %>% dplyr::select(lsoa21cd, inv_job_ratioCP, inv_job_ratioSLOW, cluster), by = "lsoa21cd")%>%
  left_join(pop_centroids %>% dplyr::select(id, total_pop, total_under_5, total_65_plus, total_disabled), by = c("lsoa21cd" = "id"))%>%
  mutate(total_in_need_pop = total_under_5 + total_65_plus + total_disabled)%>%
  dplyr::select(-total_under_5, -total_65_plus, -total_disabled)%>%
  left_join(tube_stations_main %>% dplyr::select(stop_id, stop_name, classification, upgrade_status)%>%st_drop_geometry(), by=c("fastest_station"="stop_id"))

station_catchments_summary <- station_catchments %>%
  group_by(fastest_station, stop_name, classification, upgrade_status) %>%
  summarise(
    total_population = sum(total_pop),
    total_in_need_population = sum(total_in_need_pop),
    total_in_need_cluster_1 = sum(total_in_need_pop[cluster == 1]),
    total_in_need_cluster_1_or_3 = sum(total_in_need_pop[cluster %in% c(1, 3)]),
    mean_time_ratioCP = mean(time_ratioCP),
    mean_time_ratioSLOW = mean(time_ratioSLOW),
    mean_job_ratioCP = mean(inv_job_ratioCP),
    mean_job_ratioSLOW = mean(inv_job_ratioSLOW))%>%
  mutate(pct_in_need = 100*total_in_need_population/total_population)
st_write(station_catchments_summary, "data_export_vis/station_catchment_summary.gpkg")

#Compare properties of accessible and non-accessible catchments
station_catchments_summary_accessible <- station_catchments_summary %>%
  filter(classification=='Fully Accessible')
station_catchments_summary <- station_catchments_summary %>%
  filter(classification != 'Fully Accessible')

summary(station_catchments_summary$total_in_need_cluster_1_or_3)
summary(station_catchments_summary_accessible$total_in_need_cluster_1_or_3)

summary(station_catchments_summary$mean_time_ratioCP)
summary(station_catchments_summary_accessible$mean_time_ratioCP)
summary(station_catchments_summary$mean_job_ratioCP)
summary(station_catchments_summary_accessible$mean_job_ratioCP) #so job access is reduced, even when your nearest station is step-free - but median ratio is 1.027, i.e. average access to 97% of jobs

summary(station_catchments_summary$mean_time_ratioSLOW)
summary(station_catchments_summary_accessible$mean_time_ratioSLOW)
summary(station_catchments_summary$mean_job_ratioSLOW)
summary(station_catchments_summary_accessible$mean_job_ratioSLOW)

#Compare stations TfL are exploring to the rest
station_catchments_summary <- station_catchments_summary %>%
  mutate(upgrade_status2 = if_else(upgrade_status == "No Plans", "No Plans", "Potential Upgrade"))

#Pivot data for faceted violin plot
labels <- c(
  total_population = "Total Population",
  total_in_need_population = "In-Need Population",
  pct_in_need = "Proportion of In-Need Population",
  total_in_need_cluster_1 = "In-Need Population, Cluster 1",
  total_in_need_cluster_1_or_3 = "In-Need Population, Clusters 1 & 3",
  mean_time_ratioCP = "Mean Station Time Accessibility Ratio",
  mean_time_ratioSLOW = "Mean Station Time Accessibility Ratio, \nSlower Walking Speed",
  mean_job_ratioCP = "Mean Job Accessibility Ratio",
  mean_job_ratioSLOW = "Mean Job Accessibility Ratio, \nSlower Walking Speed")
numeric_vars <- station_catchments_summary %>%
  st_drop_geometry() %>%
  dplyr::select(fastest_station, upgrade_status2, where(is.numeric)) %>%
  pivot_longer(
    cols = where(is.numeric),
    names_to = "variable",
    values_to = "value")%>%
  mutate(
    variable = factor(variable, levels = names(labels), labels = labels))

#Create violin plot faceted by variable, split by upgrade_status2
ggplot(numeric_vars, aes(x = upgrade_status2, y = value, fill = upgrade_status2)) +
  geom_violin(trim = FALSE) +
  stat_summary(aes(color = upgrade_status2),
               fun = median,
               geom = "crossbar",
               linetype = "dashed",
               size = 0.2,
               show.legend = FALSE) +
  facet_wrap(~ variable, scales = "free_y", ncol=3) +
  scale_fill_brewer(palette = "Set1")+
  guides(color = "none") +
  theme_minimal() +
  labs(
    title = "Catchment Properties of Non-Fully-Accessible Stations",
    x = NULL,
    y = "Value",
    fill = "Upgrade Status",
    caption = "Dashed lines represent distribution medians.") +
  theme(
    axis.text.x = element_blank(),  
    axis.ticks.x = element_blank(), 
    #legend.position = c(0.7, 0.1),
    plot.title = element_text(family = "Segoe UI Semibold", size = 16, hjust=0.5),
    axis.title = element_text(family = "Segoe UI Semibold", size=10),
    axis.text = element_text(family = "Segoe UI", size=9),
    legend.title = element_text(family = "Segoe UI Semibold", size = 10),
    legend.text = element_text(family = "Segoe UI", size = 9),
    strip.text = element_text(family = "Segoe UI", size = 9),
    plot.caption = element_text(family = "Segoe UI Light", size = 8, hjust=0))
#For formatting on A4, it's probably best to paste the legend in directly below

#So we can see that TfL prioritises catchments with larger populations
#But accessibility ratios are on average lower than no-plans
#Though there are higher raw populations in clusters 1+3 (probably due to having larger pops in general)
rm(numeric_vars, station_catchments, labels)

#Quick cluster map with stations
tmap_mode("view")
tm_shape(cluster_vars)+
  tm_polygons("cluster", alpha=0.8, border.alpha=0)+
  tm_shape(boroughs)+
  tm_polygons(lwd=0.5, fill=NA, alpha=0)+
  tm_shape(tube_stations_main %>% filter(classification != "Fully Accessible"))+
  tm_dots(col="upgrade_status", palette="Dark2")

# ---- Prioritise stations -----

#Z-score standardise all catchment variables
station_catchments_summary_scaled <- station_catchments_summary %>%
  dplyr::select(where(is.numeric))%>%
  st_drop_geometry()%>%
  scale()
station_catchments_summary_scaled <- station_catchments_summary %>% #Reattach to ID
  dplyr::select(fastest_station, stop_name, classification, upgrade_status)%>%
  st_drop_geometry()%>%
  bind_cols(as.data.frame(station_catchments_summary_scaled))

#We need to shortlist the top 8 based on these catchment criteria

#Variables being considered - remember we are focuing on equity:
  #In-need population in clusters 1 and 3: 60%
  #Accessibility disparities: 10% each
  #In-need population: 20%

station_catchments_summary_scaled <- station_catchments_summary_scaled %>%
  mutate(total_score = total_in_need_cluster_1_or_3*.6+mean_time_ratioCP*.1+mean_job_ratioCP*.1+total_in_need_population*.2)

#Add to overall df and rank
station_catchments_summary <- station_catchments_summary %>%
  left_join(station_catchments_summary_scaled %>% ungroup() %>% dplyr::select(fastest_station, total_score), by="fastest_station")%>%
  arrange(desc(total_score))%>%
  ungroup()%>%
  mutate(rank = row_number())

#Find ranks of TfL stations
station_catchments_summary %>% filter(upgrade_status2=="Potential Upgrade") %>% dplyr::select(stop_name, upgrade_status, rank) %>% st_drop_geometry()
st_write(station_catchments_summary, "data_export_vis/station_catchments_summary.gpkg")

rm(station_catchments_summary_accessible, station_catchments_summary_scaled)

#Note limitations of not clustering on ratioSLOW - less representative of real-world constraints (but less biased towards larger LSOAs)
#We might need a different clustering approach to account for LSOAs near accessible stations which are negatively affected by destination characteristics
#Or could it be okay to exclude this, due to sufficientarism? Also, we see above that people whose nearest station is accessible have 97% job access, CP (although it can be as low as 50%)

#Note not all stations have catchments, if they have no LSOAs which they are closest to! Limitation, except they are all Zone 1 so probably wouldn't be equity focus anyway