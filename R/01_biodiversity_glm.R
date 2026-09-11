# R/01_biodiversity_glm.R
# 模拟海拔对物种丰富度的影响

set.seed(2026)
n_plots <- 80

# 1. 模拟野外样方观测数据
survey_data <- data.frame(
  plot_id           = sprintf("PLOT_%03d", 1:n_plots),
  elevation_m       = round(runif(n_plots, 400, 3200)),
  canopy_cover_pct  = round(rnorm(n_plots, 60, 15) |> pmin(100) |> pmax(10))
)

# 2. 生成符合生态学规律的泊松响应（海拔越高多样性偏低，林冠郁闭度适中偏高）
lambda <- exp(3.2 - 0.0006 * survey_data$elevation_m + 0.008 * survey_data$canopy_cover_pct)
survey_data$richness <- rpois(n_plots, lambda = lambda)

# 3. 拟合广义线性模型 (GLM)
fit_glm <- glm(richness ~ elevation_m + canopy_cover_pct, family = poisson, data = survey_data)

# 4. 导出汇总表
write.csv(summary(fit_glm)$coefficients, "outputs/models/glm_results.csv")
message("GLM 模型分析完成，结果已保存至 outputs/models/glm_results.csv")

# --- 追加：可视化海拔梯度上的丰富度拟合曲线 ---
library(ggplot2)

# 5. 绘制海拔与物种丰富度的散点图与拟合趋势线
p_elevation <- ggplot(survey_data, aes(x = elevation_m, y = richness)) +
  geom_point(aes(color = canopy_cover_pct), size = 2.5, alpha = 0.8) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), 
              color = "#2b8cbe", fill = "#a6bddb", linewidth = 1.1) +
  scale_color_viridis_c(name = "Canopy Cover (%)") +
  theme_classic(base_size = 12) +
  labs(
    title = "Species Richness along Elevation Gradient",
    subtitle = "Poisson GLM fit with 95% confidence intervals",
    x = "Elevation (m a.s.l.)",
    y = "Species Richness (count)"
  )

# 6. 保存高质量矢量/栅格图表至 outputs/figures
ggsave("outputs/figures/richness_vs_elevation.png", 
       plot = p_elevation, width = 7, height = 5, dpi = 300)

message("图表已成功导出至 outputs/figures/richness_vs_elevation.png")