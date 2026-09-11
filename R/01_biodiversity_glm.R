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