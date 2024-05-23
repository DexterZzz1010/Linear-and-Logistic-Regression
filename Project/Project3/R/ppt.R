# 加载必要的包
library(officer)
library(flextable)

# 创建数据框
data <- data.frame(
  Variable = c("log(Higheds)", "Children", "Seniors", "log(Income)", "log(GRP)", "Persperhh", 
               "Fertility", "Urban", "Transit", "log(Apartments)", "Builton", "log(Population)", "NewParts"),
  GVIF = c(5.315311, 8.695745, 10.580993, 2.815877, 3.602205, 6.345686, 
           2.398688, 5.278295, 5.220666, 7.579518, 4.191415, 7.079126, 1.984210),
  Df = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2),
  GVIF_sqrt = c(2.305496, 2.948855, 3.252844, 1.678058, 1.897948, 2.519065, 
                1.548770, 2.297454, 2.284878, 2.753093, 2.047295, 2.660663, 1.186853)
)

# 创建 flextable 表格
ft <- flextable(data)
ft <- theme_vanilla(ft)
ft <- autofit(ft)

# 创建一个新的 PowerPoint 幻灯片
ppt <- read_pptx()

# 添加一张幻灯片
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")

# 添加标题
ppt <- ph_with(ppt, "Variance Inflation Factors (VIF) for Predictors", location = ph_location_type(type = "title"))

# 添加表格到幻灯片
ppt <- ph_with(ppt, ft, location = ph_location_type(type = "body"))

# 保存 PowerPoint 文件
print(ppt, target = "VIF_Table.pptx")

compare <- data.frame(
  `Resid. Df` = c(277, 275),
  `Resid. Dev` = c(29136, 28635),
  Df = c(NA, 2),
  Deviance = c(NA, 500.76)
)

# 创建 flextable 表格
ft <- flextable(compare)
ft <- theme_vanilla(ft)
ft <- set_caption(ft, "Model Comparison Results")
ft <- autofit(ft)

# 创建一个新的 PowerPoint 幻灯片
ppt <- read_pptx()

# 添加一张幻灯片
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")

# 添加标题
ppt <- ph_with(ppt, "Model Comparison Results", location = ph_location_type(type = "title"))

# 添加表格到幻灯片
ppt <- ph_with(ppt, ft, location = ph_location_type(type = "body"))

# 保存 PowerPoint 文件
print(ppt, target = "Model_Comparison_Results.pptx")

# 创建数据框
reduce_vif <- data.frame(
  Variable = c("log(Income)", "log(GRP)", "Persperhh", "Fertility", "Transit", 
               "Builton", "log(Population)", "NewParts"),
  GVIF = c(1.590543, 2.773222, 2.180905, 1.602168, 3.886604, 
           3.677435, 4.253619, 1.793513),
  Df = c(1, 1, 1, 1, 1, 1, 1, 2),
  GVIF_sqrt = c(1.261167, 1.665299, 1.476789, 1.265768, 1.971447, 
                1.917664, 2.062430, 1.157247)
)

# 创建 flextable 表格
ft <- flextable(reduce_vif)
ft <- theme_vanilla(ft)
ft <- set_caption(ft, "Variance Inflation Factors (VIF) for Reduced Model")
ft <- autofit(ft)

# 创建一个新的 PowerPoint 幻灯片
ppt <- read_pptx()

# 添加一张幻灯片
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")

# 添加标题
ppt <- ph_with(ppt, "Variance Inflation Factors (VIF) for Reduced Model", location = ph_location_type(type = "title"))

# 添加表格到幻灯片
ppt <- ph_with(ppt, ft, location = ph_location_type(type = "body"))

# 保存 PowerPoint 文件
print(ppt, target = "Reduced_Model_VIF_Table.pptx")





# 加载必要的包
library(officer)
library(flextable)

# 创建数据框
reduce_model_coff <- data.frame(
  Variable = c("(Intercept)", "log(Income)", "log(GRP)", "Persperhh", "Fertility", 
               "Transit", "log(Population)", "NewPartsSvealandandNo", "NewPartsNorrlandandNo"),
  Estimate = c(-0.1234, 0.1900, 0.02528, -0.2809, 0.03827, -0.002201, 0.8988, -0.04231, -0.02556),
  `Std. Error` = c(0.03403, 0.006806, 0.001676, 0.004465, 0.004430, 0.00004277, 0.0006707, 0.001150, 0.002223),
  `z value` = c(-3.626, 27.916, 15.085, -62.904, 8.640, -51.469, 1340.215, -36.802, -11.499),
  `Pr(>|z|)` = c(0.000288, "< 2e-16", "< 2e-16", "< 2e-16", "< 2e-16", "< 2e-16", "< 2e-16", "< 2e-16", "< 2e-16")
)

# 创建 flextable 表格
ft <- flextable(reduce_model_coff)
ft <- theme_vanilla(ft)
ft <- set_caption(ft, "Coefficients of Reduced Poisson Model")
ft <- autofit(ft)

# 创建一个新的 PowerPoint 幻灯片
ppt <- read_pptx()

# 添加一张幻灯片
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")

# 添加标题
ppt <- ph_with(ppt, "Coefficients of Reduced Poisson Model", location = ph_location_type(type = "title"))

# 添加表格到幻灯片
ppt <- ph_with(ppt, ft, location = ph_location_type(type = "body"))

# 保存 PowerPoint 文件
print(ppt, target = "Reduced_Model_Coefficients_Table.pptx")






coefficients_data <- data.frame(
  Variable = c("(Intercept)", "log(Apartments)", "log(Builton)", "log(Higheds)", 
               "log(Seniors)", "Transit", "Urban", "log(Vehicles)", 
               "NewPartsSvealandandNo", "NewPartsNorrlandandNo"),
  Estimate = c(-4.3430775, -0.0428626, -0.0649671, -0.0214063, 
               0.1304264, -0.0002049, 0.0009794, 0.5546852, 
               -0.0056819, -0.1042360),
  StdError = c(0.1692348, 0.0105524, 0.0098844, 0.0112766, 
               0.0205683, 0.0002269, 0.0003492, 0.0264783, 
               0.0060574, 0.0109022),
  zValue = c(-25.663, -4.062, -6.573, -1.898, 
             6.341, -0.903, 2.805, 20.949, 
             -0.938, -9.561),
  PrValue = c("< 2e-16", "4.87e-05", "4.94e-11", "0.05766", 
              "2.28e-10", "0.36642", "0.00503", "< 2e-16", 
              "0.34824", "< 2e-16")
)

# 创建 flextable 表格
ft <- flextable(coefficients_data)
ft <- autofit(ft)
ft <- set_caption(ft, "Regression Coefficients")

# 创建一个新的 PowerPoint 幻灯片
ppt <- read_pptx()

# 添加一张幻灯片
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")

# 添加标题
ppt <- ph_with(ppt, "Regression Coefficients", location = ph_location_type(type = "title"))

# 添加表格到幻灯片
ppt <- ph_with(ppt, ft, location = ph_location_type(type = "body"))

# 保存 PowerPoint 文件
print(ppt, target = "Regression_Coefficients_Table.pptx")







