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

