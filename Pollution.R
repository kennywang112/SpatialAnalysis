library(tidyverse)
library(sp)
library(spacetime)
library(showtext)
library(rworldmap)
library(RColorBrewer)
library(gstat)
# https://airtw.moenv.gov.tw/cht/Query/InsValue.aspx
data <- read_csv("Data/taiwan.csv", locale = locale(encoding = "Big5"))
# https://data.moenv.gov.tw/dataset/detail/AQX_P_07
station <- read_csv("Data/station.csv")

# creates a character vector of strings from "00" to "23"
numeric_cols <- sprintf("%02d", 0:23)

# preprocess
full_data <- data %>%
  mutate(across(
    all_of(numeric_cols), 
    ~ {
      # replace NA with "#"
      num_col <- as.numeric(na_if(as.character(.), "#"))
      # compute mean of column (ignoring NA)
      col_mean <- mean(num_col, na.rm = TRUE)
      # replace NA with mean
      replace_na(num_col, col_mean)
    }
  ))

# compute mean of row (ignoring NA)
full_data <- full_data %>%
  rowwise() %>%  # operate with row
  mutate(valid_mean = mean(c_across(all_of(numeric_cols)), na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(`測站`, `日期`, `測項`, valid_mean)

# filter date
full_data <- full_data %>%
  mutate(date = as.Date(`日期`, format = "%Y/%m/%d")) %>%
  filter(date < as.Date("2024-02-29")) 

pivot_data <- full_data %>%
  pivot_wider(
    names_from = `測項`, # convert to column
    values_from = valid_mean  # compute to value
  )
pivot_data$`測站` %>% unique()

# get station which is in pivot_data
get_station <- station %>% filter(sitename %in% pivot_data$`測站`)
# join station data
pivot_data <- pivot_data %>%
  left_join(get_station, by = c("測站" = "sitename")) %>%
  dplyr::select(`測站`, `日期`, PM10, twd97lon, twd97lat) %>%
  mutate(
    PM10 = ifelse(is.nan(PM10), 0, PM10),
  )

# select_lst <- c("基隆", "士林", "大同", "中山",  "古亭", "松山", "陽明",  "萬華", "三重", "土城")
# pivot_data <- pivot_data%>%filter(測站 %in% select_lst)

# Create a SpatialPoints object
spatial_points <- SpatialPoints(pivot_data[, c("twd97lon", "twd97lat")])

names(pivot_data)[1] <- "station"
names(pivot_data)[2] <- "date"

pivot_data$date <- as.Date(pivot_data$date)

STIDF_daily <- stConstruct(
  x = as.data.frame(pivot_data),
  space = c("twd97lon", "twd97lat"),
  time = "date",
  SpatialObj = spatial_points
)

STFDF_daily <- as(STIDF_daily, "STFDF")
STFDF_daily@sp@proj4string <- CRS("+init=epsg:3826") # define coordinate

pivot_data$station %>% unique()
# font_add(family = "PingFang", regular = "/System/Library/Fonts/Supplemental/PingFang.ttc")
showtext_auto()
time_range <- "2024-01-01::2024-02-29" # 時間範圍
# plot daily PM10
stplot(
  STFDF_daily[, time_range, "PM10"],  # 時間範圍和變量選擇
  mode = "xt",                      # xt 模式：時間-站點
  scaleX = 0,                       # 去掉 X 軸比例
  col.regions = terrain.colors(100), # 顏色梯度
  scales = list(
    x = list(labels = unique(pivot_data$station))
  )
)
stplot(STFDF_daily[, "2024-01-01::2024-01-30" , "PM10"], mode = "ts")

world <- getMap(resolution="low")
# stplot(STFDF_daily[, time_range, "PM10"],
#        col.regions=brewer.pal(6,"Spectral"), cuts=6, sp.layout=list(world,first=TRUE))
# STIDF_daily@sp <- spTransform(STIDF_daily@sp, CRS("+init=epsg:3826")) # 以公尺為單位

# 計算時間、空間半變異數
# cutoff是最大距離，tlags是時間間隔，width是時間窗口
cutoff <- max(spDists(coordinates(STFDF_daily))) / 2
width <- cutoff / 10
var <- variogram(PM10~1, STFDF_daily[!is.na(STFDF_daily[,"2024-01-01::2024-06-30","PM10"]$PM10),
                                    "2024-01-01::2024-06-","PM10"], width=width, cutoff=cutoff)

plot(var)            # sample variograms at each time lag
plot(var, map=FALSE)      # ST-variogram map
plot(var, wireframe=TRUE) # ST-variogram wireframe

# 模型設定，vgm內分別是sill, range, nugget, k是乘積係數，控制兩者之間的交互強度
prodSumModel <- vgmST("productSum", space = vgm(1, "Exp", 150, 0.5), time = vgm(1, "Exp", 5, 0.5), k = 5)
# 設定參數下限
pars.l <- c(sill.s = 0, range.s = 10, nugget.s = 0,sill.t = 0, range.t = 1, 
            nugget.t = 0, sill.st = 0, range.st = 10, nugget.st = 0, anis = 0, k = 0.01)
# 模型擬合
prodSumModel_Vgm <- fit.StVariogram(var, prodSumModel, lower = pars.l)

attr(prodSumModel_Vgm, "MSE")

extractPar(prodSumModel_Vgm)

# 1. 定義時空範圍
lon_range <- range(pivot_data$twd97lon) + c(-0.1, 0.1) 
lat_range <- range(pivot_data$twd97lat) + c(-0.1, 0.1)
time_range <- seq(as.Date("2024-02-25"), as.Date("2024-02-29"), by = "day")  # 最後三天

# 2. 創建空間點
lon_seq <- seq(lon_range[1], lon_range[2], by = 0.05) 
lat_seq <- seq(lat_range[1], lat_range[2], by = 0.05) 
space_grid <- expand.grid(twd97lon = lon_seq, twd97lat = lat_seq)
space_grid

# 創建 SpatialPoints 和 STFDF 對象
spatial_points <- SpatialPoints(space_grid, proj4string = CRS("+init=epsg:3826"))
grid_ST <- STFDF(
  sp = spatial_points,
  time = time_range,
  data = data.frame(PM10 = rep(NA, length(time_range) * nrow(space_grid)))  # 修正行數
)

pr <- krigeST(PM10~1, data=STFDF_daily, modelList=prodSumModel_Vgm, newdata=grid_ST)
pr
stplot(pr)

predicted_values <- as.data.frame(pr) 
slice_27 <- predicted_values %>% filter(time=='2024-02-27')

taiwan_map <- map("worldHires", "Taiwan", plot = FALSE, fill = TRUE)
world_clean <- world[!is.na(world$NAME), ]  # 去除 NAME 為 NA 的行
taiwan_map <- world_clean[world_clean$NAME == "Taiwan", ] 
taiwan_map_sf <- st_as_sf(taiwan_map)

ggplot() +
  # PM10 預測數據 (slice_27)
  geom_tile(data = slice_27, aes(x = twd97lon, y = twd97lat, fill = var1.pred)) +
  # 疊加台灣地圖
  geom_sf(data = taiwan_map_sf, color = "black", fill = NA, linetype = "solid") +
  # 疊加監測站位置
  geom_point(data = pivot_data, aes(x = twd97lon, y = twd97lat), 
             color = "#74dff2", size = 2, shape = 21, fill = "#74dff2") +
  # 添加監測站名稱標籤
  # geom_text(data = pivot_data, aes(x = twd97lon, y = twd97lat, label = station),
  #           color = "#95edd7", size = 3, vjust = -1) +
  # 色彩標度與主題
  scale_fill_viridis_c() +
  labs(
    title = "PM10 pred",
    x = "long",
    y = "lat",
    fill = "PM10") +
  coord_sf(
    xlim = range(slice_27$twd97lon), 
    ylim = range(slice_27$twd97lat))
