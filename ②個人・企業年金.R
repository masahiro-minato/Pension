# 個人・企業年金
Cal_個人企業年金 <- function(
    Nenkin.date = Nenkin.date
　){
  # 累積値列の抽出
  Nenkin.date.sum <- 
    Nenkin.date %>% 
    select(X.date,starts_with("sum.")) %>% 
    mutate_if(is.numeric, ~replace(., .==0, NA))
  # 個人年金
  kojin.n <- 801195
  end.month <- "2055-12-31"
  KojinX.date <- seq(as.POSIXct("2024-01-01"), as.POSIXct(end.month), by = "month")
  Kojin.date <- tibble(X.date = KojinX.date) %>% 
    mutate(
      kojin = as.integer(kojin.n/12)
    )%>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
    mutate(
      Kojin.cum = cumsum(kojin)
    )
  
  # 企業年金
  kigyo.n <- 743502
  KigyoX.date.whole <- seq(as.POSIXct("2024-01-01"), as.POSIXct(end.month), by = "month")
  Kigyo.date.whole <- tibble(X.date = KigyoX.date.whole)
  KigyoX.date <- seq(as.POSIXct("2024-01-01"), as.POSIXct("2039-02-28"), by = "month")
  Kigyo.date <- tibble(X.date = KigyoX.date) %>% 
    mutate(
      kigyo = as.integer(kigyo.n/12)
    )%>% 
    full_join(Kigyo.date.whole) %>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
    mutate(
      kigyo.cum = cumsum(kigyo)
    )
  # 結合
  Nenkin.date.kk <- 
    Nenkin.date.sum %>% 
    full_join(Kojin.date) %>% 
    full_join(Kigyo.date) %>% 
    select(X.date, contains("cum")) %>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
    mutate_at(.vars = vars(starts_with("sum.cum")), .funs = ~ . + Kojin.cum + kigyo.cum)
  
  Nenkin.date.kk.pivot <- 
    Nenkin.date.kk %>% 
    pivot_longer(cols = starts_with("sum.")) %>% 
    mutate_if(is.numeric, ~replace(., .==0, NA))
  
  # グラフ
  p.kk <- ggplot(data = Nenkin.date.kk.pivot, aes(x = X.date)) + 
    theme_bw() + 
    labs(title = "厚生+企業+個人年金受給額累計") +
    theme(plot.title = element_text(size = 18,  #font size and adjust
                                    hjust = 0.01,#adjust
    )) +
    ylab("受給額累計") +
    labs(color = "繰下げ月数") +
    scale_y_continuous(breaks=seq(0,140000000,length=8),limits=c(0,140000000),
                       labels = label_comma(scale = 1/10000, suffix = '万円')) +
    scale_color_discrete(labels = c("0ヶ月","12ヶ月","24ヶ月","36ヶ月","48ヶ月","60ヶ月")) +
    theme(legend.position = c(0.18, 0.9), legend.justification = c(1, 1)) +
    geom_line(aes(y = value, color = name), linewidth = 1.2) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(angle = 30, hjust = 1, size = 14),
          axis.text.y = element_text(size = 14),
          legend.title = element_text(size = 16),
          legend.text = element_text(size=14)) +
    scale_x_datetime(limits = c(as.POSIXct("2024-01-01 JST"), as.POSIXct(end.month)),
                     date_breaks = "2 year", date_labels = "%Y年%m月") + # 表示期間の設定
    geom_vline(xintercept = as.POSIXct("2029-02-22 JST"), linetype = 2, color = "darkblue") +
    geom_vline(xintercept = as.POSIXct("2039-02-22 JST"), linetype = 2, color = "darkblue") + 
    geom_vline(xintercept = as.POSIXct("2049-02-22 JST"), linetype = 2, color = "darkblue") # 垂直線
  
  plot(p.kk)
  
  return(Nenkin.date.kk)
}

Nenkin.date.kk <- Cal_個人企業年金(Nenkin.date)

## 手動計算 ----------------------
# データ読込
Nenkin.date <- read_csv("./csv/Nenkin.date.csv")
# 累積値列の抽出
Nenkin.date.sum <- 
  Nenkin.date %>% 
  select(X.date,starts_with("sum.")) %>% 
  mutate_if(is.numeric, ~replace(., .==0, NA))
# 個人年金
kojin.n <- 801195
end.month <- "2055-12-31"
KojinX.date <- seq(as.POSIXct("2024-01-01"), as.POSIXct(end.month), by = "month")
Kojin.date <- tibble(X.date = KojinX.date) %>% 
  mutate(
    kojin = as.integer(kojin.n/12)
  )%>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(
    Kojin.cum = cumsum(kojin)
  )

# 企業年金
kigyo.n <- 743502
KigyoX.date.whole <- seq(as.POSIXct("2024-01-01"), as.POSIXct(end.month), by = "month")
Kigyo.date.whole <- tibble(X.date = KigyoX.date.whole)
KigyoX.date <- seq(as.POSIXct("2024-01-01"), as.POSIXct("2039-02-28"), by = "month")
Kigyo.date <- tibble(X.date = KigyoX.date) %>% 
  mutate(
    kigyo = as.integer(kigyo.n/12)
  )%>% 
  full_join(Kigyo.date.whole) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(
    kigyo.cum = cumsum(kigyo)
  )
# 結合
Nenkin.date.kk <- 
  Nenkin.date.sum %>% 
  full_join(Kojin.date) %>% 
  full_join(Kigyo.date) %>% 
  select(X.date, contains("cum")) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate_at(.vars = vars(starts_with("sum.cum")), .funs = ~ . + Kojin.cum + kigyo.cum)

Nenkin.date.kk.pivot <- 
  Nenkin.date.kk %>% 
  pivot_longer(cols = starts_with("sum.")) %>% 
  mutate_if(is.numeric, ~replace(., .==0, NA))
  
# グラフ
p.kk <- ggplot(data = Nenkin.date.kk.pivot, aes(x = X.date)) + 
  theme_bw() + 
  labs(title = "厚生+企業+個人年金受給額累計") +
  theme(plot.title = element_text(size = 18,  #font size and adjust
                                  hjust = 0.01,#adjust
  )) +
  ylab("受給額累計") +
  labs(color = "繰下げ月数") +
  scale_y_continuous(breaks=seq(0,140000000,length=8),limits=c(0,140000000),
                     labels = label_comma(scale = 1/10000, suffix = '万円')) +
  scale_color_discrete(labels = c("0ヶ月","12ヶ月","24ヶ月","36ヶ月","48ヶ月","60ヶ月")) +
  theme(legend.position = c(0.18, 0.9), legend.justification = c(1, 1)) +
  geom_line(aes(y = value, color = name), linewidth = 1.2) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size=14)) +
  scale_x_datetime(limits = c(as.POSIXct("2024-01-01 JST"), as.POSIXct(end.month)),
                   date_breaks = "2 year", date_labels = "%Y年%m月") + # 表示期間の設定
  geom_vline(xintercept = as.POSIXct("2029-02-22 JST"), linetype = 2, color = "darkblue") +
  geom_vline(xintercept = as.POSIXct("2039-02-22 JST"), linetype = 2, color = "darkblue") + 
  geom_vline(xintercept = as.POSIXct("2049-02-22 JST"), linetype = 2, color = "darkblue") # 垂直線


plot(p.kk)

# 保存
ggsave("./PDF/厚生企業個人年金受給額.pdf", plot = p.kk, device = cairo_pdf, dpi=300, width=10, height=6)
