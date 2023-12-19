# MCMCサンプリング結果から時系列グラフを作成する関数
graph.Future.pred <- function(maintenance = MF3_maintenance, # 保守データ
                              machine = MF3_machine,         # 機器データ
                              index.Phenomenon = 0, # 0～15 尚、0はすべての現象
                              X.date = seq(as.POSIXct("2019-01-01"), as.POSIXct("2022-11-30"), by = "day"), # データの取得期間
                              # MCMCサンプリング期間
                              start.day = "2020-01-01 JST", # 開始日
                              end.day = "2022-11-30 JST",   # 終了日
                              pred_term = 30, # 予測期間
                              param = c("s_w", "s_s", "s_r", "s_t", "b_ar", "b_ope[1]", "Intercept", "lp__"), # 結果表示パラメータ
                              object.path = "./Cmdstan_files/Metis-MF3.EM.predict-smooth.fit.", # サンプリング結果の保存先
                              rhat = TRUE,
                              # 時系列グラフ
                              graph.start_day = "2022-04-01 JST", # グラフ表示開始日
                              y.breaks.conf = NA, # 信頼区間ｙ軸目盛設定 seq(0,2000,200)
                              y.breaks.pred = NA, # 予測区間ｙ軸目盛設定 seq(0,2000,200)
                              x.date_breaks = "1 month", # x軸目盛間隔
                              graph.path = "./PDF/Metis-MF3-EM-smooth-" # グラフ保存先/名前先頭共通部分のみで後ろ部分は自動設定
){
  # Future.predictionによるデータ分析結果の時系列グラフの信頼区間・予測区間グラフの縦軸目盛りを修正する場合に使用する
  # 信頼区間グラフと予測区間グラフの縦軸目盛りを個々に設定できるが目盛り最大値は自動設定されるので目盛間隔を変更するのみである
  # 現象項目のインデックスを指定して実行するとグラフが保存される
  # 戻り値としても3種類（集合グラフ,予測区間,信頼区間）のグラフが返される
  Phe15 <- 
    head(levels(fct_infreq(maintenance$Phenomenon)), n=15)
  # 現象をインデックスで選択/すべての現象の場合は「index <- 0」で現象名は"全体"と表示される
  index <- index.Phenomenon
  if(index == 0){
    Phenom <- "全体"
  }else{
    Phenom <- Phe15[index]
  }
  # 分析対象のEM現象表示
  print(str_c("分析対象のEM現象：",Phenom)) 
  
  # # 読込
  # fit <- read_rds(str_c(object.path, Phenom,"-",start_day,"~",end_day,".rds"))
  
  # 保守データから日付ごとのEM件数を取得/"全体"の場合は現象列でのフィルターを使用しない
  if(index != 0){
    EM.count <- 
      maintenance %>% 
      dplyr::filter(is.na(Peripheral_name)) %>%
      dplyr::filter(Phenomenon == Phenom) %>%
      group_by(Maintenance_date) %>%
      summarise(
        EM.count = n()
      ) %>% 
      arrange(-EM.count) %>%
      ungroup()
  }else{
    EM.count <- 
      MF3_maintenance %>% 
      dplyr::filter(is.na(Peripheral_name)) %>%
      group_by(Maintenance_date) %>%
      summarise(
        EM.count = n()
      ) %>% 
      arrange(-EM.count) %>%
      ungroup()
  }
  
  # 市場機台数
  MIF.count <- 
    machine %>% 
    group_by(機種機番,納品年月日) %>% 
    summarise(
      num = n()
    ) %>% 
    group_by(納品年月日) %>% 
    summarise(
      N.date = n()
    ) %>% 
    ungroup()
  
  # 結合
  MIF.date <- tibble(X.date = X.date)
  MIF_by.date <- 
    MIF.date %>% 
    full_join(MIF.count, by=c("X.date" = "納品年月日")) %>% 
    full_join(EM.count, by=c("X.date" = "Maintenance_date")) %>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
    mutate(
      MIF.cumsum = cumsum(N.date)
    )
  
  # データの準備
  start_day <-  which(MIF_by.date$X.date == start.day)
  end_day  <-  which(MIF_by.date$X.date == end.day)
  
  # 読込
  fit <- read_rds(str_c(object.path, Phenom,"-",start_day,"~",end_day,".rds"))

  # パラメータ表示
  fit$print(param)
  # rhatヒストグラム
  if(rhat == TRUE){
    fit %>% bayesplot::rhat() %>% hist(main=str_c("Histogram of ",Phenom))
  } 
  # b_ar 自己回帰係数
  b_ar.mean <- round(mean((fit$draws("b_ar") %>% as_draws_df)$b_ar),3)
  
  # 推定結果の図示 -----------------------------------------------------------------
  # フォント設定
  par(family="Noto Sans")
  # x軸の年月日設定
  date.plot <- 
    seq(
      from = as.POSIXct(MIF_by.date$X.date[start_day]),
      by = "days",
      len = end_day-start_day+1+pred_term
    )
  
  date_plot <- tibble(X.date = date.plot)
  # 計測区間
  date_plot.MIF_by.date.1 <- 
    date_plot %>% 
    left_join(MIF.count, by=c("X.date" = "納品年月日")) %>% 
    left_join(EM.count, by=c("X.date" = "Maintenance_date")) %>% 
    dplyr::filter(X.date <= MIF_by.date$X.date[end_day]) %>% 
    mutate_if(where(is.numeric), ~replace(., is.na(.), 0))
  
  if(pred_term != 0){
    # 未来予測区間
    date_plot.MIF_by.date.2 <- 
      date_plot %>% 
      left_join(MIF.count, by=c("X.date" = "納品年月日")) %>% 
      left_join(EM.count, by=c("X.date" = "Maintenance_date")) %>% 
      dplyr::filter(X.date > MIF_by.date$X.date[end_day])
    # 結合
    date_plot.MIF_by.date <- 
      bind_rows(date_plot.MIF_by.date.1,date_plot.MIF_by.date.2)
  }else{
    date_plot.MIF_by.date <- date_plot.MIF_by.date.1
  }
  # グラフの表示期間の設定
  limits = c(as.POSIXct(graph.start_day), as.POSIXct(MIF_by.date$X.date[end_day+pred_term]))
  # すべての成分を含んだ状態推定値の図示
  p_lambda_pois <- plotSSM.CmdStanr(fit = fit, 
                                    time_vec = date.plot,
                                    obs_vec = date_plot.MIF_by.date$EM.count,
                                    state_name = "lambda_exp", 
                                    graph_title = str_c("EM：",Phenom," λ(μ + γ + tvf + r)：すべての成分を含んだ状態推定値（自己回帰係数 b_ar=",b_ar.mean,"）"), 
                                    y_label = "件数",
                                    date_labels = "%Y/%m",
                                    date_breaks = x.date_breaks) +
    # 表示期間の設定
    scale_x_datetime(limits = limits, date_breaks = x.date_breaks, date_labels = "%Y/%m")
  # 予測期間の表示
  if(pred_term != 0){
    p_lambda_pois <- p_lambda_pois +
      geom_vline(xintercept = as.POSIXct(MIF_by.date$X.date[end_day]), linetype = 2, color = "darkblue") +
      annotate("text", x=as.POSIXct(MIF_by.date$X.date[end_day]), y=Inf, 
               hjust=-0.01, # 文字のｘ方向位置調整 マイナスで右へ移動
               vjust=2, 　　# 文字のｙ方向位置調整 プラスで下へ移動
               label="⇒ 【未来予測区間】", 
               size=5, colour="darkblue")
  }
  # y軸目盛  
  if(!is.na(y.breaks.conf[1])){
    p_lambda_pois <- p_lambda_pois + scale_y_continuous(breaks=y.breaks.conf)
  }
  # 水準成分＋周期成分
  p_mu_gamma_pois <- plotSSM.CmdStanr(fit = fit, 
                                      time_vec = date.plot,
                                      state_name = "mu_gamma_exp", 
                                      graph_title = "μ + γ：水準成分＋周期成分", 
                                      y_label = "件数",
                                      date_labels = "%Y/%m",
                                      date_breaks = x.date_breaks) +
    # 表示期間の設定
    scale_x_datetime(limits = limits, date_breaks = x.date_breaks, date_labels = "%Y/%m")
  if(pred_term != 0){
    p_mu_gamma_pois <- p_mu_gamma_pois +
      geom_vline(xintercept = as.POSIXct(MIF_by.date$X.date[end_day]), linetype = 2, color = "darkblue")
  }
  # 水準成分＋ランダム成分
  p_mu_r_pois <- plotSSM.CmdStanr(fit = fit, 
                                  time_vec = date.plot,
                                  state_name = "mu_r_exp", 
                                  graph_title = "μ + r：水準成分＋ランダム成分", 
                                  y_label = "件数",
                                  date_labels = "%Y/%m",
                                  date_breaks = x.date_breaks) +
    # 表示期間の設定
    scale_x_datetime(limits = limits, date_breaks = x.date_breaks, date_labels = "%Y/%m")
  if(pred_term != 0){
    p_mu_r_pois <- p_mu_r_pois +
      geom_vline(xintercept = as.POSIXct(MIF_by.date$X.date[end_day]), linetype = 2, color = "darkblue")
  }
  # 水準成分
  p_mu_pois <- plotSSM.CmdStanr(fit = fit, 
                                time_vec = date.plot,
                                state_name = "mu_exp", 
                                graph_title = str_c("μ：水準成分（自己回帰係数 b_ar=",b_ar.mean,"）"), 
                                y_label = "件数",
                                date_labels = "%Y/%m",
                                date_breaks = x.date_breaks) +
    # 表示期間の設定
    scale_x_datetime(limits = limits, date_breaks = x.date_breaks, date_labels = "%Y/%m")
  if(pred_term != 0){
    p_mu_pois <- p_mu_pois +
      geom_vline(xintercept = as.POSIXct(MIF_by.date$X.date[end_day]), linetype = 2, color = "darkblue")
  }
  # 予測区間
  p_pred_pois <- plotSSM.CmdStanr(fit = fit, 
                                  time_vec = date.plot,
                                  obs_vec = date_plot.MIF_by.date$EM.count,
                                  state_name = "y_pred", 
                                  graph_title = str_c("EM：",Phenom," 95%予測区間 （自己回帰係数 b_ar=",b_ar.mean,"）"), 
                                  y_label = "件数",
                                  date_labels = "%Y/%m",
                                  date_breaks = x.date_breaks,
                                  fill.ribbon = "lightgreen") +
    # 表示期間の設定
    scale_x_datetime(limits = limits, date_breaks = x.date_breaks, date_labels = "%Y/%m")
  # 予測期間の表示
  if(pred_term != 0){
    p_pred_pois <- p_pred_pois +
      geom_vline(xintercept = as.POSIXct(MIF_by.date$X.date[end_day]), linetype = 2, color = "darkblue") +
      annotate("text", x=as.POSIXct(MIF_by.date$X.date[end_day]), y=Inf, 
               hjust=-0.01, # 文字のｘ方向位置調整 マイナスで右へ移動
               vjust=2, 　　# 文字のｙ方向位置調整 プラスで下へ移動
               label="⇒ 【未来予測区間】", 
               size=5, colour="darkblue")
  }
  # y軸目盛  
  if(!is.na(y.breaks.pred[1])){
    p_pred_pois <- p_pred_pois + scale_y_continuous(breaks=y.breaks.pred)
  }
  # ドリフト成分
  p_drift_pois <- plotSSM.CmdStanr(fit = fit, 
                                   time_vec = date.plot[32:(end_day-start_day+1+pred_term)],
                                   state_name = "delta_exp",
                                   graph_title = "δ：ドリフト成分",
                                   y_label = "delta",
                                   date_labels = "%Y/%m",
                                   date_breaks = x.date_breaks) +
    # 表示期間の設定
    scale_x_datetime(limits = limits, date_breaks = x.date_breaks, date_labels = "%Y/%m")
  if(pred_term != 0){
    p_drift_pois <- p_drift_pois +
      geom_vline(xintercept = as.POSIXct(MIF_by.date$X.date[end_day]), linetype = 2, color = "darkblue")
  }
  # 周期成分
  p_cycle_pois <- plotSSM.CmdStanr(fit = fit, 
                                   time_vec = date.plot,
                                   state_name = "gamma_exp", 
                                   graph_title = "γ：周期成分", 
                                   y_label = "gamma",
                                   date_labels = "%Y/%m",
                                   date_breaks = x.date_breaks) +
    # 表示期間の設定
    scale_x_datetime(limits = limits, date_breaks = x.date_breaks, date_labels = "%Y/%m")
  if(pred_term != 0){
    p_cycle_pois <- p_cycle_pois +
      geom_vline(xintercept = as.POSIXct(MIF_by.date$X.date[end_day]), linetype = 2, color = "darkblue")
  }
  # ランダム成分
  p_random_pois <- plotSSM.CmdStanr(fit = fit, 
                                    time_vec = date.plot,
                                    state_name = "r_exp", 
                                    graph_title = "r：ランダム成分", 
                                    y_label = "r",
                                    date_labels = "%Y/%m",
                                    date_breaks = x.date_breaks) +
    # 表示期間の設定
    scale_x_datetime(limits = limits, date_breaks = x.date_breaks, date_labels = "%Y/%m")
  if(pred_term != 0){
    p_random_pois <- p_random_pois +
      geom_vline(xintercept = as.POSIXct(MIF_by.date$X.date[end_day]), linetype = 2, color = "darkblue")
  }
  # 水準成分＋時変係数成分
  p_mu_tvc_pois <- plotSSM.CmdStanr(fit = fit, 
                                    time_vec = date.plot,
                                    state_name = "mu_tvc_exp", 
                                    graph_title = "μ + tvf：水準成分＋時変係数成分", 
                                    y_label = "件数",
                                    date_labels = "%Y/%m",
                                    date_breaks = x.date_breaks) +
    # 表示期間の設定
    scale_x_datetime(limits = limits, date_breaks = x.date_breaks, date_labels = "%Y/%m")
  if(pred_term != 0){
    p_mu_tvc_pois <- p_mu_tvc_pois +
      geom_vline(xintercept = as.POSIXct(MIF_by.date$X.date[end_day]), linetype = 2, color = "darkblue")
  }
  # 時変係数ｘ稼働台数
  p_tvc_pois <- plotSSM.CmdStanr(fit = fit, 
                                 time_vec = date.plot,
                                 state_name = "tvc_exp", 
                                 graph_title = "tvf：時変係数ｘ稼働台数", 
                                 y_label = "件数",
                                 date_labels = "%Y/%m",
                                 date_breaks = x.date_breaks) +
    # 表示期間の設定
    scale_x_datetime(limits = limits, date_breaks = x.date_breaks, date_labels = "%Y/%m")
  if(pred_term != 0){
    p_tvc_pois <- p_tvc_pois +
      geom_vline(xintercept = as.POSIXct(MIF_by.date$X.date[end_day]), linetype = 2, color = "darkblue")
  }
  # 水準成分+時変係数×1台稼働
  p_mu_tvc_1_pois <- plotSSM.CmdStanr(fit = fit, 
                                      time_vec = date.plot,
                                      state_name = "mu_tvc_1_exp", 
                                      graph_title = "μ + tvc：水準成分+時変係数ｘ1台稼働", 
                                      y_label = "件数",
                                      date_labels = "%Y/%m",
                                      date_breaks = x.date_breaks) +
    # 表示期間の設定
    scale_x_datetime(limits = limits, date_breaks = x.date_breaks, date_labels = "%Y/%m")
  if(pred_term != 0){
    p_mu_tvc_1_pois <- p_mu_tvc_1_pois +
      geom_vline(xintercept = as.POSIXct(MIF_by.date$X.date[end_day]), linetype = 2, color = "darkblue")
  }
  # 時変係数×1台稼働
  p_tvc_1_pois <- plotSSM.CmdStanr(fit = fit, 
                                   time_vec = date.plot,
                                   state_name = "tvc_1", 
                                   graph_title = "tvc：時変係数", 
                                   y_label = "tvc",
                                   date_labels = "%Y/%m",
                                   date_breaks = x.date_breaks) +
    # 表示期間の設定
    scale_x_datetime(limits = limits, date_breaks = x.date_breaks, date_labels = "%Y/%m")
  if(pred_term != 0){
    p_tvc_1_pois <- p_tvc_1_pois +
      geom_vline(xintercept = as.POSIXct(MIF_by.date$X.date[end_day]), linetype = 2, color = "darkblue")
  }
  # グラフ描画
  plot <- plot_grid(p_lambda_pois, 
                    p_pred_pois,
                    p_mu_tvc_pois,
                    p_tvc_pois,
                    p_tvc_1_pois,
                    p_mu_pois,
                    p_drift_pois,
                    p_random_pois,
                    p_cycle_pois,
                    ncol = 1, 
                    align = "v")
  # now add the title
  title <- ggdraw() + 
    draw_label(
      str_c("Metis-MF3　　EM現象：", Phenom),
      fontface = 'bold',
      color = "darkblue",
      size = 30,
      x = 0,
      hjust = 0
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 100)
    )
  plot.title <- 
    plot_grid(
      title, plot,
      ncol = 1,
      # rel_heights values control vertical title margins
      rel_heights = c(0.03, 1)
    )
  # 集合グラフ保存
  ggsave(str_c(graph.path,Phenom,".時系列-",start_day,"~",end_day,".pdf"),
         plot = plot.title, device = cairo_pdf, dpi=300, width=40, height=30)
  # 予測区間グラフ保存
  ggsave(str_c(graph.path,Phenom,".予測区間-",start_day,"~",end_day,".pdf"),
         plot = p_pred_pois, device = cairo_pdf, dpi=300, width=20, height=5)
  # 信頼区間グラフ保存
  ggsave(str_c(graph.path,Phenom,".信頼区間-",start_day,"~",end_day,".pdf"),
         plot = p_lambda_pois, device = cairo_pdf, dpi=300, width=20, height=5)
  
  return(list(plot.title, p_pred_pois, p_lambda_pois))
}
