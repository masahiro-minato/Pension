# MF3のパラメータ実績にてMF4の未来予測を行う関数
MF4Future.pred <- function(# 入力データ/tsv形式
                           MF4_maintenance.tsv = "./tsv_data/Metis_MF4_2305.tsv",
                           MF4_machine.tsv = "./tsv_data/MF4_MIF_2305.tsv",
                           MF3_maintenance.tsv = "./tsv_data/Metis_MF3_2303.tsv",
                           MF3_machine.tsv = "./tsv_data/Metis_MIF_2303.tsv",
                           # MCMCサンプリング開始日と終了日
                           start_day.MF4 = "2023-03-01 JST",
                           end_day.MF4 = "2023-05-31 JST",
                           # 予測期間
                           pred_term = 30,
                           # 日次での稼働台数増加予測数 c(COOK,SINAI,VOLGA,AMUR中綴じ)
                           Num_gain = c(70,20,10,8),
                           # csv保存ディレクトリ
                           output_dir = "./csv/matrix",
                           # 並列処理
                           reduce_sum = FALSE,
                           # MCMCサンプリング
                           chains = 6,
                           parallel_chains = getOption("mc.cores", 24),
                           threads_per_chain = 2,
                           iter_warmup = 6000,
                           iter_sampling = 2000,
                           thin = 4,
                           max_treedepth = 12,
                           # タイトル
                           title = "Metis-MF4-Peripheral.MF3parm",
                           # サンプリング時間保存
                           save_exTime = FALSE,
                           # サンプリング結果保存
                           save_fit = FALSE,
                           # グラフ保存
                           save_graph = FALSE
                           ){
  #### EM件数の基本構造時系列モデルでの推定 ####
  print(Sys.time(), quote=F)
  # ファイル読込
  MF3_maintenance <- read_tsv(MF3_maintenance.tsv)　    # 保守データ
  MF3_machine <- read_tsv(MF3_machine.tsv)          　　# 機器データ
  
  # 周辺機機種ごとの時系列でのEM件数の抽出
  EM.count.MF3.Peripheral <- 
    MF3_maintenance %>% 
    dplyr::filter((Peripheral_name %in% c("COOK-C", "SINAI-H") & Treatment_location %in% c("ADF部")) | 
                    (Peripheral_name %in% c("AMUR-C(HY)", "AMUR-C中綴じ","VOLGA-E") & Treatment_location %in% c("ﾊﾟﾝﾁ部","ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部"))) %>%
    group_by(Maintenance_date, Peripheral_name) %>%
    summarise(
      EM.count = n()
    ) %>% 
    ungroup() %>% 
    pivot_wider(names_from = c(Peripheral_name),
                values_from = EM.count) %>% 
    mutate_all(~replace(., is.na(.), 0)) %>% 
    # 列名の変更
    set_colnames(c("Maintenance_date", "EM.SINAI", "EM.VOLGA", "EM.COOK", "EM.AMUR", "EM.AMUR.HY")) %>%
    # 不要列の削除
    select("Maintenance_date", "EM.COOK", "EM.SINAI", "EM.VOLGA", "EM.AMUR")
  # select("Maintenance_date", "EM.COOK", "EM.SINAI", "EM.VOLGA", "EM.AMUR", "EM.AMUR.HY")
  
  # 市場機台数
  MIF.count.MF3.Peripheral <- 
    MF3_machine %>% 
    group_by(納品年月日,ADF,フィニッシャー) %>% 
    summarise(
      N.date = n()
    ) %>% 
    # tidyr::drop_na() %>% # この行があると後処理がない場合のADFも削除される
    ungroup() %>% 
    pivot_wider(names_from = c(ADF,フィニッシャー),
                values_from = N.date) %>% 
    mutate_all(~replace(., is.na(.), 0)) %>% 
    # 周辺機機種ごとの市場機台数を集計/機種名の含まれる列の行合計を計算する
    mutate(
      MIF.COOK = select(.,contains("COOK-C")) %>% rowSums(na.rm=TRUE),
      MIF.SINAI = select(.,contains("SINAI-H")) %>% rowSums(na.rm=TRUE),
      MIF.VOLGA = select(.,contains("VOLGA-E")) %>% rowSums(na.rm=TRUE),
      MIF.AMUR = select(.,contains("AMUR-C中綴じ")) %>% rowSums(na.rm=TRUE)
      # MIF.AMUR.HY = select(.,contains("AMUR-C(HY)")) %>% rowSums(na.rm=TRUE)
    ) %>% 
    select(納品年月日, MIF.COOK, MIF.SINAI, MIF.VOLGA, MIF.AMUR)
  # select(納品年月日, MIF.COOK, MIF.SINAI, MIF.VOLGA, MIF.AMUR, MIF.AMUR.HY)
  
  # 結合
  X.date.MF3 <- seq(as.POSIXct("2019-01-01"), as.POSIXct("2023-03-31"), by = "day")
  MIF.date.MF3.Peripheral <- tibble(X.date = X.date.MF3)
  MF3.MIF_by.date <- 
    MIF.date.MF3.Peripheral %>% 
    full_join(MIF.count.MF3.Peripheral, by=c("X.date" = "納品年月日")) %>% 
    full_join(EM.count.MF3.Peripheral, by=c("X.date" = "Maintenance_date")) %>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
    # 市場機台数を累積和へ変換
    mutate_at(vars(contains("MIF")), cumsum)
  
  # データの準備
  start_day.MF3 = which(MF3.MIF_by.date$X.date == "2020-01-01 JST")  # "2020-01-01 JST"
  end_day.MF3 = which(MF3.MIF_by.date$X.date == "2023-03-31 JST")
  # pred_term = 30 # 予測期間
  data.num.MF3 <- end_day.MF3 - start_day.MF3 + 1 # MF3データ数
  # EM観測値のみをマトリクスへ変換
  names(MF3.MIF_by.date)
  # 列番号の取得
  X.date.colnum.MF3 <- which(names(MF3.MIF_by.date) == "X.date")
  # サンプリング期間におけるEM観測値をマトリクスへ変換と転置
  y3 <- t(as.matrix(MF3.MIF_by.date[start_day.MF3:end_day.MF3,][,c(-X.date.colnum.MF3, -c(2:5))]))
  # y3 <- t(as.matrix(MF3.MIF_by.date[start_day.MF3:end_day.MF3,][,c(-X.date.colnum.MF3, -c(2:6))]))
  # 稼働台数
  Num.tib.MF3 <- MF3.MIF_by.date[start_day.MF3:end_day.MF3,][,c(-X.date.colnum.MF3, -c(6:9))]
  # Num.tib.MF3 <- MF3.MIF_by.date[start_day.MF3:end_day.MF3,][,c(-X.date.colnum.MF3, -c(7:11))]
  # 正規化のために最大値を算出
  max_data.MF3 <-
    Num.tib.MF3 %>%
    summarize(across(where(is.numeric), \(x) max(x, na.rm = TRUE))) # R4.2.3以降
  max_data.MF3$MIF.COOK
  print(max_data.MF3)
  # ベクトルへの変換
  unlist(c(max_data.MF3))
  # 正規化
  for(col in names(max_data.MF3)){
    print(col)
    print(eval(parse(text = paste0("max_data.MF3$",col))))
    Num.tib.MF3 <- Num.tib.MF3 %>%
      mutate(
        !!col :=eval(parse(text = paste0("Num.tib.MF3$",col)))/eval(parse(text = paste0("max_data.MF3$",col)))
      )
  }
  # マトリクスへ変換と転置
  Num.MF3 <- t(as.matrix(Num.tib.MF3))
  
  
  # ファイル読込
  MF4_maintenance <- read_tsv(MF4_maintenance.tsv)　    # 保守データ
  MF4_machine <- read_tsv(MF4_machine.tsv)              # 機器データ
  
  # 周辺機名称
  MF4_maintenance %>% distinct(Peripheral_name) %>% dput()
  # 周辺機名称
  MF4_maintenance$Peripheral_name
  # 列名
  names(MF4_maintenance)
  # 周辺機機種ごとの時系列でのEM件数の抽出
  EM.count.MF4.Peripheral <- 
    MF4_maintenance %>% 
    dplyr::filter((Peripheral_name %in% c("COOK-D", "CATHERINE") & Treatment_location %in% c("ADF部")) | 
                    (Peripheral_name %in% c("AMUR-D(HY)", "AMUR-D中綴じ","VOLGA-H") & Treatment_location %in% c("ﾊﾟﾝﾁ部","ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部"))) %>%
    group_by(Maintenance_date, Peripheral_name) %>%
    summarise(
      EM.count = n()
    ) %>% 
    ungroup() %>% 
    pivot_wider(names_from = c(Peripheral_name),
                values_from = EM.count) %>% 
    mutate_all(~replace(., is.na(.), 0)) %>% 
    # 列名の変更
    set_colnames(c("Maintenance_date", "EM.CATHERINE", "EM.AMUR", "EM.COOK", "EM.VOLGA")) %>%
    # 不要列の削除
    select("Maintenance_date", "EM.COOK", "EM.CATHERINE", "EM.VOLGA", "EM.AMUR")
  
  # 市場機台数
  MIF.count.MF4.Peripheral <- 
    MF4_machine %>% 
    group_by(納品年月日,ADF,フィニッシャー) %>% 
    summarise(
      N.date = n()
    ) %>% 
    # tidyr::drop_na() %>% # この行があると後処理がない場合のADFも削除される
    ungroup() %>% 
    pivot_wider(names_from = c(ADF,フィニッシャー),
                values_from = N.date) %>% 
    mutate_all(~replace(., is.na(.), 0)) %>% 
    # 周辺機機種ごとの市場機台数を集計/機種名の含まれる列の行合計を計算する
    mutate(
      MIF.COOK = select(.,contains("COOK-D")) %>% rowSums(na.rm=TRUE),
      MIF.CATHERINE = select(.,contains("CATHERINE")) %>% rowSums(na.rm=TRUE),
      MIF.VOLGA = select(.,contains("VOLGA-H")) %>% rowSums(na.rm=TRUE),
      MIF.AMUR = select(.,contains("AMUR-D中綴じ")) %>% rowSums(na.rm=TRUE)
      # MIF.AMUR.HY = select(.,contains("AMUR-D(HY)")) %>% rowSums(na.rm=TRUE)
    ) %>% 
    select(納品年月日, MIF.COOK, MIF.CATHERINE, MIF.VOLGA, MIF.AMUR)
  
  # 結合
  X.date.MF4 <- seq(as.POSIXct("2023-02-01"), as.POSIXct("2023-05-31"), by = "day")
  MIF.date.MF4.Peripheral <- tibble(X.date = X.date.MF4)
  MF4.MIF_by.date <- 
    MIF.date.MF4.Peripheral %>% 
    full_join(MIF.count.MF4.Peripheral, by=c("X.date" = "納品年月日")) %>% 
    full_join(EM.count.MF4.Peripheral, by=c("X.date" = "Maintenance_date")) %>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
    # 市場機台数を累積和へ変換
    mutate_at(vars(contains("MIF")), cumsum)
  
  # データの準備
  start_day.MF4 = which(MF4.MIF_by.date$X.date == start_day.MF4)
  end_day.MF4 = which(MF4.MIF_by.date$X.date == end_day.MF4)
  pred_term = 30 # 予測期間
  data.num.MF4 <- end_day.MF4 - start_day.MF4 + 1 + pred_term # 予測期間を含むデータ数
  # EM観測値のみをマトリクスへ変換
  names(MF4.MIF_by.date)
  # 列番号の取得
  X.date.colnum.MF4 <- which(names(MF4.MIF_by.date) == "X.date")
  # サンプリング期間におけるEM観測値をマトリクスへ変換と転置
  y4 <- t(as.matrix(MF4.MIF_by.date[start_day.MF4:end_day.MF4,][,c(-X.date.colnum.MF4, -c(2:5))]))
  # 稼働台数
  Num.tib.MF4 <- MF4.MIF_by.date[start_day.MF4:end_day.MF4,][,c(-X.date.colnum.MF4, -c(6:9))]
  # 正規化のために最大値を算出
  max_data.MF4 <-
    Num.tib.MF4 %>%
    summarize(across(where(is.numeric), \(x) max(x, na.rm = TRUE))) # R4.2.3以降
  max_data.MF4$MIF.COOK
  print(max_data.MF4)
  # ベクトルへの変換
  unlist(c(max_data.MF4))
  # 正規化
  for(col in names(max_data.MF4)){
    print(col)
    print(eval(parse(text = paste0("max_data.MF4$",col))))
    Num.tib.MF4 <- Num.tib.MF4 %>%
      mutate(
        !!col :=eval(parse(text = paste0("Num.tib.MF4$",col)))/eval(parse(text = paste0("max_data.MF4$",col)))
      )
  }
  # マトリクスへ変換と転置
  Num.MF4 <- t(as.matrix(Num.tib.MF4))
  # データリストの作成
  data_list <- list(
    y3 = y3,
    y4 = y4,
    N = 4,
    Num3 = Num.MF3, # 正規化が必要
    Num4 = Num.MF4, # 正規化が必要
    T3 = end_day.MF3-start_day.MF3+1,
    T4 = end_day.MF4-start_day.MF4+1,
    N3_max = unlist(c(max_data.MF3)),
    N4_max = unlist(c(max_data.MF4)),
    grainsize = 250,
    pred_term = pred_term,
    Num_gain = Num_gain/unlist(c(max_data.MF4)) # 日次での稼働台数増加予測数
  )
  # コンパイル
  if(reduce_sum == TRUE){
    mod <- cmdstan_model("./stan/bsts-AR-tvc-rd-smo-pred-pois-periphe-mat_MF3parm.stan", cpp_options = list(stan_threads = TRUE))
    print("MCMCサンプリング並列処理")
  } else{
    mod <- cmdstan_model("./stan/bsts-AR-tvc-smo-pred-pois-periphe-mat_MF3parm.stan")
    print("MCMCサンプリング通常処理")
  }
  # 
  # マルチコア対応
  options(mc.cores = parallel::detectCores())
  # path
  # title <- "Metis-MF4-Peripheral.MF3parm"
  # output_dir <- str_c("./csv/matrix")
  output_basename <-str_c(title,".",format(Sys.time(), "%H-%M-%S"))
  object.path <- str_c("./Cmdstan_files/matrix/",title,".EM.pred.Peripheral.matrix.fit-",start_day.MF4,"~",end_day.MF4,".rds")
  # csv保存ディレクトリーの作成
  if(!dir.exists(output_dir)){
    dir.create(output_dir)}
  # MCMCサンプリング
  print("MCMCサンプリング開始")
  exTime <- system.time(
    if(reduce_sum == TRUE){
      fit <- mod$sample(data = data_list,
                        seed = 1234,
                        chains = chains,
                        parallel_chains = parallel_chains,
                        threads_per_chain = threads_per_chain,
                        iter_warmup = iter_warmup,
                        iter_sampling = iter_sampling,
                        thin = thin,
                        max_treedepth = max_treedepth,
                        refresh = 100,
                        init = 0, # エラー回避
                        output_dir = output_dir, # csvデータ保存ディレクトリ
                        output_basename = output_basename, # csvデータ保存名称
                        show_messages = FALSE
      )
    }else{
      fit <- mod$sample(data = data_list,
                        seed = 1234,
                        chains = chains,
                        iter_warmup = iter_warmup,
                        iter_sampling = iter_sampling,
                        thin = thin,
                        max_treedepth = max_treedepth,
                        refresh = 100,
                        init = 0, # エラー回避
                        output_dir = output_dir, # csvデータ保存ディレクトリ
                        output_basename = output_basename, # csvデータ保存名称
                        show_messages = FALSE
      )
    }
  )
  
  # 実行時間
  exTimeTable <- data.frame(user.self = exTime["user.self"], 
                            sys.self = exTime["sys.self"],
                            elapsed = exTime["elapsed"],
                            title = title,
                            row.names = "time")
  exTimeTable
  # 保存
  if(save_exTime == TRUE){
    write_tsv(exTimeTable, str_c("./time/exTimeTable.",title,".",start_day.MF4,"~",end_day.MF4,".tsv"))
  }
  # 結果保存
  if(save_fit == TRUE){
    fit$save_object(file = object.path)
  }
  # サンプリング結果表示
  fit$print(c( "s_z3","s_s3", "s_r3", "s_t3", "b_ar3", "b_ope4[1,1]", "b_ope4[2,2]","b_ope4[3,3]","Intercept4", "lp__"), max_rows=100)
  # rhatヒストグラム
  fit %>% bayesplot::rhat() %>% hist(main=str_c("Histogram of ",title))
  # 時系列グラフ用ｘ軸データ
  time_vec <- 
    seq(
      from = as.POSIXct("2023-03-01 JST"),
      by = "days",
      len = data.num.MF4 # end_day-start_day+1+pred_term
    )
  length(time_vec)
  
  names(MF4.MIF_by.date)[2:5]
  
  # 表示パラメータの設定
  parm <- "y_pred"
  # 表示パラメータの抽出
  parm.tib <- 
    as_tibble(fit$draws(parm) %>% as_draws_df)
  # 分位数の算出
  parm.quan <- 
    apply(parm.tib, 2, function(i){quantile(i,prob=c(0.025, 0.5, 0.975), na.rm=TRUE)})
  parm.quan <- 
    as_tibble(parm.quan) %>% 
    # 不要列の削除
    select(-.chain, -.iteration, -.draw )
  # 周辺機名称ベクトルの作成
  Peripheral.MF4 <- substring(names(EM.count.MF4.Peripheral)[-1], 4,)
  # グラフ表示用データフレームの作成
  result_df <- 
    as_tibble(t(parm.quan),.name_repair = 'unique') %>% 
    mutate(
      Peripheral = rep(Peripheral.MF4, data.num.MF4)
    ) %>% 
    # 列名設定
    set_colnames(c("lwr", "fit", "upr", "Peripheral"))
  # 時間軸項の追加
  result_df$time <- 
    rep(time_vec, times = rep(length(Peripheral.MF4), length(time_vec)))
  # レベル設定
  result_df$Peripheral <- 
    factor(result_df$Peripheral, levels=Peripheral.MF4)
  # DFとFINへ区分
  result_df.ADF <- 
    result_df %>% 
    dplyr::filter(Peripheral %in% c("COOK","CATHERINE"))
  result_df.FIN <- 
    result_df %>% 
    dplyr::filter(Peripheral %in% c("VOLGA","AMUR"))
  
  # b_ar 自己回帰係数
  b_ar <- 
    fit$draws("b_ar4") %>% 
    as_draws_df
  # b_ar平均値
  b_ar.mean <- round(fit$draws("b_ar4") %>% apply(3,mean),3)
  print(b_ar.mean)
  
  # 測定値データ
  MF4.EM_by.date <- 
    MF4.MIF_by.date %>% 
    select(-starts_with("MIF.")) %>%
    set_colnames(c("X.date",Peripheral.MF4)) %>% 
    pivot_longer(cols = all_of(Peripheral.MF4),
                 names_to = "Peripheral",
                 values_to = "EM.num")
  MF4.EM_by.date$Peripheral <- 
    factor(MF4.EM_by.date$Peripheral, levels = Peripheral.MF4)
  # ADFのみ抽出
  MF4.EM_by.date.ADF <- 
    MF4.EM_by.date %>% 
    dplyr::filter(Peripheral %in% c("COOK","CATHERINE"))
  # FINのみ抽出
  MF4.EM_by.date.FIN <- 
    MF4.EM_by.date %>% 
    dplyr::filter(Peripheral %in% c("VOLGA","AMUR"))
  
  # フォント定義
  par(family="Noto Sans")
  # グラフタイトル
  if (parm == "lambda_exp") {
    graph_title <- str_c("Metis-MF4 EM件数時系列分析 λ(μ + γ + tvf + r)：すべての成分を含んだ状態推定値")
    fill.graph <- "lightblue"
  }else if (parm == "y_pred") {
    graph_title <- str_c("Metis-MF4 EM件数時系列分析 95%予測区間（MF3のパラメータでの予測）")
    fill.graph <- "lightgreen"
  }
  # 縦軸名称
  y_label = "件数"
  # 凡例設定
  vec <- c()
  for (i in 1:length(Peripheral.MF4)) {
    vec <- 
      vec %>% 
      append(str_c(i,".",Peripheral.MF4[i]," (自己回帰係数 b_ar = ",b_ar.mean[i],")"))
  }
  vec.ADF <- c()
  for (i in 1:2) {
    vec.ADF <- 
      vec.ADF %>% 
      append(str_c(Peripheral.MF4[i]," (自己回帰係数 b_ar = ",b_ar.mean[i],")"))
  }
  vec.FIN <- c()
  for (i in 3:4) {
    vec.FIN <- 
      vec.FIN %>% 
      append(str_c(Peripheral.MF4[i]," (自己回帰係数 b_ar = ",b_ar.mean[i],")"))
  }
  # ADF+FIN
  Att.labs <- vec 
  names(Att.labs) <- Peripheral.MF4
  # グラフの表示期間の設定
  limits = c(as.POSIXct("2023-03-01 JST"), as.POSIXct(MF4.MIF_by.date$X.date[end_day.MF4] + 3600*24*pred_term))
  # ｙ軸目盛設定
  breaks=seq(0,30,5)
  
  result_df$fit
  # 図示
  p <- ggplot(data = result_df, aes(x = time)) + 
    theme_bw() + 
    labs(title = graph_title) +
    theme(plot.title = element_text(size = 18,  #font size and adjust
                                    hjust = 0.01,#adjust
    )) +
    ylab(y_label) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4, fill=fill.graph) + 
    geom_line(aes(y = fit), linewidth = 1.2) +
    geom_point(alpha = 1.0, size = 2.0,
               data = MF4.EM_by.date, mapping =  aes(x = X.date, y = EM.num, color = Peripheral)) +
    theme(axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(angle = 30, hjust = 1, size = 14),
          axis.text.y = element_text(size = 16)) +
    theme(legend.position = "none") +
    theme(strip.background = element_blank(), strip.text = element_text(size = 16, hjust = 0)) +
    facet_wrap(~Peripheral, ncol = 1, labeller = labeller(Peripheral = Att.labs)) +
    # geom_vline(xintercept = as.POSIXct("2023-05-01 JST"), linetype = 2, color = "darkblue") + # 垂直線
    scale_x_datetime(limits = limits, date_breaks = "1 month", date_labels = "%Y/%m") + # 表示期間の設定
    # scale_y_continuous(breaks = breaks) +
    coord_cartesian(ylim = c(0,20)) +
    # 未来予測期間の文字表示
    annotate("text", x = as.POSIXct("2023-06-01 JST"), y = Inf,
             hjust = -0.01, # 文字のｘ方向位置調整 マイナスで右へ移動
             vjust = 2, 　　# 文字のｙ方向位置調整 プラスで下へ移動
             label = "⇒ 【未来予測区間】",
             size = 5,
             colour = "darkblue")
  
  plot(p)
  
  Num_gain_str <- str_c(Num_gain, collapse=",")
  
  # 予測区間+取得データグラフ保存
  if(save_graph == TRUE){
    n <- 4
    # eval(parse(text = paste0(title,".EM件数_予測区間 <- p")))
    ggsave(str_c("./PDF/",title,".EM件数_予測区間-",start_day.MF4,"~",end_day.MF4,"_[",Num_gain_str,"].pdf"),
           plot = p,
           device = cairo_pdf, dpi=300, width=10, height=(n*2.5))
    saveRDS(p, file = str_c("./rds/",title,".EM件数_予測区間-",start_day.MF4,"~",end_day.MF4,"_[",Num_gain_str,"].rds"))
    # eval(parse(text = paste0("save(",title,".EM件数_予測区間,file='./rda/",title,".EM件数_予測区間.rda')")))
  }
  
  # ADF
  Att.labs <- vec.ADF
  names(Att.labs) <- Peripheral.MF4[1:2]
  # グラフの表示期間の設定
  limits = c(as.POSIXct("2023-03-01 JST"), as.POSIXct(MF4.MIF_by.date$X.date[end_day.MF4] + 3600*24*pred_term))
  # ｙ軸目盛設定
  breaks=seq(0,4,1)
  # 図示
  p.ADF <- ggplot(data = result_df.ADF, aes(x = time)) + 
    theme_bw() + 
    labs(title = graph_title) +
    theme(plot.title = element_text(size = 18,  #font size and adjust
                                    hjust = 0.01,#adjust
    )) +
    ylab(y_label) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4, fill=fill.graph) + 
    geom_line(aes(y = fit), linewidth = 1.2) +
    geom_point(alpha = 1.0, size = 0.9,
               data = MF4.EM_by.date.ADF, mapping =  aes(x = X.date, y = EM.num, color = Peripheral)) +
    theme(axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(angle = 30, hjust = 1, size = 14),
          axis.text.y = element_text(size = 16)) +
    theme(legend.position = "none") +
    theme(strip.background = element_blank(), strip.text = element_text(size = 16, hjust = 0)) +
    facet_wrap(~Peripheral, ncol = 1, labeller = labeller(Peripheral = Att.labs)) +
    # geom_vline(xintercept = as.POSIXct("2023-03-31 JST"), linetype = 2, color = "darkblue") + # 垂直線
    scale_x_datetime(limits = limits, date_breaks = "1 month", date_labels = "%Y/%m") + # 表示期間の設定
    # scale_y_continuous(breaks = breaks) +
    coord_cartesian(ylim = c(0,20)) +
    # 未来予測期間の文字表示
    annotate("text", x = as.POSIXct("2023-06-01 JST"), y = Inf,
             hjust = -0.01, # 文字のｘ方向位置調整 マイナスで右へ移動
             vjust = 2, 　　# 文字のｙ方向位置調整 プラスで下へ移動
             label = "⇒ 【未来予測区間】",
             size = 5,
             colour = "darkblue")
  
  plot(p.ADF)
  
  # 予測区間+取得データグラフ保存
  if(save_graph == TRUE){
    n <- 2
    # eval(parse(text = paste0(title,".ADF_EM件数_予測区間 <- p.ADF")))
    ggsave(str_c("./PDF/",title,".ADF_EM件数_予測区間_",start_day.MF4,"~",end_day.MF4,"_[",Num_gain_str,"].pdf"),
           plot = p.ADF,
           device = cairo_pdf, dpi=300, width=10, height=(n*2.5))
    saveRDS(p.ADF, file = str_c("./rds/",title,".ADF_EM件数_予測区間-",start_day.MF4,"~",end_day.MF4,"_[",Num_gain_str,"].rds"))
    # eval(parse(text = paste0("save(",title,".ADF_EM件数_予測区間,file='./rda/",title,".ADF_EM件数_予測区間.rda')")))
  }
  
  # FIN
  Att.labs <- vec.FIN
  names(Att.labs) <- Peripheral.MF4[3:4]
  # グラフの表示期間の設定
  limits = c(as.POSIXct("2023-03-01 JST"), as.POSIXct(MF4.MIF_by.date$X.date[end_day.MF4] + 3600*24*pred_term))
  # ｙ軸目盛設定
  breaks=seq(0,4,1)
  # 図示
  p.FIN <- ggplot(data = result_df.FIN, aes(x = time)) + 
    theme_bw() + 
    labs(title = graph_title) +
    theme(plot.title = element_text(size = 18,  #font size and adjust
                                    hjust = 0.01,#adjust
    )) +
    ylab(y_label) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4, fill=fill.graph) + 
    geom_line(aes(y = fit), linewidth = 1.2) +
    geom_point(alpha = 1.0, size = 0.9,
               data = MF4.EM_by.date.FIN, mapping =  aes(x = X.date, y = EM.num, color = Peripheral)) +
    theme(axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(angle = 30, hjust = 1, size = 14),
          axis.text.y = element_text(size = 16)) +
    theme(legend.position = "none") +
    theme(strip.background = element_blank(), strip.text = element_text(size = 16, hjust = 0)) +
    facet_wrap(~Peripheral, ncol = 1, labeller = labeller(Peripheral = Att.labs)) +
    # geom_vline(xintercept = as.POSIXct("2023-03-31 JST"), linetype = 2, color = "darkblue") + # 垂直線
    scale_x_datetime(limits = limits, date_breaks = "1 month", date_labels = "%Y/%m") + # 表示期間の設定
    # scale_y_continuous(breaks = breaks) +
    coord_cartesian(ylim = c(0,5)) +
    # 未来予測期間の文字表示
    annotate("text", x = as.POSIXct("2023-06-01 JST"), y = Inf,
             hjust = -0.01, # 文字のｘ方向位置調整 マイナスで右へ移動
             vjust = 2, 　　# 文字のｙ方向位置調整 プラスで下へ移動
             label = "⇒ 【未来予測区間】",
             size = 5,
             colour = "darkblue")
  
  plot(p.FIN)
  
  # 予測区間+取得データグラフ保存
  if(save_graph == TRUE){
    n <- 2
    # eval(parse(text = paste0(title,".FIN_EM件数_予測区間 <- p.FIN")))
    ggsave(str_c("./PDF/",title,".FIN_EM件数_予測区間_",start_day.MF4,"~",end_day.MF4,"_[",Num_gain_str,"].pdf"),
           plot = p.FIN,
           device = cairo_pdf, dpi=300, width=10, height=(n*2.5))
    saveRDS(p.FIN, file = str_c("./rds/",title,".FIN_EM件数_予測区間-",start_day.MF4,"~",end_day.MF4,"_[",Num_gain_str,"].rds"))
    # eval(parse(text = paste0("save(",title,".FIN_EM件数_予測区間,file='./rda/",title,".FIN_EM件数_予測区間.rda')")))
  }
  print(Sys.time(), quote=F)
  return(fit)
}
