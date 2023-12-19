Addingdata.to.graph <- function(
    # QISSエクセルファイルの読込
    path.excel = "./excel", 　　　　　　 # ファイルのフォルダーパス
    pattern1 = "〇日次_保守*.+\\.xlsx$", # 保守データ
    pattern2 = "〇共通_周辺機装着情報*.+\\.csv$", # 周辺機装着情報
    # 実測値の開始日
    start_day.MF4.JST = "2023-06-01 JST",
    # MF4初ロット製造開始月
    manuf.start.month = "2022/12/01",
    # 未来予測区間グラフデータ名称/拡張子は不要
    graph.name = "Metis-MF4-Peripheral.MF3parm_tmp2.EM件数_予測区間-29~120_[70,20,10,8]",
    graph.name.ADF = "Metis-MF4-Peripheral.MF3parm_tmp2.ADF_EM件数_予測区間-29~120_[70,20,10,8]",
    graph.name.FIN = "Metis-MF4-Peripheral.MF3parm_tmp2.FIN_EM件数_予測区間-29~120_[70,20,10,8]"
){
  # EM件数未来予測区間グラフへの実測値をプロットする関数
  # 戻り値：グラフデータのリスト（"ALL","ADF","FIN"）
  
  # 戻り値初期化
  return.obje <- list()
  
  # 該当エクセルファイルのパス取得
  files1 <- list.files(path = path.excel, pattern = pattern1, full.names = T)
  print(files1)
  files2 <- list.files(path = path.excel, pattern = pattern2, full.names = T)
  print(files2)
  
  # ファイル読込
  Metis_master <- 
    read_excel("./excel/●共通_周辺機マスター.xlsx", sheet = 1, skip = 0)
  Metis_standard <- 
    read_excel("./excel/●共通_周辺機標準搭載構成.xlsx", sheet = 1, skip = 0) %>% 
    select_if(~sum(!is.na(.)) > 0)
  
  Metis_sheet1 <- 
    read_excel(files1, sheet = 1, skip = 0)
  Metis_sheet2 <- 
    read.delim(files2, fileEncoding = "cp932")
  
  # 空白を削除/デバイスコードの後ろに空白が存在するため、周辺機マスタの周辺機機種略と合致しない
  Metis_sheet2$デバイスコード <- 
    gsub("[[:blank:]]", "", Metis_sheet2$デバイスコード)
  # 周辺機装着情報へ周辺機マスタを紐づけて周辺機名列を作成
  Metis_sheet2 <- 
    Metis_sheet2 %>% 
    left_join(Metis_master, by = c("デバイスコード" = "周辺機機種略")) %>% 
    rename( 周辺機名 = 周辺機開発名)
  # 機種略機番列の作成
  Metis_sheet2a <-
    Metis_sheet2 %>%
    mutate(
      機種略機番 = paste(機種略号, 機番, sep="")
    ) %>% 
    distinct_all()
  
  # 初回訪問区分他のtype変更
  Metis_sheet1 <- 
    Metis_sheet1 %>% 
    mutate(
      初回訪問区分 = as.logical(初回訪問区分),
      ｵﾌﾟｼｮﾝ機種略号= as.logical(ｵﾌﾟｼｮﾝ機種略号),
      ｵﾌﾟｼｮﾝ機番= as.logical(ｵﾌﾟｼｮﾝ機番)
    )
  # 機種略機番列の作成
  Metis_sheet1a <- 
    Metis_sheet1 %>% 
    mutate(
      機種略機番 = paste(機種略号, 機番, sep="")
    ) %>% 
    distinct_all()
  
  # 結合
  Metis_sheet1a2join <- left_join(Metis_sheet1a, Metis_sheet2a, by="機種略機番", relationship = "many-to-many")
  Metis_sheet1a2join %>% distinct_all()
  # print(names(Metis_sheet1a2join))
  
  # 列の選定
  Metis_MF4 <- Metis_sheet1a2join %>% 
    select(機種略号.x,機種略機番,機種ｺｰﾄﾞ,訪問区分,
           年月度,製造年月,納入日.x,稼動月,保守実施日,
           CE作業時間,ｺｰﾙ,EM,現象,現象SC名称,処置場所,周辺機名) %>%
    dplyr::filter(EM == 1) %>% 
    distinct_all()
  
  # 日付へ変換
  Metis_MF4$年月度 <- 
    paste(str_sub(Metis_MF4$年月度, start=1, end=4),
          str_sub(Metis_MF4$年月度, start=5, end=-1),("01"),
          sep="-") %>% 
    as.POSIXct() %>% 
    as.Date(tz = "Asia/Tokyo")
  
  Metis_MF4$製造年月 <- 
    paste(str_sub(Metis_MF4$製造年月, start=1, end=4),
          str_sub(Metis_MF4$製造年月, start=5, end=-1),("01"),
          sep="-") %>% 
    as.POSIXct() %>% 
    as.Date(tz = "Asia/Tokyo")
  
  Metis_MF4$納入日.x <- 
    paste(str_sub(Metis_MF4$納入日.x, start=1, end=4),
          str_sub(Metis_MF4$納入日.x, start=5, end=6),
          str_sub(Metis_MF4$納入日.x, start=7, end=-1),
          sep="-") %>% 
    as.POSIXct() %>% 
    as.Date(tz = "Asia/Tokyo")
  
  Metis_MF4$保守実施日 <- 
    paste(str_sub(Metis_MF4$保守実施日, start=1, end=4),
          str_sub(Metis_MF4$保守実施日, start=5, end=6),
          str_sub(Metis_MF4$保守実施日, start=7, end=-1),
          sep="-") %>% 
    as.POSIXct() %>% 
    as.Date(tz = "Asia/Tokyo")
  
  # 列名変更
  print("列名変更")
  colnames(Metis_MF4)
  Metis_MF4 <- 
    rename(Metis_MF4, 
           Model_abbreviation = 機種略号.x,
           Machine_numbers = 機種略機番,
           Model_code = 機種ｺｰﾄﾞ,
           Visit_classification = 訪問区分,
           Year_and_month = 年月度,
           Manufacturing_date = 製造年月,
           Due_date = 納入日.x,
           Working_month = 稼動月,
           Maintenance_date = 保守実施日,
           CE_Working_hours = CE作業時間,
           Call = ｺｰﾙ,
           Phenomenon = 現象,
           SC_name = 現象SC名称,
           Treatment_location = 処置場所,
           # Peripheral_machine = ...17,
           Peripheral_name = 周辺機名
    )
  
  # 製造経過月数・稼働月数
  print("製造経過月数・稼働月数")
  Metis_MF4a <- 
    Metis_MF4 %>% 
    rowwise() %>%
    mutate(
      Elapsed_mf_months = 
        (length(seq(as.Date(manuf.start.month), as.Date(Manufacturing_date), "month"))-1),
      Working_months = 
        (length(seq(as.Date(Due_date), as.Date(Maintenance_date), "month"))-1)
    ) %>% 
    ungroup()
  # 作業月の追加
  MF4_maintenance <<- 
    Metis_MF4a %>% 
    mutate(
      Maintenance_month = as.Date(paste(str_sub(Maintenance_date, start=1, end=4),
                                        str_sub(Maintenance_date, start=6, end=7),
                                        ("01"), sep="-"),tz = "Asia/Tokyo")
    ) %>% 
    distinct_all()
  
  # 周辺機名称・処置部
  # print(MF4_maintenance %>% distinct(Peripheral_name) %>% dput())
  # print(MF4_maintenance %>% distinct(Treatment_location) %>% dput())
  
  # 周辺機機種ごとの時系列でのEM件数の抽出
  EM.count.MF4.Peripheral <- 
    MF4_maintenance %>% 
    dplyr::filter((Peripheral_name %in% c("COOK-D", "CATHERINE") & 
                     Treatment_location %in% c("ADF部")) | 
                  (Peripheral_name %in% c("AMUR-D HY", "AMUR-D", "VOLGA-H(中綴じ有)", "VOLGA-H(中綴じ無)") & 
                     Treatment_location %in% c("ﾊﾟﾝﾁ部","ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部","ｽﾃｰﾌﾟﾙ部"))) %>%
    group_by(Maintenance_date, Peripheral_name) %>%
    summarise(
      EM.count = n()
    ) %>% 
    ungroup() %>% 
    pivot_wider(names_from = c(Peripheral_name),
                values_from = EM.count) %>% 
    mutate_all(~replace(., is.na(.), 0)) %>% 
    # 列名の変更
    rename(
      EM.COOK = `COOK-D`,
      EM.CATHERINE = CATHERINE,
      EM.AMUR = `AMUR-D`,
      EM.AMUR_HY = `AMUR-D HY`,
      EM.VOLGA中綴じ有 = `VOLGA-H(中綴じ有)`,
      EM.VOLGA中綴じ無 = `VOLGA-H(中綴じ無)`
    ) %>%
    # 列順が異なる場合があり下記コードではNG
    # set_colnames(c("Maintenance_date", "EM.COOK", "EM.VOLGA中綴じ有", "EM.AMUR", "EM.CATHERINE", "EM.AMUR_HY", "EM.VOLGA中綴じ無")) %>%
    # set_colnames(c("Maintenance_date", "EM.CATHERINE", "EM.COOK", "EM.AMUR", "EM.VOLGA中綴じ有", "EM.AMUR_HY", "EM.VOLGA中綴じ無")) %>%
    mutate(
      EM.VOLGA = EM.VOLGA中綴じ有 + EM.VOLGA中綴じ無,
      EM.AMUR = EM.AMUR + EM.AMUR_HY
    ) %>% 
    # 不要列の削除
    select("Maintenance_date", "EM.COOK", "EM.CATHERINE", "EM.VOLGA", "EM.AMUR")
  # 周辺機名称ベクトルの作成
  Peripheral.MF4 <- substring(names(EM.count.MF4.Peripheral)[-1], 4,)
  # 結合
  end_day.MF4.JST <- max(EM.count.MF4.Peripheral$Maintenance_date)
  X.date.MF4 <- seq(as.POSIXct("2023-02-01"), as.POSIXct(end_day.MF4.JST), by = "day")
  Date.MF4.Peripheral <- tibble(X.date = X.date.MF4)
  MF4.EM_by.date <- 
    Date.MF4.Peripheral %>% 
    full_join(EM.count.MF4.Peripheral, by=c("X.date" = "Maintenance_date")) %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
    set_colnames(c("X.date",Peripheral.MF4)) %>% 
    pivot_longer(cols = all_of(Peripheral.MF4),
                 names_to = "Peripheral",
                 values_to = "EM.num")
  # レベル設定
  MF4.EM_by.date$Peripheral <- 
    factor(MF4.EM_by.date$Peripheral, levels = Peripheral.MF4)
  
  # 未来予測区間の実測値データ
  MF4.EM_by.date.Future <<- 
    MF4.EM_by.date %>% 
    dplyr::filter(X.date >= start_day.MF4.JST)
  # ADFのみ抽出
  MF4.EM_by.date.Future.ADF <- 
    MF4.EM_by.date.Future %>% 
    dplyr::filter(Peripheral %in% c("COOK","CATHERINE"))
  # FINのみ抽出
  MF4.EM_by.date.Future.FIN <- 
    MF4.EM_by.date.Future %>% 
    dplyr::filter(Peripheral %in% c("VOLGA","AMUR"))
  
  # グラフデータの読込
  print("グラフデータ読込・・・")
  p <- readRDS(str_c("./rds/",graph.name,".rds"))
  p.ADF <- readRDS(str_c("./rds/",graph.name.ADF,".rds"))
  p.FIN <- readRDS(str_c("./rds/",graph.name.FIN,".rds"))
  
  # フォント定義
  par(family="Noto Sans")
  # 未来予測区間への実測値プロット
  p.actualresults <- 
    p + geom_point(alpha = 1.0, size = 3.0,
                   data = MF4.EM_by.date.Future, 
                   mapping =  aes(x = X.date, y = EM.num, color = Peripheral), shape = 18)
  
  p.ADF.actualresults <- 
    p.ADF + geom_point(alpha = 1.0, size = 3.0,
                       data = MF4.EM_by.date.Future.ADF, 
                       mapping =  aes(x = X.date, y = EM.num, color = Peripheral), shape = 18)
  
  p.FIN.actualresults <- 
    p.FIN + geom_point(alpha = 1.0, size = 3.0,
                       data = MF4.EM_by.date.Future.FIN, 
                       mapping =  aes(x = X.date, y = EM.num, color = Peripheral), shape = 18)
  
  # グラフ保存
  n <- 4
  ggsave(str_c("./PDF/",graph.name,"＆計測値.pdf"),
         plot = p.actualresults, device = cairo_pdf, dpi=300, width=10, height=(n*2.5))
  n <- 2
  ggsave(str_c("./PDF/",graph.name.ADF,"＆計測値.pdf"),
         plot = p.ADF.actualresults, device = cairo_pdf, dpi=300, width=10, height=(n*2.5))
  n <- 2
  ggsave(str_c("./PDF/",graph.name.FIN,"＆計測値.pdf"),
         plot = p.FIN.actualresults, device = cairo_pdf, dpi=300, width=10, height=(n*2.5))
  
  # リスト作成
  print("リスト作成")
  return.obje <- append(return.obje, list(p.actualresults))
  return.obje <- append(return.obje, list(p.ADF.actualresults))
  return.obje <- append(return.obje, list(p.FIN.actualresults))
  index <- length(return.obje)
  names(return.obje)[c(1,3)] <- c("ALL","ADF","FIN")
  
  return(return.obje)
}
