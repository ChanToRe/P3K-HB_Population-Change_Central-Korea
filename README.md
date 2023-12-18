<img src="https://user-images.githubusercontent.com/64909586/186408061-58a88e85-be08-47f2-b3b3-2c9e04a9dec6.png" height=65>

# 원삼국시대~백제 한성기 중부지역의 인구변동과 배경

---

## 소개
이 레포지토리는 주찬혁의 석사학위논문인 ｢원삼국시대~백제 한성기 중부지역의 인구변동과 배경｣의 데이터, 그래프, 코드를 정리한 레포지토리입니다. 해당 논문은 아래에서 확인하실 수 있습니다.

[주찬혁, 2023, ｢원삼국시대~백제 한성기 중부지역의 인구변동과 배경｣, 숭실대학교 석사학위논문.]()

레포지토리는 크게 3개의 디렉토리로 구성되어 있습니다. `Data`에는 연구에 사용된 데이터가 있고, `Script`에는 분석 및 그래프 산출을 위한 R코드가 있으며, `Graph`에는 최종적으로 산출된 도면이 있습니다. `Script`는 각 분석에 따라 주거지 개수:`PithouseNum`, 주거지 면적:`PithouseSize`, SPD 및 베이지안 모델:`SPD` 디렉토리로 구분됩니다. 마지막으로 `SPD`는 분석조건에 따라 중부지역 전체:`By_All`, 문화권별:`By_Cultural`, 지역별:`By_Area`로 구분됩니다. 본문에 사용하지 않은 그래프 및 스크립트는 각 `NotUsed` 폴더에 존재합니다.

---

## 파일구조

```
📦P3K-HB_Population-Change_Central-Korea
 ┣ 📂Data
 ┃ ┣ 📜Bakje_WarRecords.csv
 ┃ ┣ 📜C14_Master.csv
 ┃ ┣ 📜Goguryeo_Defense.csv
 ┃ ┗ 📜Pithouse_Master.csv
 ┣ 📂Graph
 ┃ ┣ 📂NotUsed
 ┃ ┃ ┣ 📜Demographics(Pithouse Size - 100y).tiff
 ┃ ┃ ┣ 📜Demographics(Pithouse Size 50).tiff
 ┃ ┃ ┣ 📜dexp_fit(경기남부+호서북부).tiff
 ┃ ┃ ┣ 📜dexp_fit(경기북부+영서).tiff
 ┃ ┃ ┣ 📜dexp_fit(마한문화권).tiff
 ┃ ┃ ┣ 📜dexp_fit(신라제외).tiff
 ┃ ┃ ┣ 📜dexp_fit(중도문화권).tiff
 ┃ ┃ ┣ 📜dexp_fit(중부지역).tiff
 ┃ ┃ ┣ 📜dexp_fit(한강중류).tiff
 ┃ ┃ ┣ 📜dexp_fit(한강하류).tiff
 ┃ ┃ ┣ 📜dexp_HPD(한강중류).tiff
 ┃ ┃ ┣ 📜dexp_HPD(한강하류).tiff
 ┃ ┃ ┣ 📜dexp_model(한강중류).tiff
 ┃ ┃ ┣ 📜dexp_model(한강하류).tiff
 ┃ ┃ ┣ 📜SPD(경기남부+호서북부).tiff
 ┃ ┃ ┣ 📜SPD(경기북부+영서).tiff
 ┃ ┃ ┣ 📜SPD(마한문화권).tiff
 ┃ ┃ ┣ 📜SPD(신라제외).tiff
 ┃ ┃ ┣ 📜SPD(중도문화권).tiff
 ┃ ┃ ┣ 📜SPD(한강중류).tiff
 ┃ ┃ ┗ 📜SPD(한강하류).tiff
 ┃ ┣ 📜Fig2-1_유적분포도.tiff
 ┃ ┣ 📜Fig2-2(1)_방형주거지 면적 분산.tiff
 ┃ ┣ 📜Fig2-2(2)_여철자형주거지 면적 분산.tiff
 ┃ ┣ 📜Fig3-11_여주 연양리 7호 주거지 일괄.jpg
 ┃ ┣ 📜Fig3-13_백제 한성기 편년 절대연대 부여자료.jpg
 ┃ ┣ 📜Fig3-14(1)_주거지 개수(551안).tiff
 ┃ ┣ 📜Fig3-14(2)_주거지 개수(551안 보정).tiff
 ┃ ┣ 📜Fig3-1_중부지역 주거지 탄소연대 SPD.tiff
 ┃ ┣ 📜Fig3-3(1)_dexp_HPD(중부지역).tiff
 ┃ ┣ 📜Fig3-3(2)_dexp_HPD(신라제외).tiff
 ┃ ┣ 📜Fig3-4(1)_dexp_model(중부지역).tiff
 ┃ ┣ 📜Fig3-4(2)_dexp_model(신라제외).tiff
 ┃ ┣ 📜Fig3-5(1)_dexp_HPD(마한문화권).tiff
 ┃ ┣ 📜Fig3-5(2)_dexp_HPD(중도문화권).tiff
 ┃ ┣ 📜Fig3-6(1)_dexp_model(마한문화권).tiff
 ┃ ┣ 📜Fig3-6(2)_dexp_model(중도문화권).tiff
 ┃ ┣ 📜Fig3-7(1)_dexp_HPD(경기북부+영서).tiff
 ┃ ┣ 📜Fig3-7(2)_dexp_HPD(경기남부+호서북부).tiff
 ┃ ┣ 📜Fig3-8(1)_dexp_model(경기북부+영서).tiff
 ┃ ┣ 📜Fig3-8(2)_dexp_model(경기남부+호서북부).tiff
 ┃ ┣ 📜Fig3-9_주거지 개수(475안).tiff
 ┃ ┣ 📜Fig4-1_인구분포 변화.tiff
 ┃ ┣ 📜Fig4-3_토기가마 분포와 인구분포.png
 ┃ ┣ 📜Fig4-7_백제 전쟁 기록.tif
 ┃ ┗ 📜Fig4-8_이형토기 출토 대형주거지.tif
 ┣ 📂Script
 ┃ ┣ 📂NotUsed
 ┃ ┃ ┣ 📂PithouseSize
 ┃ ┃ ┃ ┣ 📜PithouseSize_100.R
 ┃ ┃ ┃ ┗ 📜PithouseSize_50.R
 ┃ ┃ ┗ 📂SPD
 ┃ ┃ ┃ ┣ 📜BaysianModel(dExp_model_한강중류).R
 ┃ ┃ ┃ ┣ 📜BaysianModel(dExp_model_한강하류).R
 ┃ ┃ ┃ ┣ 📜SPD(Pithouse_경기남부+호서북부).R
 ┃ ┃ ┃ ┣ 📜SPD(Pithouse_경기북부+영서).R
 ┃ ┃ ┃ ┣ 📜SPD(Pithouse_마한문화권).R
 ┃ ┃ ┃ ┣ 📜SPD(Pithouse_신라제외).R
 ┃ ┃ ┃ ┣ 📜SPD(Pithouse_중도문화권).R
 ┃ ┃ ┃ ┣ 📜SPD(Pithouse_한강중류).R
 ┃ ┃ ┃ ┗ 📜SPD(Pithouse_한강하류).R
 ┃ ┣ 📂PithouseNum
 ┃ ┃ ┣ 📜Fig3-14(1)_주거지 개수(551안).R
 ┃ ┃ ┣ 📜Fig3-14(2)_주거지 개수(551안 보정).R
 ┃ ┃ ┗ 📜Fig3-9_주거지 개수(475안).R
 ┃ ┣ 📂SPD
 ┃ ┃ ┣ 📂By_All
 ┃ ┃ ┃ ┣ 📜Fig3-1_SPD(중부지역).R
 ┃ ┃ ┃ ┣ 📜Fig3-4~5_BaysianModel(신라제외).R
 ┃ ┃ ┃ ┗ 📜Fig3-4~5_BaysianModel(중부지역).R
 ┃ ┃ ┣ 📂By_Area
 ┃ ┃ ┃ ┣ 📜BaysianModel(dExp_model_경기남부+호서북부).R
 ┃ ┃ ┃ ┗ 📜BaysianModel(dExp_model_경기북부+영서).R
 ┃ ┃ ┗ 📂By_Cultural
 ┃ ┃ ┃ ┣ 📜BaysianModel(dExp_model_마한문화권).R
 ┃ ┃ ┃ ┗ 📜BaysianModel(dExp_model_중도문화권).R
 ┃ ┣ 📜Fig2-2_주거지 면적 분산.R
 ┃ ┗ 📜Fig4-7_백제 전쟁 기록.R
 ┣ 📜.gitignore
 ┗ 📜README.md
```

---

## 패키지 및 버전

```
R version 4.3.1 (2023-06-16 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 11 x64 (build 22621)

Matrix products: default


locale:
[1] LC_COLLATE=Korean_Korea.utf8  LC_CTYPE=Korean_Korea.utf8
[3] LC_MONETARY=Korean_Korea.utf8 LC_NUMERIC=C
[5] LC_TIME=Korean_Korea.utf8

time zone: Asia/Seoul
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base

other attached packages:
 [1] dplyr_1.1.2        latex2exp_0.9.6    coda_0.19-4        truncnorm_1.0-9
 [5] nimbleCarbon_0.2.1 nimble_1.0.1       rcarbon_1.5.0      here_1.0.1
 [9] showtext_0.9-6     showtextdb_3.0     sysfonts_0.8.8

loaded via a namespace (and not attached):
 [1] xfun_0.39              spatstat.sparse_3.0-2  lattice_0.21-8
 [4] numDeriv_2016.8-1.1    vctrs_0.6.3            tools_4.3.1
 [7] doSNOW_1.0.20          spatstat.utils_3.0-3   generics_0.1.3
[10] goftest_1.2-3          parallel_4.3.1         tibble_3.2.1
[13] proxy_0.4-27           fansi_1.0.4            pkgconfig_2.0.3
[16] spatstat_3.0-6         Matrix_1.6-0           KernSmooth_2.23-21
[19] lifecycle_1.0.3        compiler_4.3.1         stringr_1.5.0
[22] deldir_1.0-9           spatstat.linnet_3.1-1  codetools_0.2-19
[25] spatstat.explore_3.2-1 snow_0.4-4             class_7.3-22
[28] pracma_2.4.2           pillar_1.9.0           classInt_0.4-9
[31] spatstat.model_3.2-4   iterators_1.0.14       rpart_4.1.19
[34] abind_1.4-5            foreach_1.5.2          nlme_3.1-162
[37] spatstat.geom_3.2-2    tidyselect_1.2.0       stringi_1.7.12
[40] sf_1.0-14              splines_4.3.1          polyclip_1.10-4
[43] rprojroot_2.0.3        grid_4.3.1             cli_3.6.1
[46] magrittr_2.0.3         utf8_1.2.3             e1071_1.7-13
[49] spatstat.data_3.0-1    tensor_1.5             igraph_1.5.0
[52] knitr_1.43             mgcv_1.8-42            rlang_1.1.1
[55] Rcpp_1.0.11            spatstat.random_3.1-5  glue_1.6.2
[58] DBI_1.1.3              jsonlite_1.8.7         R6_2.5.1
[61] units_0.8-2
```

대부분의 패키지는 `install.packages()` 명령어를 통해 설치할 수 있습니다. 다만, 종종 `nimbleCarbon` 패키지가 설치되지 않는 경우가 있습니다. 이 경우, `devtools` 패키지를 설치한 후, 아래의 명령어를 통해 설치할 수 있습니다.

```R
library(devtools)
install_github('ercrema/nimbleCarbon')
```

---

## License

CC-BY 3.0