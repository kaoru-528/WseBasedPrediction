# WseBasedPrediction

Author: Kaoru Matsui

## 実績

- 松井 薫, 肖 霄, “Predicting the number of software faults using wavelets and nonlinear regression ,”日本オペレーションズ・リサーチ学会 2024 年春季研究発表会，3 月 7 日 - 8 日, 2024.

- Kaoru Matsui, Xiao Xiao, “On predicting software intensity using wavelets and nonlinear regression ,” The 3rd International Workshop on Dependable Computing for Complex Systems, June 24 – 27, 2024 - [発表スライド](https://drive.google.com/file/d/1YU_mzj9MkPr99nMFulSWOj3QVM9BrbDL/view?usp=drive_link)

# WseBasedPrediction について

**WseBasedPrediction**は，ソフトウェアフォールト発見数データから，ウェーブレット縮小推定を拡張し,フォールト発見数（欠陥数）を予測するものです．
現在以下の手法が実装されています.
| 手法名 | 備考 |
| ------------- | ------------- |
| Quadratic-based Prediction | 2015 Xiao |
| Periodic-based Prediction | 2023 Matsui |

# WseBasedPrediction の使い方

このリポジトリを clone した後, 必要なパッケージをインストールして WseBasedPrediction 直下で`/src/WseBasedPrediction.R`をインポートして使用してください.

詳しい使い方は PeriodicBasedPrediction/Example/exampleUsage.R をご覧ください．

> [!WARNING]
> このプログラムは高速化するために並列処理を実装しています. 8 コア以上の cpu を用いることを推奨します.

# WseBasedPrediction で使える各関数について

## loadData()

データセットを読み込むための関数です. txt 形式で, 1 行目にテスト時刻, 2 行目にフォール発見数が記載されているものに限ります.

```
loadData(
    dataPath = "データセットのパス"
)
```

## quadraticBasedPrediction()

回帰関数を二次関数 : $`a*(x^2+b)+c`$をベースに予測する関数です. 引数は以下をとることができます. データ変換, 閾値決定アルゴリズム, 閾値法の詳しい内容は後述します.

> [!WARNING]
> 予測値の精度向上のために,初期値を網羅的に与えています. 実行時間は約 1min です.

```
PeriodicBasedPrediction(
    data =  データセット
    dt =  ("none", "A1", "A2", "A3", "B1", "B2", "Fi", "Fr"), #データ変換の指定
    thresholdName = ("ldt", "ut", "lut", "lht"), #閾値決定アルゴリズムの指定
    thresholdMode = ("h", "s"), #閾値法の指定
    var = データ変換の際の分散を指定(デフォルトは1),
    index = 分割データのデータ長を指定(デフォルトは3),
    initThresholdvalue = 閾値の初期値(適当で良い)
    predictionPercentage = 予測期間の割合設定
)
```

### データ変換

データ変換は`dt`によって指定することができます. WseBasedPrediction では次の表のデータ変換が実装され, 変換式に違いがあります. すべての変換において逆変換の式に違いがあるため, 逆変換の式のみ参考に載せておきます.
| 変数名 | 変換名 | 逆変換の式 |
| ------------- | ------------- | ------------- |
| none | データ変換を行わない | 式なし |
| A1 | Anscombe transformation 1 | $`(s_i^2-3/2)/4`$ |
| A2 | Anscombe transformation 2 | $`(s_i^2-1/2)/4`$ |
| A3 | Anscombe transformation 3 | $`(s_i^2)/4+\sqrt{3/2}/(4*s_i)-11/(8*s_i^2)+5*\sqrt{3/2}/(8*s_i^3)-1/8`$ |
| B1 | Bartlet transformation 1 | $`(b_i^2-2)/4`$ |
| B2 | Bartlet transformation 2 | $`(b_i^2)/4`$ |
| Fi | Fisz transformation | 複雑なため省略 |
| Fr | Freeman transformation |　$`(f_i^2+f_i^{-2}-2)/4`$　|

### 閾値決定アルゴリズム

閾値決定アルゴリズムは`thresholdName`によって指定することができます. WseBasedPrediction では次の表の閾値決定アルゴリズムが実装されています.
| 変数名 | 閾値決定アルゴリズム名 | 備考 |
| ------------- | ------------- | ------------- |
| ldt | Level-dependent-Threshold | dt="none"を指定した場合のみ適用化 |
| ut | Universal-Threshold | dt="none"以外を指定した場合のみ適用化 |
| lut | Level-dependent Universal Threshold | dt="none"以外を指定した場合のみ適用化 |

> [!CAUTION] > `lht` は未実装.

### 閾値法

閾値法は`thresholdMode`によって指定することができます. WseBasedPrediction では次の表の閾値法が実装されています.
| 変数名 | 閾値決定アルゴリズム名 |
| ------------- | ------------- |
| s | Soft thresholding method |
| h | Hard thresholding method |

### 実行結果

`PeriodicBasedPrediction()`は予測値と係数の時系列データを回帰した際の最も精度の良い回帰関数の回帰係数を返します.

```
result = PeriodicBasedPrediction()

result
$predictionData #予測値
$regressionCoefficient
        a         b         c
1  scaling coefficient c00
2  denoised wavelet coefficient d30
3  denoised wavelet coefficient d31
4  denoised wavelet coefficient d32
5  denoised wavelet coefficient d33
6  denoised wavelet coefficient d20
7  denoised wavelet coefficient d21
8  denoised wavelet coefficient d30
```

## PeriodicBasedPrediction()

回帰関数を周期関数 : $`a*sin(b*x+c)+d`$をベースに予測する関数です. 引数は以下をとることができます. データ変換, 閾値決定アルゴリズム, 閾値法の詳しい内容は`quadraticBasedPrediction()`と同様です.

> [!WARNING]
> 予測値の精度向上のために,初期値を網羅的に与えています. 実行時間は約 1.5h です.

```
PeriodicBasedPrediction(
    data =  データセット
    dt =  ("none", "A1", "A2", "A3", "B1", "B2", "Fi", "Fr"), #データ変換の指定
    thresholdName = ("ldt", "ut", "lut"), #閾値決定アルゴリズムの指定
    thresholdMode = ("h", "s"), #閾値法の指定
    var = データ変換の際の分散を指定(デフォルトは1),
    index = 分割データのデータ長を指定(デフォルトは3),
    initThresholdvalue = 閾値の初期値(適当で良い)
    predictionPercentage = 予測期間の割合設定
)
```

### 実行結果

`PeriodicBasedPrediction()`は予測値と係数の時系列データを回帰した際の最も精度の良い回帰関数の回帰係数を返します.

```
result = PeriodicBasedPrediction()

result
$predictionData #予測値
$regressionCoefficient
    a         b         c         d
1  scaling coefficient c00
2  wavelet coefficient d30
3  wavelet coefficient d31
4  wavelet coefficient d32
5  wavelet coefficient d33
6  wavelet coefficient d20
7  wavelet coefficient d21
8  wavelet coefficient d30
```
