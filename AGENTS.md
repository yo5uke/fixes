# AGENTS.md

`fixes` は、DiD のイベントスタディと推論(TWFE から staggered 対応の推定量まで)を行い、結果を可視化する R パッケージ。

## 設計と API 契約

- 動的効果は `event_study()`、集計 ATT は `att()`、単一の DiD 係数は `did()`。推定量は `estimator = "twfe"|"cs"|"sa"|"bjs"|"twm"|"flex"` で選ぶ。
- 3 関数とも `f(formula, data, time, timing, unit, ...)` の形で、モデルは `outcome ~ treatment + covariates | fe` の式で受ける。このシグネチャは 2.0.0 まで変更しない。`formula` と `data` は順序が逆でも受け付ける。
- 処置の指定は 3 通りで、`timing` の形で決まる(`R/formula_interface.R`)。
  - `timing` なし: 右辺の先頭は D(処置群かつ処置後 = 1)。ユニットごとの開始時点は D から求める。
  - `timing` が列: 右辺は共変量だけ。
  - `timing` が値: 右辺の先頭は処置群ダミー。
  取り違え(処置群ダミーを D として渡す、`timing` 列の横に D を書く、など)は完全一致の検査でエラーにする。
- `time` は必須。`unit` は、FE が単純な列で `time` ともう 1 列だけのときに FE から推定し、使った unit/time を `message()` で表示する。結果の `print()` にも毎回出す。
- 旧引数(`outcome`/`treatment`/`fe`/`covariates`)は末尾に置き、`deprecate_soft` で案内する(2.0.0 で削除予定)。案内には書き換えた formula の呼び出しを出す。位置指定の旧呼び出しは使えない。
- 公開関数は薄い窓口で、推定は `.event_study_impl()`/`.did_impl()`/`.att_impl()` が担う(旧引数のインターフェースのまま)。`run_es()` などの旧 API と旧引数はここを直接呼ぶ。impl の挙動は変えない。推定量ごとの FE と共変量の検査は、formula 経路だけで行う。
- `R/deprecated.R` の旧 API は廃止予定だが、後継と数値的・契約的に同一のまま残す。削除も挙動変更も不可(`tests/testthat/test-deprecated.R` が検証)。
- `es_result` の構造は全推定量で揃え、`plot.es_result()` はどの推定量の結果も受け付ける。
- `did_result` は生のモデルオブジェクトを持たない。`$coeftable`、`$vcov`、`$fit_stats`、`$df.t` を使う。
- 新 API の引数は snake_case。`staggered = NULL` は処置の与え方(列なら staggered、値なら一括、D 列なら開始時点が 1 つのとき一括)から推定し、明示指定を優先する。

## 依存関係

- fixest は Imports ではなく **Suggests**。既定の推定は内部の FE-OLS エンジン(`R/fe_ols.R`、`R/fe_solver.R`)で動く。
- fixest が必要なのは、旧 `sunab` 経路、多元クラスタ、`vcov_args`、iid/hetero/HC1・一元クラスタ以外の vcov、`~ id^year` のような列以外の FE/クラスタ指定。新たに fixest に触れる箇所は `.require_fixest()` で囲む。
- CI には fixest なしでビルドする job がある。fixest・did・didimputation・modelsummary・plotly を使うテストには `skip_if_not_installed()` が必須。

## テスト

- 推定量は参照実装との数値一致で担保する。許容誤差は `1e-6`(エンジン単体のオラクルは `1e-8`)。
  TWFE/`did()` は `fixest::feols`、CS は `did::att_gt()`、SA は `fixest::sunab()`、BJS は `didimputation` と比べる。
- **新しい推定量はテストを先に書く。** 既存テストを壊さない。
- 新旧 API の同値性は、`call` 属性を除いて `expect_identical` で検証する。formula と旧引数の同値性も同じ(`tests/testthat/test-formula.R`)。例外は、twfe で `timing` 列を使う場合。内部で作る処置群列の名前が `model_formula` に入るため、この属性も除いて比べる。
- Rcpp 関数には R 実装との一致テストを付ける(浮動小数は `1e-12`、整数は `expect_identical`)。

## 推定量を触るとき

論文は `papers/`(git 管理外)にある。各 PDF には同名の `.txt` があり、PDF を直接読めない場合はこちらを読む。**推定量の数式を実装・変更する前に、該当論文を読む。** ウェイト・集計・識別条件は記憶ではなく論文に従う。

| 推定量 | 論文 |
|---|---|
| `sa` | Sun and Abraham (2021) |
| `cs` | Callaway and Sant'Anna (2021) |
| `bjs` | Borusyak et al. (2024) |
| `twm` | Woodridge (2025) |
| `flex` | Deb et al. (2024) |
| `honest_sensitivity()` | Rambachan and Roth (2023)(参照実装 `HonestDiD`) |

- TWM の `trends = TRUE` は、単位 FE があると `d_g * t` が処置セル指標と代数的に共線になる。処置後セル(`s >= g`)だけを入れてトレンド列を足す(Wooldridge 2025, Sec. 8)。そのため出力は `relative_time >= 0` のみで、事前トレンド検定はできない。
- Rcpp は先にプロファイルし、推測で最適化しない。classic 経路の高速化は、R 側の簿記(整数のグループコード化)と、`[y | X]` の組み立て・残差・VCOV の meat・event-study 設計行列を作る C++ カーネル(`R/fe_ols.R` から呼ぶ)の両方で得た。エクスポートを足したら `Rcpp::compileAttributes()` を実行する。OpenMP カーネルはスレッド数によらずビット同一でなければならない。

## 既知の制約

- 約 100 万行のパネル(50,000 ユニット × 20 期、fixest は 8 スレッド)で同じ設計の `fixest::feols` と比べると、classic event-study は HC1 では同程度(1.1 秒対 1.2 秒)、一元クラスタでは約 1.4 倍遅い(1.0 秒対 0.7 秒)。classic 経路の差の内訳は、まだプロファイルしていない。`did()` は約 3 倍遅い(0.17 秒対 0.06 秒)。時間は FE の反復デミーンと FE の整数コード化に分散していて、単独で大きい箇所はない。
- `estimator = "twm"` に共変量を足すと回帰の列が増え(3 コホート × 20 期、共変量 1 つで 57 列から 133 列)、N·p² の交差積と VCOV の meat が支配的になる。100 万行で共変量なしの約 3.5 倍かかる。
- CS はバランスパネルに揃えてから推定する(NA の行を落とし、全期間そろわないユニットを落とす。`did::att_gt()` の既定と同じ)。`timing` が `±Inf` の行も欠損として落ちるため、never-treated を `Inf` で表すデータでは対照群が消える(未対応)。
- `honest_sensitivity()` は ARP-conditional のみ。FLCI/hybrid は未実装。`es_vcov` を持つのは `estimator = "twfe"` だけで、他の推定量では `betahat`/`sigma` を明示する。
- Synthetic DiD とベイズ event study は未実装。

## 規約と落とし穴

- 固定効果の式は片側(`~ id + year`)。baseline は `[-lead_range, lag_range]` の範囲内。
- 不規則な時間軸は `time_transform = TRUE` + `unit`(このとき `timing` は元の時間値ではなくインデックス。D 列なら開始時点をインデックス上で求めるので調整不要)。`staggered = TRUE` とは併用不可。
- データ列名に `unit` を使わない(ggplot2 と衝突する)。
- ウェイトが 0/NA の行はエンジンが黙って落とし、負のウェイトはエラー。
- 既定の vcov は HC1。cluster 指定かつ既定 vcov なら CRV1 に切り替わる。p 値は Student-t(df = N − K_full、一元クラスタなら G − 1)。
- `README.md` は `README.Rmd` から手動 knit で生成する。`README.md` を直接編集しない。
- コード中のコメント(R/、src/、roxygen)は、利用者に必要な内容だけを最小限にする。論文参照、アルゴリズムの説明、コードから読めない制約まで。Phase や版数、開発中の計測値、使い捨てスクリプトへの言及は書かない。

## Git

- ユーザーがその場で明示的に頼まない限り、commit も push もしない。完了した作業は作業ツリーに残して報告する。**一度の許可は次回に持ち越さない。**
- 履歴を書き換える、または未コミットの作業を消す操作(`reset --hard`、`push --force`、`clean -f`、`branch -D` など)や、`--no-verify` は、明示的な指示がない限り使わない。
- ステージする前に `git status`/`git diff` で対象を確認する。`git add -A` や `git add .` で一括ステージしない。
- commit は論理的な関心ごと(fix / tests / docs / リリースメタデータ)に分ける。1 つにまとめてよいのは、変更が本当に同じ理由に基づく場合だけ。
- メッセージは英語で `type: imperative summary`、必要なら `type(scope): summary`。type は `feat`、`fix`、`docs`、`test`、`ci`、`refactor`、`build`、`chore`。例: `fix(run_did): report the estimation sample, not nrow(data)`。AI エージェントの署名や Co-Authored-By は付けない。

## レビュー

- 実装役とレビュー役は、タスクごとにユーザーが指示する。指示がなければ、実装を依頼されたツールが実装し、もう一方はレビューに回る。
- レビューでは、API 契約・数値一致・上記の規約を基準に、正しさを読む。好みのスタイルや仮定の edge case には口を出さない。
- レビュー役は、明白で具体的なバグだけを修正してよい(クリーンに直すための小さなリファクタを含む)。設計上の意図かバグか判断がつかなければ、変更せず報告する。

## リリース

DESCRIPTION(`Version`/`Date`)、NEWS.md、README.Rmd(knit し直して README.md を再生成)、cran-comments.md、NAMESPACE(export/import が変わったとき)をすべて更新する。その後、次の順に実行し、それぞれがクリーンなことを確認してから進む。

1. `devtools::document()`: ✖ の警告なし
2. `devtools::test()`: 失敗 0
3. `devtools::check()`: error / warning / note すべて 0

CRAN 提出の場合のみ、さらに `devtools::check(args = "--as-cran")`(0/0/0)と `devtools::check_win_devel()` を実行する。win-builder の結果は DESCRIPTION のメンテナ宛てメールに届くので、クリーンなことを確認してから提出する。通常のリリースでは、CI が `--as-cran` を Windows・macOS・Ubuntu(r-devel 含む)で回すため、この 2 つは不要。
