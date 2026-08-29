# ggbipartite v0.9.7 / v0.10.0 更新計画

- 状態: 方向性合意済み、公開 schema 詳細は WP-6 着手前に確定、実装未着手
- 更新日: 2026-08-30
- 調査基準: `origin/master@d5096b71a80d688134cb40e2d166c40d684b3dea`
  （ggbipartite 0.9.6）
- 対象: 系統樹と二部ネットワークの可視化、入力検証、レイアウト計算

## 1. 結論

次期開発は、バグ修正と構造刷新を一つの版へまとめず、次の二段階で
進める。

1. **v0.9.7（安定化）**
   - 確認済みの P0・P1 バグを回帰テスト付きで修正する。
   - `testthat`、CI、厳密な入力契約を導入する。
   - 文書、vignette、依存宣言を実際の配布状態と一致させる。
   - 新機能は追加しない。
2. **v0.10.0（構造刷新）**
   - 新しい layout / composer 経路では、入力を一度だけ検証・正規化する。
   - 全レイヤーが共有する標準レイアウトを導入する。
   - 既存 API を段階的に移行し、即時削除しない。
   - 高水準 composer はリリースを妨げない optional な Should 項目とする。

優先順位は、**正確性・再現性 > 利用の簡単さ > 機能の豊富さ**とする。
パッケージの責任範囲は、入力検証、レイアウト計算、可視化までとし、
ネットワーク指標や統計解析は既存の解析パッケージへ委ねる。

## 2. 調査範囲と証拠の限界

調査は上記の `origin/master` revision に対して読み取り中心で行った。
`R CMD build --no-build-vignettes` で生成した source tarball に対する
`R CMD check --no-manual --no-build-vignettes` は `Status: OK` だった。
Rd examples と README のレンダリングも別に完了した。ただし、manual と
vignette を含む完全な release check が成功したことを意味しない。

- `tests/` が存在せず、既知バグを検出する回帰テストがない。
- `.github/workflows/` が存在せず、OS・R・Bioconductor 世代間の互換性を
  検証していない。
- `.Rbuildignore` が `^vignettes$` を除外しており、3本の vignette は
  tarball に収録も検証もされない。
- `DESCRIPTION` は `VignetteBuilder: quarto` を宣言しており、上記の除外と
  整合しない。
- ローカル checkout は0.9.5で、`master` は調査時点で0.9.6の
  `origin/master` より6コミット遅れている。

したがって、実装開始時には対象 revision を再確認し、最新の remote 差分で
バグが解消・変化していないかを再検証する。行番号は調査基準 revision の
ものであり、将来の revision ではずれる可能性がある。

## 3. 既存バグの優先度

### 3.1 P0: 無警告で図の意味を壊すバグ

| ID | 問題 | 証拠 | v0.9.7 の受入条件 |
|---|---|---|---|
| BUG-001 | interaction group を `paste0(row, column)` で作るため、`("a", "bc")` と `("ab", "c")` が同じ polygon へ結合される | `R/stat-bipnet.R:629-643` | 各 edge に衝突しない内部 ID を割り当て、adversarial ID の group 数・頂点数をテストする |
| BUG-002 | metadata key の重複により join が増殖し、box と interaction 頂点が無警告で重複する | `R/bipartite_network.R:154-191` | row・column metadata key の一意性を join 前に検証し、重複 ID を列挙してエラーにする |

### 3.2 P1: 公開機能を壊す、または誤座標を返すバグ

| ID | 問題 | 証拠 | v0.9.7 の受入条件 |
|---|---|---|---|
| BUG-003 | `to_longer()` が任意の `.rowname` を受け取っても固定名 `row` を参照し、無名行列では ID を `NA` にする | `R/to_longer.R:32-47` | default と custom 名は正しい ID 列を返す。`.rowname = NULL` と無名・空・重複 dimnames は、strict ID 契約に基づく情報量のあるエラーにする |
| BUG-004 | long 形式の同一 row-column cell が複数あると、集約前の `pivot_wider()` で list-column となり失敗する | `R/stat-bipnet.R:575-625` | 変換前に重複 cell を検出し、合意済み契約に従い既定ではエラーにする |
| BUG-005 | `adjust_box_height = TRUE` で左右の高さが等しい場合に gap が `NULL`、singleton で `Inf` になる | `R/global_layout.R:95-185` | equal・singleton の双方で有限座標と有限 gap を返す |
| BUG-006 | `.x0` が column 側へ加算されず、`.y0` が box 座標へ伝播しない | `R/global_layout.R:85-94`; `R/bipartite_network.R:120-146` | 任意の有限 origin に対する平行移動不変条件をテストする |
| BUG-007 | tip 以外の node が tip と同じ label を持つと、tip 座標の抽出に混入し得る | `R/stat-bipnet.R:97-169,275-305` | `isTip` を優先して抽出し、label 衝突 fixture で tip のみを結合する |
| BUG-008 | `geom_nodemarquee()` の既定 mapping に必須の `label` がない | `R/geom_nodemarquee.R:395-419` | 引数を追加せず `ggtree(tree) + geom_nodemarquee()` が build できる |
| BUG-009 | 座標 key は character、numeric metadata key は numeric のままで join が失敗する | `R/bipartite_network.R:154-191` | 入力境界で両側の ID を character へ正規化してから join する |

### 3.3 P2: API 整理時に確定する候補

次の項目は専用の回帰テストを追加して公称契約を確定し、v0.9.7 の公開動作に
直接影響する場合は同版で修正する。それ以外は v0.10.0 の API 移行へ含める。

- `stat_bipnet(inherit.aes = FALSE)` と required aesthetics の不整合。
- `type = "row"` / `"column"` と既定 polygon geom の不整合。
- `compute_interaction_coords()` が optional とされる `interaction_size` 列を
  実質的に必須扱いする問題。
- `compute_interaction_coords()` の暗黙的な `by = intersect(...)` が、意味上の
  key ではない同名列まで join key に含め得る問題。

### 3.4 独立再現で得た決定的観測

次の観測は調査基準 revision を独立展開して再現した。WP-1 で、これらを
repository 内の恒久的な回帰テストへ移す。

- BUG-001: 2 tuple、8 polygon vertices に対して unique group は1だった。
- BUG-002: duplicate metadata により box 数が2から3、interaction vertices が
  16から24へ増えた。
- BUG-003: custom 名と `NULL` は error、無名2 x 2行列は row / column ID が
  すべて `NA` になった。
- BUG-004: duplicate cell を含む layer は warning の後に0行となった。
- BUG-005: equal-height で gap が `NULL`、singleton で調整 gap が `Inf` に
  なった。
- BUG-006: `.x0 = 100, .y0 = 100` で row `xmin = 100`、column
  `xmin = 14.18`、両側の `ymin` は0のままだった。
- BUG-007: tip と内部 node の label 衝突例で、期待した tip y=1ではなく、
  y=(1+99)/2=50が結合された。
- BUG-008: default call は必須 `label` 欠落で失敗し、明示 mapping では成功した。
- BUG-009: metadata join は character key と integer key の型不一致で失敗した。

## 4. 入力契約

### 4.1 最終契約

入力の曖昧さを暗黙に補正しない。既存の正常入力との互換性を保つが、
誤描画につながる入力は明示的エラーへ変更する。

| 対象 | 既定ポリシー |
|---|---|
| ID | 入力境界で character へ正規化し、以後は完全一致で比較する |
| 大文字小文字・空白 | 自動修正しない。疑わしい差異は診断に示す |
| 行列名 | rownames・colnames は非空かつ一意であることを必須とする |
| 無名行列 | 既定ではエラー。自動 ID 生成は将来の明示 option に限定する |
| interaction 値 | 有限の非負数のみを許可する |
| `NA`・`Inf`・負値 | エラーにする |
| ゼロ cell | 「edge なし」として許可する |
| 合計ゼロの行・列 | 実データ上の対象外として ID を列挙してエラーにする |
| 重複 long cell | 既定ではエラー。明示的な集約関数がある場合だけ集約する |
| metadata key | row・column とも一意を必須とする |
| tree と data の ID 不一致 | 差集合を示してエラーにする |
| drop / prune | v0.10.0 の明示 option とし、既定では実行しない |
| 順序 | tree があれば tip 順、なければ入力順を保持する |
| 自動ソート | アルファベット順・相互作用量順へ勝手に変更しない |

singleton は中央へ配置する。左右の高さが等しい場合は指定 gap を維持し、
常に有限の座標を返す。全セルがゼロの入力も、合計ゼロの行・列を含むため
エラーとなる。

重複 cell の既定 error、zero-sum node の禁止、tree mismatch の既定 error は、
監査から一意に導かれた bug fix ではなく、設計協議で採用した product contract
である。既存の downstream helper に重複値の合計処理があっても、現在の公開
経路はその前段で失敗するため、暗黙集約を正常動作として維持しない。

### 4.2 version ごとの適用範囲

v0.9.7 では、既存の raw interaction / metadata entry points に対して次を
適用する。

- 無名・空・重複 ID、非有限値、負値、zero-sum row / column を早期検証する。
- duplicate long cell と duplicate metadata key を既定でエラーにする。
- numeric / factor ID を character へ正規化する。
- `.rowname = NULL` は strict ID contract と整合しないため、原因と代替を示す
  error にする。
- これらのうち既存の曖昧入力を止める変更は `NEWS.md` へ列挙する。

v0.10.0 では、上記を canonical validator へ集約し、すべての新 API 経路で
一度だけ適用する。tree と data の集合照合、tree tip 順、明示的 drop / prune、
明示的な duplicate-cell aggregation hook は、新しい統合入力境界で実装する。
既存 tree helper に両方の集合が渡らない場合、v0.9.7 で完全照合を推測して
追加しない。

## 5. v0.10.0 の目標アーキテクチャ

### 5.1 データフロー

```text
matrix / long data / phylo / metadata
                  |
                  v
       validate and normalize once
                  |
                  v
         layout_bipartite()
                  |
                  v
         bipartite_layout
       /          |          \
      v           v           v
  geom layers  plot composer  user inspection
```

検証・正規化・座標計算は、入力オブジェクトを変更しない pure internal
functions へ分割する。各 `Stat` が個別に wide/long 変換、join、座標計算を
繰り返す構造を廃止する。隠れた global cache は使用しない。

### 5.2 公開レイアウト API

新しい安定 API は次を基本とする。

```r
layout <- layout_bipartite(
  data,
  row = NULL,
  column = NULL,
  weight = NULL,
  interaction = "abundance",
  row_tree = NULL,
  column_tree = NULL,
  metadata_row = NULL,
  metadata_column = NULL
)
```

dispatch と列指定は次のように固定し、列名を推測しない。

- matrix: `is.matrix(data)` で判定し、`row`、`column`、`weight` は指定しない。
  dimnames を ID として使用する。
- data.frame / tibble: `row`、`column`、`weight` を明示的に指定する。
- `interaction` は `"abundance"` または `"binary"` とし、既定は
  `"abundance"`。値の分布から mode を自動推定しない。
- binary mode の正の weight を presence とみなすことは、明示された mode の
  意味として characterization test と文書で固定する。

戻り値の class は `bipartite_layout` とし、少なくとも次の名前付き要素を
安定契約とする。

- `$nodes`: row・column node の座標、`side`、`id`、`order` を持つ tibble
- `$interactions`: edge / ribbon の座標、`edge_id`、row / column ID、weight を
  持つ tibble
- `$tree_links`: tree と network の接続点、`side`、`id` を持つ tibble
- `$params`: gap、scale、origin、順序などの確定済み parameter
- `$trees`: row / column ごとの元 tree と、描画に必要な検証済み tree geometry

座標列の最終 schema と欠損可能性は、WP-6 の最初に contract test と短い
design note で確定してから export する。内部計算用 field は公開契約に
含めない。`plot_bipartite(bipartite_layout)` は `$trees` を使うため、tree の
再指定を要求しない。別の tree を使う場合は layout を再作成し、検証済みの
ID・順序と描画 tree がずれないようにする。

新しい推奨経路では、各 `geom_bipnet_*()` が明示的な `layout` 引数を受け、
対応する計算済み tibble を identity layer として描画する。composer も同じ
layout を使う。この経路では検証・layout 計算は一度だけである。既存の
raw-data layer 呼び出しは互換 wrapper として残り、layout を明示しない限り
layer ごとに計算し得る。この legacy 例外は文書化し、隠れた cache で回避
しない。

### 5.3 高水準 composer

composer は `plot_bipartite()` とし、raw data と `bipartite_layout` の双方を
受け付ける。raw data の場合だけ、内部で `layout_bipartite()` を一度呼ぶ。
layout を渡した場合は、その `$trees` と確定済み parameter を使用する。

戻り値 class は `ggbipartite_plot` とし、裸の `patchwork` を返さない。

```text
$components
  $row_tree
  $row_link
  $network
  $column_link
  $column_tree
$layout
```

存在しない component は `NULL` とし、tree 0本、片側1本、両側2本を同じ
契約で扱う。component は意味名で取得・編集できるようにする。

- abundance 表示: connector 方式を既定とする。
- binary 表示: tip 直接整列を既定とする。
- topology と枝長を保持する。
- 自動 node rotation や crossing 最適化は行わない。
- panel 幅は安全な既定値を持ち、利用者が上書きできる。
- `patchwork` は `Suggests` として宣言する。
- component と layout の作成は `patchwork` なしでも可能にする。
- 複数 panel の `print()` / `as_patchwork()` 時だけ dependency を確認し、
  未導入なら解決方法を含むエラーを返す。
- `aplot` は主 composer の直接依存にしない。

composer は v0.10.0 の Should 項目であり、標準 layout と既存 layer の移行が
完了していれば、composer を次版へ送っても v0.10.0 をリリースできる。

### 5.4 API lifecycle

- v0.9.7 では、正常な既存 API の意図的な破壊を行わない。
- 無効・曖昧で誤描画につながる入力がエラーになることは、correctness fix と
  して `NEWS.md` に明記する。
- v0.10.0 では、小さな stable core を定義する。
- 置換対象の低水準 helper は `lifecycle` で superseded / deprecated とする。
- v0.10.0 で deprecate した API は v0.11.x を通して保持し、削除は早くても
  v0.12.0 とする。
- 先頭ドット付き exports、`%>%` 再 export、公開引数名は、style 修正だけを
  理由に即時削除・改名しない。

## 6. 実装計画

### 6.1 v0.9.7: 安定化

#### WP-0: 実装前の baseline 確定

- remote の最新 revision と調査基準との差を確認する。
- ユーザーの既存差分を混入させない作業単位を作る。
- source tarball の `R CMD check` 結果を再取得する。
- P0・P1 の最小再現を最新 revision でも再実行する。

**完了条件:** 対象 commit、R、Bioconductor、主要 dependency version を記録し、
各 claim の再現可否が明示されている。

#### WP-1: テスト・CI 基盤

- `testthat (>= 3.0.0)` と `Config/testthat/edition: 3` を導入する。
- `vdiffr` と `covr` を test-only dependency として `Suggests` に宣言する。
- matrix、long data、metadata、tree の小さな決定論的 fixture を追加する。
- 現行の正常動作を characterization tests で固定する。
- 入力検証、座標不変条件、join cardinality、公開 wrapper を unit test 化する。
- GitHub Actions で required CI matrix を構成する。
- canonical build job が source tarball を一度だけ生成し、SHA-256 を記録して
  全 check lane へ同じ artifact を配布する。
- required status checks を repository ruleset / branch protection で強制し、
  release workflow はすべての required jobs に依存させる。
- `v*` tag ruleset は手動作成を制限し、required jobs を内部で再利用する
  検証済み release automation だけに tag 作成を許可する。
- coverage は計測成功だけを gate とし、率の閾値を置かない。

**完了条件:** 意図的に旧バグを再現する fixture が、修正前コードに対して
失敗し、各修正後に成功する。

#### WP-2: P0 修正

- BUG-001 の衝突しない edge ID を実装する。
- BUG-002 の metadata key 一意性検証を実装する。
- バグ修正と無関係な整形・改名を同じ差分へ混ぜない。

**完了条件:** adversarial ID と duplicate metadata の構造テストが成功し、
既存の代表 abundance / binary 図を build できる。

#### WP-3: P1 修正

- BUG-003 から BUG-009 を、1 bug と対応する回帰テストを一つの意味的変更
  単位として修正する。
- 入力契約に反するケースでは、原因、対象引数、問題の ID を含む英語の
  error message を返す。
- join では key 型と cardinality を join 前に検証する。

**完了条件:** 確認済み P1 の最小再現がすべて成功し、正常な既存 examples が
維持される。

#### WP-4: 文書・依存関係の整合

- build wiring / dependency 宣言、英語文書の内容変更、roxygen 生成物の更新を
  別の意味的 commit に分ける。
- README を install、最小例、代表図、vignette link へ縮小する。
- 英語の getting-started と tree integration vignette を配布対象にする。
- 日本語文書は補助文書として維持できるが、英語文書を正本とする。
- `.qmd` を正本とし、生成済み HTML、`*_files`、`.DS_Store` を除外する。
- `^vignettes$` の全体除外を削除する。
- `library(tidyverse)` を必要な個別 package へ置換し、不要な meta-package
  dependency を追加しない。
- 実行する文書で使用する package を `Suggests` に漏れなく宣言する。
- `dplyr::.by`、`tidyr::nest(.by = )`、`ggplot2::linewidth` など、使用中の API
  から実際の dependency floor を導出する。対応コードへ書き換えない場合は、
  `DESCRIPTION` に最低 version を宣言する。
- exact-min dependency job で宣言した最低 version を導入し、floor lane を
  「古い R + 最新 dependency」だけの検証にしない。
- `testthat`、`vdiffr`、`covr`、`patchwork`、`cowplot` などは、実際に使用する
  場合だけ宣言し、未宣言の利用を残さない。
- README は package check と別の docs job で `quarto render README.qmd` を
  実行する。
- package vignette の Darkly / Superhero theme は削除するか、local preview
  専用であり配布結果には反映されないことを明記する。
- roxygen、Rd、NAMESPACE を再生成し、2回目の生成が無差分になることを
  確認する。

**完了条件:** build 済み tarball に意図した vignette が入り、全 required
CI lane で vignette と examples を実行できる。`inst/doc` の合計は CRAN の
一般的な5 MB guideline 以下、source tarball は推奨される10 MB未満を満たす。

#### WP-5: v0.9.7 リリース判定

- canonical build job が生成した checksum 付きの同一 tarball を、全 required
  lane で検証する。
- release / Bioconductor release lane で `R CMD check --as-cran` を行う。
- ERROR、WARNING、新規かつ未説明の NOTE がない。
- exact-min dependency lane と README docs job が成功する。
- fixed Linux lane で、承認済み SVG を使う `vdiffr` test
  （`cran = FALSE`）が成功する。
- `NEWS.md` に修正、strict input behavior、互換性への影響を記載する。
- P2候補をそれぞれ fixed / deferred と判定し、deferred の行き先を記録する。
- version を v0.9.7 とする。release automation は required jobs の成功後に
  tag と GitHub Release を作成し、tag ruleset は同 automation 以外による
  `v*` tag 作成を制限する。

### 6.2 v0.10.0: 構造刷新

#### WP-6: 標準入力・共有 layout

- 入力検証、ID 正規化、tree 照合、順序決定を pure functions へ分離する。
- `bipartite_layout` を実装する。
- export 前に input signature と各 tibble の列 schema を design note で確定する。
- `$nodes`、`$interactions`、`$tree_links`、`$params`、`$trees` の schema を
  テストする。
- origin、gap、singleton、equal-height の座標不変条件を固定する。
- 入力オブジェクトが変更されないことをテストする。

#### WP-7: 既存 layer の移行

- `geom_bipnet_box()`、`geom_bipnet_interaction()`、
  `geom_bipnet_point()` を共有 layout へ移行する。
- 各 geom の明示的な `layout` 経路では、計算済み tibble を identity layer
  として使用する。
- layer ごとの wide/long 再変換と layout 再計算を除去する。
- raw-data 呼び出しを互換 wrapper として維持し、この legacy 経路では
  layer ごとに計算し得ることを文書化する。
- low-level coordinate API と新 schema の対応を文書化する。

#### WP-8: lifecycle 整理

- exports を stable / experimental / superseded / deprecated / internal に分類する。
- 置換先のある helper に lifecycle badge と warning を追加する。
- `NEWS.md` と `v0.9.x -> v0.10.0` migration guide を作成する。
- 旧関数ごとに後継、引数差、戻り値差を示す。

#### WP-9: optional composer

- `plot_bipartite()` と `ggbipartite_plot` を実装する。
- tree 0本、片側1本、両側2本を integration test する。
- component の取得、編集、再合成をテストする。
- `patchwork` 未導入時の component 作成と描画時 error をテストする。
- abundance connector と binary tip alignment を固定する。

#### WP-10: v0.10.0 リリース判定

- WP-6、WP-7、WP-8 を必須とする。
- WP-9 は未完でも、既知リスクと次版予定を記載すれば defer できる。
- 新旧 API の代表図が同じ入力意味論を保持することを確認する。
- v0.9.7 と同じ required CI / documentation gate を通す。
- `lintr` を新規・変更された file 全体に対して blocking にする。

## 7. 品質ゲート

### 7.1 CI matrix

version 番号は CI 作成時に公式対応表で再確認する。調査時点の候補は次の
とおり。

| Lane | OS | R | Bioconductor | 目的 |
|---|---|---|---|---|
| release | Ubuntu / Windows / macOS | 4.6.x | 3.23 | 現行利用環境 |
| devel | Ubuntu | 4.6.x | 3.24 | ggtree 上流変更の早期検知 |
| oldrel | Ubuntu | 4.5.x | 3.22 | 直前世代との互換性 |
| declared floor | Ubuntu または固定 container | 4.1.0 | 3.13 | 最古 R / Bioc と互換 dependency の検証 |
| exact minima | Ubuntu または固定 container | 宣言下限 | 対応世代 | `DESCRIPTION` に宣言した各 dependency 下限の検証 |

- Ubuntu release の canonical build job で一度だけ `R CMD build` を行い、
  source tarball と SHA-256 を artifact として保存する。
- 各 lane で `BiocManager::valid()` を確認する。
- 各 check lane は canonical artifact を取得し、再 build せず同じ tarball を
  `R CMD check` する。
- floor lane が失敗した場合、無条件に skip しない。後方互換修正か、実際に
  維持できる R / dependency floor への引き上げを選ぶ。
- exact-min lane は、R / base を除く version 制約付き hard dependency
  （Depends / Imports / LinkingTo）と、check で実行する version 制約付き
  Suggests を宣言下限へ pin する。CRAN package を最新版へ解決しない。
  Bioconductor package に下限を宣言する場合は、公式世代内でその下限を
  検証する。
- R-devel は対応する Bioconductor devel が公式提供されている期間だけ
  required とする。非対応世代を強制的に混在させない。
- PR gate は ERROR / WARNING を拒否し、新規・未説明 NOTE も拒否する。
- required jobs は repository ruleset / branch protection の required status と
  する。release automation は同じ required workflow を再利用し、成功後だけ
  tag / GitHub Release を作成する。`v*` tag ruleset は同 automation だけを
  bypass actor として許可する。

### 7.2 テスト階層

1. **入力契約テスト**
   - 型、非有限値、負値、ID、一意性、tree mismatch、zero-sum side。
2. **数値・構造テスト**
   - edge group の単射性、join cardinality、有限座標、平行移動不変性、順序。
   - BUG-001 は「1 group = 1 tuple」と、各 abundance tuple の4 vertices を
     assert する。
   - BUG-005 は equal 時の gap、singleton の有限 gap、非空 layer を
     assert する。
   - BUG-006 は全 box / interaction 座標の移動量と面積不変を assert する。
   - BUG-008 は build 成功だけでなく、label 値と対象 node 数を assert する。
3. **layer integration test**
   - abundance、binary、metadata、tree 0/1/2本。
4. **文書・example test**
   - Rd examples、README の最小例、配布 vignette。
5. **限定的 visual regression**
   - adversarial ID と unique metadata を含む abundance 図。
   - binary tree-tip alignment 図。

visual regression は固定 Linux lane の local / CI 監視に限定し、CRAN check の
失敗条件にはしない。BUG-001 / BUG-002 の correctness は画像ではなく group 数、
ID 一意性、box / vertex cardinality の通常テストで保証する。承認済み SVG は
repository に commit し、`vdiffr` test は `cran = FALSE` で実行する。

### 7.3 Coverage、lint、style

- coverage は当初 information 扱いとし、一律 80% などの閾値を置かない。
- meaningful baseline ができた後、重要 module の低下防止を検討する。
- v0.9.7 の `lintr` は advisory とする。
- v0.10.0 から、新規・変更された file 全体の `lintr` を blocking とする。
- `styler::style_pkg()` の一括適用は行わない。
- 変更箇所から負債を増やさない ratchet 方式を採用する。
- 新規コードは原則 `|>`、既存 `%>%` は周辺を触る時だけ段階的に移行する。
- 2-space indent、`<-`、英語コメント、原則80字を適用する。
- コメントは処理内容より理由を説明する。
- バグ修正、API 変更、機械的整形を別差分にする。

### 7.4 性能

現時点では性能 benchmark や速度目標を計画へ含めない。正確性、入力契約、
共有 layout の完成を優先する。大規模データの代表例と利用要求が得られた
場合に、sparse backend と併せて別計画として評価する。

## 8. 文書方針

- 英語の README、API 文書、getting-started vignette を正本とする。
- 日本語ガイドは補助文書として維持する。
- 翻訳の完全同期は release blocker にしない。
- README は install、最短 quick start、代表図、詳細文書への link に絞る。
- 入力契約、metadata、abundance / binary、tree alignment は基本 vignette に
  集約する。
- marquee、RPANDA、投稿図向け構成などは advanced 文書へ分離する。
- `.qmd` を source of truth とし、authoring 時に生成した HTML や `*_files`
  assets を package build 入力にしない。
- `R CMD build` が vignette source から生成する `inst/doc` HTML は、容量 gate を
  満たした上で tarball に含める。
- 調査時点の local authoring HTML / assets は約27.8 MBであり、配布対象に
  しない。この値は `quarto::html` が実際に生成する `inst/doc` 容量の見積もり
  ではないため、canonical tarball で別に計測する。
- package vignette build と異なる theme / font 依存を前提にしない。
- 実行する文書の dependency は `Suggests` に宣言する。
- `library(tidyverse)` は避け、必要な個別 package を明示する。

## 9. 新機能の優先度

### 9.1 共有 layout 完成後に需要検証する順序

1. interaction 固有 metadata
2. `treeio` / `treedata` の属性保持
3. interaction 総量とは独立した node size
4. 複数 web / facet workflow

これらも、自動的に release scope へ入れない。代表 workflow、受入テスト、
既存 package で代替できない理由を確認してから実装する。

### 9.2 証拠が得られるまで保留

- crossing 低減の順序最適化
- `phyloseq` adapter
- `TreeSummarizedExperiment` adapter
- curved abundance ribbon
- sparse backend
- signed interaction

### 9.3 実装しない、または別 package に委ねる項目

- `bipartite` の network / species indices、null model、robustness の再実装
- 通常の matrix を包むだけの `bipartite` 専用 adapter
- tripartite / multipartite
- circular / unrooted tree と中央 network の統合
- interactive UI、3D 表示

## 10. 実装順序と変更単位

```text
test and CI baseline
  -> P0 fixes
  -> P1 fixes
  -> documentation alignment
  -> v0.9.7
  -> canonical input and shared layout
  -> existing layer migration
  -> lifecycle migration
  -> optional composer
  -> v0.10.0
```

- 各 bug fix は対応する回帰テストと同じ意味的変更単位にする。
- formatter だけの変更は、機能変更と別 commit にする。
- 公開 API の変更には roxygen、NEWS、migration note を同時に含める。
- 依存追加には、必要性と core 未導入時の挙動を記載する。
- push、tag、GitHub issue 作成は、この計画の自動的な実行範囲に含めない。

## 11. リスクと対策

| リスク | 影響 | 対策 |
|---|---|---|
| 調査基準と実装開始 revision の差 | 修正済みコードとの衝突、再現不能 | WP-0 で claim を再検証する |
| strict validation が既存の曖昧入力を止める | patch release での挙動変更 | 誤描画防止を優先し、NEWS と error message で移行方法を示す |
| `ggtree` / Bioconductor 世代差 | 特定環境だけの check failure | 公式に整合する release / devel / oldrel lane を使う |
| dependency floor の未宣言 | 古い環境で install 後に実行時失敗 | 使用 API から下限を導出し、exact-min lane で検証する |
| vignette dependency と容量 | tarball 肥大化、build failure | `.qmd` のみを正本にし、依存と生成物を監査する |
| visual snapshot の脆弱性 | font / device 差による偽陽性 | correctness は数値テスト、画像は2例の CI 監視に限定する |
| 公開 layout schema の早期固定 | 後続機能で破壊的変更が必要 | 安定 field を最小化し、内部 field を契約外にする |
| 全体整形による巨大差分 | review 困難、blame 消失 | ratchet 方式と独立 formatting commit を使う |
| required check が手順だけに依存 | 未検証 release / tag の作成 | ruleset と workflow dependency で機械的に強制する |

## 12. リリースチェックリスト

### v0.9.7

- [ ] 最新 revision で P0・P1 の再現状態を確認した
- [ ] `testthat (>= 3.0.0)`、edition 3 と CI が導入されている
- [ ] BUG-001 から BUG-009 に回帰テストがある
- [ ] 入力契約と error message が文書化されている
- [ ] P2候補がそれぞれ fixed / deferred に分類されている
- [ ] dependency floor が宣言され、exact-min lane が成功する
- [ ] required CI matrix がすべて成功する
- [ ] canonical artifact の checksum が記録されている
- [ ] 同じ tarball の `R CMD check --as-cran` が成功する
- [ ] fixed Linux lane の限定的 `vdiffr` test が成功する
- [ ] required status と release workflow dependency が設定されている
- [ ] README と vignette の役割が分離されている
- [ ] `quarto render README.qmd` の docs job が成功する
- [ ] 配布 vignette と `DESCRIPTION` / `.Rbuildignore` が整合する
- [ ] `inst/doc` は5 MB以下、source tarball は10 MB未満である
- [ ] `NEWS.md` に correctness fix と互換性影響が記載されている
- [ ] 新機能が混入していない

### v0.10.0

- [ ] `bipartite_layout` の schema と不変条件がテストされている
- [ ] matrix / long の明示 dispatch と列 schema が design note にある
- [ ] layout / composer 経路では入力が一度だけ検証・正規化される
- [ ] 主要 geom が明示的な `layout` 経路で共有 layout を使用する
- [ ] raw-data API の互換 wrapper が動作する
- [ ] layout の `$trees` だけで tree component を再構成できる
- [ ] deprecated / superseded API に移行先がある
- [ ] migration guide がある
- [ ] 新規・変更コードの `lintr` が成功する
- [ ] required CI / vignette / `R CMD check --as-cran` が成功する
- [ ] composer を defer する場合、次版予定と残る制約が明記されている

## 13. 参照資料

- [tidyverse style guide](https://style.tidyverse.org/)
- [ユーザー指定の R コーディング規則記事](https://zenn.dev/cp_r/articles/2747e7cb35684c)
- [Writing R Extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html)
- [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html)
- [Bioconductor installation and version mapping](https://bioconductor.org/install/)
- [Bioconductor release announcements](https://bioconductor.org/about/release-announcements/)
- [BiocManager validity checks](https://bioconductor.github.io/BiocManager/reference/valid.html)
- [Quarto HTML vignettes for R packages](https://quarto-dev.github.io/quarto-r/articles/hello.html)
- [patchwork plot assembly](https://patchwork.data-imaginist.com/articles/guides/assembly.html)
- [aplot manual](https://cran.r-project.org/web/packages/aplot/aplot.pdf)
- [lintr configuration](https://lintr.r-lib.org/articles/lintr.html)
- [styler introduction](https://styler.r-lib.org/articles/introducing_styler.html)
- [vdiffr visual expectations](https://vdiffr.r-lib.org/reference/expect_doppelganger.html)
- [testthat third edition](https://testthat.r-lib.org/articles/third-edition.html)
- [r-lib/actions standard check workflow](https://github.com/r-lib/actions/blob/v2-branch/examples/check-standard.yaml)
- [r-lib/actions coverage workflow](https://github.com/r-lib/actions/blob/v2-branch/examples/test-coverage.yaml)
- [GitHub required status checks](https://docs.github.com/en/pull-requests/how-tos/merge-and-close-pull-requests/troubleshooting-required-status-checks)
