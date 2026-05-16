# Mini-Scheme 拡張アイデア集

## 前提

現状の処理系は、概ね次の構成まで到達している。

```text
Text -> Parser -> SExpr -> Analyzer -> AST -> Evaluator -> Value
```

`SExpr` 層が独立しているため、マクロ展開や別フロントエンドを差し込みやすい。`AST` と `Evaluator` が分かれているため、直接評価以外に VM、抽象機械、コード生成バックエンドを追加しやすい。`Value` / `Env` / `Eval` は mutable pair、closure、組み込み関数、エラー、IO を扱う実行時基盤になっている。

評価軸は以下とする。

| 評価 | 意味 |
|---|---|
| 難易度 | 1 が小改修、5 が設計変更を伴う大改造 |
| 面白さ | 1 が実用補助、5 が処理系・PL 的にかなり面白い |
| 相性 | このコードベースの既存構造をどれだけ活かせるか |

---

## すぐ効く実用拡張

| 案 | 難易度 | 面白さ | 相性 | 概要 |
|---|---:|---:|---:|---|
| `display` / `newline` / `print` | 1 | 2 | 5 | `showValueIO` を使う IO builtin。デモコードをかなり動かせるようになる。 |
| `caar` / `cadr` / `cdar` / `cddr` 系 | 1 | 2 | 5 | `car` / `cdr` の合成 builtin。Horn clause solver や Mandelbrot 例の足りない部品になる。 |
| エラー表示の改善 | 2 | 2 | 4 | source location、parse/analyze/eval の段階、backtrace を表示する。Megaparsec と AST に位置情報を足すと本格化する。 |
| REPL 改善 | 2 | 2 | 4 | `haskeline` による履歴、複数行入力、`:load`、`:env`、`:quit` など。処理系の完成度が上がる。 |
| target regressions の spec 化 | 1 | 3 | 5 | 手元の面白い Scheme 断片を `test/spec` に整理し、現在動くものと将来機能が必要なものを分ける。 |

最初にやるならこのジャンルが一番費用対効果が高い。特に `display` / `newline` / `cadr` 系は、提示されたデモコードをかなり前進させる。

---

## 言語機能拡張

| 案 | 難易度 | 面白さ | 相性 | 概要 |
|---|---:|---:|---:|---|
| Common Lisp 風 `define-macro` | 3 | 4 | 5 | `SExpr -> SExpr` の展開器を Analyzer 前に挟む。現在のアーキテクチャが一番活きる。 |
| R5RS `syntax-rules` | 5 | 5 | 3 | hygienic macro。identifier に scope 情報を持たせる必要があり、単なるマクロ追加ではなく名前解決の再設計になる。 |
| `call/cc` | 5 | 5 | 3 | 第一級継続。横山デモ系の大部分が動く。直接スタイル evaluator に後付けするより CEK/CESK 化が筋が良い。 |
| 限定継続 `shift` / `reset` | 5 | 5 | 2 | `call/cc` より現代的で、継続境界を扱える。実装には continuation 表現の明示化が必要。 |
| 例外 / `catch` / `throw` | 3 | 3 | 4 | `EvalError` とは別に Scheme レベルの例外を入れる。continuation を明示化すると扱いやすい。 |
| concurrency primitives | 3 | 3 | 4 | `spawn`, `chan`, `send!`, `recv!` などを Haskell の `forkIO` / STM で実装する。 |
| contract system | 3 | 4 | 4 | `(define/contract ...)` のように動的契約を追加する。Scheme らしく、predicate builtin と相性が良い。 |

一癖ある課題としては `define-macro` と `call/cc` が強い。`define-macro` は現在の `SExpr` 層の存在理由を説明しやすく、`call/cc` は処理系の評価モデルを根本から考える題材になる。

---

## 評価器・抽象機械

| 案 | 難易度 | 面白さ | 相性 | 概要 |
|---|---:|---:|---:|---|
| TCO trampoline | 3 | 4 | 4 | `eval` の戻り値を `Done` / `TailCall` に変え、末尾呼び出しをループで処理する。 |
| CPS evaluator | 4 | 4 | 3 | 継続を Haskell 関数として明示する。`call/cc` の準備にはなるが、そのままだと継続を値として扱いにくい。 |
| CEK machine | 4 | 5 | 4 | `Control`, `Env`, `Kont` に分けた小ステップ評価器。TCO、backtrace、stepper、call/cc の土台になる。 |
| CESK machine | 4 | 5 | 5 | CEK に `Store` を足して、mutable cell と `set!` を明示する。現在の `IORef` ベース実装を処理系らしい heap/store に置き換えられる。 |
| Stepper / environment diagram | 3 | 5 | 5 | 評価を 1 step ずつ記録し、`let`, closure capture, `set!`, `do` の環境変化を可視化する。 |
| Algebraic effects 化 | 4 | 4 | 3 | `Reader` / `State` / `Error` / `IO` / `Cont` を effect handler へ分離する。構造は綺麗だが成果物は普通の interpreter に見えやすい。 |

このジャンルで最もおすすめなのは `Stepper` と `CEK/CESK`。前者はデモ映えし、後者は `call/cc` や抽象解釈へ進むための理論的な足場になる。

---

## コンパイル・VM・バックエンド

| 案 | 難易度 | 面白さ | 相性 | 概要 |
|---|---:|---:|---:|---|
| Bytecode VM | 3 | 5 | 5 | `AST -> [Instr] -> VM` を追加する。既存 evaluator と同じ spec を通せるので検証しやすい。 |
| ANF 変換 | 3 | 4 | 4 | 複雑な式を `let` 列に正規化する。VM、CPS、最適化、コード生成の前処理として使える。 |
| Closure conversion | 4 | 5 | 4 | closure の環境 capture を明示的な environment record に変換する。WASM/LLVM へ進む前の王道パス。 |
| Lambda lifting | 4 | 4 | 3 | nested lambda を top-level function に持ち上げる。closure conversion と組み合わせると compiler らしくなる。 |
| WASM backend | 5 | 5 | 3 | ブラウザで Mini-Scheme を動かせる。値表現、heap、GC、IO 境界の設計が重い。 |
| LLVM JIT | 5 | 4 | 2 | ネイティブコード生成。`llvm-hs` のバージョン制約、boxing、GC で周辺作業が重くなりやすい。 |
| Template Haskell quasiquoter | 5 | 5 | 3 | `[scheme| ... |]` を Haskell コンパイル時に解析し、TH AST へ展開する。Haskell から Scheme 関数を直接使える。 |

このコードベースで一番バランスが良いのは Bytecode VM。既存の Parser/Analyzer/spec runner をそのまま使い、評価先だけを増やせる。次に ANF と closure conversion を足すと、WASM や LLVM へ進む道が見える。

---

## 解析・検証・テスト

| 案 | 難易度 | 面白さ | 相性 | 概要 |
|---|---:|---:|---:|---|
| Differential testing | 3 | 5 | 5 | ランダム生成した Mini-Scheme プログラムを Guile/Racket と比較する。既存 `SchemeSpecRunner` と相性が良い。 |
| Property-based testing | 2 | 4 | 5 | Parser round-trip、`quote` 表示、list 操作、`equal?` の性質などを QuickCheck/Hedgehog で検証する。 |
| Golden test runner | 2 | 3 | 5 | `.scm` と期待出力を golden file として管理する。デモコード集の回帰確認に向く。 |
| Static arity check | 3 | 3 | 4 | 明らかな arity error を評価前に検出する。動的型 Scheme の範囲内で軽量静的解析を足す。 |
| 0-CFA / k-CFA | 5 | 5 | 4 | 関数適用の到達可能性を抽象解釈で解析する。CEK/CESK を作った後に特に自然。 |
| Soft typing / HM 型推論 | 4 | 5 | 3 | 動的型 Scheme に任意の静的型推論を足す。typed subset を切り出すと最適化や TH 展開に繋がる。 |
| e-graph 最適化 | 5 | 5 | 3 | `(+ x 0) = x` などの等式を書き、equality saturation で最適式を選ぶ。発表映えするが実装は重い。 |

実用性と面白さの両立なら differential testing が強い。R5RS portable spec の仕組みが既にあるため、乱択生成へ拡張しやすい。

---

## Haskell らしい再設計・EDSL

| 案 | 難易度 | 面白さ | 相性 | 概要 |
|---|---:|---:|---:|---|
| Tagless Final interpreter | 4 | 5 | 3 | `class Sym repr` を作り、評価、pretty print、compile などを instance として切り替える。 |
| Typed Mini-Scheme subset | 4 | 5 | 3 | 動的 Scheme とは別に typed subset を作る。Tagless Final や TH 展開と相性が良い。 |
| PHOAS AST | 4 | 4 | 2 | lambda 束縛を Haskell 関数で表す。capture-avoiding substitution は綺麗だが `set!` と相性が悪い。 |
| Normalization by Evaluation | 5 | 5 | 2 | typed lambda subset に対して正規化器を作る。現在の full Mini-Scheme からは別方言寄り。 |
| Haskell host FFI | 3 | 4 | 4 | Haskell 関数を Scheme 環境へ登録し、Scheme から呼ぶ。`VBuiltin` の一般化で実装しやすい。 |
| Scheme callback into Haskell app | 3 | 4 | 4 | Haskell アプリ側から Scheme closure を呼ぶ API を整える。埋め込み言語として価値が出る。 |
| Capability-based sandbox | 3 | 4 | 4 | `load`, `display`, `spawn` などを capability で制限する。安全な埋め込み言語として面白い。 |

Tagless Final は Haskell 度が最も高いが、既存の dynamically typed AST をそのまま活かすというより、typed subset を横に作る性格が強い。既存コードを活かすなら Host FFI や sandbox の方が自然。

---

## 実行時・メモリ管理

| 案 | 難易度 | 面白さ | 相性 | 概要 |
|---|---:|---:|---:|---|
| explicit heap/store | 4 | 5 | 4 | `IORef` と Haskell GC に任せず、`Addr -> Object` の store を持つ。CESK や GC 実験の土台。 |
| mark-sweep GC | 5 | 5 | 3 | closure/env/pair を自前 heap に置いた上で到達可能性を辿る。処理系課題らしさが強い。 |
| copying GC | 5 | 5 | 3 | Cheney copying GC など。WASM backend と組み合わせると意味が出る。 |
| cycle-aware printer 拡張 | 2 | 3 | 5 | 循環構造を `#0=(1 2 . #0#)` のように読みやすく表示する。 |
| resource limit | 3 | 4 | 4 | step count、heap size、time budget を Eval/VM に持たせる。無限ループや sandbox と相性が良い。 |

本格処理系らしさを出すなら explicit heap/store が入り口。いきなり GC へ行くより、まず `IORef` を store address に置き換える方が設計上の学びが大きい。

---

## 推奨ルート

### ルート A: デモコードをたくさん動かす

| 順序 | 内容 |
|---:|---|
| 1 | `display`, `newline`, `print`, `cadr` 系を追加 |
| 2 | TCO |
| 3 | `call/cc` |
| 4 | `define-macro` |
| 5 | 浮動小数点数と非整数 `/` |

提示された Scheme 断片を動かすことを重視するルート。成果が目に見えやすい。

### ルート B: 処理系として一段進める

| 順序 | 内容 |
|---:|---|
| 1 | Bytecode VM |
| 2 | ANF 変換 |
| 3 | Closure conversion |
| 4 | WASM backend |

既存 evaluator と VM/backend の結果を spec runner で比較できる。コンパイルらしさが強い。

### ルート C: PL 理論寄りにする

| 順序 | 内容 |
|---:|---|
| 1 | CEK/CESK machine |
| 2 | Stepper / environment diagram |
| 3 | `call/cc` |
| 4 | 0-CFA / k-CFA |

発表で「なぜこの評価器設計なのか」を説明しやすい。Haskell の ADT と pattern matching がよく活きる。

### ルート D: Haskell EDSL として攻める

| 順序 | 内容 |
|---:|---|
| 1 | typed Mini-Scheme subset |
| 2 | Tagless Final signature |
| 3 | Eval / pretty / bytecode の複数 interpretation |
| 4 | Template Haskell quasiquoter |

Haskell らしさは最も強い。ただし既存 Mini-Scheme とは別方言を横に育てる面がある。

---

## 現時点の最有力候補

総合的には、以下の 3 つが特に良い。

1. **Bytecode VM**
   - 難易度 3、面白さ 5。
   - 既存の Parser/Analyzer/spec runner を最大限使える。
   - 「直接評価」から「コンパイルして実行」へ進めるため、成果物が分かりやすい。

2. **CEK/CESK + Stepper**
   - 難易度 4、面白さ 5。
   - `Env`, closure, `set!`, `do`, `letrec` の挙動を可視化できる。
   - `call/cc` や 0-CFA の土台になる。

3. **Common Lisp 風 `define-macro`**
   - 難易度 3、面白さ 4。
   - `SExpr` 層を挟んだ設計の意味が最も自然に出る。
   - `let**`, `when`, `unless`, `while` などをユーザー定義できる。

短期で動くものを増やすなら小さい builtin 追加から始める。発表やレポートで一癖ある方向へ行くなら Bytecode VM か CEK/CESK が最も見栄えする。
