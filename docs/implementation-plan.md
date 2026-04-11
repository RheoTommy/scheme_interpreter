# 実装計画

コースB: HaskellによるMini-Schemeの実装

## アーキテクチャ

```
String ─→ Parser ─→ SExpr ─→ マクロ展開 ─→ SExpr ─→ Analyzer ─→ AST ─→ Evaluator ─→ Value
                               (SExpr → SExpr)                             ↑
                                    |                              最適化が必要になったら
                                    |                              AST → IR を導入
                                    └── eval が必要
```

- **SExpr**: 最小限のデータ型 (`Atom | Pair SExpr SExpr | Nil`)。Parserの出力。マクロ展開と `quote` の中身もこの型。
- **AST**: Mini-Schemeの構文を反映した代数的データ型 (`Lambda | If | App | ...`)。Analyzerによる構文検証済み。
- **Value**: 実行時の値 (`VNum | VBool | VString | VSymbol | VPair | VClosure | VBuiltin | VNil`)。
- **IR**: 最適化が必要になった段階でAST→IRの変換を導入する。現時点では不要。

---

## 必須課題

### R1. Parser

テキストを `SExpr` にパースする。

- トークナイザ: `(`, `)`, `.`, `'`, 文字列, 数値, 識別子, `#t`, `#f`
- パース時に `'X` → `(quote X)` を変換
- 文字列のエスケープシーケンス (`\\`, `\"`, `\n` 等)
- 不正な入力にはエラーを返す

**方針**: `ReadP`（`base`に含まれるため追加依存なし）または手書きの再帰下降パーサ。

### R2. SExpr → AST 変換 (Analyzer)

`SExpr` を型付きASTに変換し、不正な構文を拒否する。

- 各特殊形式に対して `SExpr` の構造をパターンマッチ
- アリティ・構造の検証（`cond` の節数、`let` の束縛の形など）
- 脱糖: `(define (f x) body)` → `(define f (lambda (x) body))`
- `quote` の中身は `SExpr` のまま保持（ASTに変換しない）

### R3. Evaluator

環境中でASTを評価する。

- `Env`: フレームの連結リスト。各フレームは `IORef (Map String (IORef Value))`
- `eval :: Expr -> EvalM Value`（`EvalM` はモナドスタック: `ReaderT Env (ExceptT SchemeError IO)` 等）
- `apply :: Value -> [Value] -> EvalM Value`
- 全特殊形式の実装: `lambda`, `if`, `cond`, `and`, `or`, `let`, `let*`, `letrec`, `set!`, `begin`, `do`
- `Body` 内の `define` は現在のフレームに追加

### R4. 組み込み関数

仕様に記載された約35個の関数。`VBuiltin` 値としてグローバル環境に事前ロードする。

### R5. REPL とエラーハンドリング

- プロンプト付きのRead-Eval-Printループ
- `ExceptT` で構造化されたエラー（`ParseError`, `TypeError`, `UnboundVar`, `ArityError` 等）をキャッチして表示
- 処理系自体はクラッシュしない
- `load` によるファイルの読み込みと評価

---

## オプション課題

### O1. 末尾呼び出しの最適化 (TCO)

**難易度**: 中

**方針**: トランポリン方式。evalの戻り値を変更:

```haskell
data EvalResult = Done Value | TailCall Env Expr
```

末尾位置（`if` の各分岐、`begin`/`body` の最後の式、`let`/`letrec` の本体、`cond`/`and`/`or`）では再帰せず `TailCall`
を返す。トップレベルのループで駆動する。

call/cc (O6) のためにCPSベースのevalを選択した場合、TCOは自然に得られる。

### O2. ホスト連携: Scheme → Haskell

**難易度**: 低

**方針**: `VBuiltin` による組み込み関数の仕組みを拡張する。

- 任意の `[Value] -> IO Value` 型のHaskell関数をScheme環境に登録可能にする
- Haskellのオブジェクトを不透明に渡すなら `Value` に `VOpaqueObj Dynamic` を追加

### O3. ホスト連携: Haskell → Scheme

**難易度**: 低

**方針**: Evaluatorをライブラリとして公開する:

```haskell
-- 公開API
newEnv :: IO Env
evalString :: Env -> String -> IO Value
callScheme :: Env -> String -> [Value] -> IO Value
```

`.cabal` で `library` と `executable` を分離し、Evaluatorをエクスポートする。

### O4. Ctrl-C ハンドリング

**難易度**: 中〜高

**方針**: GHCの非同期例外を使用。

- GHCランタイムはデフォルトでSIGINTを `UserInterrupt` に変換する
- REPLの各イテレーションのevalを `catch` で囲む
- 課題: 純粋な無限ループ（例: `(define (f) (f))`）には例外が届くために割り込みポイントが必要。`-fno-omit-yields`
  フラグ、またはevalループ内での明示的な `yield` の挿入で対応。

### O5. Common-Lisp風マクロ

**難易度**: 中

**方針**: マクロ展開はSExprレベルで、AST変換の前に行う。

- `define-macro` でクロージャをマクロテーブルに格納
- SExpr → AST 変換前に再帰的に展開: 先頭がマクロ名なら、マクロクロージャを**未評価**の引数SExprに適用し、結果を再展開
- フロー: `SExpr → マクロ展開 → SExpr → Analyzer → AST`
- 展開時にEvaluatorが必要（マクロ本体は展開時に評価される）

### O6. call/cc（第一級継続）

**難易度**: 高

**方針**: Evaluator全体をCPS変換する。

```haskell
eval :: Env -> Expr -> (Value -> IO Value) -> IO Value
```

`call/cc` は現在の継続 `k` をキャプチャし、`Value` として包む。呼び出されると現在の継続を破棄して `k` にジャンプする。

- TCOはCPSの自然な帰結として得られる
- 直接スタイルのEvaluatorに後付けすると大改造になる — **最初に決断すること**

### O7. 並行計算プリミティブ

**難易度**: 中〜高

**方針**: Haskellの軽量スレッドとSTMを活用。

- `(spawn expr)` → `forkIO`
- `(chan)` → `newTChan`
- `(send! ch val)` / `(recv! ch)` → STM操作
- Haskellのランタイムのおかげで他の言語よりも実装が容易
- `Env` の共有可変状態の扱いに注意が必要

---

## 推奨実装順序

```
R1 (Parser)
 → R2 (Analyzer: SExpr → AST)
   → R3 (Evaluator) + R4 (組み込み関数)
     → R5 (REPL)
       → O5 (マクロ)        ← SExpr層が活きる
         → O1 (TCO)
           → O2, O3 (ホスト連携)
             → O4 (Ctrl-C)
```

- マクロ (O5) はSExpr→AST のパイプラインが揃った段階で自然に追加できるため、TCOより先に実装する
- call/cc (O6) を実装したい場合は、R3の前にCPSベースで書く判断をすること
- 最適化が必要になった段階で AST → IR 変換を導入する（現時点では不要）
