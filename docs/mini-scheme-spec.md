# Mini-Scheme 仕様

## A. 構文

### 記法

| 記法                 | 意味          |
|--------------------|-------------|
| `X*`               | Xの0回以上の繰り返し |
| `X+`               | Xの1回以上の繰り返し |
| `[X]`              | Xは高々1個      |
| 大文字で始まる名前          | 非終端記号       |
| `(`, `)`, `.`, `'` | 終端記号        |

### 文法

```
Toplevel ::= Exp
           | Define
           | (load String)

Define   ::= (define Id Exp)
           | (define (Id Id* [. Id]) Body)

Exp      ::= Const
           | Id
           | (lambda Arg Body)
           | (Exp Exp*)
           | (quote S-Exp)
           | (set! Id Exp)
           | (let [Id] Bindings Body)
           | (let* Bindings Body)
           | (letrec Bindings Body)
           | (if Exp Exp [Exp])
           | (cond (Exp Exp+)* [(else Exp+)])
           | (and Exp*)
           | (or Exp*)
           | (begin Exp*)
           | (do ((Id Exp Exp)*) (Exp Exp*) Body)

Body     ::= Define* Exp+

Arg      ::= Id
           | (Id* [Id . Id])

Bindings ::= ((Id Exp)*)

S-Exp    ::= Const
           | Id
           | (S-Exp* [S-Exp . S-Exp])

Const    ::= Num | Bool | String | ()
```

### リテラル

| 型          | 構文                             | 備考                      |
|------------|--------------------------------|-------------------------|
| **Num**    | `42`, `-3`                     | 最低限10進整数をサポート           |
| **Bool**   | `#t`, `#f`                     |                         |
| **String** | `"hello"`                      | バックスラッシュ `\` によるエスケープ必須 |
| **Nil**    | `()`                           | 空リスト                    |
| **Id**     | `[0-9A-Za-z!$%&*+-./<=>?@^_]+` | 数値として解釈されるものを除く         |

### 糖衣構文

| 糖衣構文                    | 展開形                              |
|-------------------------|----------------------------------|
| `'X`                    | `(quote X)`                      |
| `(define (f x y) body)` | `(define f (lambda (x y) body))` |

### 注意事項

1. `(quote X)` に加えて `'X` 記法もサポートすること
2. 文字列はバックスラッシュ `\` によるエスケープをサポートすること
3. 識別子から数値として解釈されるトークンを除外すること
4. `let*` は単一のキーワードであり、`let` + `*` ではない
5. `cond` は `else` 節を含めて最低1つの節が必要。`else` 節は高々1つ

---

## B. 組み込み関数

### 整数演算

| 関数                        | シグネチャ             |
|---------------------------|-------------------|
| `number?`                 | `any -> bool`     |
| `+`                       | `num* -> num`     |
| `-`                       | `num+ -> num`     |
| `*`                       | `num* -> num`     |
| `/`                       | `num num -> num`  |
| `=`, `<`, `<=`, `>`, `>=` | `num num -> bool` |

### リスト操作

| 関数         | シグネチャ                    |
|------------|--------------------------|
| `null?`    | `any -> bool`            |
| `pair?`    | `any -> bool`            |
| `list?`    | `any -> bool`            |
| `symbol?`  | `any -> bool`            |
| `car`      | `pair -> any`            |
| `cdr`      | `pair -> any`            |
| `cons`     | `any any -> pair`        |
| `list`     | `any* -> list`           |
| `length`   | `list -> num`            |
| `memq`     | `any list -> list \| #f` |
| `last`     | `list -> any`            |
| `append`   | `list* -> list`          |
| `set-car!` | `pair any -> void`       |
| `set-cdr!` | `pair any -> void`       |

### ブール値

| 関数         | シグネチャ         |
|------------|---------------|
| `boolean?` | `any -> bool` |
| `not`      | `any -> bool` |

### 文字列

| 関数               | シグネチャ               |
|------------------|---------------------|
| `string?`        | `any -> bool`       |
| `string-append`  | `string* -> string` |
| `symbol->string` | `symbol -> string`  |
| `string->symbol` | `string -> symbol`  |
| `string->number` | `string -> num`     |
| `number->string` | `num -> string`     |

### その他

| 関数           | シグネチャ             |
|--------------|-------------------|
| `procedure?` | `any -> bool`     |
| `eq?`        | `any any -> bool` |
| `neq?`       | `any any -> bool` |
| `equal?`     | `any any -> bool` |
| `load`       | `string -> void`  |

---

## C. 末尾呼び出しの最適化 (TCO) — オプション

末尾位置の関数呼び出しはコールスタックを消費せずに実行すること。例:

```scheme
(define (even? x) (if (= x 0) #t (odd?  (- x 1))))
(define (odd?  x) (if (= x 1) #t (even? (- x 1))))
```

`(even? 1000000)` がスタックオーバーフローなしで完了しなければならない。

---

## D. Common-Lisp風マクロ — オプション

```scheme
(define-macro (positive x)
  (list '> x 0))

;; (positive (+ a 1)) は (> (+ a 1) 0) に展開される
```

- 関数と異なり、引数は展開前に**評価されない**
- マクロ本体はS式を返し、呼び出し箇所を置き換える
- 展開は**評価の前**に行われる

例: マクロによる `let*` の定義:

```scheme
(define (let*-expander vars body)
  (if (null? vars)
    (cons 'begin body)
    (list 'let (list (car vars)) (let*-expander (cdr vars) body))))

(define-macro (let* vars . body)
  (let*-expander vars body))
```

---

## 全般的な要件

- 構文に合致しない入力はエラーとすること
- 実行時エラーによって処理系自体が停止しないようにすること（できる範囲で）
