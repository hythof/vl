# VLとは
この文章はプログラミング言語 VL の開発背景と設計/実装を説明します


# 解決したい問題
プラットフォーム毎に開発言語を使い分けと共通化がとにかく大変
複雑なプログラムのデバッグがとにかく大変


# 解決方法
静的型付けな純粋関数型言語 VL を用いて各プログラミング言語のソースコードへ変換する
変換後の関数名は変換元と一致
Haskell の簡潔さと純粋さ、C言語からメモリ操作の便利さを併せ持つ事が目標
強力なREPLと過去に遡れるデバッガを提供

## コンパイル過程
1. VL -> PT
   - 複数のテキストファイルから具象構文木へ変換
   - 構文チェック

2. PT -> AST
   - 具象構文木から抽象構文木へ変換
   - 定数畳み込み
   - 末尾再帰の最適化

3. AST -> ターゲット言語
   - 各プログラミング言語へ変換
   - なるべく読みやすく
   - 1ファイルに詰め込む

## ターゲット言語
- Javascript (Web Browser)
- Swift      (iOS)
- Java       (Android)
- Go         (Windows / Mac / Linux)
- C#         (Unity, Xamarin)
- Erlang     (High Availability Server)
- C          (High Performance)

## 対象領域
- 学習、デバッグ、テストを簡単に
- バイナリ操作
- 文字列操作
- アルゴリズムの記述
- 有限オートマトンの記述
- 非同期処理
- スレッドやパケットのスケジューリング制御

# 実装方針
ターゲット言語毎に開発する部分を少なく保つ
例外は使用せず失敗は戻り値で表現
テスト駆動開発

## ターゲット言語毎に開発
- if
- for
- 変数の定義、参照、更新
- 関数の定義、呼び出し
- 基本型の対応付けと相互変換
  - string
  - int8 .. int64
  - uint8 .. uint64
  - float32, float64
  - big int
  - big float
- 構造体の対応付けと入れ子対応
  - array
  - struct
- コンソール関連
  - read
  - write
- マルチスレッド関連
  - mutex
- バイト順関連
  - little or big

## 標準ライブラリ
- 文字列操作
- 単体テスト
- デバッガー
- モナド


# 配布方法
TBD


# 参考
- Haxe
- Emscripten
- CoffeeScript, TypeScript, Dart
- Functional Reactive Programming


# やること
[] 言語シンタックス設計
[] REPL開発
[] 標準ライブラリ実装
[] PT変換
[] AST変換
[] C変換
[] Erlang変換
[] Javascript変換
[] Swift変換
[] Java変換
[] Go変換
[] C#変換
[] 標準ライブラリ開発
