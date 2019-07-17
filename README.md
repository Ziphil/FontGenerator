<div align="center">
<h1>シャレイア語フォント生成</h1>
</div>

## 概要
シャレイア語のフォントを Haskell コードから生成するための Stack プロジェクトです。
生成したフォントは[こちら](http://ziphil.com/conlang/database/7.html)で配布しています。

## 下準備

### Haskell と Stack の準備
このリポジトリは Haskell のビルドツールである [Stack](https://www.haskellstack.org/) のプロジェクトになっています。
適当な方法で Stack をインストールしてください。

### FontFotge と Python の準備
フォントファイルの生成には [FontForge](https://fontforge.github.io/) の Python スクリプティング機能を使います。
Python 上で `import fontforge` が通るようになっていれば問題ありません。

Windows の場合は、FontForge の Windows 用インストーラを用いると、FontForge がデフォルトで呼び出せるようになっている Python が勝手に付いてきます。
FontForge をインストールした場所の `bin` フォルダ内にある `ffpython.exe` というファイルがそれです。
このフォルダにパスを通してください。

### 環境に合わせてコードを修正
Haskell から Python を呼び出せるようにするため、`application/Main.hs` にある `generateOption = def` という記述を、以下のように書き換えてください。
```haskell
generateOption = with &~ do
  python .= "python"  -- ここを Python のコマンド名にする
```

この書き換えを行わないと `ffpython` を呼び出すようになっているので、Windows の場合で FontForge インストーラに付属の Python を用いるのであれば、書き換える必要はありません。

## 実行
Stack プロジェクトとして実行してください。
```
stack build
stack exec font-generator-exe
```

実行すると最初に `<?> Generate` と聞かれるので、フォントファイルを生成するなら `Y` を入力し、生成しないなら `N` を入力してください。
フォントファイルを生成しない場合、フォントのプレビューとなる SVG ファイルのみが生成されます。

次に `<?> Code` と聞かれるので、生成したいフォントファイルのコード名を指定してください。
何も入力しないと全てのフォントを生成します。