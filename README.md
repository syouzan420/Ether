# Ether
openGLで表示する ボール  
## 使ひ方
ether.exe はwindowsで動く實行ファイルです  
自分はcygwinのターミナルを使つてゐるけれども  
Windows PowerShell などからでも  
プロンプトから  
./ether  
と入力すれば 實行できます  
ボールの個數を 列と行の數を指定することで 變更できます  
./ether 3 2  
なら 列數3 行數2 の合計6個のボールを扱ふことができます  
タイマーの間隔を變へたいときは ３つめの引數で指定します  
./ether 2 2 100  
デフォルトでは 1列 1行にあるボールが 横方向に10移動した状態で始まりますが  
最初に移動させるボールや その位置を 變更できます  
./ether 3 3 10 0 1 5 10  
とした場合 ３列 3行 のボールが設定され (1列 2行)のボールが 横方向に5 縦方向に10移動した状態で  
シミュレーションが始まります  
ちなみに ボール同士は 縦方向と横方向に バネによつて接続されてゐる といふ設定です  
「これ以上バネが縮んだり伸びたりしてはならない」といふ制約を設けず  
マイナス方向に縮んだり と現實ではあり得ないことも あえて起こしてゐます  
私は このシュミレーションに 「エーテル」といふ名前をつけてゐます