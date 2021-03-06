# 數據驅動下之不動產價格分析與預測暨模型視覺化 - 以複數年之高雄不動產實價登錄資料為例

不動產的取得往往需付出龐大的價格去取得，不管是平常生活起居的房屋、商業辦公的大樓，或者其他形式的不動產。
而近年，外在經濟因素與環境因素，不動產成為投資工具，導致不動產價格呈現高漲不跌的型態。因而衍生了居住正義、都市更新等相關議題，
儘管政府實施許多相關政策，但不動產價格還是容易受各種消息面影響，如何取得合理價格的問題仍然存在。

過往有許多研究與文獻透過各種學理理論、政策制度層面、亦或是探討其他因素所帶來的影響，來評估不動產價格是否在合理區間。
而比較少關注龐大的不動產數據中，眾多變數是否具有值得參考的價值提供給使用者。

因此，本研究從臺灣經濟新報數據庫之不動產實價登錄系統，蒐集2019年7月至2021年6月高雄市地區不動產數據作為研究樣本。
基於數據驅動的理念，利用數據探勘技術中的分群方法，將資料檔內的數值型與非數值型的混合資料，
以相關與距離兩判別要素，將資料檔內的變數予以集群，以失去微量的訊息量為代價，而達到變數減量之目的。
如此，有利於建立後續的不動產價格分析與預測之模型。再針對不同集群建構一般線性迴歸模型進行不動產價格預估與各模型間比較。
最後整合至R-Shiny，開發視覺化分析系統，作為結果之呈現。
