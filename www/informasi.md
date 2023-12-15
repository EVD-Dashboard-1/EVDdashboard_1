### Dataset `Otomotif Mobil`

Dataset ini merupakan data bawaan dari _package_ `datasets` pada aplikasi R. Data ini memiliki beberapa peubah, yaitu:
1. **mpg** (miles/(us) gallon), yakni jarak tempuh suatu mobil dibandingkan dengan konsumsi 1 galon bahan bakar.
2. **cyl** (number of cylinders), yakni banyaknya silinder pada mesin mobil.
3. **disp** (displacement), yakni volume mesin sebagai alat ukur tenaga yang dihasilkan.
4. **hp** (horsepower), yakni tenaga mesin yang dihasilkan dalam satuan horsepower.
5. **drat** (rear axle ratio), yakni rasio torsi mesin penggerak depan.
6. **wt** (weight), yakni berat mobil.
7. **qsec** (1/4 mile time), yakni kemampuan akselerasi mobil.
8. **vs** (engine type), yakni jenis mesin mobil (0=V-shaped, 1=Straight).
9. **am** (transmission type), yakni jenis transmisi mobil (0=otomatis, 1=manual).
10. **gear** (number og forward gears), yakni banyaknya gear depan mobil.

Sebagai catatan bahwa peubah respon pada analisis regresi adalah peubah yang menggunakan skala data kontinu seperti: **mpg**, **cyl**, **disp**, **drat**, **wt**, **qsec**, atau **gear**. Sedangkan peubah penjelas dapat berupa skala data kategorik maupun kontinu.

### Dataset `Karakteristik Batu`

Dataset ini merupakan data bawaan dari _package_ `datasets` pada aplikasi R. Data ini memiliki beberapa peubah, yaitu:
1. **area** (pixel), yakni luasan area berpori.
2. **per** (perimeter in pixel), yakni ukuran perimeter.
3. **shape** (perimeter/sqrt), yakni ukuran  perimeter per satu kaki luas area.
4. **perm** (permeability), yakni permeabilitas dalam mili-Darcies.

Sebagai catatan bahwa peubah respon pada analisis regresi adalah peubah yang menggunakan skala data kontinu (semua peubah pada dataset `Karakteristik Batu` dapat digunakan sebagai peubah respon).

### Dataset `Temperatur dan Tekanan`

Dataset ini merupakan data bawaan dari _package_ `datasets` pada aplikasi R. Data ini memiliki beberapa peubah, yaitu:
1. **temperature** (derajat Fahrenheit), yakni suhu.
2. **pressure** (pascal), yakni tekanan yang dihasilkan.

Sebagai catatan bahwa peubah respon pada analisis regresi adalah peubah yang menggunakan skala data kontinu (semua peubah pada dataset `Temperatur dan Tekanan` dapat digunakan sebagai peubah respon).

### Statistik Deskriptif 

Statistik deskriptif dimaksud adalah ringkasan ukuran pemusatan dan penyebaran data. Beberapa statistik pemusatan data yakni _Mean_ dan _Median_. Sedangkan statistik penyebaran data yakni _Min_, Kuartil Pertama, Kuartil Ketiga, dan _Max_. Tujuan dari statistik deskriptif adalah untuk memberikan informasi awal terkait pemusatan dan penyebaran peubah.

### Korelasi antar Peubah 

Korelasi yang dimaksud adalah nilai keeratan hubungan antar peubah pada dataset, umumnya menggunakan peubah dengan skala data kontinu (korelasi pearson). Nilai Korelasi berada pada rentang -1 sampai dengan +1, di mana semakin besar nilai korelasi, menunjukkan semakin erat hubungan linier antar 2 peubah tersebut. Nilai korelasi positif menunjukkan hubungan linier yang searah, sedangkan nilai korelasi negatif menunjukkan hubungan linier yang berkebalikan.

### Garis Regresi

Garis regresi adalah sebuah garis lurus sebagai hasil persamaan matematis yang menjelaskan hubungan antara peubah penjelas terhadap peubah respon. Garis regresi yang dimaksud adalah garis regresi linier, yang  berarti berderajat satu untuk nilai parameternya. Dengan menggunakan garis regresi tersebut, dapat diketahui seberapa besar pengaruh perubahan pada peubah penjelas terhadap peubah respon. Selain itu juga dapat digunakan untuk memprediksi nilai baru di peubah respon jika diketahui sebuah nilai di peubah penjelas. Dalam regresi linear sederhana, garis regresi dinyatakan sebagai $Y = a + b X$ di mana:

- $Y$ adalah peubah respon,
- $X$ adalah peubah penjelas,
- $b$ adalah gradien/kemiringan/slope (koefisien regresi), yang mengukur seberapa besar tingkat perubahan pada peubah respon sebagai akibat perubahan pada peubah penjelas, dan
- $a$ adalah intercept, yaitu nilai peubah respon awal (ketika tidak ada pengaruh dari peubah penjelas). 

### Tren Garis Pemulusan

Garis pemulusan _smoothing_ adalah garis matematis yang bertujuan untuk membantu peneliti memahami pola data secara lebih halus (tanpa fluktuasi). Hal tersebut karena pada umumnya garis hubungan antara peubah penjelas (X) dengan peubah respon (Y) memiliki pola yang acak meskipun memiliki kecenderungan tertentu. Dengan adanya garis pemulusan tersebut, maka pola acak tersebut akan diperhalus sehingga peneliti dapat memanfaatkannya untuk mengetahui pola umum tren pada hubungan peubah penjelas dengan peubah respon.  

Pada aplikasi ini, nilai pemulusan antara 0 sampai dengan 1, di mana semakin mendekati angka 1 maka tren pemulusan akan semakin landai (mendekati garis lurus).

### Rangkuman Model Regresi

Rangkuman model regresi menunjukkan ringkasan dari Analisis Regresi Linier Sederhana (1 peubah penjelas berpengaruh terhadap 1 peubah respon). Informasi yang terdapat pada rangkuman yakni besarnya nilai koefisien regresi sesuai dengan penjelasan pada Tab Garis Regresi di atas.

### Tabel ANOVA

Tabel ANOVA merupakan hasil analisis dari pengujian hipotesis pengaruh peubah penjelas terhadap peubah respon. Nilai acuan dari pengujian hipotesis adalah nilai _p value_ di mana jika nilai tersebut kurang dari taraf nyata (biasanya 0.05), maka diputuskan untuk menolak Hipotesis Nol. Dengan kata lain peubah penjelas memiliki pengaruh yang signifikan terhadap peubah respon.

### Tampilan Sisaan pada Plot

Sisaan merupakan selisih antara nilai peubah respon hasil prediksi persamaan regresi dikurangi dengan nilai peubah respon asli (aktual). Sisaan umumnya tidak dapat dihindari dalam pemodelan statistika, namun dapat diminimalisir agar garis regresi yang terbentuk memiliki akurasi prediksi yang baik.

### Selang Kepercayaan Regresi 

Selang Kepercayaan Regresi merupakan suatu selang/rentang nilai di mana nilai prediksi dari peubah respon memiliki peluang 95% akan muncul. Semakin besar taraf nyata yang dipakai maka selang prediksi akan semakin sempit.
