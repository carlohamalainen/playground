module SampleData (a) where

import Numeric.LinearAlgebra.HMatrix

a :: Matrix Double
a = fromLists
    [ [4.5534, 5.2065]
    , [4.5333, 6.3411]
    , [3.5642, 6.3327]
    , [4.0223, 3.7151]
    , [5.6059, 6.6184]
    , [4.8863, 5.6616]
    , [5.7646, 5.2273]
    , [5.6699, 4.7744]
    , [5.3758, 4.0340]
    , [5.7667, 5.0950]
    , [6.5235, 4.7433]
    , [5.5768, 7.3101]
    , [5.4260, 5.1901]
    , [4.9571, 4.8266]
    , [3.7577, 4.9860]
    , [4.4950, 4.3873]
    , [5.3259, 7.0718]
    , [3.8886, 5.6370]
    , [5.4682, 5.0749]
    , [5.3226, 6.1233]
    , [5.1000, 4.9669]
    , [5.3014, 4.9023]
    , [5.0238, 4.4434]
    , [4.9779, 4.3845]
    , [4.9912, 6.6046]
    , [5.9295, 5.7685]
    , [4.9096, 5.0869]
    , [2.3589, 6.7044]
    , [4.5139, 5.0236]
    , [5.1960, 5.2900]
    , [5.9597, 3.5801]
    , [6.3803, 5.4753]
    , [4.0585, 3.5527]
    , [5.7609, 4.0117]
    , [5.2379, 5.9494]
    , [4.7916, 5.3512]
    , [5.0247, 4.1277]
    , [4.9719, 4.6019]
    , [5.1404, 5.2564]
    , [5.2376, 5.2200]
    , [4.3285, 3.2886]
    , [3.9550, 3.7942]
    , [5.9658, 3.2271]
    , [4.7802, 4.9275]
    , [6.4145, 3.2789]
    , [4.0758, 5.7019]
    , [4.4059, 3.9945]
    , [6.4213, 3.8936]
    , [6.4076, 6.7906]
    , [3.9710, 7.1624]
    , [0.1807, 0.5277]
    , [0.9630, 1.2760]
    , [2.9630, 1.9842]
    , [0.4597, 1.9931]
    , [2.7175, 1.6682]
    , [1.8198, 0.9350]
    , [1.0555, 0.3799]
    , [0.6469, 1.2241]
    , [2.6931, 0.5339]
    , [1.7111, 0.6679]
    , [0.3672, 1.9248]
    , [1.3927, 2.4470]
    , [0.1220, 1.5958]
    , [1.1489, 3.0533]
    , [2.5319, -0.5293]
    , [1.5318, 1.0227]
    , [0.2403, 1.0953]
    , [1.3480, 2.6145]
    , [0.3022, 1.5013]
    , [3.0193, 0.6762]
    , [-0.7937, 1.5542]
    , [0.3405, 1.6424]
    , [1.7715, 1.1826]
    , [0.1797, -1.0275]
    , [1.0179, 2.0231]
    , [1.6545, -2.4915]
    , [2.2577, 1.1095]
    , [0.0729, -0.5513]
    , [0.8301, 0.6449]
    , [0.5283, 2.3986]
    , [0.8858, 0.4865]
    , [1.3695, 2.9172]
    , [0.3814, 1.7784]
    , [1.8001, 0.7535]
    , [1.4267, 0.0965]
    , [-4.4959, -3.9339]
    , [-3.6255, -4.6379]
    , [-6.3705, -4.8193]
    , [-3.3849, -3.0320]
    , [-3.7205, -3.9697]
    , [-3.1811, -2.6489]
    , [-1.9525, -5.0864]
    , [-4.3237, -3.7597]
    , [-4.9805, -5.0462]
    , [-2.8204, -3.3813]
    , [-3.1054, -2.6950]
    , [-4.1429, -2.9765]
    , [-4.5935, -6.1154]
    , [-3.7511, -3.3183]
    , [-5.1298, -3.9913]
    ]

ans1 :: [Double]
ans1 = 
    [ -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0423
    , -0.0416
    , -0.0416
    , -0.0417
    , -0.0416
    , -0.0417
    , -0.0416
    , -0.0416
    , -0.0416
    , -0.0417
    , -0.0416
    , -0.0416
    , -0.0417
    , -0.0416
    , -0.0417
    , -0.0416
    , -0.0416
    , -0.0416
    , -0.0417
    , -0.0416
    , -0.0417
    , -0.0416
    , -0.0416
    , -0.0416
    , -0.0413
    , -0.0416
    , -0.0415
    , -0.0417
    , -0.0415
    , -0.0416
    , -0.0416
    , -0.0416
    , -0.0417
    , -0.0416
    , -0.0416
    , -0.0416
    , 0.2381
    , 0.2381
    , 0.2382
    , 0.2380
    , 0.2380
    , 0.2379
    , 0.2381
    , 0.2380
    , 0.2381
    , 0.2380
    , 0.2379
    , 0.2380
    , 0.2382
    , 0.2380
    , 0.2381
    ]
