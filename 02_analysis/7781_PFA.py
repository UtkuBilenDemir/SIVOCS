# %% Import PFA
from utils.principal_feature_analysis import PFA
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.decomposition import PCA
import numpy as np

# %%
fata_num = pd.read_csv("../01_data/fata_num_questions.csv")
fata_num = fata_num.drop(fata_num.columns[0], axis=1)

# %% Get rid of nas
fata_num_o = fata_num.fillna(0)

# %% Look for the optimal number of features with regular PCA
pca_model = PCA(n_components=len(fata_num_o.columns))
pca_model.fit_transform(fata_num_o)


plt.figure(figsize=(16, 10))
plt.title('Erklärte Varianz (kumulativ) mit Respekt auf die verwendeten Hauptkomponenten')
plt.plot(pca_model.explained_variance_ratio_.cumsum())
plt.xticks(np.arange(0, 20, step=2))

# %%
list(enumerate(pca_model.explained_variance_ratio_.cumsum()))

# %%
pfa_model = PFA(n_features=25)
pfa_model.fit(fata_num_o)
x = pfa_model.features_
pfa_model.indices_
x[1]
fata_num_o.columns[pfa_model.indices_]
#%%
pfa_model.features_[0]
# %% Why not the same features?
# PFA delivers always different answers????
feature_list = []
for i in range(1000):
    pfa_model = PFA(n_features=20)
    pfa_model.fit(fata_num_o)
    x = pfa_model.features_
    pfa_model.indices_
    feature_list.append(fata_num_o.columns[pfa_model.indices_])
    
# %%
feature_list = [list(x) for x in feature_list]

# %%
feature_list = sum(feature_list, [])

# %% 
unique, counts = np.unique(feature_list, return_counts=True)

# %%
freq_features = list(zip(unique, counts))

# %%
freq_features = [freq_features[i] for i in np.argsort(counts)[::-1]]

# %% OUT 1
freq_features[-10:-1]


# %% OUT 2

freq_features


# %%
freq_features = pd.DataFrame(freq_features)
freq_features.to_csv("PFA_feature_importance.csv")

# --------------- CONTINUE in R, remove features and do FA


# %% 



# %%
pfa_model_40 = PFA(n_features=40)
pfa_model_40.fit(fata_num_o)

fata_num_o.columns[pfa_model_40.indices_]
# %%
pfa_model_10 = PFA(n_features=10)
pfa_model_10.fit(fata_num_o)

fata_num_o.columns[pfa_model_10.indices_]

# %% Let's try factor analysis one more time
from factor_analyzer import FactorAnalyzer
# %% Barlett
df = fata_num_o
from factor_analyzer.factor_analyzer import calculate_bartlett_sphericity
chi_square_value,p_value=calculate_bartlett_sphericity(df)
chi_square_value, p_value


# %% KMO 
from factor_analyzer.factor_analyzer import calculate_kmo
kmo_all,kmo_model=calculate_kmo(df)
kmo_model

# %% Kaiser Criterion
fa = FactorAnalyzer()
fa.fit(df)

# Check Eigenvalues
ev, v = fa.get_eigenvalues()
kaiser = sum(ev > 1)
print(f"Due to Kaiser Criterion the optimal number of factors is {kaiser}")

# %% Create scree plot using matplotlib
plt.scatter(range(1,df.shape[1]+1),ev)
plt.plot(range(1,df.shape[1]+1),ev)
plt.axhline(y = 1, color = 'r', linestyle = '-')
plt.title('Scree Plot with Kaiser')
plt.xlabel('Factors')
plt.ylabel('Eigenvalue')
plt.grid()
plt.show()

# %% Create factor analysis object and perform factor analysis
fa15 = FactorAnalyzer(15, rotation="varimax")
fa15.fit_transform(df)
fa15.loadings_

# %%
fa15.loadings_
# %%
fa15.get_factor_variance()

# %%
fa15.get_uniquenesses()

# %%
fa15.get_communalities().mean()
# %% Create factor analysis object and perform factor analysis
fa25 = FactorAnalyzer(25, rotation="varimax")
fa25.fit_transform(df)
fa25.loadings_

# %%
fa25.loadings_
# %%
fa25.get_factor_variance()

# %%
fa25.get_uniquenesses()

# %%
fa25.get_communalities().mean()
# %%

list(zip(df.columns, fa25.get_communalities()))
# %%

fata_num_o.columns[pfa_model_40.indices_]
# %%
