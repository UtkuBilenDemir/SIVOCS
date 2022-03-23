#%%
import pandas as pd
# %%
snf_df = pd.read_csv("./snf_df_-_unique_project.csv", encoding="utf-8-sig")
# %%

prn_df = pd.read_csv("../00_data/P3_PersonExport.csv", sep=";", encoding="utf-8-sig")
# %%

len(set(prn_df["Person ID SNSF"]))

# %%

len(prn_df["Person ID SNSF"])
# %%
snf_df["First Name"] = " "
snf_df["Last Name"] = " "
for i,x in enumerate(snf_df["ID"]):
    try:
        snf_df.loc[i, "First Name"] = [prn_df.loc[j, "First Name"] for j,y in enumerate(prn_df["Person ID SNSF"]) if y==x]
        snf_df.loc[i, "Last Name"] = [prn_df.loc[j, "Last Name"] for j,y in enumerate(prn_df["Person ID SNSF"]) if y==x]
    except:
        snf_df.loc[i, "First Name"] = [prn_df.loc[j, "First Name"] for j,y in enumerate(prn_df["Person ID SNSF"]) if y==x][0]
        snf_df.loc[i, "Last Name"] = [prn_df.loc[j, "Last Name"] for j,y in enumerate(prn_df["Person ID SNSF"]) if y==x][0]
        print( [prn_df.loc[j, "First Name"] for j,y in enumerate(prn_df["Person ID SNSF"]) if y==x])

# %%

snf_df.to_csv("snf_df_-_fname_lname.csv", encoding='utf-8-sig')

# %%
