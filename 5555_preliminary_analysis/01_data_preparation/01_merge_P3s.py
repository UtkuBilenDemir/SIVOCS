#%%
import pandas as pd
import altair as alt
# %%
snf_df = pd.read_csv("../00_data/zsi_sample_contacts_final.csv", encoding = 'iso-8859-1')
#snf_df.columns.values[0] = "ID"
snf_df.rename(columns={'ResponsibleApplicantNumber':'ID'}, inplace=True)

# %%
prn_df = pd.read_csv("../00_data/P3_PersonExport.csv", sep=";", encoding='utf-8-sig')




#%%
prn_df["Person ID SNSF"] 

#%%
[i for i, j in zip(list(snf_df["ID"]), list(prn_df["Person ID SNSF"])) if i == j]

#%% Create the necessary columns
snf_df["pr_responsible_applicant"] = " "
snf_df["pr_applicant"] = " "
snf_df["First Name"] = " "
snf_df["Last Name"] = " "

#%%

for i,id in enumerate(snf_df["ID"]):
    match_index = [j for j,y in enumerate(prn_df["Person ID SNSF"]) if y==id]
    print(match_index)
    """Multiple entries == multiple orgs (projects are identical)"""
    if len(match_index) > 1:
        print(f"id is {id}, length is {len(match_index)}")
        snf_df.loc[i, "pr_responsible_applicant"] = ";".join(map(str, list(set(prn_df.loc[match_index, "Projects as responsible Applicant"]))))
        snf_df.loc[i, "pr_applicant"] = ";".join(map(str, list(set(prn_df.loc[match_index, "Projects as Applicant"]))))
        snf_df.loc[i, "First Name"] = str(list(prn_df.loc[match_index[0], "First Name"])[0])
        snf_df.loc[i, "Last Name"] = str(list(prn_df.loc[match_index[0], "Last Name"])[0])

    else:
        """Add the projects into the new dataset"""
        snf_df.loc[i, "pr_responsible_applicant"] = ";".join(map(str, list(prn_df.loc[match_index, "Projects as responsible Applicant"])))
        snf_df.loc[i, "pr_applicant"] = ";".join(map(str, list(prn_df.loc[match_index, "Projects as Applicant"])))
        snf_df.loc[i, "First Name"] = str(list(prn_df.loc[match_index, "First Name"])[0])
        snf_df.loc[i, "Last Name"] = str(list(prn_df.loc[match_index, "Last Name"])[0])



#%% calculate the numbers of each

number_of_provided_projects = [len(x.split(";")) for x in snf_df["ProjectNumbers"]]
number_of_associated_projects = [len(x.split(";")) for x in snf_df["pr_responsible_applicant"]]

#%%

provided = pd.DataFrame(pd.Series(number_of_provided_projects).value_counts())
associated = pd.DataFrame(pd.Series(number_of_associated_projects).value_counts())
#%%
provided["number_of_projects"] = list(provided.index.values)
provided["freq."] = list(provided[0])


associated["number_of_projects"] = list(associated.index.values)
associated["freq."] = list(associated[0])
associated = associated.reset_index(drop=True)
associated = associated.sort_values("number_of_projects")
#%%
provided = provided.iloc[:,1:]
associated = associated.iloc[:,1:]
provided["number_of_projects"] = [str(x) for x in list(provided["number_of_projects"])]
associated["number_of_projects"] = [str(x) for x in list(associated["number_of_projects"])]
provided["freq."] = list(provided["freq."])
associated["freq."] = list(associated["freq."])
#%%
import plotly.express as px
fig = px.bar(provided, x='number_of_projects', y='freq.', text="freq.", title="Projects provided by SNSF")
fig.update_layout(
    xaxis = dict(
        tickmode = 'linear',
        tick0 = 0,
        dtick = 1
    )
)
fig.show("browser")

#%%

import plotly.express as px
fig = px.bar(associated, x='number_of_projects', y='freq.', text="freq.", title="Associated projects")
fig.update_layout(
    xaxis = dict(
        tickmode = 'linear',
        tick0 = 0,
        dtick = 1
    )
)

fig.update_traces(marker_color='red')
fig.show("browser")

#%%

snf_df.to_csv("snf_df.csv", encoding='utf-8-sig')

snf_df.to_excel('test.xlsx', encoding='utf-8-sig')

#%% TESTS
len(set(snf_df["ResponsibleApplicantNumber"]))


#%%
# %%
len(set(prn_df["Person ID SNSF"]))
# %%

# %%

# %%
