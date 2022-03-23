#%%
import pandas as pd

#%%
snf_df = pd.read_csv("./snf_df.csv")
# %%

prn_df = pd.read_csv("../00_data/P3_PersonExport.csv", sep=";")
grt_df = pd.read_csv("../00_data/P3_GrantExport_with_abstracts.csv", sep=";")
# %% find genders
snf_df["gender"] = " "
for j,y in enumerate(snf_df["ID"]):
    snf_df.loc[j, "gender"] = [prn_df.loc[i, "Gender"] for i,x in enumerate(prn_df["Person ID SNSF"]) if x == y][0]
# %% number of projects

snf_df["number of projects"] = list(map(len, [x.split(";") for x in snf_df["ProjectNumbers"]]))

# %% find project index in the grantExport

def find_project_ind(df, df_tm, id_col="ProjectNumbers", id_tm_col="Project Number"):
    for i,x in enumerate(df[id_col]):
        ids = [int(q) for q in x.split(";")]

#%%preffered project dictionary
app_dict = {2:0,
            3:0,
            4:0,
            11:1,
            13:1,
            14:1,
            17:1,
            18:0,
            22:1,
            24:0,
            31:1,
            38:0,
            44:1,
            45:0,
            50:1,
            55:0,
            72:0,
            82:0,
            85:0,
            87:1,
            92:0,
            154:0,
            161:1,
            182:1,
            186:0,
            187:0,
            193:0,
            214:1,
            223:0,
            232:0,
            258:0,
            291:1,
            302:0,
            308:0,
            330:1,
            335:1,
            346:1,
            366:1,
            369:1,
            371:0,
            379:1,
            380:1,
            396:2,
            402:2,
            403:1,
            415:1,
            417:1,
            420:2,
            422:2,
            423:1,
            424:2,
            425:1,
            428:0,
            430:0,
            435:1,
            438:0,
            446:0,
            454:1,
            456:1,
            465:1,
            466:0,
            471:1,
            473:1,
            478:1,
            486:1,
            487:0,
            488:0,
            491:0,
            495:1,
            503:0,
            504:0,
            507:0,
            512:0,
            513:0,
            514:0,
            518:1,
            542:0,
            552:1,
            553:1,
            554:1,
            568:2,
            569:1,
            573:0,
            574:0,
            577:3,
            578:2,
            579:0,
            581:1,
            583:0,
            584:1,
            585:1,
            586:1,
            589:0,
            592:1,
            596:2,
            597:0,
            600:0,
            643:0,
            669:0,
            680:0,
            686:0,
            690:1,
            702:1,
            704:0,
            705:1,
            737:1,
            757:0,
            777:0,
            781:0,
            833:1,
            848:1,
            862:1,
            883:0,






            }

#%%
snf_df["Titles"] = ""
snf_df["Disciplines"] = ""
snf_df["Domains"] = ""
snf_df["Enddate"] = ""
snf_df["SP_ProjectNumber"] = ""
snf_df["SP_Title"] = ""
snf_df["SP_Domain"] = ""
snf_df["SP_Discipline"] = ""
snf_df["SP_DisciplineNumber"] = ""
snf_df["SP_Country"] = ""
snf_df["SP_University"] = ""
snf_df["SP_Institution"] = ""
snf_df["SP_Enddate"] = ""
snf_df["SP_Funding"] = ""
snf_df["SP_Amount"] = ""
for i,x in enumerate(snf_df["ProjectNumbers"]):
    ids = [int(q) for q in x.split(";")]    
    temp_disciplines = []
    temp_domains = []
    temp_titles = []
    temp_dates = []
    for id in ids:
        matched_index = [j for j,y in enumerate(grt_df["Project Number"]) if y == id][0]
        print(matched_index)
        #"|".join(snf_df.loc[i, "Disciplines"], grt_df.loc[matched_index, "Discipline Name Hierarchy"])
        print(grt_df.loc[matched_index, "Discipline Name Hierarchy"])
        #snf_df.loc[i, "Disciplines"] = snf_df.loc[i, "Disciplines"] + "|" + grt_df.loc[matched_index, "Discipline Name Hierarchy"]
        #snf_df.loc[i, "Disciplines"] = [snf_df.loc[i, "Disciplines"], grt_df.loc[matched_index, "Discipline Name Hierarchy"]]
        temp_domains.append(grt_df.loc[matched_index, "Discipline Name Hierarchy"])
        temp_disciplines.append(grt_df.loc[matched_index, "Discipline Name"])
        temp_titles.append(grt_df.loc[matched_index, "Project Title"])
        temp_dates.append(grt_df.loc[matched_index, "End Date"])
        if len(ids) == 1:
                snf_df.loc[i, "SP_ProjectNumber"] = snf_df.loc[i, "ProjectNumbers"]
                snf_df.loc[i, "SP_Title"] = grt_df.loc[matched_index, "Project Title"]
                snf_df.loc[i, "SP_Domain"] = grt_df.loc[matched_index, "Discipline Name Hierarchy"]
                snf_df.loc[i, "SP_Discipline"] = grt_df.loc[matched_index, "Discipline Name"]
                snf_df.loc[i, "SP_DisciplineNumber"] = grt_df.loc[matched_index, "Discipline Number"]
                snf_df.loc[i, "SP_Country"] = grt_df.loc[matched_index, "Institution Country"]
                snf_df.loc[i, "SP_University"] = grt_df.loc[matched_index, "University"]
                snf_df.loc[i, "SP_Institution"] = grt_df.loc[matched_index, "Institution"]
                snf_df.loc[i, "SP_Enddate"] = snf_df.loc[i, "Enddate"]
                snf_df.loc[i, "SP_Funding"] = grt_df.loc[matched_index, "Funding Instrument"]
                snf_df.loc[i, "SP_Amount"] = grt_df.loc[matched_index, "Approved Amount"]
        else:
            id = ids[app_dict[i]]
            matched_index = [j for j,y in enumerate(grt_df["Project Number"]) if y == id][0]
            snf_df.loc[i, "SP_ProjectNumber"] = snf_df.loc[i, "ProjectNumbers"].split(";")[app_dict[i]]
            snf_df.loc[i, "SP_Title"] = grt_df.loc[matched_index, "Project Title"]
            snf_df.loc[i, "SP_Domain"] = grt_df.loc[matched_index, "Discipline Name Hierarchy"]
            snf_df.loc[i, "SP_Discipline"] = grt_df.loc[matched_index, "Discipline Name"]
            snf_df.loc[i, "SP_DisciplineNumber"] = grt_df.loc[matched_index, "Discipline Number"]
            snf_df.loc[i, "SP_Country"] = grt_df.loc[matched_index, "Institution Country"]
            snf_df.loc[i, "SP_University"] = grt_df.loc[matched_index, "University"]
            snf_df.loc[i, "SP_Institution"] = grt_df.loc[matched_index, "Institution"]
            snf_df.loc[i, "SP_Enddate"] = grt_df.loc[matched_index, "End Date"]
            snf_df.loc[i, "SP_Funding"] = grt_df.loc[matched_index, "Funding Instrument"]
            snf_df.loc[i, "SP_Amount"] = grt_df.loc[matched_index, "Approved Amount"]



    snf_df.loc[i, "Disciplines"] = " | ".join(temp_disciplines)
    snf_df.loc[i, "Domains"] = " | ".join(temp_domains)
    snf_df.loc[i, "Titles"] = " | ".join(temp_titles)
    snf_df.loc[i, "Enddate"] = " | ".join(temp_dates)
    
# %%


snf_df.to_csv("02_snf_df_-_matched.csv")

# %%
