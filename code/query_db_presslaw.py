# coding: utf-8

# Replication Files for Arnold, Engst, Gschwend: 
# "Scaling Court Decisions with Citation Networks"
# Query the DB indexed with ElasticSearch
# Chris Arnold, Cardiff Uni
# December 2020


#-- Housekeeping ---------------------------------------------------------------
import pickle
import pandas as pd
from code_helpers.helpers_db_query import get_mlt_documents, from_files_to_db_entries



#-- Querying the DB and Writing the Pickles ------------------------------------


#-- 1 Images in Press -----------------

#-- More Like This Query
df = pd.read_csv(
    filepath_or_buffer='../data/1_original_seed_decisions/presslaw_images_overview.csv',
    sep=';')
titles = df['title'].tolist()
# save courts
courts = pd.DataFrame(df['court'])
courts.to_csv("../data/3_for_estimation/presslaw_images/courts.csv", sep=';')
# Implement query from db
docs = get_mlt_documents(titles)
# write data
pickle_out = open("../data/2_pickles/presslaw_images_mlt.pickle","wb")
pickle.dump(docs, pickle_out)
pickle_out.close()

#-- Exact Query
rslt = from_files_to_db_entries("../data/1_original_seed_decisions/presslaw_images_decisions/")
pickle_out = open("../data/2_pickles/presslaw_images_exact.pickle","wb")
pickle.dump(rslt, pickle_out)
pickle_out.close()



#-- 2 Online Linking -------------------------

#-- More Like This Query
df = pd.read_csv(
    filepath_or_buffer='../data/1_original_seed_decisions/presslaw_online_linking_overview.csv',
    sep=';')
titles = df['title'].tolist()
# save courts
courts = pd.DataFrame(df['court'])
courts.to_csv("../data/3_for_estimation/online_linking_images/courts.csv", sep=';')
# Implement query from db
docs = get_mlt_documents(titles)
# write data
pickle_out = open("../data/2_pickles/presslaw_online_linking_mlt.pickle","wb")
pickle.dump(docs, pickle_out)
pickle_out.close()

#-- Exact Query
rslt = from_files_to_db_entries("../data/1_original_seed_decisions/presslaw_online_linking_decisions/")
pickle_out = open("../data/2_pickles/presslaw_online_linking_exact.pickle","wb")
pickle.dump(rslt, pickle_out)
pickle_out.close()
