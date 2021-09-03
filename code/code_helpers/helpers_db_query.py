# coding: utf-8

# Replication Files for Arnold, Engst, Gschwend: 
# "Scaling Court Decisions with Citation Networks"
# Chris Arnold, Cardiff Uni
# December 2020

#-- Housekeeping ---------------------------------------------------------------
# Libraries
import os
from elasticsearch import Elasticsearch, helpers

# Setup DB Access. Anonymized for Privacy Reasons
es = Elasticsearch(hosts=[{'host': foo, 'port': bar}])


#-- 1 Exact Queries ------------------------------------------------------------
def get_documents(input_docs):
    query = {"docs": [{"_id": input_doc} for input_doc in input_docs]}
    rslt = es.mget(
        body={'ids': input_docs},
        _source_include=["cleaned_text", "location", "raw",
            "keywords", "links", "aktenzeichen", "fundstelle"],
        index = 'rechtsprechung',
        doc_type = 'rechtsprechung')
    return rslt


def from_files_to_db_entries(path_of_case_files):
    # Reading in files
    all_files = os.listdir(path_of_case_files)
    txt_files = list(filter(lambda x: x[-4:] == '.txt', all_files))
    txt_files_relative = [path_of_case_files + file for file in txt_files]
    print("read the files")
    # retrieve ids from file names
    ids_to_look_up = []
    for i in range(len(txt_files_relative)):
        ids_to_look_up.append(txt_files_relative[i][-17:-4])
    print("there are the following nr of IDs:",len(ids_to_look_up))
    print("These are the IDs", ids_to_look_up)
    # Query documents
    docs = get_documents(ids_to_look_up)
    # rough error catch
    return(docs["docs"])



#-- 2 More Like This Queries ---------------------------------------------------
# formulate query
def build_mlt_query(input_titles):
    '''takes list of input titles'''
    return {
        "query": {
            "more_like_this" : {
                "fields" : ["cleaned_text"],
                "like" : [input_title for input_title in input_titles],
                "min_term_freq" : 1,
                "max_query_terms" : 12
            }
        }
    }


# more like this query
def get_mlt_documents(input_titles):
    query = build_mlt_query(input_titles)
    rslt = es.search(body=query,
                         doc_type='rechtsprechung',
                         index='rechtsprechung',
                         size=500,
                         _source_include=["cleaned_text", "location", "raw",
                            "keywords", "links", "aktenzeichen", "fundstelle"])
    return rslt['hits']['hits']
