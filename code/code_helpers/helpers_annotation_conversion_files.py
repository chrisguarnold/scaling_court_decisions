# coding: utf-8

# Replication Files for Arnold, Engst, Gschwend: 
# "Scaling Court Decisions with Citation Networks"
# Converting a ready made document with annotation from the db into .csv outputs
# Chris Arnold, Cardiff Uni
# December 2020


#-- Housekeeping -----------------------------------------------------
import csv
import re
import numpy as np
import itertools
from itertools import compress



#-- Translate Documents into Data  -----------------------------------------------------

def link_id_maker(linkdict):
    '''function that makes a case distinction for all the different db entry types'''
    # case 1
    if "db" in linkdict:
        # First kind a db
        if linkdict['db'] == 'juris-lr':
            linkid_temp = linkdict['abk']
            linkdict['link_id'] = linkid_temp.replace(' ','')
        # Second kind a db
        elif linkdict['db'] == 'juris-n':
            if 'enbez' in linkdict:
                linkid_temp = linkdict['abk'] + linkdict['enbez']
                linkid_temp = re.sub('[§]', 'Par', linkid_temp)
                linkdict['link_id'] = linkid_temp.replace(' ','')
        # third kind a db
        elif linkdict['db'] == 'juris-r':
            if 'az' in linkdict:
                linkid_temp = linkdict['az'] + linkdict['gericht']
            elif 'fundstelle' in linkdict:
                linkid_temp = linkdict['fundstelle']
            linkdict['link_id'] = linkid_temp.replace(' ','')
        # fourth kind a db
        elif linkdict['db'] == 'juris-cg':
            try:
                linkid_temp = linkdict['sw']
                linkdict['link_id'] = linkid_temp.replace(' ','')
            except:
                print("found a missing sw")
                # print(linkdict)
        # fifth kind a db
        elif linkdict['db'] == 'diverse-lr':
            if 'enbez' in linkdict:
                linkid_temp = linkdict['abk'] + linkdict['enbez']
                linkid_temp = re.sub('[§]', 'Par', linkid_temp)
                linkdict['link_id'] = linkid_temp.replace(' ','')
        # sixth kind of db
        elif linkdict['db'] == 'juris-pk':
            linkid_temp = linkdict['sw']
            linkdict['link_id'] = linkid_temp.replace(' ','')
        # sixth kind of db
        elif linkdict['db'] == 'gportal':
            linkid_temp = linkdict['sw']
            linkdict['link_id'] = linkid_temp.replace(' ','')
        # remove whitespace in all subcases
        # linkdict['link_id'] = linkid_temp.replace(' ','')
    # # case 2
    if 'f' in linkdict:
        linkdict['link_id'] = linkdict['sw'].replace(' ','')
    # # case 3
    # if 'fundstelle' in linkdict:
    if 'fundstelle' in linkdict:
        linkdict['link_id'] = linkdict['fundstelle'].replace(' ','')
    # Case 4 nur az (doc 7)
    return(linkdict)


def link_id_maker_for_1_doc(link_dict_list):
    """Loops the link_id_maker over all entries in a document"""
    link_dict_list_w_link_id = [link_id_maker(one_link_dict) for one_link_dict in link_dict_list]
    return(link_dict_list_w_link_id)



# Wrapper function that executes all
def preparing_data(rslt):
    # Generate Aktenzeichen
    aktenzeichen = []
    for i in range(len(rslt)):
        try:
            aktenzeichen.append(rslt[i]["_source"]["aktenzeichen"])
        except:
            aktenzeichen.append("None")
    # Generate Links
    links = []
    for i in range(len(rslt)):
        try:
            links.append(rslt[i]["_source"]["links"])
        except:
            links.append("None")
    links_topic_i = [link_id_maker_for_1_doc(one_doc) for one_doc in links]
    # Table the links to see how many different links in each list
    links_unique = []
    links_all = []
    links_all_joined = []
    length_overall = 0
    for i in range(len(links_topic_i)):
        links_in_one_doc =[]
        for j in range(len(links_topic_i[i])):
            try:
                links_in_one_doc.append(links_topic_i[i][j]['link_id'])
            except:
                pass
        # unique links
        links_in_one_doc_unique = list(set(links_in_one_doc))
        links_in_one_doc_all = list(links_in_one_doc)
        links_unique.append(links_in_one_doc_unique)
        links_all.append(links_in_one_doc_all)
        # for checking
        length_overall += len(links_in_one_doc_unique)
        links_all_joined.extend(links_in_one_doc_unique)
    # Checks
    # print('overall nr of links as added step by step:', length_overall)
    # print('overall unique links', len(set(links_all_joined)))
    # print('They should be different')
    # Denerate oc IDs erstellen
    doc_id = []
    for i in range(len(rslt)):
        try:
            doc_id.append(rslt[i]["_id"])
        except:
            pass
    return aktenzeichen, links, links_unique, links_all, links_all_joined, doc_id


#-- Now Translate into data for stan -------------------------------------------

def link_in_set_checker(links_in_one_doc, one_link_of_set):
    ''' checks whether there is a link in the set for one link of one document'''
    is_it_there = bool(set(links_in_one_doc).intersection([one_link_of_set]))
    return(is_it_there)


# Wrapper
def turn_data_into_csvs_for_stan(gertyp, gerort, spruchkoerper, datum, aktenzeichen, links, links_unique, links_all, links_all_joined, doc_id, data_location):
    # get rid of those that have no links at all
    '''
    Key variables
    ================
    links_unique_cleaned    list of list. Clean list of list with links for each
                            legal decision.

    links_all_joined        a list. Was list of list, but flattened
    links_all               List of list with all links (also multiple times)
                            in a decision.
    '''
    # This filters all decisions for which there is no link at all
    mask_source = [entry != "None" for entry in links]
    links_unique_cleaned = list(itertools.compress(links_unique, mask_source))
    links_all_cleaned = list(itertools.compress(links_all, mask_source)) # wrong
    doc_id_cleaned = list(itertools.compress(doc_id, mask_source))
    # J number of legal documents
    J = len(doc_id_cleaned)
    # K number of different sources overall
    K = len(set(links_all_joined))
    #  N
    N = J*K
    # y[N] dummy if user i follows elite j
    set_of_all_links = set(links_all_joined)
    # y connection matrix
    y = []
    y_count = []
    for i in range(len(links_unique_cleaned)):  # This goes through the dec level!
        links_in_one_doc = links_unique_cleaned[i]
        all_links_in_one_doc = links_all_cleaned[i]
        # check in one document for existence
        y.extend([link_in_set_checker(links_in_one_doc, one_link) for one_link in set_of_all_links])
        # count in one document all occurrences
        y_count.extend([all_links_in_one_doc.count(one_link) for one_link in set_of_all_links])
    y = [int(element) for element in y]
    #  jj[N] verdict/decision for observation y_n
    jj = []
    for i in range(J):
        jj.extend([i]*K)
    #  kk[N]
    kk = [] # legal document/source for observation y_n
    for i in range(J):
        kk.extend(list(range(K)))

    # This filters those legal sources out, that appear only once --------------
    # Step 1: Get a mask
    # y into matrix: Rows nr. of cases, cols nr of sources
    y_matrix = np.asarray(y).reshape(J,K)
    # colsums
    citations = y_matrix.sum(axis=0)
    # colsums > 1 == True
    mask_sources_to_keep_one_decision = (citations > 1).tolist()
    # TF vector * nr of decisions (gets mask to right shape)
    mask_sources_to_keep = mask_sources_to_keep_one_decision*J
    # Step 2: Select with the mask
    y_cit_filter = list(compress(y, mask_sources_to_keep))
    y_count_cit_filter = list(compress(y_count, mask_sources_to_keep))
    set_of_all_links_cit_filter = list(compress(set_of_all_links, mask_sources_to_keep_one_decision))
    K_cit_filter = sum(mask_sources_to_keep_one_decision)
    N_cit_filter = len(y_cit_filter)
    J_cit_filter = int(N_cit_filter/K_cit_filter)
    jj_cit_filter = []
    # re-create counters
    for i in range(J_cit_filter):
        jj_cit_filter.extend([i]*K_cit_filter)
    #  kk[N]
    kk_cit_filter = [] # legal document/source for observation y_n
    for i in range(J_cit_filter):
        kk_cit_filter.extend(list(range(K_cit_filter)))
    # check whether you kicked out a complete case -----------------------------
    y_cit_filter_rowsums = np.asarray(y_cit_filter).reshape(J_cit_filter,K_cit_filter).sum(axis=1)
    assert(all(item > 0 for item in y_cit_filter_rowsums.tolist()))

    # write out as .csv --------------------------------------------------------
    myFile = open(data_location+'J.csv', 'w', newline='')
    with myFile:
        writer = csv.writer(myFile, delimiter = ',')
        writer.writerow([J])

    myFile = open(data_location+'K.csv', 'w', newline='')
    with myFile:
        writer = csv.writer(myFile, delimiter = ',')
        writer.writerow([K])

    myFile = open(data_location+'N.csv', 'w', newline='')
    with myFile:
        writer = csv.writer(myFile, delimiter = ',')
        writer.writerow([N])

    myFile = open(data_location+'jj.csv', 'w', newline='')
    with myFile:
        writer = csv.writer(myFile, delimiter = ',')
        writer.writerows([jj])

    myFile = open(data_location+'kk.csv', 'w', newline='')
    with myFile:
        writer = csv.writer(myFile, delimiter = ',')
        writer.writerows([kk])

    myFile = open(data_location+'y.csv', 'w', newline='')
    with myFile:
        writer = csv.writer(myFile, delimiter = ',')
        writer.writerows([y])

    myFile = open(data_location+'ids.csv', 'w', newline='')
    with myFile:
        writer = csv.writer(myFile, delimiter = ',')
        writer.writerows([doc_id_cleaned])

    myFile = open(data_location+'aktenzeichen.csv', 'w', newline='')
    with myFile:
        writer = csv.writer(myFile, delimiter = ',')
        writer.writerows([aktenzeichen])

    myFile = open(data_location+'gertyp.csv', 'w', newline='')
    with myFile:
        writer = csv.writer(myFile, delimiter = ',')
        writer.writerows([gertyp])

    myFile = open(data_location+'gerort.csv', 'w', newline='', encoding='utf-8')
    with myFile:
        writer = csv.writer(myFile, delimiter = ',')
        writer.writerows([gerort])

    myFile = open(data_location+'spruchkoerper.csv', 'w', newline='', encoding='utf-8')
    with myFile:
        writer = csv.writer(myFile, delimiter = ',')
        writer.writerows([spruchkoerper])

    myFile = open(data_location+'datum.csv', 'w', newline='', encoding='utf-8')
    with myFile:
        writer = csv.writer(myFile, delimiter = ',')
        writer.writerows([datum])

    myFile = open(data_location+'links_in_order.csv', 'w', newline='', encoding='utf-8')
    with myFile:
        writer = csv.writer(myFile, delimiter = ',')
        writer.writerows([set_of_all_links])

    # Writing out citation filtered data
    myFile = open(data_location+'y_cit_filter.csv', 'w', newline='', encoding='utf-8')
    with myFile:
        writer = csv.writer(myFile, delimiter = ',')
        writer.writerows([y_cit_filter])

    myFile = open(data_location+'y_count_cit_filter.csv', 'w', newline='', encoding='utf-8')
    with myFile:
        writer = csv.writer(myFile, delimiter = ',')
        writer.writerows([y_count_cit_filter])

    myFile = open(data_location+'jj_cit_filter.csv', 'w', newline='', encoding='utf-8')
    with myFile:
        writer = csv.writer(myFile, delimiter = ',')
        writer.writerows([jj_cit_filter])

    myFile = open(data_location+'kk_cit_filter.csv', 'w', newline='', encoding='utf-8')
    with myFile:
        writer = csv.writer(myFile, delimiter = ',')
        writer.writerows([kk_cit_filter])

    myFile = open(data_location+'set_of_all_links_cit_filter.csv', 'w', newline='', encoding='utf-8')
    with myFile:
        writer = csv.writer(myFile, delimiter = ',')
        writer.writerows([set_of_all_links_cit_filter])

    myFile = open(data_location+'K_cit_filter.csv', 'w', newline='', encoding='utf-8')
    with myFile:
        writer = csv.writer(myFile, delimiter = ',')
        writer.writerow([K_cit_filter])

    myFile = open(data_location+'N_cit_filter.csv', 'w', newline='', encoding='utf-8')
    with myFile:
        writer = csv.writer(myFile, delimiter = ',')
        writer.writerow([N_cit_filter])

    myFile = open(data_location+'J_cit_filter.csv', 'w', newline='', encoding='utf-8')
    with myFile:
        writer = csv.writer(myFile, delimiter = ',')
        writer.writerow([J_cit_filter])
