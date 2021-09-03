# coding: utf-8

# Replication Files for Arnold, Engst, Gschwend: 
# "Scaling Court Decisions with Citation Networks"
# Help Managing Meta Data
# Chris Arnold, Cardiff Uni
# December 2020

 #-- Housekeeping --------------------------------------------------------------
import xml.etree.ElementTree as ET
import pickle
from code_helpers.helpers_annotation_conversion_files import turn_data_into_csvs_for_stan, preparing_data


#-- Extact XML Information -----------------------------------------------------

# extracting information
def meta_data_from_xml_extractor(one_raw_xml_str):
    """Takes one xml file as raw string as inputself.
    Returns list of variables extracted from the meta data
    """
    # parse the xml
    root = ET.fromstring(one_raw_xml_str)
    # read from the xml
    for quelle in root.iter('quelle'):
        for gericht in quelle.iter('gericht'):
            try:
                gertyp = gericht.find('gertyp').text
            except:
                gertyp = 'NaN'
            try:
                gerort = gericht.find('gerort').text
            except:
                gerort = 'NaN'
            try:
                spruchkoerper = quelle.find('spruchkoerper').text
            except:
                spruchkoerper = 'NaN'
        datum = quelle.find('datum').text
        aktenzeichen = quelle.find('aktenzeichen').text
    # prep output
    metadata = [gertyp, gerort, spruchkoerper, datum, aktenzeichen]
    return(metadata)



def meta_data_from_several_raw_xml_extractor(input_collection):
    """Loops over a complete selection of xml Documents
    Returns list of list with variables extracted from the meta data"""
    input_collection_meta_data_temp = []
    for i in range(len(input_collection)):
        # this catches cases where the DB does not find queries:
        try:
            xml_temp = input_collection[i]["_source"]["raw"]
            one_meta_data = meta_data_from_xml_extractor(xml_temp)
            input_collection_meta_data_temp.append(one_meta_data)
        except:
            how_long_is_previous_input = len(input_collection_meta_data_temp[i-1])
            input_collection_meta_data_temp.append(['NaN']*how_long_is_previous_input)
    input_collection_meta_data = list(map(list, zip(*input_collection_meta_data_temp)))
    return(input_collection_meta_data)


def read_from_pickle_write_to_csv(pickle_file, data_location, nr_of_decisions = 25):
    # load data
    pickle_in = open(pickle_file,"rb")
    doc_temp = pickle.load(pickle_in)
    # prevent from too many observations in the mlt query
    if len(doc_temp) > nr_of_decisions:
        doc = doc_temp[1:nr_of_decisions]
    else:
        doc = doc_temp
    # meta data from raw xml
    meta_data = meta_data_from_several_raw_xml_extractor(doc)
    gertyp = meta_data[0]
    gerort = meta_data[1]
    spruchkoerper = meta_data[2]
    datum = meta_data[3]
    aktenzeichen = meta_data[4]
    # meta data from parsed xml
    aktenzeichen_bad, links, links_unique, links_all, links_all_joined, doc_id = preparing_data(doc)
    # write out as .csv
    turn_data_into_csvs_for_stan(
        gertyp, gerort, spruchkoerper, datum, aktenzeichen,
        links, links_unique, links_all, links_all_joined, doc_id, data_location)
