# coding: utf-8

# Replication Files for Arnold, Engst, Gschwend: 
# "Scaling Court Decisions with Citation Networks"
# Convert Into Data for Estimation
# Chris Arnold, Cardiff Uni
# December 2020

#-- Housekeeping -----------------------------------------------------
import pickle
import pandas as pd
from code_helpers.helpers_meta_data import read_from_pickle_write_to_csv



#-- 1 Images in Press -----------------
read_from_pickle_write_to_csv(
    "../data/2_pickles/presslaw_images_mlt.pickle",
    "../data/3_for_estimation/presslaw_images/mlt/")

read_from_pickle_write_to_csv(
    "../data/2_pickles/presslaw_images_exact.pickle",
    "../data/3_for_estimation/presslaw_images/exact/")


#-- 2 Online Linking -------------------------
read_from_pickle_write_to_csv(
    "../data/2_pickles/presslaw_online_linking_mlt.pickle",
    "../data/3_for_estimation/presslaw_online_linking/mlt/")

read_from_pickle_write_to_csv(
    "../data/2_pickles/presslaw_online_linking_exact.pickle",
    "../data/3_for_estimation/presslaw_online_linking/exact/")
