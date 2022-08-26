import pytest

import os
from pathlib import Path

import numpy as np
import pandas as pd
import rasterio

def load_total_sediment_file(txt_total_sediment_file):
    """Load the total sediment file of CNWS written in CNWS dict_output map
    Parameters
    ----------
    txt_total_sediment_file: str or pathlib.Path
        File Path of the sediment file
    Returns
    -------
    dict_output: dict
        The dict_output sediment data dictionary contains the following data:
        - *erosion* (float): total amount of erosion (kg)
        - *deposition* (float): total amount of deposition (kg)
        - *river* (float): total amount of sediment run-off to the river (kg)
        - *outside_domain* (float): total amount of sediment run-off out of the \
        catchment (kg)
        - *buffers* (float): total amount of sediment trapped in buffers (kg)
        - *endpoints* (float): total sewer of sediment trapped in sewers and \
        ditches (kg)
    """
    file = open(txt_total_sediment_file, "r")
    Lines = file.readlines()
    dict_output = {}
    for line in Lines:
        line = line.split(" ")
        # hardcode
        tag = " ".join(line[:-2])
        if tag == "Total erosion:":
            dict_output["erosion"] = float(line[-2])
        elif tag == "Total deposition:":
            dict_output["deposition"] = float(line[-2])
        elif tag == "Sediment leaving the catchment, via the river:":
            dict_output["river"] = float(line[-2])
        elif tag == "Sediment leaving the catchment, not via the river:":
            dict_output["outside_domain"] = float(line[-2])
        elif tag == "Sediment trapped in buffers:":
            dict_output["buffers"] = float(line[-2])
        elif tag == "Sediment entering sewer system:":
            dict_output["endpoints"] = float(line[-2])
    return dict_output

def load_raster(rst, return_bounds=False):
    """load raster with rasterio
    Parameters
    ----------
    rst: str or pathlib.Path
        File path of the file, .rst arr.
    return_bounds: bool, default False
        Flag to indicate whether a bounds of the arr should be returned.
    Returns
    -------
    arr: numpy.ndarray
        Array format of raster file.
    bounds: list
        List of bounds (xmin,ymin,xmax,ymax).
    profile: rasterio.profiles
        See :class:`rasterio.profiles`
    """
    # load
    try:
        with rasterio.open(rst) as src:
            arr = src.read()[0]
            profile = src.profile
            if return_bounds:
                bounds = src.bounds
    except rasterio.errors.RasterioIOError as e:
        logger.error(e)
        msg = f"could not open {rst}"
        raise Exception(msg)
    if return_bounds:
        return arr, profile, bounds
    else:
        return arr, profile


def _get_filenames(path):
    """"""
    return set(
        [item.name for item in path.iterdir() if ".aux.xml" not in item.name]
    )

def equal_rst(rst_file_1, rst_file_2, rtol=1e-8, atol=1e-8):
    """Check if idrisi/sdat files are the same"""
    s1, s2 = load_raster(rst_file_1), load_raster(rst_file_2)
    assert np.allclose(s1[0], s2[0], rtol=rtol, atol=atol, equal_nan=True), \
            "file %s and %s differ" % (rst_file_1, rst_file_2)

def equal_table(tdbl_file_1, tdbl_file_2, rtol=1e-8, atol=1e-8, skiplines=0):
    """Check if table data are the same"""
    tbl_1 = pd.read_csv(tdbl_file_1, skiprows=skiplines, delimiter="\t")
    tbl_2 = pd.read_csv(tdbl_file_2, skiprows=skiplines, delimiter="\t")
    pd.testing.assert_frame_equal(tbl_1, tbl_2, rtol=rtol, atol=atol)

def equal_total_sediment(sed_file_1, sed_file_2, rtol=1e-8, atol=1e-8):
    """Check if total sediment output data of two files are the same"""
    sed1 = load_total_sediment_file(sed_file_1)
    sed2 = load_total_sediment_file(sed_file_2)
    for val1, val2 in zip(sed1.values(), sed2.values()):
        assert np.allclose(val1, val2, rtol=rtol, atol=atol)

def _compare_folder(
    folder_benchmark,
    folder_output,
    year="2018",
    name="molenbeek",
    scenario="scenario_1",
):
    """Compare the setup of the folder/file structure

    Notes
    -----
    The filecmp, https://docs.python.org/3/library/filecmp.html, is not used as the file
    content can differ with a given precision on value-level. This functions checks
    which files/folders, specific functions are defined to test the data content.
    """
    # Check main folder is user defined name
    assert _get_subdir_names(folder_benchmark) == _get_subdir_names(folder_output)
    assert _get_subdir_names(folder_output) == {name}

    # Check data and catchment subfolders
    assert _get_subdir_names(folder_benchmark / name).issuperset(
        _get_subdir_names(folder_output / name)
    )
    assert {"Data_Bekken", scenario}.issubset(_get_subdir_names(folder_output / name))

    # Check scenario subfolders
    assert _get_subdir_names(folder_output / name / scenario) == {
        year,
        "modelinput",
        "modeloutput",
        "postprocessing",
    }

    # check 'modelinput' files
    assert _get_filenames(
        folder_benchmark / name / scenario / "modelinput"
    ) == _get_filenames(folder_output / name / scenario / "modelinput")

    # check 'modeloutput' files
    assert _get_filenames(
        folder_benchmark / name / scenario / "modelinput"
    ) == _get_filenames(folder_output / name / scenario / "modelinput")

    # check 'Data_bekken' files
    assert _get_filenames(folder_benchmark / name / "Data_Bekken") == _get_filenames(
        folder_output / name / "Data_Bekken"
    )

    # check 'year' files
    assert _get_filenames(folder_benchmark / name / scenario / year) == _get_filenames(
        folder_output / name / scenario / year
    )


def _compare_rst_folder(ref, new, ext='.rst'):
    """ Compare RST files in two folders
    """
    for file_name in ref.glob(f"*{ext}"):

        # adjust the numerical tolerance as function of the data; cumulative
        # data need to have more relative tolerance
        # TODO sgobeyn/daanr - revise these categories
        if file_name.name in [
            f"WATEREROS (mm per gridcel){ext}",
            f"RUSLE{ext}",
            f"Capacity{ext}",
            f"SediExport_kg{ext}",
            f"SediIn_kg{ext}",
            f"SLOPE{ext}",
        ]:
            rtol, atol = 1e-5, 1e-3
        elif file_name.name in [f"WATEREROS (kg per gridcel){ext}"]:
            rtol, atol = 1e-4, 1e-3
        elif file_name.name in [
            f"LS{ext}",
            f"AspectMap{ext}",
            f"sewer_in{ext}",
            f"cumulative{ext}",
            f"UPAREA{ext}",
            f"SediOut_kg{ext}",
        ]:
            rtol, atol = 1e-5, 1e-8
        else:
            rtol, atol = 1e-8, 1e-8
        equal_rst(
            file_name,
            (new / file_name.name),
            rtol=rtol,
            atol=atol,
        )


def _compare_rst(
    folder_benchmark,
    folder_output,
    year="2018",
    name="molenbeek",
    scenario="scenario_1",
    ext='.rst'
):
    """Compare RST files for a scenario"""
    """rst files in input and outfolder should be the same"""
    for folder in ['modelinput', 'modeloutput']:
        _compare_rst_folder( folder_benchmark / name / scenario / folder,
                folder_output / name / scenario / folder,
                ext)


def _compare_table(
    folder_benchmark,
    folder_output,
    year="2018",
    name="molenbeek",
    scenario="scenario_1",
):
    """rst files in input and outfolder should be the same"""
    for folder in ["modelinput", "modeloutput"]:
        for file_name in (folder_benchmark / name / scenario / folder).glob("*.txt"):
            if file_name.name == "Total sediment.txt":
                equal_total_sediment(
                    file_name,
                    (folder_output / name / scenario / folder / file_name.name),
                    rtol=1e-8,
                    atol=1e-8,
                )
            elif file_name.name == "Total sediment VHA.txt":
                equal_table(
                    file_name,
                    (folder_output / name / scenario / folder / file_name.name),
                    rtol=1e-8,
                    atol=1e-8,
                    skiplines=1,
                )
            else:
                equal_table(
                    file_name,
                    (folder_output / name / scenario / folder / file_name.name),
                    rtol=1e-8,
                    atol=1e-8,
                    skiplines=0,
                )


