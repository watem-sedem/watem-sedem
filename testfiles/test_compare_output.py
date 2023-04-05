from pathlib import Path

from test_benchmark import _compare_folder, _get_filenames, _compare_rst_folder, equal_table, equal_total_sediment


# not feasible as we don't have subdirs
#_compare_folder(Path('testfiles/molenbeek/modeloutput_ref'), Path('testfiles/molenbeek/modeloutput'))


# check 'modeloutput' files

def test_modeloutput():
    # test saga output
    refoutput = Path("testfiles/molenbeek/modeloutput_ref_sdat")
    output = Path("testfiles/molenbeek/modeloutput_sdat")

    assert _get_filenames(refoutput) == _get_filenames(output)
    _compare_rst_folder(refoutput, output, ext='.sdat')
    refoutput = Path("testfiles/molenbeek/modeloutput_ref")
    output = Path("testfiles/molenbeek/modeloutput")

    assert _get_filenames(refoutput) == _get_filenames(output)
    _compare_rst_folder(refoutput, output, ext='.rst')



def test_modelinput_ref():
    """This tests the file under modelinput_ref

    The two files under modelinput_ref are two files
    that are actually being written by the model,
    so these should be checked as well.
    """
    refoutput = Path("testfiles/molenbeek/modelinput_ref")
    output = Path("testfiles/molenbeek/modelinput")

    assert _get_filenames(refoutput).issubset(_get_filenames(output))
    _compare_rst_folder(refoutput, output, ext='.rst')

    refoutput = Path("testfiles/molenbeek/modelinput_ref_sdat")
    output = Path("testfiles/molenbeek/modelinput_sdat")

    assert _get_filenames(refoutput).issubset(_get_filenames(output))
    _compare_rst_folder(refoutput, output, ext='.sdat')

def test_tables():
    refoutputs = [Path("testfiles/molenbeek/modeloutput_ref"),
            Path("testfiles/molenbeek/modeloutput_ref_sdat")]
    output = Path("testfiles/molenbeek/modeloutput")
    for refoutput in refoutputs:
        for file_name in refoutput.glob("*.txt"):
            if file_name.name == "Total sediment.txt":
                equal_total_sediment(
                    file_name,
                    (output / file_name.name),
                    rtol=1e-8,
                    atol=1e-8,
                )
            elif file_name.name == "Total sediment segment.txt":
                equal_table(
                    file_name,
                    (output / file_name.name),
                    rtol=1e-8,
                    atol=1e-8,
                    skiplines=1,
                )
            elif file_name.name == "calibration.txt":
                print("don't compare calibration file")
            else:
                equal_table(
                    file_name,
                    (output / file_name.name),
                    rtol=1e-8,
                    atol=1e-8,
                    skiplines=0,
                )
