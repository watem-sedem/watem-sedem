from pathlib import Path

from test_benchmark import _compare_folder, _get_filenames, _compare_rst_folder


# not feasible as we don't have subdirs
#_compare_folder(Path('testfiles/molenbeek/modeloutput_ref'), Path('testfiles/molenbeek/modeloutput'))


# check 'modeloutput' files

def test_modeloutput():
    refoutput = Path("testfiles/molenbeek/modeloutput_ref")
    output = Path("testfiles/molenbeek/modeloutput")

    assert _get_filenames(refoutput) == _get_filenames(output)
    _compare_rst_folder(refoutput, output)


def test_modelinput_ref():
    """This tests the file under modelinput_ref

    The two files under modelinput_ref are two files
    that are actually being written by the model,
    so these should be checked as well.
    """
    refoutput = Path("testfiles/molenbeek/modelinput_ref")
    output = Path("testfiles/molenbeek/modelinput")

    assert _get_filenames(refoutput).issubset(_get_filenames(output))
    _compare_rst_folder(refoutput, output)
