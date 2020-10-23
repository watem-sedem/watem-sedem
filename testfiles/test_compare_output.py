from pathlib import Path

from test_benchmark import _compare_folder, _get_filenames, _compare_rst_folder


# not feasible as we don't have subdirs
#_compare_folder(Path('testfiles/molenbeek/modeloutput_ref'), Path('testfiles/molenbeek/modeloutput'))


# check 'modeloutput' files

def test_all():
    refoutput = Path("testfiles/molenbeek/modeloutput_ref")
    output = Path("testfiles/molenbeek/modeloutput")

    assert _get_filenames(refoutput) == _get_filenames(output)

    _compare_rst_folder(refoutput, output)
