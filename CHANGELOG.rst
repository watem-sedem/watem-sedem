=========
Changelog
=========

latest
------

- Extending documentation
- Bugfix routing: if no lower pixel found in parcel, no river pixel in window W,
  check if a lower pixel exists with other land use code.
- Renamed model binaries and folder structure:

    - console version: CN_WSModel > cn_ws
    - gui version: CN_WS > cn_ws_gui
    - LT verison: CN_WS_LongTerm > cn_ws_gui_LT

- Moved logic of lpr file to a seperate .pas file
- Remove spurious warnings when building the binary
- remove VHA tag in files:

    - The option "Output per VHA river segment" became "Output per river segment"
    - Discharge_VHA.txt was renamed to Discharge_segments.txt
    - Total sediment VHA.txt was renamed to Total sediment segments.txt
    - Cumulative sediment VHA.txt was renamed to Cumulative sediment segments.txt
    - Sediment_VHA.txt was renamed to Sediment_segments.txt
    - Sediment concentration_VHA.txt was renamed to Sediment concentration segments.txt
    - Clay content sediment VHA.txt was renamed to Clay content sediment segments.txt
- Enable the output per segment option by default when river routing is used

NEMO final
----------

This version was delivered to VMM at the end of the NEMO project. It includes
some small improvements and a fix for a bug discovered by Jan Coppens (Thanks!)

- Allow relative paths in ini-file
- Simplify logic in CN_WSmodel.lpr
- Make bufferparameters 'Volume', 'Height Dam', 'Height opening', 'Opening area',
  'Discharge coefficient', 'Width dam' only mandatory in ini-file when using the
  CN module
- Bugfix: Set Target2 to -99 when Part2 is 0 (as is done for Target1)
- Added TC formula of Verstraeten et al 2007 as an option
- Bugfix: Renaming SModel Desmet1996 to McCool1987
- Extending documentation

NEMO
----

This version includes several changes needed for the NEMO project

- Change type of parcel raster to integer
- Overwrite the system decimal seperator to '.'
- Add documentation with sphinx
- Don't write Total Sediment.txt when calibrating
- Bugfix: read outlet raster when 'Manual outlet selection' is enabled
- Improving memory management (releasing memory of rasters when they are not used anymore)
- Change buffer extension ID to buffer_id + 16384
- Routing: do not jump to border pixels of raster in findlower function
- Removing 'Sediment trapped in open water' from Total sediment.txt
- Round sewer input in Total sediment.txt to two decimals
- Cleaning code: removing initialisation of unused variables
- Determine outlet after routing when 'Manual outlet selection' is enabled
- Refactoring: rename variable FLUX_IN to UpArea
- Use sewermap to reduce uparea
- Add optional LS correction factor

version 20200114
----------------

- do not route sewers to rivers
- reduce sediout of sewer pixel with fraction defined in sewermap
- write capacity to raster
- removing outputfile 'Sewer output sediment.txt'
- Adding 'Sediment entering sewer system' to Total sediment.txt
- Bugfix: typo in variable name (2nd target vs 1st target)
- If Target is -99 set distance to 0 in routing table
- Change option 'Include tillage' to 'Include tillage direction'
- Make calculation of tillage erosion optional

- Change Fluxout from RRaster to single value
-

- Ring checking?
- Use function to find lower pixel
- Update routing algorithm (inverse routing)

- Allow to stop routing algorithm in rivers and outlets
- After using 'dedicated' routing in ditch, dam or buffer make sure to use
  'normal' routing algorithm
- Reduce upstream area with trapping efficiency of buffer


version 2.1.0 (2019109)
-----------------------

- Add option 'Only Routing'
- Make routing table an optional output
- Endtime model variable is now only mandatory in ini-file when needed
- Add check to make sure number of river segments in rasters and files are
  the same
- Set the ktc value of a dam to ktc_low instead of 7
- Flow to neighbour river cell even if that cell is higher as the origin
- Make sure that sediment transport uses the same routing as upstream area
- Fix calculation of distance between source and target (wrong in peculiar cases)
- Use river direction as input for river routing
- Write sewer export to rasterfile
- Add files to build a package on debian linux (for deploy on notebook server)

TO DO: finish the changelog for this release
 (done up to commit c11799ddc5f69e356d59ed5b7d3070e2a94813f3)

version 2.0.0
-------------

Version used for calibration with DHMVII.

- Add calculation of cumulative river sediment
- Don't write empty routing lines in routing.txt, use tab seperation
- Use proportion for upstream edges

version 1.4.0
-------------

This version was made for the third steering group of the 'calibration DHMVII
project'.

- remove name of ini-file from files-section in ini-file
- simplify code
- add option 'Buffer reduce area'
- enabling range checking to avoid errors
- PTEF is 100-based
- Remove sediment trapping in open water (-5 in parcel map) pixels and assign
  ktc 9999 to those pixels
- bugfix to prevent out of range when calculating adjusted slope
- Change ktc values to float (previously integer) and adapt calibration
  accordingly
- Skip ktc low values higher than ktc high in calibration mode
- Adding Force Routing option
- Don't route cells without lower cells to themselve
- Improved error message when input directory is missing
- Add River Routing option
- Change default value of Create ktc map to True
- Refactoring code

version 1.3.0
-------------

This version was made for the second steering group of the 'calibration DHMVII
project'.

- Use adjusted slope calculation in LS calculation
- Don't calculate slope and aspect twice
- Add -9999 as no data value in all output rasters
- Improve memory allocation for rasters
- Refactoring code to read idrisi rasters

version 1.2.0
-------------

This version was made for the first steering group of the 'calibration DHVMII
project'.

- raise exception when ini-file does not exist
- make key words in ini-file of LT version the same as in other version
- bugfix: distance calculation near buffers
- write routing table as output of console version
- raise exception if no outlet is present in outletmap
- add search radius to ini-file (make it a variable, not a constant)
- add calibration method (loop over all combinations of ktc low and ktc high in
  a certain amount of steps and write output to calibration.txt)
- add functionality to use different L (Desmet1996_Vanoost2003 and
  Desmet1996_McCool) and S models (Desmet1996, Nearing1997)
- return non-zero on unsuccesful exit
- refactoring and cleaning code
- amount of sediment at outlet is now calculated as sum of incoming sediment
  in the segment that contains the outlet

version 1.1.0
-------------

- three code bases (gui, console and long-term version) are merged to a
  common code base
- add function to write routing table
- correct distance calculation
- improved exception handling
- cleaning code

Version 1.0.0
-------------

Initial version of CN-WS, as developed at KU Leuven.
