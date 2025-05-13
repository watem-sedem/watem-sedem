=========
Changelog
=========

Version 5.0.3
-------------
- Bugfix when working with utm grids, see https://github.com/watem-sedem/watem-sedem/pull/171
- Add checks on min/max coordinates when using latlon grids, see https://github.com/watem-sedem/watem-sedem/pull/182
- Improving documentation
- Adding more debugging information via adr2line 

Version 5.0.2
-------------
This release was created to auto-publish the binaries for watem-sedem on GitHub.

For windows: watem_sedem.exe
For linux: watem_sedem
For debian/ubuntu linux also an apt package is provided.

Version 5.0.1
-------------
This release contains some small updates to the documentation and all
model extensions are disabled by default in the model code. The user has to
enable them in the ini-file. 

Version 5.0.0
-------------
This release contains a name change from CN-WS to WaTEM/SEDEM (in code watem_sedem) 
in order to follow the initial aim of the KU Leuven for developing WaTEM/SEDEM: 
CN was added in later versions. The CN-module is now refactored to an extension. 
With this release, a distinction is made between the basic WaTEM/SEDEM model 
and the extensions (CN, buffers, forced routing, etc..).

In this version following changes were created:

- Restructuring the *.ini-file:

  - The section "User Choices" was split in two new sections:

    - Options
    - Extensions

  - The section "Variables" was split in two new sections:

    - Parameters
    - Parameters Extensions

  - The section "Output maps" was renamed to "Output"
  - The option "Only WS" was renamed to "Curve Number"

- The executable/build cn_ws was renamed to watem_sedem
- The naming in the folder structure was updated from cn_ws to watem_sedem
- Multiple additions/corrections to the documentation


Version 4.2.3
-------------
- Multiple additions/corrections to the documentation.
- Fix of routing to sewers-bug introduced in CN in version 4.2.0
  (see 6735f759b5029c63a4531e96aa823484722e48b3). Discharge observed in
  segments is now correct again.
- Add GitHub actions.

Version 4.2.2
-------------
- Implements option that allows only cardinal routing to the river. This
  option is usefull to avoid ordinal routing between two grass strip pixels.
  As a consequency, the cardinal routing is a way to maximize impact of grass
  strip pixels in the simulation.

Version 4.2.1
-------------

- Add parcel connectivity grasstrips as a parameter.
- Write 'routing_missing.txt' only when 'write routing table' is enabled in
  the ini-file.
- Disable option to calculate R-factor in CN-WS, referring to the R-factor
  python package: https://watem-sedem.github.io/rfactor/index.html.
- Enable install with cn_ws with conda.
- Several documentation updates, additions and clarifications.
- Refactoring code

version 4.2.0
-------------

- Extending documentation
- Bugfix routing: if both targets have a different land cover than the source
  pixel, and the highest target is a gras strip, than route to the other target.
- Renamed model binaries and folder structure:

    - console version: CN_WSModel > cn_ws
    - gui version: CN_WS > cn_ws_gui
    - LT version: CN_WS_LongTerm > cn_ws_gui_LT

- Moved logic of lpr file to a separate .pas file
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
- Make content of Total Sediment.txt dependent on used options in ini-file
- Correct tillage erosion:
    - sin --> tan in formula Van Oost et al., (2000)
    - use the same routing for tillage erosion as in the water erosion 
- Add SEDTIL_IN and SEDTIL_OUT-files to output
- Bugfix: do not overwrite determined routing for buffer pixels with standard routing
- Bugfix: reduce uparea from buffer outlet pixel when Buffer reduce area is enabled
- Add file rfactor.txt to modeloutput when the rfactor is calculated from a rainfall timeseries
- adjusting sewer routing CN to current sewer routing WS.
- Enable option to input precomputed R-factor for a single event in CN-WS.
- Make CN-WS compatible with SAGA-grids (new option).
- Make integer rasters int32-compatible (requires Freepascal 3.2): this opens the option to run
  larger catchments (with int16, the number of parcels are limited to 32768).
- Correct calculation S-factor Desmet (use tan in stead of arctan)

version 4.1.0
-------------

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

version 4.0.0
-------------

This version includes several changes needed for CNWS to be compatible with the
NEMO model of VMM (see
https://www.vmm.be/water/kwaliteit-waterlopen/waterkwaliteitsmodellen/nemo_tw.pdf/@@download/file/NEMO_TW.pdf
)

- Change type of parcel raster to integer
- Overwrite the system decimal separator to '.' cn_ws can now be used for both a
  float comma and point decimal system
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

version 3.0.0
-------------

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
- Ring checking
- Use function to find lower pixel
- Update routing algorithm (inverse routing)
- Allow to stop routing algorithm in rivers and outlets
- After using 'dedicated' routing in ditch, dam or buffer make sure to use
  'normal' routing algorithm
- Reduce upstream area with trapping efficiency of buffer


version 2.1.0
-------------

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

version 2.0.0
-------------

Version used for calibration with an updated version of the digital elevation
model of Flanders (DHMVII).

- Add calculation of cumulative river sediment
- Don't write empty routing lines in routing.txt, use tab separation
- Use proportion for upstream edges

version 1.4.0
-------------

This version was made for the third steering group of the 'calibration DHMVII
project'.

- remove name of ini-file from files-section in ini-file
- add option 'Buffer reduce area'
- enabling range checking to avoid errors
- PTEF is changed to base-100 (percentage) while it used to be 1-based
- Remove sediment trapping in open water (-5 in parcel map) pixels and assign
  ktc 9999 to those pixels
- bugfix to prevent out of range when calculating adjusted slope
- Change ktc values to float (previously integer) and adapt calibration
  accordingly
- Skip ktc low values higher than ktc high in calibration mode
- Adding Force Routing option
- Don't route cells without lower cells to themselves
- Improved error message when input directory is missing
- Add River Routing option
- Change default value of Create ktc map to True
- Refactoring code
- Numerous model simplifications

version 1.3.0
-------------

This version was made for the second steering group of the 'calibration DHMVII
project'.

- Use adjusted slope calculation in LS calculation
- Restructure code so slope and aspect are calculated once, instead of twice
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
- return non-zero on unsuccessful exit
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
