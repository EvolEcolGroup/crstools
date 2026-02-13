This is resubmission of a new package following the first submission on 
2026-02-10.

## Test environments
- Github Actions R-CMD-check (ubuntu-20.04): r-devel, r-release, r-oldrel
- Github Actions R-CMD-check (windows): r-release
- Github Actions R-CMD-check (macOS): r-release
- R-hub r-devel: linux, m1-san, macos-arm64, windows

# Results
All tests passed in all environments, with an expected NOTE for a new submission:
> checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Andrea Manica <am315@cam.ac.uk>’
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    georeference (17:43)

"georeference" is a correct technical term.

# Changes following first submission:
- Add a doi for a preprint that described the functionality of the package
- A function with examples was not exported; that was a mistake and it is now
exported.