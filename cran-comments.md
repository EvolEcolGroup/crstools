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
    Pozzi (21:3)
    al (21:12)
    et (21:9)
    georeference (19:43)
    pre (21:46)

"georeference" is a correct technical term. The other words are part of the
pre-print title and author list.

# Changes following first submission:
- Add a doi for a preprint that described the functionality of the package
- A function with examples was not exported; that was a mistake and it is now
exported.