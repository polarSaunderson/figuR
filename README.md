# figuR

**PRIVATE BRANCH OF THE pkg03_figuR PACKAGE**

## Overview
The figuR package is a personal package that allows easier customisation of figures. 
It uses base R, but the functions use a syntax that I find more intuitive, and thus it is more configurable whilst being essentially the same.

## Public Version
The public version of this repo can be accessed at: 
  https://github.com/polarSaunderson/figuR

## To-Do
### 2023-08-13
- [ ] Work on general arguments for pre_plot that can be overwritten
  - e.g. nameCex, so x and y are both set, but can be xNameCex or yNameCex

### 2023-08-02
- [ ] Expand arguments & document `pre_plot()`
- [X] Add `arrange_subplots()`
- [ ] Document functions
- [-] Use `pre_plot()` in `kulaR` for creating kulaBars
  - actually, that just uses a simple matrix and add_axis
- [X] Create `plot_points()`, `plot_lines()`, etc.
