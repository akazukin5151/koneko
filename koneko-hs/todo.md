# todo

## bugs
- run download in background so grid is responsive
    - still a delay for artistlistview
- mode 2: when download finishes, going next doesn't clear the first image. it will clear when you go back and next again
    - mode 3 and 4 too, but the entire first page isn't cleared

## features
- download progress indicator
- check cached contents to see if they need to be re-downloaded or continue an interrupted download
- continuously show new images in current slice as they finish downloading

## code
- restore catch exceptions in python
- [ ] error messages shouldn't crash anyway, just display in footer
- refactoring and de-duplicate code
    - reducing the size of St
