# todo

## bugs
- run download in background so grid is responsive
    - still a delay for artistlistview
- mode 2: when download finishes, going next doesn't clear the first image. it will clear when you go back and next again
    - mode 3 and 4 too, but the entire first page isn't cleared
- remove file extension and number from artist label in artist list view

## features
- download progress indicator
- check cached contents to see if they need to be re-downloaded or continue an interrupted download
- prefetch next page
- continuously show new images in current slice as they finish downloading
- more ergonomic views
    - artist list: a border surrounding every row with the artist name as the border label. inside that row would be the profile pic + 3 previews, each inside a box as well. this is the only reason to keep a separate config for image x/ycoords for this view.
    - pixiv post: show single image with in depth info next to it (eg, caption, tags, comments). the challenge is that images might be big enough to cover that section

## code
- restore catch exceptions in python
- [ ] error messages shouldn't crash anyway, just display in footer
- refactoring and de-duplicate code
    - reducing the size of St

## double check
- move magic numbers into config (see old config)
