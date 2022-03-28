# todo

downloading from scratch (then prefetch) broken
vvv
## fix bug where quick consecutive IPC actions fails
whenever a function need to send a message and get a reply:
- only send (message, callback function) and return (do nothing else)
- internally:
    - generate unique int
    - update mapping with {k=int, v=cb}
    - send the msg with the int
- when the global centralized handler in background receives the response with the int
    - lookup the mapping with the int
    - find the cb
    - apply the cb to response

## bugs
- run download in background so grid is responsive
    - still a delay for artistlistview
- mode 2: when download finishes, going next doesn't clear the first image. it will clear when you go back and next again
    - mode 3 and 4 too, but the entire first page isn't cleared
- remove file extension and number from artist label in artist list view

## features
### feature parity
- prefetch next page
- continuously show new images in current slice as they finish downloading
- check cached contents to see if they need to be re-downloaded or continue an interrupted download
- actions on selected image (view post)
- download progress indicator (a progress bar would be a new feature, above parity)
- interactive lscat assistant

### new features
- more ergonomic views
    - artist list: a border surrounding every row with the artist name as the border label. inside that row would be the profile pic + 3 previews, each inside a box as well. this is the only reason to keep a separate config for image x/ycoords for this view.
    - pixiv post: show single image with in depth info next to it (eg, caption, tags, comments). the challenge is that images might be big enough to cover that section

## code
- restore catch exceptions in python
- error messages shouldn't crash anyway, just display in footer
- refactoring and de-duplicate code
    - reducing the size of St

## double check
- move magic numbers into config (see old config)
