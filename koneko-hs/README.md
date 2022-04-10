# koneko-hs

Ongoing rewrite of koneko in Haskell using Brick, to enable a more responsive, less buggy, and easier to maintain code

## Known bugs

- Some emojis may take up more space than needed in some terminal emulators. This is not really fixable because it is inconsistent based on the emoji and terminal emulator in question. It's possible to just remove all emojis, but the effort probably isn't worth the difference of one square. See also:
    - https://github.com/jtdaugherty/brick/issues/265
    - https://github.com/jtdaugherty/brick/issues/356
    - https://github.com/jtdaugherty/vty/issues/175
