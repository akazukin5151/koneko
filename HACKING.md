# Hacking

> Developer guide to the codebase, plus some design notes and philosophy

## 'Functional core, imperative shell' + MVA?

> "There are two roles of code: code that does work, and code that coordinates work" ([Sonmez](https://simpleprogrammer.com/there-are-only-two-roles-of-code/))

If there is one sentence to keep in mind while reading the code, this is it.

Although it seems contradictory, I think the code has uses both the functional-core-imperative-shell and the [model-view-adapter](https://en.wikipedia.org/wiki/Model%E2%80%93view%E2%80%93adapter) OOP patterns.
* The models are the functional core.
* The views accepts user input (inbound IO) and interacts with the user (outbound IO) -- this is the imperative shell.
* The adapters mediates between the functional core and the imperative shell. They launches the views, get user input, calls the appropriate models, and send that to the appropriate views. 

In other words, the adapter organises code, while the models and view does the actual work.

| Role of code | Model (functional core) | Inbound IO (imperative shell) | Outbound IO (imperative shell) | Adapter (organises everything)
| --- | --- | --- | --- | ---
| Does work | <ul><li>colors</li><li>config \*</li><li>data</li><li>pure</li></ul> | <ul><li>api</li><li>config \*</li><li>download</li><li>picker</li></ul> | <ul><li>config \*</li><li>files</li><li>printer</li><li>screens</li><li>utils</li></ul>  |
| Organizes work |  | <ul><li>assistants</li><li>prompt</li></ul> | <ul><li>lscat</li></ul> | <ul><li>cli</li><li>lscat_app</li><li>main</li><li>ui</li></ul>

The overall code is neither OOP nor functional: the only communication between all modules is through function returns, not through 'notifying' different objects. The only (major?) exception is that `downloads.py` will directly trigger a callback to `lscat.py`.

Now, this paints a rosy picture but in reality, there was no initial planning so the code was originally not written towards any structure or pattern. What still needs work is:
* The config contains a functional core, inbound IO, and outbound IO. Better split them up

Here are some good tips:
* "The way to figure out the separation is by doing as much as you can without mutation, and then encapsulating the mutation separately" (Gary Bernhardt)
* "Create your application to work without either a UI or a database so you can run automated regression-tests against the application, work when the database becomes unavailable, and **link applications together without any user involvement**" ([Alistair Cockburn](https://github.com/jschairb/sandbox/wiki/HexagonalArchitecture)) (Emphasis mine; it is possible to adapt koneko to work with sites other than pixiv)
    * At the very least, only the api and download modules need to be replaced.
    * `data.py` will need to be adapted to your new API structure.
    * The config already handles username and passwords, but you can add something like an API key or OTP auth
    * The navigation routes (eg pages, prompt handling) are in `ui.py`, while the starting dispatch logic is in `main.py`


## Cache directory structure

```sh
$ cd ~/.local/share/koneko
$ tree -d  # (Edited: .koneko and history are files not directories)
.
├── cache                     # ├── KONEKODIR
│   ├── 2232374               # │   ├── Artist pixiv ID                             ├── Mode 1 and 2
│   │   ├── 1                 # │   │   ├── Page 1                                  │   ├── Mode 1
│   │   ├── 2                 # │   │   ├── Page 2                                  │   ├── Mode 1
│   │   └── individual        # │   │   └── Posts with a single image               │   └── Mode 2
│   │       └── 76695217      # │   │       └── ID of posts with multiple images    │       └── Mode 2

│   ├── following             # │   ├── Following users mode                        ├── Mode 3
│   │   └── *your_id*         # │   │   └── Your pixiv ID                           │
│   │       ├── 1             # │   │       ├── Page 1                              │
│   │       │   └── .koneko   # │   │       │   └── Stores number of artists info   │
│   │       └── 2             # │   │       └── Page 2                              │
│   │           └── .koneko   # │   │           └── Stores number of artists info   │

│   ├── illustfollow          # │   ├── Illust follow mode                          ├── Mode 5
│   │   ├── 1                 # │   │   ├── Page 1                                  │
│   │   └── 2                 # │   │   └── Page 2                                  │

│   ├── search                # │   ├── Search users mode                           ├── Mode 4
│   │   └── gomzi             # │   │   └── Search string
│   │       └── 1             # │   │       └── Page 1

│   ├── history               # │   ├── History file, for frequent "mode"

│   ├── testgallery           # │   ├── (Internal/debugging use)
│   └── testuser              # │   └── (Internal/debugging use)
└── pics                      # └── Images for main and info screen
```


## UML diagrams

### Flowchart of modes and their connections:

![Flowchart UML](http://plantuml.com:80/plantuml/png/dPDD2y8m38Rl_HM5dZtejfk8YYY2Dy6BY1IDTHWtwGVYltVMhfkrAdWgIzuyUPUcGwMvrEQCX1W5Eww0ZgJEbTuAZWZorlNn-PaBwFdFQObONlD2RBajK8bFBO7BtR6Efmq1qLJaGrsPDKsjZIvb4u3BydGRem4I6A7zphgTtyXS77Ldu6f_oYkb-uNNhZtA5lnQp2H04ONuR0lnFCAq0mOD4ig4XR-Fp094pGud7pCZ0YDVcURYB2M1fPGo2NiIN9IjhE8nBv-alaKQjUjeqS5db3qkPfMN29gyBOUjRmJjuV-I8XpyOcHHN_znwuqBXqE6KEohHtG7)

### Simplified UML diagram of the classes:

![Classes UML](http://plantuml.com:80/plantuml/png/rLdTZzGs47_FNn6fBx1xYNZZ1Pr08DgJMXL1wGkgB6_YtJNdnAdjt76Y-xzt_9NOiHDGYqIBX5ZFs3FZttmwF9SA2pLsh2WKLOoKjxmdjxmey44gWHjLDWnBMNQOzlKBH_kPyw5yzkpHYwjIaFRnzVNdeeG_UoFhqMEpg3vx2HSq2DuD2es28IuGRVqMpMY8b1Ed7yRfFBh3ZBPOaPWSY3rnf3ZgU4k0UIcAq5AXg5I_0ClOQkshwvjkOAGZlP9rFPkGjsqfVS3DBJuH8zy9v8UF112YZ8rIeIDdZD-R4uY1Ldqe0rir3bNEo9Mz7ywqRKd_HxMgPkcgaKvItjzat5JNRycV8mLIYPKIz30guk4ePtXBYqnA1vzWP5IbFFtFBH41Ez1s86TyH_aelBRvu40_qJi16udCU1h-KjwZ0pboGS1Zv4XKSqxt3OBs2WtW9OYP8tSRwlebxphU9_gZUDttv9CzwmcWH--M8H7Y1Gj94EvRrFBxdd4SHhCr32MY174RBrlU-X4pHiH3rcLfvCR-IPoIUgBQXd4DlFSz1RTuoEuekKUq2n4vW29_qTaNPu284PTUAVipOAzeSpaJDXXS5b7xbqcJ51oVvPfZqYS0xULLsJpWVXCrb8cqVTxMsC8C65M9iB41ZPBesW7NcAoxCTdwsYJhTxHn3sNC6NcoLg26Sz5ABoBbzsDdF9AfKRjA1tsLe5yKAyKfE901f2XoqV-lZYpC2bh2xn2vR_YtR0hs-AztH0ZQakXqhgjTfXOoqhY1cAGQm3OZEUIqL0uCFzYCYy9B4t81jVTbAShIFHz87rDCaOeeKy6BgF8C3ZsEZ251fgZvE7Q3HY8iSepWkxagw1tRbV5BxN7fi2Blnhg-NW8OByqE3raH8Rh5UbRNsVeJnfGDJTgVtaC1aDyjiYmpcPSkeQdl21RD-HlRUTdsO1wUxJCPEuEQ7qkCut_pEcb5JOgvBk9XYWMMV5j5mtXWjA7gmQqVO2IntMDZalDc_6SdWxcHJC7nfPvkBbMajvpBUUplpQs1kugDwDkj2byBlrrUHpB9CTbWDTdtcqKvux9eO8PMh8Kbz1FfYS2A2mlkEEZCdfAGzdA0zX3ak5LkbBvo3K1kn6ciTrQs9KvV-n-9B5QGgLRLh9uGz7HM-ND56XsxMdkkNbUJcILsDID7jQ9X5PRmeNQzSjXtVvkTyGHW1iWWRHtPZsGfn_GeJpUCG4Hf8ZOmAAcuvaGi_IiFXOg_ueWvatkIxtGng78tpBTvjp-wzxHxfG_lAktBgahWNUs8goTMvjkLQrocCK5Ntzh5rYlulPv0-z9KM9Y-tL8R2LLNdTsQ7u-IABSmWnHqPxUSXhvWFoZLEsImx757PAQYxEDtp4g5DoAqaH8rk3aJHsr68U0Uo4Ban9lfwwFalTTW7JV9teDbGVaoC9afB01fN_f3Nh-rqlbY8YzmCylWui0vmBNuqTg-WPXLTwICQA0SKI-3FMviFWNWUm0q9RgGZgt_lTDf6xlRY4ifxZTCq5GD70O6Vp_1DO8m63MXOHPgGqZ67eETho86lw_xY3M16dx4Yk0D60kWmvOSG1wGO_33xs6f6m9kx905OqOkk4z7r9ctcSl4tWi-Z-NzwBvwBSHfMn2MLlNy5hotRhsdwepWelPvbXGlL-qopw950iyRtTCgIeTKZ1kf4eNsOP04n4BUsYvVXF40cjxS_6G-eUYHh7pqxkg7rw-VFdtov77v3le68mgW2aVxdYgAMSdu2Lg57t50qFmBGPQC95Yhio2uTOG6N0c93ZASwwEFu_WUKuMCyCbVR-shOV9K_5bcVgHp1GC0QNqPR8d2b7dsqNT26mwBZqZIjBmiAsvm2-QiDFZegRzbni1MbMcVmK0GlG2AuZdfM_r_3Ny3)


### Actor-ish model of the ui.AbstractUI class
  
![AbstractUI UML](http://plantuml.com:80/plantuml/png/VLLDRzim3BthLt2t1aqFQRQBWNGjsai7h0rQz6oeDfCGaILFajC6o_xx95bs_94c9uQFugCUIVdMMAvjLGXcoGg4ktTZDS_isoDS52f70xCfpAVmVGe_9emvH6buCwWPMopWjui0Wm8pIqh2Oi4y15StK15SNYQ30EQyLmxqGGdScIsiNBRA7w6yveCPA2dsavWeP4fWIP-qSp4ivvQ_SuFW3PPfv3RQFlLDC1CeECcqv7OJlRoig-4Vd6mgr9hanUI89V3hzVa9DFwfq5YcqLPQDZvJAccmb_xLQU24QR5OXZ0Pjp4msEwAUGvUpgtJAsPG7r8edYVm42sWWg_HU6bf5wuJ0TOORS9mYKSV3jSkJE8ufC5dDLuVQoL_85CAlgyfONOUlluw-poouxZeioddtID7lThefIPfWUHAiRLMLGcbnXNQR4DopS3zZcq8MaZyj6cbHS1Yttg-VlzxRUuSwRUwppDkTmSVDxDqrmmHOsMyzvHYAFDcvfPQ5QLbXyCXhDUJkqh8U6Ap0hRqR3q5ezucJjNKAUWaDRMcTTL-eRgwrjmjwLBZZbHblY8k8vvwtArHBFqiSPFx1OD8uwGw0ErInWT28-nL1QRI-BDp8e5x9LUa2zu-AaslxWLo3Lo8K9r9Cvvji2dn1SDZPy0_D6rDpoNAE9xZkeuW6unUSARKjcxutv_47FuAajj_trgs_jQVOEm735dzN4S2Rmx7iB7IkfeTQbeHUbrFl2udHh4p2hTkBFrdWR7_)
  
### Actor-ish model of the ui.Gallery classes (extending AbstractUI)

![Gallery UML](http://plantuml.com:80/plantuml/png/RL71QiCm3BthAtHCA3liq4iWeuD2Fw1iLwFL9dWuLh0LIY0VFzjnM3TwilJq93q_dQ9Ga7bq85QSmGivPyA28siJiSaFlY9vZSSMW6x20J-Y2G1zZv_MtD6ED1Xi1aRXckIk5liUCXDVtd2sU3Xq3tn8IC87JKAbD74KzHtPGp0o1_y0PCuNHOCsHt0BXcHFjZ4bxl2Qn2NHlvrRDOley6pZv6Y9V_Phldl7f8k4yvuKHXFRAcLNL60r1HudQLfOrnMA2nV6PA6DKlvTFJiYUxSyCs5WjT5L9gP1_wserJcrAQltxS_HzuBMessIJiRhV4-07m00)


### Actor-ish model of the ui.User classes (extending AbstractUI)
  
![User UML](http://plantuml.com:80/plantuml/png/TOun2iCm40JxUyMMDYPLKWKKdCelI2q4Ee90jk1qAW9wl2KbRTgPMTWrgnCjUoGD6Xclp98nxSaphaY2sqn4nc1BQ678Yk6CePxYya09M9Oxtib-0zk3QQR6c_LEtV0_MThA1a_2MkJuGv-3RYv6bW_LMjk7bG_VVnAT)


### Actor-ish model of the ui.Image class

![Image UML](http://plantuml.com:80/plantuml/png/bLHDJyCm3BtdLvWRQ73XMAbeIBk0n3PfshaAZJiYkYJ4ITd4-Eya_Tf71nezLCvxp-wp7NLCZbldroLpqfK8Jsk-GdZH0XbBqpe0mX9p9xM2D6KyTzh2aj2o-8Ax1_0IHgEaqTwpS0fOv19uf7SeWjn7fHG76GdCvKPM4MmIk6cgF2zcKx3uuP4Si-YyLHr6HYj29hZZhvmmv8QeJQ_Z11R17D9Usv12VwfISv70f8r0nZufTYChxh2NC853hBKnaMHAlgKc-HQCbSg5aoeqs-rszS1c1bN3ns6TJ6XFTYKZWWA-IgdUlw_wAiSsprGw5WpQxAAifhCAhImaYkkRVpNSsvdYnlrgHGKoCu4BrKzzSDggFElTa95AeRtOHYpNtwM_fh-osXkOMopGvM-rfNOo49w36x9tBDUhpDko5hJB6E0NjnF5G_iHFTVMYQS4baOS2h3T6p5KWrs49YkfFO4vlmxJyjrABhsxu_2j-1jWFm00)


### Functions in download.py

![download UML](http://plantuml.com:80/plantuml/png/ZLDR3eCW4Fpd55x07g7fJGYLRRiDB0RNDYOUlhLLAGAcNuQPVS3CUEfOUUwj4OnieDBkISOfVMW78Tpv3WrNICXo8HPGTGCrAoezaB9G8CdjyrsBHE1ZzeA6mKj5EjczDl8DQ3DwvNflSfXyWCSBGZeChRg2R9ppkYkGr5iXUgpg2Q_7uqQQw3GN7GjAYIoyQSffKDBsxzOCi_PcbOrUNB0k3vSq287O6HsAnA-1LLswPzRck7mAHTRmk2oudKB9m92ew3NHpXBnqpidhlNEjYo-d_UkCRdMdD2TYazCJq1w-gzV)


### AbstractImageAdjuster classes in assistants.py

![ImageAdjuster UML](http://plantuml.com:80/plantuml/png/vLL1JiCm4Bpd5JbI5V82LBK28RWWn8LoifnusooAdM2lMw3qxwHDavROBZqWSI2tU3RnpEoiFJR4DLbPH15F3McUKPmLt9XuidkzatmE4_5Y3O6EBo_F9kUn1Z4OZLPHN3z9yWYl5kkbc1DfJ2s1IP8jDenNxL_NCULMfefZmGn-W8kRl5oocLMP1q81hE0f51vi8u2PYcTW-eHIoFa2IwixGliFDLIbfhD1VrtoznIOB1V0R7KK5kLIXGf0SmCks6baNC-DYsiWgnKZtyve6AhA4blm0iKEJvAhKifIxQMsIhU-r5NhO6SVk12evjwUticg00cAzgrqZ1vwfBe2mhJxExICcu9xMDQSWiawI80eCtSDPjmMnDuSP8PAC86oYLhjJcFAj7NZY0FU1FTZsy_gzvnvqnN8b3SiZJ7eakjSw6iFANx-Hue3XzddXQqk-lCGEVFHb4r_m5i_dNcxgTbiDxx6Me31UbfF6xcWaOUUUORGVygSC9fTDZhAJoWe__M41C-IVKnSAhU0yvp-YClH0BaDZdmzxnLlhJsbi14UHMDGOdDJVm40)


## Dependencies

There are two dependencies that aren't necessary: returns and placeholder (both on the right edge). Code can always be rewritten to remove those two dependencies. That said, they don't introduce their own dependency tree, so only a total of three dependencies are added. Compared to the core dependencies (pixivpy, pixcat, and blessed), that's insignificant.

![dep tree](dependencies.png)

```sh
$ pipdeptree -v
2.0.0b1
$ pipdeptree --graph-output png -p koneko > dependencies.png
```

