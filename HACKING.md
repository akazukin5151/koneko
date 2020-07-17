# Hacking

## Pure and impure modules

|          | Pure | Impure
| -------- | ---- | ---
| Frontend |      | <ul><li>assistants</li><li>main</li><li>prompt</li><li>ui</li><li>lscat_app</li><li>screens</li></ul>
| Backend  | <ul><li>colors</li><li>data</li><li>pure</li></ul> | <ul><li>api</li><li>cli</li><li>config \*</li><li>files</li><li>download</li><li>lscat</li><li>picker</li><li>printer</li><li>utils</li></ul>

* 'Pure' means functions are referentially transparent and globally pure (mutations may happen in local scope)
* 'Frontend' means interacts with user (and catch and process user inputs)
* 'Backend' means everything else
* (Ideally the) 'backend' does work, 'frontend' organises work (if otherwise, please open an issue or make a PR).

\* Config has impure IO functions, safe functions that will return defaults on failure, and interactive functions


## MVC? MVA?

While the code is not intentionally structured according to any design pattern, you might be able to analyse it through the lens of the Model-View_Controller pattern. The only communication method between all modules is through function returns, not through 'notifying' different objects (most of them are functions). The only exception is `downloads.py` will directly communicate to `lscat.py` through a callback. Therefore, it seems to fit the [Model-View-Adapter](https://en.wikipedia.org/wiki/Model%E2%80%93view%E2%80%93adapter) pattern better.

Model:

* api
* colors
* config
* data
* download
* files
* pure
* utils

View:

* assistants
* lscat
* printer
* prompt
* screens

Controller / Adapter:

* cli
* lscat_app
* main
* picker
* ui


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

![Classes UML](http://plantuml.com:80/plantuml/png/rLdTZzGs47_FNn6fBx1xYNZZ1Pr08DgJMXL1wGkgB6_YtJNdnAdjt76Y-xzt_9NOiHDGYqIBX5ZFs3FZttmwF9SA2pLsh2WKLOoKjxmdjxmey44gWHjLDWnBMNQOzlKBH_kPyw5yzkpHYwjIaFRnzVNdeeG_UoFhqMEpg3vx2HSq2DuD2es28IuGRVqMpMY8b1Ed7yRfFBh3ZBPOaPWSY3rnf3ZgU4k0UIcAq5AXg5I_0ClOQkshwvjkOAGZlP9rFPkGjsqfVS3DBJuH8zy9v8UF112YZ8rIeIDdZD-R4uY1Ldqe0rir3bNEo9Mz7ywqRKd_HxMgPkcgaKvItjzat5JNRycV8mLIYPKIz30guk4ePtXBYqnA1vzWP5IbFFtFBH41Ez1s86TyH_aelBRvu40_qJi16udCU1h-KjwZ0pboGS1Zv4XKSqxt3OBs2WtW9OYP8tSRwlebxphU9_gZUDttv9CzwmcWvuwI-v06uOA590ZtBMhvVSyu3eFPseKIIOAuZPSjP_s86IFY8UknD71ZzoG-IHrHRQEuXjtxde9NF6864Dg5Y1n0aR_ex8epGCI8owwK_HcWLxIvd0aR32wBeFtBP4aAZazopJ7P4u3sygfi7d2_YHhA1De-RsjiOGQCgaJOs816INJfW6jCrjsOP7rjSlKxshY7AkQCF5ahKCEvQAKNaVBxiJCUoPIeNQM3lahGBugLsXGSo01I5Bde_rT75cO57U5tYDmt_5ls17lyrpiY16r9T3hNr2vJ1nbft3zibG7RZEIGqr8vCFnWCouABqt81TJUbwKeI_Ty87rCCKKgefXwnqSPK-KP77eS6KE2J57pSUm6ZKHOv1Z1TtDLq3ksA-ENkkFIOKNUZNLzlGGmNfeT7h8YGdIBzQoki_KdZ2aRchG_lOS28BzRP5bcCYvTGbFV4IoQypUsyxBjmJmyssSoTWOrFvOOn__cTTAAcXHpNSJ350ii-RQAXl70Q4FLWri_m4HYkyV68EVD-CzE1dCZcOBZIptTNAf8RpcNyzZVcri3TnKRqRTR5RuMVhkyZcIIOxB1QhBlDujoncNHl0ojM0jBw2VI4u4L5nRSST2PFIKXxUK0xI78SQlSA7tb6e3SYDDOxwniIvo-zZyIMQmWKwsgMJqXwEciykUAD3fsjVPSlQucCqliQaQEQaN3AYpXE-rwvR3l_ZOxumd03911sZgo7ybIZkbHdcuOWOZIH6nWK59npObO-bSU2XL_nH5p9lSYt-bYKELkc6_pRdzqxsdtIX_ULTgNLPN0kzeHLq-ipBShrxXCOuAklhMBhLVmUps1zgMfi31zkwKs4ggkEhirFnubKMvX1Ydepcuv3Nt1Vb2gTyXWsUEEo4n5sSVlc9KARqHe8oLgS7CcZjgCGi0za8N8YJVJrqV9Uw_1EcwIlGVBWl9bO39JM03IllI7l7vhfVF5H5xWPfR1nO5pW6lnexLz0p6hxaWPqK0vebu6UzpOV0h0zm1eIdGX7Ll_UwVJDdQt4PTIt6-OeAaQE0mC_dw2QmHXC6f2mopKXf2CFGOxNqKCVb_t3Mi2DVo65S4RC1P0XoqvW3mWn-27dyDIDmJSs24BnenSS9-Eg3DlCvU9lHTy7SlxqNtqMeZJjY0ihUhvBNXltNfFrHd1HUtpB2bUhjfbdaMB19utkgTLb0wf63TI9Ghjmo09Y8MyjLs-2EC1DBsv-SbyGj4ZMVdetTKFhry_VVhaoUFo7VGDHXH05OxsFLKKivBn4xGAF-A0eFaNWYmPIR1MPq5mwmWDk18I7MGurqVV6l4zfWePuPE_tjfNm-If-BFC_4Zd2WO0qleos1A5AVFie--4DXmM7v6aQNbPLjpW5inPQV3HKt_BZ86jAjC-Wu4WUW4KnNFIj_g_6lu6)


### Actor-ish model of the ui.AbstractUI class
  
![AbstractUI UML](http://plantuml.com:80/plantuml/png/rLdTZzGs47_FNn6fBtUxbuWtNj091Bhsf5P5KFg2AikRU3U-S-BKSUuuqVtVE_vAxDW9g4L2887MCvuF_sOyCmul1uc575kMPP9AHl9xtf5xdcNuC4Y1AvbN30z3tcBQbQyixLVE-_p5YujNLxaWzUNrzUSiXpzxhUlYKY-Apqx36VM2jxr4es282qHhjqKnAZ8C4wVhnqaUFM16Qon9IFRKdZYI7BMy9i0yPvdlgP1qaBy1I_jg_2lBsxPdf2MT7CfoTY7jsr9xZwjxV29QlrN83tS44AACZOD4HyuOVzGIY85L9PI0hHZx8kNaojwF3QrhqlrLh9fPXcgaavOFjuambUKxylT8WPHZAGKzZ98uEF8PtXomnqk10sdm0-MZS0BpVhKTQG_YKAb_xuc86KC3Hw4T94wNY71FFz470RR9aE32NyexT217BWZ4aHo9h9g45a4xYNg87LWwShkXB5_pjkLTTAGWY_ST-MHa7G7iF53owDCWYR0O2C9TZMh-s36E_HGtVg58ao1sutahX3zZneXuIiOnJkSvQ4KSZqKKKjXlOxg6v8UEGemSVXeUslhu7C2mE_OSc0O0OyJUm9Zz6JoJj3etXFKQfKLY_wDlKWoLAmIAes-O1-7hgxnwmjqcXYYHTlkqhw672J2A767j0ne7eieB76FoxbPVw1jzdx-ZZtkeTDR9a_42LPoBskLDWj-DhOv8eepj2dK5YWZzB5kfNvv00f0iIzs5_oMoSClh6dz2vhxbtx9l6F7V7uWGj2Q1TAhndQSkCzAmnvYQ0xvfpJwdfaFFy9EvSK5wAK8geVQkI0MtTCzxqeKKNR82Ygv-V1n2gguanv4n9CYKDNTZsoia_9B727wOguBQiLqdlzH1bm7Bqc4io-ibWE5Ixt2GPG6YMwmNPPciFtvEcTIatUa3583XksMMOKOZrTbtzJt1ecg-iP_dxGYcuTcMIVZfrVnGOvZ_cyU9AsfKp5KHzw-OvyctDTIF1qOhAf_i-Wdc4zCzDeOzvyOFCYVCRMLAbIzrU7t4B3xpNDp3w6zkzOANRUJYRbNvMZBkqZQYAOyD5PQJVtyOr1C13APoQCnAMKG_aOu8BBamu8wzkkVJbOJmzT0il1jlZ6kZRsmx63QoDhGx6zjIfuxzdrHcAyWKgsPM9GGzDJ8jbyNPiIjLvCfrCud9ObSoSfGh5bPXyLzsrol2RXOmXJQS1_GuQHW-sGreCKTtB4VN34z5hZFMSoYglreYrFwLGhxXhn3HCgat_4uLWo9rmdJJj_k3Sq-xLxhohb2nB8e8tjN-k2gnCksk7ECy3Gsg79Ssjzx0x_KBjCzrXOLPt2wLar1rPMFN_7WSYBGBFLP1hxRBQGJqzeDHjMFmbXrkoP2eAFlmLRDIuRKAvUI0Abmrn5AhKGWu1p8GMVLw5hiRUESicC1DklVWcLU-D4nw9lD0skTEoDatNjfOJEG5Rdhf7Hmu1pWMFnhVDn0pvewK4GMK9QgbjySEqISFV0U0eWG7KhdrmmUTrc6ujRgOOd_3F4rbps5WSESJN248OrBbEsQWreHe2DBOkIgYyVkwhruJg5uV3U7r60kW_PRiGUwHG_3z3sjn63nk6905OqREEqzBPCFhn672w7bVpj9nj1_45kgKBmWFnlJyCdmqONsaia5mKFDOYugNhNQ9HzBY0iyRxKChk0wn6ZjIHGRDCo5AY8M-jLsk2ECUBBsz_KL_K54ZMNxn_kgdcvldpvyzkypVGzzWH09K_g3VKKanona_GQjm8muecdyXk2KZyTQo4GJNbb11AE6YWmuR-k0x97x4L2AjV8hNE_E6c28LVgIPd-nSmW10QbS6Qo8nPOvzT9tGfCFYazAWMrwI5JQu1NCswdqGLTyvGs2BGhTFRo08NW1Pzf9qjVhFYNy1)
  
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

