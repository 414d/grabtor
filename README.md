grabtor
=======

A graphic battery status monitor adapted for xmobar

The sample xmobar config to show only the output of grabtor is:
```
Config { font = "xft:DejaVuSansMono"
       , borderColor = "black"
       , border = TopB
       , bgColor = "black"
       , fgColor = "grey"
       , position = Top
       , lowerOnStart = True
       , pickBroadest = False
       , persistent = False
       , hideOnStart = False
       , commands = [Run Com "grabtor" [] "" 10]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "} %grabtor% {"
       }
```

In other words you should add the item **Run Com "grabtor" [] "" 10** to the **commands** list and the string **grabtor** with **sepChar** around it to the **template**.

Help is:
```
$ grabtor -h
```
