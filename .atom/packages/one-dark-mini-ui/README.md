## One Dark Mini UI theme

Minimized Atom One Dark UI Theme, for better multi pane programming.

Not suitable for users who use mouse to click on the tab or users who mainly use multi tab over multi pane.

Modified from Atom One Dark UI Theme. See [one-dark-ui](https://atom.io/themes/one-dark-ui) for more details.

### Comparison

#### Mini
![After](http://i.imgur.com/rwlgWmM.png)

#### Original
![Before](http://i.imgur.com/4C7f1Vi.png)

### Changes

#### Smallest tab bar

![Tab Bar](http://i.imgur.com/hVojaQd.png)

This is the most important change. I usually do horizontal and vertical split programming workflow as inherited from when I was using Vim, and tab bar takes too much space on vertical stacking alignment. Not only more space gained, overall the screen looks less "bulky" too.

The original height I believe was designed to be easily clicked on. But if you rely on shortcut keys it will not be a problem. Plus if you do split pane, you might not need a stacking tabs anyway. The tab acts like file name indicator for each pane. Smaller tabs makes horizontal splitter looks more like one thin line.

There is a problem with characters with tail like g or q that goes out of the tab, but I think the trade off worths it because I have no problem guessing name of files. Also tab with icon like Settings will be overflowed badly, but I rarely open Settings when I am working so that's fine.

#### Line number smaller than actual program

![Line Number](http://i.imgur.com/aysiFbn.png)

I think the default design was distracting. Smaller font not only improve the concentration on actual code, but you gain a bit of horizontal space too especially if you have side by side split pane.

#### Smaller status bar, pane divider and tree view

Status bar is only a little bit smaller. Pane divider reduced to thin line at all times. Tree view got line spacing reduced.

#### Accent color changed to yellow-green

![Accent1](http://i.imgur.com/IdfiusO.png)

![Accent2](http://i.imgur.com/LUgBuoh.png)

![Accent3](http://i.imgur.com/hTej5oQ.png)

Because it is my favorite color.

If you don't like it, the place to change is `ui-variables.less`. First search for number `68` in the code, there are two of them which is the hue. Next search for `@base-accent-color` which you can adjust saturation and brightness of that hue.

Note that not all blue has been changed to green. Like hyperlink color or file changed indicator dot stay with the same color.

### Settings changes

Layout mode (Auto, Compact, Spacious) has been removed. Because this theme is even more compact than Compact.
