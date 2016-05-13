## 0.6.0 - The contrast Release
before going into the changes, I want to say that this is the first log entry I make since for the first time I feel this release needs one. Behind the curtains I did a lot of changes to the look and feel of the UI. Without pushing it as an actual patch. I did this to deliver the package as a whole, I even considered this to be the fist major push. Yett I didn't because there will still be some bugs in this new look I have not discovered yet.

So why the changes? Nuclues became dull to me, but I couldn't grasp why. Then I noticed it looked a bit grey, because for the UI I lightened the syntax-background-color to create my panel color and darkened the syntax-text-color to get the UI text color. And I coulnd't unsee that overall greyness. So I decided to add more contrast. To achieve this I made the following changes.

* Buttons now have one bright border and text color and will stand of from the background.
  * Disabled buttons however are only visible through subtle text, this removes distraction from unusable elements.
* `theme-background-color-light`, used for background other than the editor has been changed into a darker tone from the editor color, is now called `theme-background-color-dark`.
* input through `atom-text-editor[mini]` now looks different from a click button to make its function more clear.



## 0.1.0 - First Release
* Every feature added
* Every bug fixed
