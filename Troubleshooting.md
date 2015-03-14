# Introduction #

What to do if...

# Details #

if you've set up a scene and a camera and you're getting the infamous black screen of death (nothing appears):
  * If you are using a perspective projection, set the field of view to a wide angle
  * Set the near and far clipping planes to values you are sure include everything in the scene e.g. 0.001 and 100000
  * Make sure your camera is pointed at something