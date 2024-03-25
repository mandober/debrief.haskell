# FRP Intro

## From "Haskell School of Expression" by Paul Hudak
"The Haskell School of Expression - Learning FP through Multimedia"

13. A Module of Simple Animations
13.1 What is an Animation?

An animation is a continuous, time-varying sequence of static images, called frames, displayed in a rapid succession of above 30 fps (30 Hz).

The 'drawlnWindow' operation was accumulative - the previous image is not erased, but the new one is just rendered over it, and this is bad for certain kinds of animation such as movement. There are several standard ways of dealing with this problem:

* *XOR mode* means a pixel color is computed by XOR-ing the desired color with the background color. If you draw the same thing twice in the same place, then the screen reverts to the background color, effectively "erasing" what was just drawn. The advantage of this approach is that you don't have to redraw an entire frame at each time-step; it is only necessary to redraw the moving parts of the scene. The advantage is its applicability, because the animation must be drawn over a homogeneous background, otherwise the image being drawn will change colors at the boundaries of the underlying colors, according to the XOR algorithm. `a ⊕ 0 = a`, `a ⊕ a = 0`, `b ⊕ a ⊕ a = b`

* *Clearing the screen* before drawing each frame. The advantage is generality; it will work regardless of the nature of the animation. The disadvantage is that the entire frame must be redrawn, and thus if it takes longer than approx. 33 ms (the period of one cycle at 30 Hz) to draw a frame, the animation will flicker.

* *Buffering* 
Every graphics window has its own memory, as a 2D array, into which an image is drawn. If one image is drawn over another, this memory is simply overwritten with the representation of the new image. The graphics hardware then reads from this memory and displays on the screen what is there, but there is no synchronization between this reading and the above writing. Thus, there will typically be some overlap between these processes, which often results in a certain degree of flicker.
* *Double Buffering* 
To prevent this problem, many graphics systems have a *secondary memory, called a buffer*, into which a new frame is drawn. Once this memory is completely written with the new image, the graphics hardware is told to use this new memory instead of the old. Thus, the graphics hardware never "sees" the process of writing into the memory, and the flicker is thus eliminated. The old memory is then available as the buffer for the next frame. So, a double buffering.

## Representing an Animation

We can't just use a list of frames as it's not enough for all manupulations we could do on an animation (rwd, fwd, skip, inc/dec playback speed, etc.).

An animation will be represented as a function from time to an image. And because there may be different kinds of "images" that are time-varying, we define a polymorphic animation:

```hs
type Time = Double
type Animation a = Time -> a
```

This representation of an animation (that is time-dependent) has another important property: the animation will run at the right speed, regardless of how long it takes to compute each frame. If computing a frame takes longer than about 33 ms, then the resulting animation may not be very smooth, but it will still run at exactly the right speed, and will run at that speed regardless of how fast the underlying CPU is.

```hs
newtype Behavior a = Behavior (Time -> a)

-- A simple interface to animate yields an animator for picture behaviors:
animateB :: String -> Behavior Picture -> IO ()
animateB s (Behavior pf) = animate s (picToGraphic . pf)

-- polymorphic fns to lift ops on the static values to operations on Behs
lift0 :: a -> Behavior a
lift0 a = Behavior (\ _ -> a)

lift1 :: (a -> b) -> Behavior a -> Behavior b
lift1 ab (Behavior ta) = Behavior (\t -> ab (ta t))

lift2 :: (a -> b -> c) -> Behavior a -> Behavior b -> Behavior c
lift2 abc (Behavior ta) (Behavior tb) = Behavior $ \t -> abc (ta t) (tb t)
```
