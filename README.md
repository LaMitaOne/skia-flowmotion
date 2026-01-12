# skia-flowmotion
Delphi RAD Studio Skia Flowmotion component alpha v0.43  
    
 <img width="1139" height="1137" alt="Unbenannt" src="https://github.com/user-attachments/assets/ed3f782e-1559-442f-b122-ad931a2ec215" />
  
     
# Skia-FlowMotion    
    
**Animated Image Flow Gallery – Delphi FMX + Skia4Delphi (Google's Skia)**    
    
Started as a simple port of my D7 VCL Flowmotion component...  
**3 days later**: Fully ported to Skia/FMX + extreme improvements (rotation with handle, particles on click, perspective-correct shadows, holographic animated background, tech brackets...).   
VCL Version: https://github.com/LaMitaOne/Flowmotion     
   
From "just show some search result covers in my player" to **this beast** in about 3 months total 0->VCL->FMXsskia4delphi (with pauses).    
No big deal to port old code when Skia is involved 😉 btw its even my first component and...playing with canvas at all... ^^      
   
**Features at a glance (v0.43):**   
- Pinterest-like masonry layout   
- freefloat Layout with save/load positions   
- Animated appearance (slide-in, “falling” effects, breathing)   
- Select / move or zoom it into a target rect     
- HotTrack & HotZoom hover effects     
- Dragging selected or all free floating     
- Paging with falling animation    
- ActivationZone -> move selected to defined areas and trigger event   
- Free rotation with corner handle   
- Particle explosions on click   
- Realistic drop shadows
- ZoomSelectedToFull, RotateAllBy, and more functions   
- Holographic background (triple layer sine-wave refraction)   
- HotZoom, Breathing, Glow, Captions, SmallPic overlays   
- Full live editor in demo (colors, sizes, toggles)     
    
**Try it!**     
Grab the repo, run the sample, and watch the magic.     
Feedback, stars or ideas very welcome – happy to discuss code or add features!   
   
Sample video: https://www.youtube.com/watch?v=R_yIg2XfF24   
    
**Let's make Delphi UIs awesome again!** 🚀     
     
    Zipped sample exe, that way its small enough for upload  
    
   
If you want to tip me a coffee.. :)     
   
<p align="center">
  <a href="https://www.paypal.com/donate/?hosted_button_id=RX5KTTMXW497Q">
    <img src="https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif" alt="Donate with PayPal"/>
  </a>
</p>
   
   
Requirements    
    
Delphi 11+ or 12 (RAD Studio)    
Skia4Delphi installed and enabled   
   
   
**Current Status**  
Alpha – everything from the old VCL version is implemented now, still lot to do, but getting usable slowly 😄   
   
If you find a bug: feel free to eat it yourself :D   
I've already got a huge list of ideas and todos, so it's probably on my radar anyway...  
Especially **full freefloat layout** is still making problems with some things,  
but **normal layout** is getting very stable now – even with SelectedDraggable and not zoom to center.  
Physics are running almost perfectly fine already.   
     
BUT: if you see how something quickly get fixed, I'd really appreciate any help / PR / issue report :)      
       
**----Latest Changes----**       
      
   v 0.43 --- Day 6   
    - skFLM now automatically resizes large images   
    - Added MaxInternalPicSize property (default 720px)   
    - Implemented real-time collision avoidance   
      Images now dynamically move out of the way when the selected is dragged across the screen   
      This creates a natural, magnetic interaction effect when KeepSpaceforZoomed is combined with   
      SelectedMovable   
    - We disable breathing/hotzoom if we are currently dragging the image   
      This prevents the "Jitter/Flicker" effect caused by size changes while moving   
    - Implemented dynamic shadow scaling based on zoom/breath state   
    - Wall Sliding: Hotzoom and breathing effects now respect screen edges.   
      Images smoothly slide against borders.   
    - lots fine tuning and bugfixes   
   v 0.42 --- Day 5  
    - Fixed some mem leaks at clear and showpage    
    - Added SurfaceEffects -> sueGlow, sueAmbient    
    - Added function RotateAllBy   
    - Added function ZoomSelectedToFull   
   v 0.41 --- Day 5    
    - FKeepSpaceforZoomed now working   
      Layout keeps space free under centered Selected pic   
    - added onSmallpicclicked   
    - added techbracketwidth property   
   v 0.4 --- Day 4   
    - added propertys RotateDotColor, FRotateDotHotColor, FRotateDotDownColor  
    - added property ShowSmallPicOnlyOnHover  
    - fixed Glowwidth, Hotwidth  
    - added internal TargetRotation  
    - added putalltoAngle function  
   v 0.3 --- Day 3   
    - Added property smallpicmargin, effects rotatedot too   
    - middleclick on rotate now resets rotation   
    - rotate dot now changes color onmousedown   
    - added ResetAllRotations   
    - lot small improvements and bugfixes   
   v 0.2 --- Day 2    
    - Caption & Smallimg working now   
    - Fixed last missing functions from vcl version   
    - Added TSurfaceEffect - sueNone, sueShadow   
    - Added property RoundEdges   
    - Added TPictureBorderType - btTech, btFull   
    - Added propertys AlphaStatic, AlphaHotPhase, AlphaHotSelected   
   v 0.1 --- Day 1   
    - Ported basic VCL Flowmotion functionality to Skia.    
    - Added basic particle effects on click.    
    - Added corner dot for rotating images.    
    - Added basic glitch effect while rotating.     
    - Added Shadow effect under selected image.    
    - Added HotTrack TechBrackets.     
    - Implemented "Holographic" Background Effect:    
      Draws the background image three times (Normal + 2 Ghost layers).     
      Layers are offset by Sine waves (WaveX, WaveY) to simulate liquid refraction     
      or a heat haze over the entire picture.    
    - fixed shadow perspective when rotated   
    - improved pos calculation of rotatecircle for mousedown   
    - added lots more of functions from vcl version    

        
     
Homepage https://lamita.jimdosite.com/  
   
