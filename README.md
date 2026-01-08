# skia-flowmotion
Delphi RAD Studio Skia Flowmotion Alpha v0.2 
  
[![Skia-FlowMotion Demo Video](https://img.youtube.com/vi/Tisq6QcFHXs/maxresdefault.jpg)](https://www.youtube.com/watch?v=Tisq6QcFHXs)
   
This is an animated image flow gallery built using Delphi's cross-platform FireMonkey framework powered by Skia4Delphi (Google's Skia graphics library).   
    
      
If you want to tip me a coffee.. :)   

<p align="center">
  <a href="https://www.paypal.com/donate/?hosted_button_id=RX5KTTMXW497Q">
    <img src="https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif" alt="Donate with PayPal"/>
  </a>
</p>
    
  
### Features:  
- Pinterest-like masonry layout   
- freefloat Layout with save/load positions   
- Animated appearance (slide-in, “falling” effects, breathing)   
- Select / move or zoom it into a target rect     
- HotTrack & HotZoom hover effects     
- Dragging selected or all free floating     
- Paging with falling animation    
- ActivationZone -> move selected to defined areas and trigger event   

    
 ----Latest Changes   
   v 0.2   
    - Caption & Smallimg working now   
    - Fixed last missing functions from vcl version   
    - Added TSurfaceEffect - sueNone, sueShadow   
    - Added property RoundEdges   
    - Added TPictureBorderType - btTech, btFull   
    - Added propertys AlphaStatic, AlphaHotPhase, AlphaHotSelected   
   v 0.1    
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
     
    Zipped sample exe, that way its small enough for upload  
    
  
       
This project aims to bring way more and better features to Flowmotion using the power of Skia4Delphi.   
Some already implemented now, rotating of images, particle effects on click, shadow for selected, feels a bit like that microsot surface tables long ago, only better:   
<img width="2560" height="1440" alt="Unbenannt" src="https://github.com/user-attachments/assets/79007956-eb90-487b-8255-f3497653c812" />

     
Requirements    
    
Delphi 11+ or 12 (RAD Studio)    
Skia4Delphi installed and enabled   
   
   
Current Status   
Alpha, everything of vcl version implemented now, checking for bugs, but getting usable slowly  😄   

   
Homepage https://lamita.jimdosite.com/  
   
