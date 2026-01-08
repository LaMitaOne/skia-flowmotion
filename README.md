# skia-flowmotion
Delphi RAD Studio Skia Flowmotion Alpha first running prototype, not everything implemented so far, but basic animation core things working already 
  
[![Skia-FlowMotion Demo Video](https://img.youtube.com/vi/Tisq6QcFHXs/maxresdefault.jpg)](https://www.youtube.com/watch?v=Tisq6QcFHXs)
   
This is a very early alpha prototype – the first quick attempt to see if a performant, animated image flow gallery can be built using Delphi's cross-platform FireMonkey framework powered by Skia4Delphi (Google's Skia graphics library).   
    
What's already working?   
    
Loading single images or lists of images      
Layout, dragging selected or all   
Hotzoom, Breathing, Hottrack  
Animated "fly-in" effects when adding images     
Background image support    

 ----Latest Changes   
   v 0.1    
   - Ported basic VCL Flowmotion functionality to Skia.    
    - Added basic particle effects on click.    
    - Added corner dot for rotating images.    
    - Added basic glitch effect while rotating.     
    - Added Shadow effect under selected image.    
    - Added HotTrack TechBrackets.     
    - Implemented "Holographic" Background Effect:     
      Replaced static wave lines with a "Tri-Layer Ghost" technique.     
      Draws the background image three times (Normal + 2 Ghost layers).     
      Layers are offset by Sine waves (WaveX, WaveY) to simulate liquid refraction     
      or a heat haze over the entire picture.     
     
    Zipped sample exe, that way its small enough for upload  
    
You can already see that the core animation engine and smooth rendering work beautifully thanks to Skia – images fly in nicely, and the foundation feels solid.   
Performance not exactly like vcl version, but with 200 pics still fine, but on 1000s we get slow...but thats cool since we have paging       
    
What's not working yet   
   
Captions, hints   
activation zones   
Save/load positions   
Many bugs and incomplete features   
       
This project aims to bring way more and better features to Flowmotion using the power of Skia4Delphi.   
Some already implemented now, rotating of images, particle effects on click, shadow for selected:   
<img width="2560" height="1440" alt="Unbenannt" src="https://github.com/user-attachments/assets/79007956-eb90-487b-8255-f3497653c812" />

     
Requirements    
    
Delphi 11+ or 12 (RAD Studio)    
Skia4Delphi installed and enabled   
   
   
Current Status   
Alpha prototype – runs, looks promising, but not really usable yet.    
I plan to continue development and eventually make it a full-featured, smooth image gallery component.
Feedback, ideas, bug reports, and even contributions are very welcome!   
If you're into Delphi, FMX, or Skia – feel free to star, watch, or open issues.  
More coming soon... just not today or tomorrow 😄   
