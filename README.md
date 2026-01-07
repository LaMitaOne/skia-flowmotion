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
    
You can already see that the core animation engine and smooth rendering work beautifully thanks to Skia – images fly in nicely, and the foundation feels solid.
Performance not exactly like vcl version, but with 200 pics still fine, but on 1000s we get slow...but thats cool since we have paging       
    
What's not working yet   
   
Captions, hints   
activation zones   
Save/load positions   
Many bugs and incomplete features   
   
This is purely a proof-of-concept to show that a full modern replacement for my VCL-only Flowmotion component is possible – cross-platform (Windows, macOS, Android, iOS) and with much better graphics performance.  
   
This project aims to bring similar (and probably way more and better) features to Flowmotion using the power of Skia4Delphi.   
   
Requirements    
    
Delphi 11+ or 12 (RAD Studio)    
Skia4Delphi installed and enabled   
   
   
Current Status   
Alpha prototype – runs, looks promising, but not really usable yet.    
I plan to continue development and eventually make it a full-featured, smooth image gallery component.
Feedback, ideas, bug reports, and even contributions are very welcome!   
If you're into Delphi, FMX, or Skia – feel free to star, watch, or open issues.  
More coming soon... just not today or tomorrow 😄   
