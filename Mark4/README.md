# MARK3 Docs

## Prerequisites

 - Visual Studio 2017 Community Edition
   Technically it's also possible to run this on a Mac or Linux, but because
   I'm using MSTest, Unit Tests won't. Though some sources claim they could -
   they might work on Visual Studio for Mac, but not in Rider
 - .NET Framework 4.6.1
 - MonoGame SDK 3.7.1 (For building content, also, it's a part of the build)
    - Visual C++ Redistributable 2013 x64
      Monogame devs forgot to put it as a part of the installer. Without it,
      you'll get "Missing FreeImage.dll" errors
      You can get it from here:
      https://www.microsoft.com/en-gb/download/details.aspx?id=40784
