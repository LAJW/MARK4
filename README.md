# MARK4

A Game Jam 2020 game attempt. I didn't buy tickets, so couldn't force someone to make assets for me.

Theme: Repair

## How to play

### Objective
 - Collect 3 ship parts (purple squares) and bring them to the ship (pink square) to fix it and win the game
 - The area is irradiated - you're constantly taking damage. You need to heal yourself to survive.
 - Collect stimpacks (green quares) to restore health
 - Collect RadX (orange squares) to resist radiation
 - Collect MedX (blue squares) to resist damage
 - Kill or avoid sentries (orange triangles) - they're guarding the parts

### Controls
 - WASD - movement
 - left click - shoot
 - R - reset

## Build dependencies:

### Windows 
 - Visual Studio 2019
 - .NET Framework 4.7.2
 - Monogame Content Pipeline 3.7.1 http://community.monogame.net/t/monogame-3-7-1-release/11173 - contains a tool for managing all game resources - images, meshes, audio, etc. Monogame is included in the project, but you won't be able to build the assets file and run the game without this.
 - Visual C++ Redistributable 2013 x64 -
   Monogame devs forgot to put it as a part of the installer. Without it,
   you'll get "Missing FreeImage.dll" errors
   You can get it from here: https://www.microsoft.com/en-gb/download/details.aspx?id=40784


### Mac/Linux
 - IntelliJ Rider (probably comes with Mono Runtime - .NET port for Linux)
 - Monogame Content Pipeline 3.7.1 http://community.monogame.net/t/monogame-3-7-1-release/11173

## Running

 - F5 in Visual Studio
 - Some other key in Rider

## Testing

 - Visual Studio: Test Explorer -> Play Button
 - Tests should be detected by Rider
