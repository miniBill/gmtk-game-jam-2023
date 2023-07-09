# TODO

- better enemies
  - maybechase
  - mayberandom
  - start random
- movement - redo
- credits
- gamepad

# Credits

- Name - Hannah
- Gameplay - Liv
- Based on a true story - [redacted]
- Main character is a mummy wrapped in toilet paper - Liv
- Art - Emily
- Music - GabrielExists - https://gabriel-exists.bandcamp.com/
- Background - https://unsplash.com/photos/qVotvbsuM_c

# Ideas

- Two player game - one wants up, the other down

# Sound effects needed

- Menu sounds
  - ?
- Background music
  - Sneaky
  - Spotted
- You got spotted
- You flipped a roll
- You won
- You got caught

# Audio mixing

If you have a pause function, then play "Sneaky" without "Base" and without anything else. Again, please interpolate

The "MenuIntro" track is intended to make the startup of the game less sudden, so it's not just "loop start wall of sound" at the first moment. Play the Intro first, at startup, and start all other tracks the moment it ends

## Game

Let X be the current panic level, where 0 is "you're completely alone and far from any guards" and 100 is "you're epsilon away from being caught.
"Base" track volume: Regardless of X, volume is always 100%.
"Sneaky" track volume: When X is 0, volume is 100%. When X is 25, volume is 25%. The volume percentage scales linearly and is limited to be between 100% and 0%
"Chase" track volume: When X is 25, volume is 25%. When X is 50, volume is 100%. The volume percentage scales linearly and is limited to be between 100% and 0%.
"Panic" track volume: When X is 50, volume is 0%. When X is 85, volume is 100%. The volume percentage scales linearly and is limited to be between 100% and 0%.

## Menu

When inside the menu, the "Base" track volume is at 100% and the "Menu" track volume is at 100%. All other tracks are muted.
When in game, the "Menu" track volume is at 0%.
When transitioning to the game from the menu, or to the menu from the game, quickly (0.5s perhaps) interpolate to 0% on the tracks you're going from, before you interpolate the volumes of the tracks you're going to, to their respective values according to the above.

# Making a spritesheet

- `montage sprite_* -tile 6x1 -geometry '16x16+0+0' -background None spritesheet.png`
- `convert spritesheet.png -flop spritesheet_flipped.png`
