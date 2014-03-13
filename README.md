Necromancer Simulator 2014
==========================

Necromancer Simulator 2014 is a roguelike made in 7 days using Haskell for the 7DRL competition.

The development started on 9.3.2014 at 12:00 UTC+2.

The development ends on 16.3.2014 at 12:00 UTC+2.

## Development log

### Day 1

The basic framework for the game is done and the required datatypes are designed and implemented. There isn't much to show right now so we won't add any screenshots for this day. Tomorrow we'll try to get map drawing and player movement done.

### Day 2

![Day 2](/screenshots/day2.png "Screenshot of day 2")

World map drawing and drain life, force bolt and fireball spells are done. Player movement will (hopefully) come tomorrow. The characters on the console window can have two colors. For example the trees on the world map blend from green to dark green.

The dark bottom area of the window will be filled with information about your own necromancer tower, which you can upgrade to make yourself and your zombies stronger.

### Day 3

![Day 3](/screenshots/day3.png "Screenshot of day 3")

Village generator is slowly taking shape. The player can move around in the world map and enter any village. Next we are going to add villagers and zombies.

The generator uses [binary space partitioning](http://en.wikipedia.org/wiki/Binary_space_partitioning) to split the map into smaller areas and then it generates a random house in each area and adds roads to the borders of the area. The village generator is defined in [TownGenerator.hs](TownGenerator.hs).

### Day 4

Not much happened, because half of our development team didn't have time to work on this game today. We managed to get zombie and npc drawing working.

### Day 5

Coming soon (:tm:).
