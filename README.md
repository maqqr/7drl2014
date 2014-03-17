Necromancer Simulator 2014
==========================

Necromancer Simulator 2014 is a roguelike made in 7 days using Haskell for the 7DRL competition.

The development started on 9.3.2014 at 12:00 UTC+2.

The development ended on 16.3.2014 at 12:00 UTC+2.

Status: __FAILED__

You can download and try the game in its current state here: [Releases](https://github.com/maqqr/7drl2014/releases)

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

We started to work on line of sight functions and added drawing of player, npcs, zombies and corpses. Town generator adds doors to the houses. Your tower's stats are displayed on the world map.

### Day 6

The town generator generates npcs to the town map. The game is currently unplayable and won't be finished, unless a miracle happens during the last night.

### Day 7

We tried to quickly finish the game but didn't have enough time. We made a better world map, added ability to raise undead when you are in a village (r-key raises undead). We had drain life, fireball and force bolt spells, but didn't have time to make a targeting system, so the player can't actually cast them. The biggest problems right now are that the player can't cast spells, the player can't die, and on the world map, the player can't enter the king's castle, because it isn't finished. We made the castle's layout, but it doesn't have any npcs.

There is a screenshot of a test village with hundreds of villagers. The gray &'s are corpses, which you can sense through walls. You can see where all of your zombies are, and you can see one tile around them.
![Day 7](/screenshots/day7.png "Screenshot of day 7")

And here is a screenshot of the improved world map:
![Day 7](/screenshots/day7b.png "Screenshot of day 7")
