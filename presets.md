# mcpscr-lite: Presets

`mcpscr-lite` provides the possibility to create presets.


## Contents:
- [What are presets?](#what-are-presets)
- [How do presets work?](#how-do-presets-work)
- [Limitations](#limitations)
- [Preset Architecture](#preset-architecture)
  - [Root](#root)
  - [PresetContent](#presetcontent)
  - [PresetConstantEntry](#presetconstantentry)
- [Annexes](#annexes)
  - [Preset Constants](#preset-constants)
  - [Clarifications](#clarifications)

## What are presets?

The presets specific to `mcpscr-lite` are JSON files, which contain specific randomisation instructions.
These instructions can target various parts and features of the game that the user wishes to randomise.
These instructions come with a handful of characteristics that directly affect the randomisation. This
allows for unique properties for each preset.


## How do presets work?

Presets work alongside a script that builds a form of "API", which allows for easy access to various parts of the game
without having to do too much work. This "API" provides constants to game mechanics and features (for now), the values of 
which the `mcpscr-lite` can easily access and randomise.

The process runs as such: 
- Parse all the Java classes of the game
- Use that data to construct the "API"
- Patch the game code to use the "API"
- Parse the preset JSON file
- For each instruction, randomise the game content provided by the "API"

You can still alter the intensity, probability and seed all within the CLI before
the randomisation process happens.

## Limitations

Like everything, presets do come with certain constraints that are mostly due to the early development stages of the project
as of writing this.
- Only a single preset can be applied
  - For now, every preset chosen resets the source, so only one can be chosen at a time

## Preset Architecture

The following is how the JSON data must be structured in a JSON preset file. It assumes that the user is 
already familiar with the fundamentals of the JSON scripting language.

The `presets` folder must contain two distinct directories:
- Client-side presets directory: `presets/client`
- Server-side presets directory: `presets/server`

These directories are used to determine the nature of the instance
targeted by the preset. They'll determine where the "API" is created
and will patch specific code based on the instance type.

The tables of keys are listed below.

### Root

| Key           | Type             | Description                            | Required? |
|---------------|------------------|----------------------------------------|-----------|
| `name`        | `str`            | Preset name                            | Yes       |
| `description` | `str`            | Preset description                     | No        |
| `content`     | `PresetContent`  | [PresetContent](#presetcontent) object | Yes       |

### PresetContent

| Key           | Type                        | Description                                                 | Required? |
|---------------|-----------------------------|-------------------------------------------------------------|-----------|
| `vars`        | `list[PresetConstantEntry]` | List of [PresetConstantEntry](#presetconstantentry) objects | Yes       |

### PresetConstantEntry

| Key     | Type             | Description                                       | Required? |
|---------|------------------|---------------------------------------------------|-----------|
| `id`    | `str`            | Preset [constant](#preset-constants) to randomise | Yes       |
| `range` | `list[int, int]` | Min-max range of value                            | Yes       |


# Annexes

## Preset constants

| Key                             | Description             | Type  |
|---------------------------------|-------------------------|-------|
| `WORLD_DUNGEON_COUNT_PER_CHUNK` | Dungeon count per chunk | `int` |

## Clarifications

- The `mcpscr-lite` "API" is always referenced between inverted commas/in a  double-quote block as it isn't an actual API, but
  rather a code injector that moves actual game code to a separate custom class, and replacing where it used to be with
  an attribute or method call from said class; it isn't a standardised API

> [Back to top](#mcpscr-lite-presets)
