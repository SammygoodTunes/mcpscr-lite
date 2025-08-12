# mcpscr-lite

A light-weight CLI version of an unreleased Minecraft source code randomiser.

Supports only alpha (and possibly beta) versions of the game.

![Preview](https://github.com/SammygoodTunes/mcpscr-lite/blob/main/assets/ss.png?raw=true)
![Preview](https://github.com/SammygoodTunes/mcpscr-lite/blob/main/assets/ss2.png?raw=true)

> [!NOTE]
> The MCP itself is NOT included in the repository, and therefore must be provided by the user.

<!-- (Pre-)release **VERSION** is now **[available](https://github.com/SammygoodTunes/mcpscr-lite/releases/tag/)**. -->

## Contents:
- [Setup](#setup)
- [Information](#information)
  - [Origins](#origins)
  - [Randomisation features](#randomisation-features)
  - [Presets](#presets)
  - [Compatibility](#compatibility)
  - [Dependencies](#dependencies)

## Setup

> **<ins>Requires:</ins>** Python 3.12 or above.
> 
> Before installing the necessary modules, it is recommended to set up a virtual environment. This allows for a clean workspace and avoids installing packages to your global environment.

Set up a new virtual environment. For the sake of conventions, we'll call ours `venv`:

```bash
python -m venv venv   # Windows
python3 -m venv venv  # MacOS / Unix	
```

Activate it using:

```bash
venv\Scripts\activate     # Windows
source venv/bin/activate  # MacOS / Unix
```

In order to install the necessary modules, Poetry is recommended but not required. You can install it using the following:

```bash
pip install poetry
```

Install the pre-requisites:

```bash
poetry install
```

> If you prefer not to install the development dependencies, then feel free to use the ```--only main``` flag.
> 
> If you prefer not to use `poetry`, then you may manually install the dependencies listed [here](#dependencies) with pip.

Run `mcpscr-lite` using:

```bash
python -m mcpscr  # Windows
python3 -m mcpscr # MacOS / Unix
```

A file explorer window will prompt you to locate the MCP folder. Once selected, it'll start
a series of processes in order to properly prepare the environment.

## Information

### Origins

The original MCPSCR (Mod Coder Pack Source Code Randomiser) was created in early 2020 in Python for Minecraft 1.12
and went through several phases. Originally, it used to randomise single `.java` files, before a search algorithm was
implemented; this allowed for randomising the entirety of the game's source code.
In late 2022, the MCPSCR was re-vamped: it was GUI-based (Qt) and used regex patterns to find specific tokens in Java code.
Additionally, it supported the newest version of the game (from 1.16 to 1.20 at the time).
This particular light-weight version however takes a step back to re-visit the older Alpha and Beta versions of Minecraft. 
So, it is only designed to support the MCP dedicated to those versions.

Various images on the Internet of MCPSCR (Python) and MCPSCR (Qt) are available for viewing below:

**MCPSCR (Python):**

- [Example of randomised entity model](https://www.reddit.com/r/PhoenixSC/comments/lwrhds/this_is_normal_right_i_mean_i_think_it_is_but_im/)

**MCPSCR v2 (Qt):**

- [Screenshot of the code viewer](https://media.discordapp.net/attachments/583007909902942210/1158878146494480527/image.png?ex=688dda21&is=688c88a1&hm=5f7de8d3e71bdae344347105a995c3485533d9647710ff987a10c0bc0627efd0&=&format=webp&quality=lossless)

- [Screenshot of the file viewer](https://media.discordapp.net/attachments/660468731217969187/1047247800490020986/image.png?ex=688e7504&is=688d2384&hm=3b33dbe5ff906b0aeb7a767082a2cf1f4ad76a8444d2a1dec709f50a29ee955c&=&format=webp&quality=lossless)

### Randomisation features

- `float`/`double` randomisation
- `int` randomisation
- `bool` randomisation
- `++`/`--` randomisation
- `Block` randomisation [only tested on b1.7.3]

### Presets

- `World Gen` — All terrain generation and noise-related files
- `Models` — All entity model files
- `Entity` — All entity handler files
- `All` — All files

### Compatibility

This project was tested on the following operating systems:
- Debian 12

If you are using an operating system/distribution that isn't listed above, 
then the game may or may not work as intended.

> Only Windows and Linux systems will be supported.

### Dependencies

| Name     | Version | Required? |
|----------|---------|-----------|
| javalang | ^0.13.0 | Yes       |



> [Go back to the top](#mcpscr-lite)
