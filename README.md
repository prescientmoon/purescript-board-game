# Purescript halogen proof of concept

A small board game proof of concept making use of:

- purescript
- halogen
- svg
- profunctor lenses

## Setting up the repository locally

This guide assumes you have [pnpm](https://pnpm.js.org/), [purescript](https://www.purescript.org/) and [spago](https://github.com/purescript/spago) installed locally

- Clone this project

  ```sh
  git clone git@github.com:Mateiadrielrafael/purescript-board-game.git && cd purescript-board-game
  ```

- Install all the dependencies

  ```sh
  pnpm install
  ```

## Running a development server

```
pnpm dev
```

## Building for production

This step assumes you have [zephyr](https://github.com/coot/zephyr) installed.

```
pnpm build
```
