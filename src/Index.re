open Reprocessing;

let width = 600;
let height = 600;
let stepSize = 20;

type coordinates = (int, int)

type gameState = {
  foodPositions: list(coordinates),
  snake: list(coordinates),
  isGameOver: bool,
};

let setup = env => {
  Env.size(~width, ~height, env);
  {snake: [(0, 0), (20, 0), (40, 0), (60, 0), (80, 0)], foodPositions: [(300, 300)], isGameOver: false};
};

let draw_food = (state, env) => {
  Draw.fill(Constants.blue, env);
  state.foodPositions |> List.iter(pos => {
    Draw.rect(~pos=pos, ~width=20, ~height=20, env);
  })
};

let draw_snake = (state, env) => {
  Draw.fill(Constants.green, env);
  state.snake |> List.iter(pos => {
    Draw.rect(~pos=pos, ~width=20, ~height=20, env);
  });
};

let draw = (state, env) => {
  let bgColor = state.isGameOver ? Constants.red : Constants.black;
  Draw.background(bgColor, env);
  draw_food(state, env);
  draw_snake(state, env);
  state;
};

let isCollision = (newHeadPos, snake) => {
  snake |> List.exists(pos => pos == newHeadPos);
}

let handleKey = (state, env) => {
  if (state.isGameOver) {
    state;
  } else {
    let (squareX, squareY) = List.nth(state.snake, List.length(state.snake) - 1);
    let newHeadPos =
      switch (Env.keyCode(env)) {
      | Right => ((squareX + stepSize) mod width, squareY)
      | Left => ((squareX - stepSize + width) mod width, squareY)
      | Up => (squareX, (squareY - stepSize + height) mod height)
      | Down => (squareX, (squareY + stepSize) mod height)
      | _ => (squareX, squareY)
      };
    let snakeTail = List.tl(state.snake);
    let isGameOver = isCollision(newHeadPos, snakeTail);
    let hasEetenApple = List.exists(pos => newHeadPos == pos, state.foodPositions);
    let newSnake = 
      switch (hasEetenApple) {
      | true => List.append(state.snake, [newHeadPos]);
      | false => List.append(snakeTail, [newHeadPos]);
      };
    {...state, snake: newSnake, isGameOver};
  }
};

run(~setup, ~draw, ~keyTyped=handleKey, ());