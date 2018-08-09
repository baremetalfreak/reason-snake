open Reprocessing;

let width = 600;
let height = 600;
let stepSize = 20;
let stepDeltaTime = 0.2;

type directionT =
  | Dleft
  | Dright
  | Dup
  | Ddown;

type coordinates = (int, int);

type gameState = {
  apples: list(coordinates),
  snake: list(coordinates),
  isGameOver: bool,
  direction: directionT,
  deltaTime: float,
};

let getRandomPostion = () => (
  Random.int(width / stepSize) * stepSize,
  Random.int(height / stepSize) * stepSize,
);

let setup = env => {
  Env.size(~width, ~height, env);
  {
    snake: [(0, 0), (20, 0), (40, 0), (60, 0), (80, 0)],
    apples: [
      getRandomPostion(),
      getRandomPostion(),
      getRandomPostion(),
      getRandomPostion(),
    ],
    isGameOver: false,
    direction: Dright,
    deltaTime: 0.0,
  };
};

let isCollision = (head, snake) => snake |> List.exists(pos => pos == head);
let nextState = state => {
  let (squareX, squareY) =
    List.nth(state.snake, List.length(state.snake) - 1);
  let head =
    switch (state.direction) {
    | Dright => ((squareX + stepSize) mod width, squareY)
    | Dleft => ((squareX - stepSize + width) mod width, squareY)
    | Dup => (squareX, (squareY - stepSize + height) mod height)
    | Ddown => (squareX, (squareY + stepSize) mod height)
    };
  let body = List.tl(state.snake);
  let isGameOver = isCollision(head, body);
  let hasEaten = List.exists(pos => head == pos, state.apples);
  let snake =
    hasEaten ?
      List.append(state.snake, [head]) : List.append(body, [head]);
  let apples =
    hasEaten ?
      [
        getRandomPostion(),
        ...List.filter(pos => pos != head, state.apples),
      ] :
      state.apples;
  {...state, snake, isGameOver, apples, deltaTime: 0.0};
};

let drawFood = (state, env) => {
  Draw.fill(Constants.blue, env);
  state.apples
  |> List.iter(pos => Draw.rect(~pos, ~width=20, ~height=20, env));
};

let drawSnake = (state, env) => {
  Draw.fill(Constants.green, env);
  state.snake |> List.iter(pos => Draw.rect(~pos, ~width=20, ~height=20, env));
};

let draw = (state, env) => {
  let bgColor = state.isGameOver ? Constants.red : Constants.black;
  Draw.background(bgColor, env);
  drawFood(state, env);
  drawSnake(state, env);
  let deltaTime = state.deltaTime +. Env.deltaTime(env);
  switch (deltaTime > stepDeltaTime, state.isGameOver) {
  | (false, false) => {...state, deltaTime}
  | (true, false) => nextState(state)
  | (false, true) => state
  | (true, true) => state
  };
};

let handleKey = (state, env) =>
  switch (state.isGameOver, state.direction, Env.keyCode(env)) {
  | (false, Dright, Left) => {...state, direction: Dright}
  | (false, _, Left) => {...state, direction: Dleft}
  | (false, Dleft, Right) => {...state, direction: Dleft}
  | (false, _, Right) => {...state, direction: Dright}
  | (false, Ddown, Up) => {...state, direction: Ddown}
  | (false, _, Up) => {...state, direction: Dup}
  | (false, Dup, Down) => {...state, direction: Dup}
  | (false, _, Down) => {...state, direction: Ddown}
  | (true, _, Space) => setup(env)
  | (_, _, _) => state
  };

run(~setup, ~draw, ~keyTyped=handleKey, ());