open Reprocessing;

let width = 600;
let height = 600;
let stepSize = 20;

type coordinates = (int, int)

type gameState = {
  foodPosition: coordinates,
  snake: list(coordinates),
};

let setup = env => {
  Env.size(~width, ~height, env);
  {snake: [(0, 0)], foodPosition: (300, 300)};
};

let draw_food = (state, env) => {
  Draw.fill(Constants.blue, env);
  Draw.rect(~pos=state.foodPosition, ~width=20, ~height=20, env);
};

let draw_snake = (state, env) => {
  Draw.fill(Constants.green, env);
  state.snake |> List.iter(pos => {
    Draw.rect(~pos=pos, ~width=20, ~height=20, env);
  });
}

let draw = (state, env) => {
  Draw.background(Constants.black, env);
  draw_food(state, env);
  draw_snake(state, env);
  state;
};

let handleKey = (state, env) => {
  let (squareX, squareY) = List.nth(state.snake, List.length(state.snake) - 1);
  let newHeadPos =
    switch (Env.keyCode(env)) {
    | Right => ((squareX + stepSize) mod width, squareY)
    | Left => ((squareX - stepSize + width) mod width, squareY)
    | Up => (squareX, (squareY - stepSize + height) mod height)
    | Down => (squareX, (squareY + stepSize) mod height)
    | _ => (squareX, squareY)
    };
  {...state, snake: List.append(List.tl(state.snake), [newHeadPos])};
};

run(~setup, ~draw, ~keyTyped=handleKey, ());