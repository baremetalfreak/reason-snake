open Reprocessing;

let width = 600;
let height = 600;
let stepSize = 20;

type coordinates = (int, int)

type gameState = {squarePosition: coordinates, foodPosition: coordinates};

let setup = env => {
  Env.size(~width, ~height, env);
  {squarePosition: (0, 0), foodPosition: (300, 300)};
};

let draw_food = (state, env) => {
  Draw.fill(Constants.blue, env);
  Draw.rect(~pos=state.foodPosition, ~width=20, ~height=20, env);
};

let draw = (state, env) => {
  Draw.background(Constants.black, env);
  Draw.fill(Constants.green, env);
  Draw.rect(~pos=state.squarePosition, ~width=20, ~height=20, env);
  draw_food(state, env);
  state;
};

let handleKey = (state, env) => {
  let (squareX, squareY) = state.squarePosition;
  let newPos =
    switch (Env.keyCode(env)) {
    | Right => ((squareX + stepSize) mod width, squareY)
    | Left => ((squareX - stepSize + width) mod width, squareY)
    | Up => (squareX, (squareY - stepSize + height) mod height)
    | Down => (squareX, (squareY + stepSize) mod height)
    | _ => state.squarePosition
    };
  {...state, squarePosition: newPos};
};

run(~setup, ~draw, ~keyTyped=handleKey, ());