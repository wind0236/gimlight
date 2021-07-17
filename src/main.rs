use rltk::{BTerm, GameState, RltkBuilder};

struct State;
impl GameState for State {
    fn tick(&mut self, ctx: &mut BTerm) {
        ctx.cls();
        ctx.print(1, 1, "hello world");
    }
}

fn main() -> rltk::BError {
    let cx = RltkBuilder::simple80x50()
        .with_title("Roguelike Tutorial")
        .build()?;

    let gs = State;
    rltk::main_loop(cx, gs)
}
