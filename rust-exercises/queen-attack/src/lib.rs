#[derive(Debug)]
pub struct ChessPosition(i32, i32);

#[derive(Debug)]
pub struct Queen {
    position: ChessPosition
}

impl ChessPosition {
    pub fn new(rank: i32, file: i32) -> Option<Self> {
        if rank < 0 || rank > 7 || file < 0 || file > 7 { None }
        else { Some(ChessPosition(rank, file)) }
    }
}

impl Queen {
    pub fn new(position: ChessPosition) -> Self {
        Queen { position }
    }

    pub fn can_attack(&self, other: &Queen) -> bool {
        let on_same_rank: bool = self.position.0 == other.position.0;
        let on_same_file: bool = self.position.1 == other.position.1;

        let rank_delta: i32 = (self.position.0 - other.position.0).abs();
        let file_delta: i32 = (self.position.1 - other.position.1).abs();
        let on_diagonal: bool = rank_delta == file_delta;

        on_same_rank | on_same_file | on_diagonal
    }
}
