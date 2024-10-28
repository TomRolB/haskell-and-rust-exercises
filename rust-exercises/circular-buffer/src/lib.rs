use crate::Error::{EmptyBuffer, FullBuffer};

#[derive(Debug, PartialEq)]
pub enum Error {
    EmptyBuffer,
    FullBuffer,
}

pub struct CircularBuffer<T:Clone> {
    buffer: Vec<Option<T>>,
    size: usize,
    next: usize,
}

impl<T:Clone> CircularBuffer<T> {
    pub fn new(size: usize) -> CircularBuffer<T> {
        CircularBuffer {
            buffer: vec![None; size],
            size: 0,
            next: 0,
        }
    }

    pub fn read(&mut self) -> Result<T, Error> {
        let position: usize = (self.buffer.capacity() + self.next - self.size) % self.buffer.capacity();

        match self.buffer[position].clone() {
            None => {
                Err(EmptyBuffer)
            }
            Some(element) => {
                self.buffer[position] = None;
                self.size -= 1;
                Ok(element.clone())
            }
        }
    }

    pub fn write(&mut self, byte: T) -> Result<(), Error> {
        if self.is_full() {
            Err(FullBuffer)
        } else {
            self.buffer[self.next] = Some(byte);
            self.next = (self.next + 1) % self.buffer.capacity();
            self.size += 1;
            Ok(())
        }
    }

    pub fn clear(&mut self) {
        self.buffer = vec![None; self.buffer.capacity()];
        self.size = 0;
    }

    pub fn is_empty(&self) -> bool {
        self.size == 0
    }

    pub fn is_full(&self) -> bool {
        self.buffer[self.next].is_some()
    }
}
