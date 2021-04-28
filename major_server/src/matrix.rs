#[derive(Debug)]
pub struct Matrix<T> {
  pub size: (usize, usize),
  pub content: Vec<T>
}

impl<T> Matrix<T> {
  pub fn new(n: usize, m: usize, e: T) -> Matrix<T> where T: Clone {
    Matrix {
      size: (n, m),
      content: vec![e; n * m]
    }
  }

  pub fn elems(&self) -> Vec<&[T]> {
    let mut res = Vec::new();
    res.reserve(self.size.0);
    for x in 0..self.size.0 {
      let i = x * self.size.1;
      res.push(&self.content[i..i + self.size.1]);
    }
    return res;
  }

  pub fn update(&mut self, x: usize, y: usize, f: impl Fn(&T) -> T) {
    let i = x * self.size.1 + y;
    self.content[i] = f(&self.content[i]);
  }
}


