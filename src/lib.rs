#![cfg_attr(test, feature(box_patterns, assert_matches))]

mod diagnostic;
mod irs;
mod stages;

pub use diagnostic::Diagnostic;
pub use irs::{bitcode, bytecode, common, syntax_tree, tokens};
pub use stages::{codegen, lexing, link, lowering, parsing, resolution};

#[cfg(test)]
mod tests;

#[derive(Debug, Clone)]
pub struct Array<I: TryInto<usize>, T> {
    v: Vec<T>,
    _i: std::marker::PhantomData<I>,
}

impl<I: TryInto<usize> + TryFrom<usize>, T> Array<I, T> {
    pub fn new() -> Self {
        Self {
            v: Vec::default(),
            _i: std::marker::PhantomData,
        }
    }

    pub fn len(&self) -> I {
        unsafe { self.v.len().try_into().unwrap_unchecked() }
    }
}

impl<I: TryInto<usize> + TryFrom<usize>, T> Default for Array<I, T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<I: TryInto<usize>, T> Array<I, T> {
    fn enumerate(&self) -> Enumerate<I, T> {
        Enumerate { i: 0, v: self }
    }
}

pub struct Enumerate<'a, I: TryInto<usize>, T> {
    i: usize,
    v: &'a Array<I, T>,
}

impl<'a, I: TryInto<usize> + TryFrom<usize>, T> std::iter::Iterator for Enumerate<'a, I, T> {
    type Item = (I, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        let i: I = match self.i.try_into() {
            Ok(i) => i,
            Err(_) => panic!("Too much stuff!"),
        };
        self.v.get(self.i).map(|e| {
            self.i += 1;
            (i, e)
        })
    }
}

impl<I: TryInto<usize>, T> std::ops::Index<I> for Array<I, T> {
    type Output = T;

    fn index(&self, i: I) -> &Self::Output {
        if let Ok(i) = i.try_into() {
            &self.v[i]
        } else {
            panic!("Too many thingies!")
        }
    }
}

impl<I: TryInto<usize>, T> std::ops::IndexMut<I> for Array<I, T> {
    fn index_mut(&mut self, i: I) -> &mut Self::Output {
        if let Ok(i) = i.try_into() {
            &mut self.v[i]
        } else {
            panic!("Too many thingies!")
        }
    }
}

impl<I: TryInto<usize>, T> std::ops::Deref for Array<I, T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.v
    }
}

impl<I: TryInto<usize>, T> std::ops::DerefMut for Array<I, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.v
    }
}
