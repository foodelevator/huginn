use std::{marker::PhantomData, ops};

pub trait Index: TryInto<usize> + TryFrom<usize> {}

impl<I: TryInto<usize> + TryFrom<usize>> Index for I {}

#[derive(Debug, Clone)]
pub struct Array<I: Index, T> {
    v: Vec<T>,
    _i: PhantomData<I>,
}

impl<I: Index, T> Array<I, T> {
    pub fn new() -> Self {
        Self {
            v: Vec::default(),
            _i: PhantomData,
        }
    }

    pub fn push(&mut self, val: T) -> I {
        let res = from(self.v.len());
        self.v.push(val);
        res
    }

    pub fn keys(&self) -> impl Iterator<Item = I> {
        (0..self.v.len()).map(from)
    }

    pub fn values(&self) -> impl Iterator<Item = &T> {
        self.v.iter()
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.v.iter_mut()
    }

    pub fn into_values(self) -> impl Iterator<Item = T> {
        self.v.into_iter()
    }

    pub fn enumerate(&self) -> Enumerate<I, T> {
        Enumerate { i: 0, v: self }
    }
}

impl<I: Index, T> FromIterator<T> for Array<I, T> {
    fn from_iter<Iter: IntoIterator<Item = T>>(iter: Iter) -> Self {
        let iter = iter.into_iter();
        let cap = iter.size_hint().1.unwrap_or(0);

        let mut v = Vec::with_capacity(cap);
        for e in iter {
            v.push(e);
        }

        Self { v, _i: PhantomData }
    }
}

impl<I: Index, T> Default for Array<I, T> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Enumerate<'a, I: Index, T> {
    i: usize,
    v: &'a Array<I, T>,
}

impl<'a, I: Index, T> Iterator for Enumerate<'a, I, T> {
    type Item = (I, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        let i: I = from(self.i);
        self.v.get(self.i).map(|e| {
            self.i += 1;
            (i, e)
        })
    }
}

impl<I: Index, T> ops::Index<I> for Array<I, T> {
    type Output = T;

    fn index(&self, i: I) -> &Self::Output {
        &self.v[into(i)]
    }
}

impl<I: Index, T> ops::IndexMut<I> for Array<I, T> {
    fn index_mut(&mut self, i: I) -> &mut Self::Output {
        &mut self.v[into(i)]
    }
}

impl<I: Index, T> ops::Deref for Array<I, T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.v
    }
}

impl<I: Index, T> ops::DerefMut for Array<I, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.v
    }
}

fn from<I: TryFrom<usize>>(n: usize) -> I {
    unsafe { n.try_into().unwrap_unchecked() }
}

fn into<I: TryInto<usize>>(i: I) -> usize {
    unsafe { i.try_into().unwrap_unchecked() }
}
