import functools as f
import operator as op

class Tree:
    def __init__(self, x):

        self.rt = x
        self.child = []

    def add_child(self, a):

        self.child.append(a)

    def root(self):

        return self.rt

    def ith_child(self, i):

        return self.child[i]

    def num_children(self):

        return len(self.child)

class Pre(Tree):
    def preorder(self):

        return f.reduce(op.add, map(lambda c: c.preorder(), self.child), [self.root()])