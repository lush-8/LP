class Tree:
    def __init__(self, x):

        self.rt = x
        self.child = []

    def addChild(self, a):

        self.child.append(a)

    def root(self):

        return self.rt

    def ithChild(self, i):

        return self.child[i]
    
    def __iter__(self):
        
        queue = [self]
        
        while queue:
            node = queue.pop(0)
            yield node.root()
            queue.extend(node.child)