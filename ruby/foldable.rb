#!/usr/bin/ruby

module Foldable
    def null?
       self.foldr(true) {|_,_| false}
    end

    def foldr1 &block
        if self.null? then
            raise "empty structure"
        else
            # esto es mientras tanto para poder probar los otros métodos
            if self.class == Rose then
                self.foldr(self.elem, &block)
            else
                self.foldr(self.head, &block)
            end
        end
    end

    def length
        self.foldr(0) {|_,l| l+1}
    end

    def all &block
        self.foldr(true) {|x,r| block.call(x) and r}
    end

    def any &block
        self.foldr(false) {|x,r| block.call(x) or r}
    end

    def to_arr
        # falta
    end

    def elem_to_find
        # falta
    end
end

class Array
    include Foldable

    def foldr e, &b
        if self == [] then
            e
        else
            head, *tail = self
            yield(head, tail.foldr(e, &b))
        end
    end
end

class Rose
    attr_accessor :elem, :children
    include Foldable

    def initialize elem, children = []
        @elem = elem
        @children = children
    end

    def add elem
        @children.push elem
        self
    end

    def foldr e, &b
        r = e
        @children.reverse_each {|child| r = child.foldr(r, &b)}
        b.call(@elem, r)
    end

    def avg

    end
end