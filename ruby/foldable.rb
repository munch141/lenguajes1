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
            if self.is_a? Rose then
                r = self.elem
                self.children.reverse_each {|child| r = child.foldr(r, &block)}
                r
            elsif self.is_a? Array then
                head, *tail = self
                tail.foldr(head, &block)
            end
        end
    end

    def length
        self.foldr(0) {|_,l| l+1}
    end

    def all? &block
        self.foldr(true) {|x,r| block.call(x) and r}
    end

    def any? &block
        self.foldr(false) {|x,r| block.call(x) or r}
    end

    def to_arr
        self.foldr([]) {|x,arr| [x].push *arr}
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
            b.call(head, tail.foldr(e, &b))
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
        i = 1
        self.foldr1 do |x,avg|
            i += 1
            (avg.to_f*(i-1)+x)/i
        end
    end
end