#!/usr/bin/ruby

class Array
    def foldr e
        if self == [] then
            e
        else
            head, *tail = *self
            yield(head, tail.foldr(e, &Proc.new))
        end
    end
end

class Rose
    attr_accessor :elem, :children
    def initialize elem, children = []
        @elem = elem
        @children = children
    end

    def add elem
        @children.push elem
        self
    end

    def foldr e
        if children == [] then
            yield(@elem, e)
        else
            yield(
                @elem,
                children.foldr(e) do
                    |child, acc| yield(child.foldr(e, &Proc.new), acc)
                end
            )
        end
    end
end