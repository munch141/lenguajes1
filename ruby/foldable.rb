#!/usr/bin/ruby

class Array
    def foldr e, b
        if self == [] then
            e
        else
            head, *tail = *self
            b.call(head, tail.foldr(e,b))
        end
    end
end