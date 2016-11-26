class Interval
end

def infimo(i1, i2)
    if i1.lower == i2.lower then
        [i1.lower, (i1.lower_in and i2.lower_in)]
    elsif i1.lower > i2.lower then
        [i1.lower, i1.lower_in]
    else
        [i2.lower, i2.lower_in]
    end
end

def supremo(i1, i2)
    if i1.upper == i2.upper then
        [i1.upper, (i1.upper_in and i2.upper_in)]
    elsif i1.upper < i2.upper then
        [i1.upper, i1.lower_in]
    else
        [i2.upper, i2.upper_in]
    end
end

class Literal < Interval
    attr_accessor :lower_in, :lower, :upper, :upper_in

    def initialize(lower_in, lower, upper, upper_in)
        @lower_in = lower_in
        @lower = lower
        @upper = upper
        @upper_in = upper_in
    end

    def self.make_valid_literal(lower_in, lower, upper, upper_in)
        if (lower == upper and not (lower_in and upper_in)) or
           (upper < lower) then
           Empty.instance
        else
            Literal.new(lower_in, lower, upper, upper_in)
        end
    end
    
    def to_s
        (@lower_in ? "[" : "(") +
        (@lower.to_s) +
        "," +
        (@upper.to_s) +
        (@upper_in ? "]" : ")")
    end

    def intersection other
        other.intersection_literal self
    end

    def intersection_literal other
        lower, lower_in = infimo(self,other)
        upper, upper_in = supremo(self,other)
        Literal.make_valid_literal(lower_in, lower, upper, upper_in)
    end

    def intersection_right_infinite other
        lower, lower_in = infimo(self,other)
        Literal.make_valid_literal(lower_in,lower,self.upper,self.upper_in)
    end

    def intersection_left_infinite other
        upper, upper_in = supremo(self,other)
        Literal.make_valid_literal(self.lower_in,self.lower,upper,upper_in)
    end
end

class RightInfinite < Interval
    attr_accessor :lower_in, :lower

    def initialize(lower_in, lower)
        @lower_in = lower_in
        @lower = lower
    end
    
    def to_s
        @lower_in ? "[" : "(" + @lower.to_s + ",)"
    end

    def intersection other
        other.intersection_right_infinite self
    end

    def intersection_literal other
        lower, lower_in = infimo(self,other)
        Literal.make_valid_literal(lower_in,lower,other.upper,other.upper_in)
    end

    def intersection_right_infinite other
        lower, lower_in = infimo(self,other)
        RightInfinite.new(lower_in,lower)
    end

    def intersection_left_infinite other
        Literal.make_valid_literal(self.lower_in, self.lower,
                                   other.upper, other.upper_in)
    end
end

class LeftInfinite < Interval
    attr_accessor :upper, :upper_in

    def initialize(upper, upper_in)
        @upper = upper
        @upper_in = upper_in
    end
    
    def to_s
        "(," + @upper.to_s + @upper_in ? "]" : ")"
    end

    def intersection other
        other.intersection_left_infinite self
    end

    def intersection_literal other
        upper, upper_in = supremo(self,other)
        Literal.make_valid_literal(other.lower_in,other.lower,upper,upper_in)
    end

    def intersection_right_infinite other
        Literal.make_valid_literal(other.lower_in, other.lower,
                                   self.upper, self.upper_in)
    end

    def intersection_left_infinite other
        upper, upper_in = supremo(self,other)
        LeftInfinite.new(upper,upper_in)
    end
end

class AllReals < Interval
    @@instance = AllReals.new

    def self.instance
        @@instance
    end

    def to_s
        "(,)"
    end

    def intersection other
        other
    end

    def intersection_literal other
        other
    end

    def intersection_right_infinite other
        other
    end

    def intersection_left_infinite other
        other
    end

    private_class_method :new
end

class Empty < Interval
    @@instance = Empty.new

    def self.instance
        @@instance
    end

    def to_s
        "empty"        
    end

    def intersection other
        self.instance
    end

    def intersection_literal other
        self.instance
    end

    def intersection_right_infinite other
        self.instance
    end

    def intersection_left_infinite other
        self.instance
    end

    private_class_method :new
end