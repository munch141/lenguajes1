def infimo(i1, i2)
    if i1.lower_value == i2.lower_value then
        [i1.lower_value, (i1.lower_inclusive? and i2.lower_inclusive?)]
    elsif i1.lower_value > i2.lower_value then
        [i1.lower_value, i1.lower_inclusive?]
    else
        [i2.lower_value, i2.lower_inclusive?]
    end
end

def supremo(i1, i2)
    if i1.upper_value == i2.upper_value then
        [i1.upper_value, (i1.upper_inclusive? and i2.upper_inclusive?)]
    elsif i1.upper_value < i2.upper_value then
        [i1.upper_value, i1.upper_inclusive?]
    else
        [i2.upper_value, i2.upper_inclusive?]
    end
end

class Interval
    def initialize(lower_in, lower_value, upper_value, upper_in)
        @lower = [lower_in, lower_value]
        @upper = [upper_in, upper_value]
    end

    def lower_inclusive?
        @lower[0]
    end

    def lower_exclusive?
        not @lower[0]
    end

    def lower_value
        @lower[1]
    end

    def lower_infinite?
        self.lower_value == nil
    end

    def upper_inclusive?
        @upper[0]
    end

    def upper_exclusive?
        not @upper[0]
    end

    def upper_value
        @upper[1]
    end

    def upper_infinite?
        self.upper_value == nil
    end

    def to_s
        (self.lower_inclusive? ? "[" : "(") +
        (self.lower_infinite? ? "" : self.lower_value.to_s) +
        "," +
        (self.upper_infinite? ? "" : self.upper_value.to_s) +
        (self.upper_inclusive? ? "]" : ")")
    end
end

class Literal < Interval
    def self.make_valid_literal(lower_in, lower, upper, upper_in)
        if (lower == upper and not (lower_in and upper_in)) or
           (upper < lower) then
           Empty.instance
        else
            Literal.new(lower_in, lower, upper, upper_in)
        end
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
        Literal.make_valid_literal(
            lower_in,lower,self.upper_value,self.upper_inclusive?)
    end

    def intersection_left_infinite other
        upper, upper_in = supremo(self,other)
        Literal.make_valid_literal(
            self.lower_inclusive?,self.lower_value,upper,upper_in)
    end
end

class RightInfinite < Interval
    def initialize(lower_in,lower_value)
        @lower = [lower_in,lower_value]
        @upper =[false,nil]
    end

    def intersection other
        other.intersection_right_infinite self
    end

    def intersection_literal other
        lower, lower_in = infimo(self,other)
        Literal.make_valid_literal(
            lower_in,lower,other.upper_value,other.upper_inclusive?)
    end

    def intersection_right_infinite other
        lower, lower_in = infimo(self,other)
        RightInfinite.new(lower_in,lower)
    end

    def intersection_left_infinite other
        Literal.make_valid_literal(
            self.lower_inclusive?,self.lower_value,
            other.upper_value,other.upper_inclusive?)
    end
end

class LeftInfinite < Interval
    def initialize(upper_value, upper_in)
        @lower = [false,nil]
        @upper = [upper_in, upper_value]
    end

    def intersection other
        other.intersection_left_infinite self
    end

    def intersection_literal other
        upper, upper_in = supremo(self,other)
        Literal.make_valid_literal(
            other.lower_inclusive?,other.lower_value,upper,upper_in)
    end

    def intersection_right_infinite other
        Literal.make_valid_literal(
            other.lower_inclusive?, other.lower_value,
            self.upper_value, self.upper_inclusive?)
    end

    def intersection_left_infinite other
        upper, upper_in = supremo(self,other)
        LeftInfinite.new(upper,upper_in)
    end
end

class AllReals < Interval
    def initialize
        @lower = [false,nil]
        @upper = [false,nil]
    end

    @@instance = AllReals.new

    def self.instance
        @@instance
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
    def initialize
        @lower = [false,nil]
        @upper = [false,nil]
    end

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