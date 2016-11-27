#!/usr/bin/ruby

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

def minimo(i1,i2)
    if (i1.lower_value == i2.lower_value) then
        [i1.lower_value, (i1.lower_inclusive? or i2.lower_inclusive?)]
    else
        i1.lower_value < i2.lower_value ?
            [i1.lower_value,i1.lower_inclusive?] :
            [i2.lower_value,i2.lower_inclusive?]
    end
end

def maximo(i1,i2)
    if (i1.upper_value == i2.upper_value) then
        [i1.upper_value, (i1.upper_inclusive? or i2.upper_inclusive?)]
    else
        i1.upper_value > i2.upper_value ?
            [i1.upper_value,i1.upper_inclusive?] :
            [i2.upper_value,i2.upper_inclusive?]
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

    def is_empty?
        self == Empty.instance
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

    def union other
        other.union_literal self
    end

    def union_literal other
        if self.intersection(other).is_empty? then
            if (self.lower_value == other.upper_value) and
               (self.lower_inclusive? or other.upper_inclusive?) then
                Literal.make_valid_literal(
                    other.lower_inclusive?,other.lower_value,
                    self.upper_value,self.upper_inclusive?)
            elsif (self.upper_value == other.lower_value) and
                  (self.upper_inclusive? or other.lower_inclusive?) then
                Literal.make_valid_literal(
                    self.lower_inclusive?,self.lower_value,
                    other.upper_value,other.upper_inclusive?)
            else
                raise ("la unión de " + self.to_s + " y " + other.to_s +
                       " no es contigua")
            end
        else
            lower,lower_in = minimo(self,other)
            upper,upper_in = maximo(self,other)
            Literal.make_valid_literal(lower_in, lower, upper, upper_in)
        end
    end

    def union_right_infinite other
        if self.intersection(other).is_empty? then
            if (self.upper_value == other.lower_value) and
               (self.upper_inclusive? or other.lower_inclusive?) then
                RightInfinite.new(self.lower_inclusive?,self.lower_value)
            else
                raise ("la unión de " + self.to_s + " y " + other.to_s +
                       " no es contigua")
            end
        else
            lower,lower_in = minimo(self,other)
            RightInfinite.new(lower_in, lower)
        end
    end

    def union_left_infinite other
        if self.intersection(other).is_empty? then
            if (self.lower_value == other.upper_value) and
               (self.lower_inclusive? or other.upper_inclusive?) then
                LeftInfinite.new(self.upper_value,self.upper_inclusive?)
            else
                raise ("la unión de " + self.to_s + " y " + other.to_s +
                       " no es contigua")
            end
        else
            upper,upper_in = maximo(self,other)
            LeftInfinite.new(upper, upper_in)
        end
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

    def union other
        other.union_right_infinite self
    end

    def union_literal other
        if self.intersection(other).is_empty? then
            if (other.upper_value == self.lower_value) and
               (other.upper_inclusive? or self.lower_inclusive?) then
                RightInfinite.new(other.lower_inclusive?,other.lower_value)
            else
                raise ("la unión de " + self.to_s + " y " + other.to_s +
                       " no es contigua")
            end
        else
            lower,lower_in = minimo(self,other)
            RightInfinite.new(lower_in, lower)
        end
    end

    def union_right_infinite other
        lower,lower_in = minimo(self,other)
        RightInfinite.new(lower_in, lower)
    end

    def union_left_infinite other
        if self.intersection(other).is_empty? then
            if not ((other.upper_value == self.lower_value) and
                    (other.upper_inclusive? or self.lower_inclusive?)) then
                raise ("la unión de " + self.to_s + " y " + other.to_s +
                       " no es contigua")
            end
        end

        AllReals.instance
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

    def union other
        other.union_left_infinite self
    end

    def union_literal other
        if self.intersection(other).is_empty? then
            if (other.lower_value == self.upper_value) and
               (other.lower_inclusive? or self.upper_inclusive?) then
                LeftInfinite.new(other.upper_value,other.upper_inclusive?)
            else
                raise ("la unión de " + self.to_s + " y " + other.to_s +
                       " no es contigua")
            end
        else
            upper,upper_in = maximo(self,other)
            LeftInfinite.new(upper, upper_in)
        end
    end

    def union_right_infinite other
        if self.intersection(other).is_empty? then
            if not ((self.upper_value == other.lower_value) and
                    (self.upper_inclusive? or other.lower_inclusive?)) then
                raise ("la unión de " + self.to_s + " y " + other.to_s +
                       " no es contigua")
            end
        end

        AllReals.instance
    end

    def union_left_infinite other
        upper,upper_in = maximo(self,other)
        LeftInfinite.new(upper, upper_in)
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

    def union other
        self.instance
    end

    def union_literal other
        self.instance
    end

    def union_right_infinite other
        self.instance
    end

    def union_left_infinite other
        self.instance
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

    def union other
        other
    end

    def union_literal other
        other
    end

    def union_right_infinite other
        other
    end

    def union_left_infinite other
        other
    end

    private_class_method :new
end


class Array
    def each_ineq_op &b
        aux = self
        begin
            b.call(aux[1,3], aux[3])
            aux = aux.drop(4)
        end while not aux.empty?
    end
end

def get_interval ineq
    case ineq[0]
        when "<"
            LeftInfinite.new(ineq[1].to_f, false)
        when "<="
            LeftInfinite.new(ineq[1].to_f, true)
        when ">"
            RightInfinite.new(false, ineq[1].to_f)
        when ">="
            RightInfinite.new(true, ineq[1].to_f)
        else
            raise "comparador inválido: " + ineq[0]
        end
end

def parse_line l
    pila_or = []
    pila_and = []
    interval = nil
    
    l.each_ineq_op do |ineq,op|
        interval = get_interval(ineq)
        
        if op == "&" then
            pila_and.push(interval)
        elsif op == "|" then
            if not pila_and.empty? then
                pila_and.push(interval)
                interval = AllReals.instance
                pila_and.each {|x| interval = interval.intersection(x)}
                pila_and = []
            end
            pila_or.push(interval)
        end
    end

    if not pila_and.empty? then
        pila_and.push(interval)
        acc = AllReals.instance
        pila_and.each {|x| acc = acc.intersection(x)}
        acc
    elsif not pila_or.empty? then
        pila_or.push(interval)
        acc = Empty.instance
        pila_or.each {|x| acc = acc.union(x)}
        acc
    else
        interval
    end
end

# MAIN!

variables = Hash.new
text = File.open(ARGV[0]).read
text.each_line do |line|
    l = line.split
    i = parse_line l
    if variables.has_key? l[0] then
        variables[l[0]] = variables[l[0]].union(i)
    else
        variables[l[0]] = i
    end
end

variables.each_with_index {|x| puts x[0].to_s + " in " + x[1].to_s}

while line != "exit"
    
end