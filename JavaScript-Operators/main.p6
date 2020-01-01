my $tmp = "";
sub infix:<js+> ($self, $other) {
    if $self ~~ Int && $other~~Int {
        $tmp = $self ~ $other;
        return $self + $other
    }
    elsif $self ~~ Int && $other ~~ Str {
        return $tmp != "" ?? ($tmp ~ $other) !! ($self ~ $other)
    }
    elsif $self ~~ Array || $self ~~ Array {
        return $self.join(",") ~ $self.join(",")
    }
    else {
        return $self ~ $other
    }
}