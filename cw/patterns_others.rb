class Dots
  ALL_DOTS = %w(A B C D E F H G I)

  HIDDEN_FROM_A = { 'B' => 'C', 'E' => 'I', 'D' => 'G' }
  HIDDEN_FROM_B = { 'E' => 'H' }
  HIDDEN_FROM_C = { 'B' => 'A', 'E' => 'G', 'F' => 'I' }
  HIDDEN_FROM_D = { 'E' => 'F' }
  HIDDEN_FROM_E = {}
  HIDDEN_FROM_F = { 'E' => 'D' }
  HIDDEN_FROM_G = { 'D' => 'A', 'E' => 'C', 'H' => 'I' }
  HIDDEN_FROM_H = { 'E' => 'B' }
  HIDDEN_FROM_I = { 'H' => 'G', 'E' => 'A', 'F' => 'C' }

  def self.hidden_from(dot, already_used)
    paths = Dots.const_get("HIDDEN_FROM_#{dot}")
    paths.values - paths.values_at(*already_used).compact
  end
end

def count_patterns_from(next_dot, length, already_used = [next_dot])
  return 0 if length < 1 || length > 9
  return 1 if length == 1
  visibles = Dots::ALL_DOTS - Dots.hidden_from(next_dot, already_used) - already_used
  visibles.map { |dot| count_patterns_from(dot, length - 1, already_used + [dot]) }.inject(&:+)
end