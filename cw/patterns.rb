# Feel free to write and use any additional functions, variables, objects, etc. as you wish


class Device
    ## THIS IS NOT CONSTANT!
    Available_moves_original = [
        ['B', 'D', 'E', 'F', 'H'],           # AAA
        ['A', 'C', 'D', 'E', 'F', 'G', 'I'], # BBB
        ['B', 'D', 'E', 'F', 'H'],           # CCC
        ['A', 'B', 'C', 'E', 'G', 'H', 'I'], # DDD
        ['A', 'B', 'C', 'D', 'F', 'G', 'H', 'I'], # E
        ['A', 'B', 'C', 'E', 'G', 'H', 'I'], # FFF
        ['B', 'D', 'E', 'F', 'H'],           # GGG
        ['A', 'C', 'D', 'E', 'F', 'G', 'I'], # HHH
        ['B', 'D', 'E', 'F', 'H']
    ].freeze
    def available_moves
      avai = Available_moves_original.map(&:clone) # a DEEP copy
      if !@board[to_num 'B']          # if someone has taken B
        avai[to_num 'A'] += ['C']
        avai[to_num 'C'] += ['A']
      end
      if !@board[to_num 'D']
        avai[to_num 'A'] += ['G']
        avai[to_num 'G'] += ['A']
      end
      if !@board[to_num 'E']
        avai[to_num 'A'] += ['I']
        avai[to_num 'I'] += ['A']
        avai[to_num 'C'] += ['G']
        avai[to_num 'G'] += ['C']
        avai[to_num 'B'] += ['H']
        avai[to_num 'H'] += ['B']
        avai[to_num 'D'] += ['F']
        avai[to_num 'F'] += ['D']
      end
      if !@board[to_num 'F']
        avai[to_num 'C'] += ['I']
        avai[to_num 'I'] += ['C']
      end
      if !@board[to_num 'H']
        avai[to_num 'G'] += ['I']
        avai[to_num 'I'] += ['G']
      end
      avai
    end
    def to_num alpha
      alpha.ord - 'A'.ord
    end
    def initialize()
      @board = [true, true, true, true, true, true, true, true, true] # all available now
    end
    def goto point
      @board[to_num point] = false # check
    end
    def uncheck point
      @board[to_num point] = true
    end
    def my_count_patterns_from (point, length)
      count = 0
      if length == 1
        1
      else
        now = available_moves
        # print now
        now[to_num point].each {
          |p|
          if @board[to_num p]
            #print p
            #print "\n"
            goto p
            count += my_count_patterns_from(p, length-1)
            uncheck p
          end
        }
        count
      end
    end
end


  def count_patterns_from(first_point, length)
    # Your code here
    if length <= 0 || length >= 9
      0
    else
      d = Device.new
      d.goto first_point
      d.my_count_patterns_from(first_point, length)
    end
  end

count_patterns_from('E', 3)