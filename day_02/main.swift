import Foundation

enum Color: String, CaseIterable {
  case blue, green, red
}

struct Cube {
  let color: Color
  let value: Int

  init?(_ descriptor: String) {
    let frags = descriptor.trimmingCharacters(in: .whitespaces).components(separatedBy: " ")
    guard
      frags.count == 2,
      let value = Int(frags[0], radix: 10),
      let color = Color(rawValue: frags[1])
    else {
      print("\u{001b}[43m Invalid cube: \(descriptor) \u{001b}[0m")
      return nil
    }
    self.color = color
    self.value = value
  }
}

typealias CubeArray = [Cube]

extension CubeArray {
  var totals: Dictionary<Color, Int> {
    get {
      return Color.allCases.reduce(into: Dictionary<Color, Int>()) {
        result, color in
          result[color] = self.filter{ $0.color == color }.compactMap{ $0.value }.reduce(0, +)
      }
    }
  }

  init(_ descriptor: String) {
    self.init()
    self.append(contentsOf: descriptor.components(separatedBy: ",").compactMap{
      Cube($0)
    })
  }
}

struct Round {
  let cubes: CubeArray

  init(_ descriptor: String) {
    self.cubes = .init(descriptor)
  }
}

typealias RoundArray = [Round]

extension RoundArray {
  init(_ descriptor: String) {
    self.init()
    self.append(contentsOf: descriptor.components(separatedBy: ";").compactMap({
      Round($0)
    }))
  }
}

struct Game {
  let id: Int
  let rounds: RoundArray

  var minimumPower: Int {
    get {
      return self.maximums.compactMap{ Int($0.value) }.reduce(1, *)
    }
  }

  private var maximums: Dictionary<Color, Int> {
    get {
      return Color.allCases.reduce(into: Dictionary<Color, Int>()) { result, color in
        result[color] = self.rounds.compactMap{ $0.cubes.totals[color] }.reduce(0) { max($0, $1) }
      }
    }
  }

  init?(_ descriptor: String) {
    let frags = descriptor.components(separatedBy: ":")
    guard
      frags.count == 2,
      let id = Int(frags[0].components(separatedBy: "Game")[1].trimmingCharacters(in: .whitespaces))
    else {
      print("\u{001b}[43m Invalid game: \(descriptor) \u{001b}[0m")
      return nil
    }
    self.id = id
    self.rounds = .init(frags[1])
  }

  func isPossible(withCubes cubes: CubeArray) -> Bool {
    return cubes.first{ self.maximums[$0.color] ?? 0 > $0.value } == nil
  }
}

var result = 0, power = 0
for line in try String(contentsOfFile: CommandLine.arguments[1], encoding: .utf8)
  .components(separatedBy: .newlines) {
  guard
    line != "",
    let game = Game(line) else {
    continue
  }

  if (game.isPossible(withCubes: .init("14 blue, 13 green, 12 red"))) {
    result += game.id
  }
  power += game.minimumPower
}
print("Part 1: \(result)")
print("Part 2: \(power)")
