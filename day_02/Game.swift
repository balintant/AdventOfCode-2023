import Foundation

// MARK: Enums

enum Color: String, CaseIterable {
  case blue, green, red
}

// MARK: Game Types

struct Game {
  let id: Int
  let rounds: RoundArray

  var minimumPower: Int {
    self.maximums.compactMap{ Int($0.value) }.reduce(1, *)
  }

  private var maximums: Dictionary<Color, Int> {
    Color.allCases.reduce(into: Dictionary<Color, Int>()) { result, color in
      result[color] = self.rounds.compactMap{ $0.cubes.totals[color] }.reduce(0) { max($0, $1) }
    }
  }

  init(_ descriptor: String) throws {
    let frags = descriptor.components(separatedBy: ":")
    guard
      frags.count == 2,
      let id = Int(frags[0].components(separatedBy: "Game")[1].trimmingCharacters(in: .whitespaces))
    else {
      throw NSError(domain: "Invalid game descriptor: `\(descriptor)`", code: 1)
    }
    self.id = id
    self.rounds = try .init(frags[1])
  }

  func isPossible(withCubes cubes: CubeArray) -> Bool {
    return cubes.first{ self.maximums[$0.color] ?? 0 > $0.value } == nil
  }
}

// MARK: Round Types

struct Round {
  let cubes: CubeArray

  init(_ descriptor: String) throws {
    self.cubes = try .init(descriptor)
  }
}

typealias RoundArray = [Round]

extension RoundArray {
  init(_ descriptor: String) throws {
    self.init()
    self.append(contentsOf: try descriptor.components(separatedBy: ";").compactMap({
      try Round($0)
    }))
  }
}

// MARK: Cube Types

struct Cube {
  let color: Color
  let value: Int

  init(_ descriptor: String) throws {
    let frags = descriptor.trimmingCharacters(in: .whitespaces).components(separatedBy: " ")
    guard
      frags.count == 2,
      let value = Int(frags[0], radix: 10),
      let color = Color(rawValue: frags[1])
    else {
      throw NSError(domain: "Invalid cube descriptor: `\(descriptor)`", code: 1)
    }
    self.color = color
    self.value = value
  }
}

typealias CubeArray = [Cube]

extension CubeArray {
  var totals: Dictionary<Color, Int> {
    Color.allCases.reduce(into: Dictionary<Color, Int>()) { result, color in
      result[color] = self.filter{ $0.color == color }.compactMap{ $0.value }.reduce(0, +)
    }
  }

  init(_ descriptor: String) throws {
    self.init()
    self.append(contentsOf: try descriptor.components(separatedBy: ",").compactMap{
      try Cube($0)
    })
  }
}
