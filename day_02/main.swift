import Foundation

func processArguments() throws -> (inputPath: String, targetCubes: CubeArray) {
  let inputPath: String = CommandLine.arguments[1]
  guard inputPath != "" else {
    throw NSError(domain: "No input file specified.", code: 1)
  }
  let targetCubes = CommandLine.arguments.suffix(CommandLine.arguments.count - 2).joined(separator: " ")
  guard targetCubes != "" else {
    throw NSError(domain: "No target cubes specified.", code: 1)
  }
  return (inputPath, try CubeArray(targetCubes))
}

func processGames(rawGames: any Sequence<String>, targetCubes: CubeArray) throws -> (result: Int, power: Int) {
  var result = 0, power = 0
  for rawGame in rawGames where !rawGame.isEmpty {
    let game = try Game(rawGame)
    if game.isPossible(withCubes: targetCubes) {
      result += game.id
    }
    power += game.minimumPower
  }
  return (result, power)
}

do {
  let (inputPath, targetCubes) = try processArguments()
  let reader = try LineReader(path: inputPath)
  defer { reader.close() }
  let (result, power) = try processGames(rawGames: reader, targetCubes: targetCubes)
  print("Part 1: \(result)")
  print("Part 2: \(power)")
} catch {
  print("ERROR: \(error.localizedDescription)")
  exit(1)
}
