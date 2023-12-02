import Foundation

struct LineReader: Sequence {
  let path: String
  fileprivate let file: UnsafeMutablePointer<FILE>!

  init(path: String) throws {
    self.path = path
    self.file = fopen(path, "r")
    guard file != nil else {
      throw NSError(domain: "Could not open the file", code: 1)
    }
  }

  func next() -> String? {
    var line: UnsafeMutablePointer<CChar>? = nil
    var linecap: Int = 0
    defer { free(line) }
    return getline(&line, &linecap, file) > 0 ? String(cString: line!).trimmingCharacters(in: .newlines) : nil
  }

  func close() {
    fclose(self.file)
  }

  func makeIterator() -> AnyIterator<String> {
    return AnyIterator<String> {
      return self.next()
    }
  }
}
