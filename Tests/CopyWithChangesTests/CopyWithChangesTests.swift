import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros
import SwiftSyntaxMacrosTestSupport
import XCTest
import CopyWithChanges

// Macro implementations build for the host, so the corresponding module is not available when cross-compiling. Cross-compiled tests may still make use of the macro itself in end-to-end tests.
#if canImport(CopyWithChangesMacros)
import CopyWithChangesMacros

let testMacros: [String: Macro.Type] = [
    "CopyWithChanges": CopyWithChangesMacro.self,
]
#endif

// swift-syntax 509's formatter prints `Self (` for the generated initializer call; 600+ prints `Self(`.
#if canImport(SwiftSyntax600)
private let selfCall = "Self("
#else
private let selfCall = "Self ("
#endif

final class CopyWithChangesTests: XCTestCase {
    func testExpandsStoredPropertiesInDeclarationOrder() throws {
        #if canImport(CopyWithChangesMacros)
        assertMacroExpansion(
            """
            @CopyWithChanges
            struct Report {
                let venue: String
                let sponsor: String?
                let drinks: [String]
                let complexStructure: [Date: [(String, Int)]]
                let characters: [String]?
                let budget: Double
            }
            """,
            expandedSource: """
            struct Report {
                let venue: String
                let sponsor: String?
                let drinks: [String]
                let complexStructure: [Date: [(String, Int)]]
                let characters: [String]?
                let budget: Double

                public func with(venue: String? = nil, sponsor: String?? = nil, drinks: [String]? = nil, complexStructure: [Date: [(String, Int)]]? = nil, characters: [String]?? = nil, budget: Double? = nil) -> Self {
                    let _sponsor: String? = sponsor ?? self.sponsor
                    let _characters: [String]? = characters ?? self.characters
                    return \(selfCall)
                        venue: venue ?? self.venue,
                        sponsor: _sponsor,
                        drinks: drinks ?? self.drinks,
                        complexStructure: complexStructure ?? self.complexStructure,
                        characters: _characters,
                        budget: budget ?? self.budget
                    )
                }
            }
            """,
            macros: testMacros
        )
        #else
        throw XCTSkip("macros are only supported when running tests for the host platform")
        #endif
    }

    func testSkipsStaticAndComputedProperties() throws {
        #if canImport(CopyWithChangesMacros)
        assertMacroExpansion(
            """
            @CopyWithChanges
            struct Team {
                static let maxLength = 30
                let id: Int
                var name: String
                var isLong: Bool { name.count > 10 }
            }
            """,
            expandedSource: """
            struct Team {
                static let maxLength = 30
                let id: Int
                var name: String
                var isLong: Bool { name.count > 10 }

                public func with(id: Int? = nil, name: String? = nil) -> Self {
                    return \(selfCall)
                        id: id ?? self.id,
                        name: name ?? self.name
                    )
                }
            }
            """,
            macros: testMacros
        )
        #else
        throw XCTSkip("macros are only supported when running tests for the host platform")
        #endif
    }

    func testSupportsClasses() throws {
        #if canImport(CopyWithChangesMacros)
        assertMacroExpansion(
            """
            @CopyWithChanges
            final class Box {
                let value: Int
            }
            """,
            expandedSource: """
            final class Box {
                let value: Int

                public func with(value: Int? = nil) -> Self {
                    return \(selfCall)
                        value: value ?? self.value
                    )
                }
            }
            """,
            macros: testMacros
        )
        #else
        throw XCTSkip("macros are only supported when running tests for the host platform")
        #endif
    }

    func testRejectsEnums() throws {
        #if canImport(CopyWithChangesMacros)
        assertMacroExpansion(
            """
            @CopyWithChanges
            enum Mode {
                case a
            }
            """,
            expandedSource: """
            enum Mode {
                case a
            }
            """,
            diagnostics: [
                DiagnosticSpec(message: "'@CopyWithChanges' can only be applied to a struct or a class", line: 1, column: 1),
            ],
            macros: testMacros
        )
        #else
        throw XCTSkip("macros are only supported when running tests for the host platform")
        #endif
    }

    // End-to-end: the generated `with` keeps unspecified fields and replaces specified ones.
    // A `nil` argument for an optional field means "keep the current value", not "clear it".
    func testWithCopiesAndOverrides() {
        let original = Sample(id: 1, title: "Run", note: "keep", tags: ["a"])

        XCTAssertEqual(original.with(), original)
        XCTAssertEqual(original.with(title: "Walk"), Sample(id: 1, title: "Walk", note: "keep", tags: ["a"]))
        XCTAssertEqual(original.with(note: "new"), Sample(id: 1, title: "Run", note: "new", tags: ["a"]))
        XCTAssertEqual(original.with(note: nil), original)
        XCTAssertEqual(original.with(id: 2, tags: []), Sample(id: 2, title: "Run", note: "keep", tags: []))
    }
}

@CopyWithChanges
struct Sample: Equatable {
    let id: Int
    let title: String
    let note: String?
    let tags: [String]
}
