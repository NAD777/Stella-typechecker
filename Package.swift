// swift-tools-version: 5.7
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "stella-implementation-in-swift",
    dependencies: [
        // Dependencies declare other packages that this package depends on.
        // .package(url: /* package url */, from: "1.0.0"),
        .package(name: "Antlr4", url: "https://github.com/antlr/antlr4", from: "4.12.0")
    ],
    targets: [
        // Targets are the basic building blocks of a package. A target can define a module or a test suite.
        // Targets can depend on other targets in this package, and on products in packages this package depends on.
        .executableTarget(
            name: "stella-implementation-in-swift",
            dependencies: ["Antlr4"]),
        .testTarget(
            name: "stella-implementation-in-swiftTests",
            dependencies: ["stella-implementation-in-swift"],
            resources: [
                // Copy Tests/ExampleTests/Resources directories as-is.
                // Use to retain directory structure.
                // Will be at top level in bundle.
                .copy("Resources"),
              ])
    ]
)
