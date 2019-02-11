//
//  main.swift
//  LexerProject
//
//  Created by Ahmet Gülden on 16.01.2019.
//  Copyright © 2019 Ahmet Gülden. All rights reserved.
//

import Foundation

extension String {

    func splittedBy(_ character: Character) -> [String] {
        return split(separator: character).map {String($0)}
    }

    func trimmed() -> String {
        let firstIteration = replacingOccurrences(of: "\n", with: " ")
        let secondIteration = firstIteration.replacingOccurrences(of: "\r", with: " ")
        return secondIteration.replacingOccurrences(of: "\t", with: " ")
    }

    func symbolPipedSource() -> String {
        let reservedSymbols = ["!", "/", "*", "+", "-", ":=", "==", "(", ")", ",", ";"]

        var source: String
        source = reservedSymbols.reduce(self) { (result, symbol) -> String in
            result.replacingOccurrences(of: symbol, with: " |\(symbol)| ")
        }

        source = source.replacingOccurrences(of: ">=", with: " |=*=| ")
        source = source.replacingOccurrences(of: ">", with: " |>| ")
        source = source.replacingOccurrences(of: "|=*=|", with: "|>=|")

        source = source.replacingOccurrences(of: "<=", with: " |=*=| ")
        source = source.replacingOccurrences(of: "<", with: " |<| ")
        source = source.replacingOccurrences(of: "|=*=|", with: "|<=|")

        return source
    }

    func keywordPipedSource() -> String {
        let reservedWords = ["return", "if", "else", "endi", "while", "endw",
                             "fun", "endf", "input", "output", "and", "or"];

        return reservedWords.reduce(self) { (result, word) -> String in
            let firstIteration = result.replacingOccurrences(of: " \(word) ", with: " |\(word)| ")
            let secondIteration = firstIteration.replacingOccurrences(of: "|\(word) ", with: "| |\(word)| ")
            let lastIteration = secondIteration.replacingOccurrences(of: " \(word)|", with: " |\(word)| |")
            return lastIteration
        }
    }
}

func retrieveFileContent() -> String {
    if CommandLine.arguments.count != 2 {
        fatalError("You should give source file as first argument!")
    }

    let givenFileName = CommandLine.arguments[1]
    let fileNameComponents = givenFileName.splittedBy(".")

    let fileExtension = fileNameComponents.count == 2 ? fileNameComponents.last : nil

    guard let path = Bundle.main.path(forResource: fileNameComponents.first, ofType: fileExtension) else {
        fatalError("Unable to open file: \(givenFileName)")
    }
    let content: String
    do {
        content = try String(contentsOfFile: path)
    }
    catch {
        fatalError("Unable to read from file: \(givenFileName)")
    }
    return content
}


let source = " \(retrieveFileContent().trimmed()) " // put space to head and tails
let pipedSource = source.symbolPipedSource().keywordPipedSource()
let trimmedPipedSource = pipedSource.trimmingCharacters(in: CharacterSet(charactersIn: " \n\r\t"))
let paranthesisedTrimmedPipedSource = "(\(trimmedPipedSource))"
print(paranthesisedTrimmedPipedSource)
