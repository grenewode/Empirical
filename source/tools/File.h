/**
 *  @note This file is part of Empirical, https://github.com/devosoft/Empirical
 *  @copyright Copyright (C) Michigan State University, MIT Software license; see doc/LICENSE.md
 *  @date 2017
 *
 *  @file  File.h
 *  @brief The File object maintains a simple, in-memory file.
 *  @note Status: BETA
 *
 *  @todo We need to modify this code to make sure File can also work with Emscripten,
 *        appropriately. Alternatively, we might want to have a more flexible file class
 *        that wraps this one.
 *
 */


#ifndef EMP_FILE_H
#define EMP_FILE_H

#include <fstream>
#include <functional>
#include <iostream>
#include <string>

#include "../base/vector.h"
#include "string_utils.h"

namespace emp {

  /// A class to maintin files for loading, writing, storing, and easy access to components.
  class File {
  protected:
    emp::vector<std::string> lines;

  public:
    File() : lines() { ; }
    File(std::istream & input) : lines() { Load(input); }
    File(const std::string & filename) : lines() { Load(filename); }
    File(const File &) = default;
    File(File &&) = default;
    ~File() { ; }

    File & operator=(const File &) = default;
    File & operator=(File &&) = default;

	// Return const iterator to beginning of file
    auto begin() const { return std::begin(lines); }

	// Return const iterator to end of file
    auto end() const { return std::end(lines); }

	// Return iterator to beginning of file
    auto begin() { return std::begin(lines); }

	// Return iterator to end of file
    auto end() { return std::end(lines); }

    /// How many lines are in this file?
    size_t GetNumLines() const { return lines.size(); }

    /// Compatibility with size()
    size_t size() const { return lines.size(); }

    /// Index into a specific line in this file.
    std::string & operator[](size_t pos) { return lines[pos]; }

    /// Const index into a specific line in this file.
    const std::string & operator[](size_t pos) const { return lines[pos]; }

    /// Return the first line in the file.
    std::string & front() { return lines.front(); }

    /// Return a const reference to to the first line in the file.
    const std::string & front() const { return lines.front(); }

    /// Return the last line in the file.
    std::string & back() { return lines.back(); }

    /// Return a const reference to the last line in the file.
    const std::string & back() const { return lines.back(); }

    /// Append a new line to the end of the file.
    File & Append(const std::string & line) { lines.emplace_back(line); return *this; }

    /// Append a vector of lines to the end of the file.
    File & Append(const emp::vector<std::string> & in_lines) {
      size_t start_size = lines.size();
      lines.resize(start_size + in_lines.size());
      for (size_t pos = 0; pos < in_lines.size(); pos++) {
        lines[start_size+pos] = in_lines[pos];
      }
      return *this;
    }

	// Append 2 files
    File & Append(const File & in_file) { return Append(in_file.lines); }

    /// Append to the end of a file.
    template <typename T>
    File & operator+=(T && in) { Append( std::forward<T>(in) ); return *this; }

	// Insert formatted data into file
	// This is exactly the same as operator+=
    template <typename T> auto operator<<(T &&in) {
      Append(std::forward<T>(in));
      return *this;
    }

	// Extract first line from file
    auto operator>>(std::string &out) {
      out = size() ? front() : out;
      lines.erase(begin());
    }

    /// Test if two files are identical.
    bool operator==(const File in) { return lines == in.lines; }

    /// Test if two files are different.
    bool operator!=(const File in) { return lines != in.lines; }

    /// Load a line from an input stream into a file.
    File & LoadLine(std::istream & input) {
      lines.emplace_back("");
      std::getline(input, lines.back());

      // If the input file is DOS formatted, make sure to remove the \r at the end of each line.
      if (lines.back().size() && lines.back().back() == '\r') lines.back().pop_back();

      return *this;
    }

    /// Load an entire input stream into a file.
    File & Load(std::istream & input) {
      while (!input.eof()) {
	      LoadLine(input);
      }
      return *this;
    }

    /// Load a file from disk using the provided name.
	// If file does not exist, this is a nop
    File & Load(const std::string & filename) {
      std::ifstream file(filename);
      if (file.is_open()) {
        Load(file);
        file.close();
      }
      return *this;
    }

    /// Write this file to a provided output stream.
    File & Write(std::ostream & output) {
      for (std::string & cur_line : lines) {
	      output << cur_line << '\n';
      }
      return *this;
    }

    /// Write this file to a file of the provided name.
    File & Write(const std::string & filename) {
      std::ofstream file(filename);
      Write(file);
      file.close();
      return *this;
    }

    /// Apply a string manipulation function to all lines in the file.
    File & Apply(const std::function<void(std::string &)> & fun) {
      for (std::string & cur_line : lines) {
        fun(cur_line);
      }
      return *this;
    }

    /// Purge functions that don't meet a certain criterion.
    File & KeepIf(const std::function<bool(const std::string &)> & fun) {
      emp::vector<std::string> new_lines;
      for (std::string & cur_line : lines) {
        if (fun(cur_line)) new_lines.emplace_back(cur_line);
      }
      std::swap(lines, new_lines);
      return *this;
    }

    /// Remove all lines that are empty strings.
    File & RemoveEmpty() {
      return KeepIf( [](const std::string & str){ return (bool) str.size(); } );
    }

    /// Any time multiple whitespaces are next to each other, collapse to a single WS char.
    /// Prefer '\n' if in whitespace collapsed, otherwise use ' '.
    File & CompressWhitespace() {
      Apply(compress_whitespace);
      RemoveEmpty();
      return *this;
    }

    /// Delete all whitespace; by default keep newlines.
    File & RemoveWhitespace(bool keep_newlines=true) {
      Apply(remove_whitespace);
      RemoveEmpty();
      if (!keep_newlines) {
        std::string all_lines;
        for (const std::string & cur_line : lines){
          all_lines += cur_line;
        }
        lines.resize(1);
        std::swap(lines[0], all_lines);
      }
      return *this;
    }

    /// A technique to remove all comments in a file.
    File & RemoveComments(const std::string & marker) {
      Apply( [marker](std::string & str) {
        size_t pos = str.find(marker);
        if (pos !=std::string::npos) str.resize( pos );
      } );
      return *this;
    }
  };

}

#endif
