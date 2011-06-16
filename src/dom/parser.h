/*
 * Created by David Lapsley on Mon Jun 6 2011.
 *
 * Copyright 2011 MIT Haystack Observatory 
 *  
 * This file is part of mark6.
 *
 * mark6 is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * mark6 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with mark6.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifndef _PARSER_H_
#define _PARSER_H_

// C includes.
#include <assert.h>

// C++ includes.
#include <vector>
#include <list>
#include <string>
#include <iostream>

// Boost includes.
#include <boost/foreach.hpp>
#include <boost/regex.hpp>

// Local includes.
#include<Mark6.h>

using namespace std;

// Class for containing parsed CDR records.
class CdrRecord {
 public:
  // Default constructor.
  CdrRecord() {
  }

  // Copy constructor.
  // @param r Object to copy.
  CdrRecord(const CdrRecord& r) {
    BOOST_FOREACH(string s, r.fields()) {
      insert(s);
    }
  }

  // Insert field into record.
  // @param s Field to insert.
  void insert(string s) {
    _fields.push_back(s);
  }

  // Insert field into record.
  // @param s Field to insert.
  const string& extract(size_t i) const {
    return _fields.at(i);
  }

  // Clear all of the fields.
  void clear() { _fields.clear(); }

  // Return constant reference to fields vector.
  const vector<string>& fields() const {
    return _fields;
  }

  // Dump record.
  void dump() {
    cout << "CdrRecord {" << endl;
    int  i = 0; 
    BOOST_FOREACH(string s, _fields) {
      cout << "[" << i++ << "]" << s << " ";
    }
    cout << "}" << endl;
  }
 private:
  // Vector containg field information.
  vector<string> _fields;
}; 

// Simple CDR line parser.
struct CdrParser {

  typedef vector<string> RecordType;
  typedef list<RecordType> RecordListType;

  // Constructor. Parser grammar defined here.
  // Boilerplate grammar definition.
  CdrParser() {
    
  }

  // Parse a line.
  RecordType& proc(const string& l) {
    boost::regex CMD_RE_STRING("(\\w+)=((\\w+)(:\\w+)*);");
    boost::regex QRY_RE_STRING("(\\w+)\\?((\\w+)(:\\w+)*)*;");
    boost::regex EXIT_RE_STRING("exit;");

    
    _record.clear();
  }

private:
  // The current record.
  RecordType _record;

  // Current record list for batch parsing.
  RecordListType _record_list;
};


#endif // _PARSER_H_
