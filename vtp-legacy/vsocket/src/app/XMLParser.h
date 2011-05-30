#ifndef XMLPARSER_H
#define XMLPARSER_H

#include <xercesc/sax/HandlerBase.hpp>

XERCES_CPP_NAMESPACE_USE

XERCES_CPP_NAMESPACE_BEGIN
class AttributeList;
XERCES_CPP_NAMESPACE_END

#include <vector>
#include <map>

using namespace std;

/** STL-derived data structure */
typedef vector<string> SAXStack;

/** STL-derived data structure */
typedef map<string, string> SAXHash;

/** SAXTree
  * Implements the tree that is built by parsing the XML file.
  */
typedef struct SAXTree  {
	/** Member
	  * XML tag.
	  */
	string _tag;
	/** Member
	  * XML value.
	  */
	string _value;
	/** Member
	  * XML attributes.
	  */
	SAXHash _attributes;
	/** Member
	  * List of elements that are the children of this element.
	  */
	vector<SAXTree*> _children;
	/** Member
	  * Recursive routine that prints out the tree (along with formatting).
	  */
	void print(const int& n) {
		for (int i=0; i<n; ++i)
			cout << " ";
		if (_children.empty()) 
			cout << _tag << "==" << _value << endl;
		else 
			cout << _tag << endl;
		if (_attributes.size()) {
			for (int i=0; i<(n+4); ++i)
				cout << " ";
			cout << "Attributes:" << endl;
		}
		SAXHash::iterator map_iter;
		for (map_iter=_attributes.begin(); map_iter!=_attributes.end(); ++map_iter) {
			for (int i=0; i<(n+8); ++i)
				cout << " ";
			cout << map_iter->first << "==" << map_iter->second << endl;
		}
		vector<SAXTree*>::iterator vec_iter;
		for (vec_iter=_children.begin(); vec_iter!=_children.end(); ++vec_iter)
			(*vec_iter)->print(n+4);
	}
} SAXTree;

/** STL-derived data structure */
typedef vector<SAXTree*> SAXTreeStack;

/** XMLParser
  * This class uses the Xerces parser to parse an XML file. It then builds
  * a tree which is passed back to the creation context.
  */

class XMLParser: public HandlerBase
{
private:
	/** State.
	  * Indicated whether or not any erros have occurred.
	  */
    bool _errors;
	/** State.
	  * Used to store XML data in a tree.
	  */
	SAXTree& _sax_tree;
	/** State.
	  * Used to keep track of sax_tree context.
	  */
	SAXTreeStack _sax_tree_stack;
public:
	/** Constrcutor
	  * @param xmlFile name of file to be parsed.
	  * @param config reference to a SAXTree that will contain the XML data.
	  */
    XMLParser(const string& xmlFile, SAXTree& config);
	/** Destructor.
	  */
    ~XMLParser();
	/** Handler for parser event
	  * Called at the start of an XML element.
	  */
	void startElement(const XMLCh* const name, AttributeList& attributes);
	/** Handler for parser event
	  * Called at the end of an XML element.
	  */
	void endElement(const XMLCh* const name);
	/** Handler for parser event
	  * Called whenever characeters within an XML element are encountered by 
	  * the parser.
	  */
    void characters(const XMLCh* const chars, const unsigned int length);
	/** Handler for parser event
	  * Called whenever ignorable white space encountered by the parser.
	  */
    void ignorableWhitespace(const XMLCh* const chars, const unsigned int length);
	/** Utility
	  * Called to reset the document.
	  */ 
    void resetDocument();
	/** Error handling 
	 *  Called whenever a warning condition raised by the XML parser.
	 */
	void warning(const SAXParseException& exc);
	/* Error handling 
	 *  Called whenever an error condition raised by the XML parser.
	 */
    void error(const SAXParseException& exc);
	/* Error handling 
	 *  Called whenever a fatal condition raised by the XML parser.
	 */
    void fatalError(const SAXParseException& exc);
};

#endif //  XMLPARSER_H


