#include <xercesc/util/PlatformUtils.hpp>
#include <stdlib.h>
#include <string.h>
#if defined(XERCES_NEW_IOSTREAMS)
#include <iostream>
#else
#include <iostream.h>
#endif
#include <xercesc/parsers/SAXParser.hpp>
#if defined(XERCES_NEW_IOSTREAMS)
#include <fstream>
#else
#include <fstream.h>
#endif
#include <XMLParser.h>
#include <xercesc/sax/AttributeList.hpp>
#include <xercesc/sax/SAXParseException.hpp>
#include <xercesc/sax/SAXException.hpp>
#include <iostream.h>
#include <string>
#include <cctype>

using namespace std;

// Non-atomic elements.
const string VSOCKET_TAG("vsocket");
const string ADMIN_TAG("admin");
const string SCAN_LIST_TAG("scan_list");
const string NETWORK_TAG("network");
const string TSPEC_TAG("tspec");
const string VSIE_TAG("vsie");
const string TEST_TAG("test");
const string PROFILE_TAG("profile");

// Atomic elements.
const string HOST_NAME_TAG("host_name");
const string CONTACT_TAG("contact");
const string MODE_TAG("mode");
const string LOG_FILE_TAG("log_file");
const string DEBUG_LEVEL_TAG("debug_level");
const string SCAN_FILE_TAG("scan_file");
const string LOCAL_IP_TAG("local_ip");
const string REMOTE_IP_TAG("remote_ip");
const string LOCAL_PORT_TAG("local_port");
const string CONTROL_PORT_TAG("control_port");
const string NUM_STREAMS_TAG("num_streams");
const string TRANSPORT_PROTOCOL_TAG("transport_protocol");
const string HOST_MTU_TAG("host_mtu");
const string NETWORK_MTU_TAG("network_mtu");
const string RTT_TAG("rtt");
const string PATH_BANDWIDTH_TAG("path_bandwidth");
const string SSRC_TAG("ssrc");
const string PEAK_RATE_TAG("peak_rate");
const string MIN_RATE_TAG("min_rate");
const string CC_TAG("cc");
const string SAMPLING_FREQUENCY_TAG("sampling_frequency");
const string PACKET_SIZE_TAG("packet_size");
const string SAMPLES_PER_PACKET_TAG("samples_per_packet");
const string BITS_PER_SAMPLE_TAG("bits_per_sample");
const string TVG_TAG("tvg");
const string TVG_DURATION_TAG("tvg_duration");


// ---------------------------------------------------------------------------
//  XMLParser: Constructors and Destructor
// ---------------------------------------------------------------------------
XMLParser::XMLParser(const string& xmlFile, SAXTree& config): 
_errors(false), _sax_tree(config)
{
    bool error_occurred = false;
    SAXParser* parser = NULL;
    try
    {
    	XMLPlatformUtils::Initialize();
    	parser = new SAXParser;
    	parser->setValidationScheme(SAXParser::Val_Auto);
    	parser->setDoNamespaces(false);
    	parser->setDoSchema(false);
    	parser->setValidationSchemaFullChecking(false);
    	parser->setDocumentHandler(this);
    	parser->setErrorHandler(this);
    	parser->parse(xmlFile.c_str());
    } catch (const XMLException& e) {
    	cerr << "\nError: '" << xmlFile << "'\n"
                << "Exception message is:  \n"
                << e.getMessage() << "\n" << endl;
    } catch (...) {
    	cerr << "\nUnexpected exception during parsing: '" << xmlFile << "'\n";
        error_occurred = true;
    }
    delete parser;
    XMLPlatformUtils::Terminate();
}

XMLParser::~XMLParser()
{
}


// ---------------------------------------------------------------------------
//  XMLParser: Implementation of the SAX DocumentHandler interface
// ---------------------------------------------------------------------------
void XMLParser::startElement(const XMLCh* const name, 
							AttributeList&  attributes)
{
	char* t = XMLString::transcode(name);
	string tag(t);
	SAXTree* new_tree=NULL;
	if (tag == VSOCKET_TAG) {
		// Root of tree.
		new_tree=&_sax_tree;
	} else {
		new_tree=new SAXTree;
		SAXTree* p=_sax_tree_stack.back();
		p->_children.push_back(new_tree);
	}
	unsigned int length = attributes.getLength();
	for (unsigned int index=0; index < length; ++index) {
		char* aname = XMLString::transcode(attributes.getName(index));
		char* vname = XMLString::transcode(attributes.getValue(index));
		new_tree->_attributes[aname]=vname;
	}
	new_tree->_tag=tag;
	_sax_tree_stack.push_back(new_tree);
	XMLString::release(&t);
}

void XMLParser::endElement(const XMLCh* const)
{
	_sax_tree_stack.pop_back();
}

void XMLParser::characters(const XMLCh* const chars, 
						const unsigned int length)
{
	char* v= XMLString::transcode(chars);
	string value;
	int l=0;
	for (int i=0; i<length; ++i) {
		if (isalnum(v[i]))
			++l;
	}
	if (l>0)
		value=string(v);
	else
		value="";
	if (_sax_tree_stack.size()) {
		SAXTree* p=_sax_tree_stack.back();
		if (p) 
			p->_value=value;
	}
	XMLString::release(&v);
}

void XMLParser::ignorableWhitespace(const   XMLCh* const chars, 
									const unsigned int length)
{
}

void XMLParser::resetDocument()
{
}


// ---------------------------------------------------------------------------
//  XMLParser: Overrides of the SAX ErrorHandler interface
// ---------------------------------------------------------------------------
void XMLParser::error(const SAXParseException& e)
{
    _errors = true;
    cerr << "\nError: File " << e.getSystemId()
		 << ", Line " << e.getLineNumber()
		 << ", Char " << e.getColumnNumber()
         << "\n  Message: " << e.getMessage() << endl;
}

void XMLParser::fatalError(const SAXParseException& e)
{
    _errors = true;
    cerr << "\nFatal Error: File " << e.getSystemId()
		 << ", Line " << e.getLineNumber()
		 << ", Char " << e.getColumnNumber()
         << "\n  Message: " << e.getMessage() <<  endl;
}

void XMLParser::warning(const SAXParseException& e)
{
     cerr << "\nWarning: File " << e.getSystemId()
		 << ", Line " << e.getLineNumber()
		 << ", Char " << e.getColumnNumber()
         << "\n  Message: " << e.getMessage() <<  endl;
}

