/*
 *  Options.cpp
 *  vrtp
 *
 *  Created by David Lapsley on Mon Feb 23 2004.
 *  Copyright (c) 2004 __MyCompanyName__. All rights reserved.
 *
 */

#ifdef LINUX
#include <unistd.h>
#endif // LINUX

#include <iostream>
#include <Options.h>

#ifdef XMLPARSING
#include <XMLParser.h>
#endif

Options::Options(int argc, char* const argv[])
{
  char opt;
  opterr = 0;
  if (argc<2) {
    Options::usage(cerr);
    exit(1);
  }
  // Defaults.
  _mode = TVG;
  _debug_level = 0;
  
  _remote_ip = "2.2.2.2";
  _data_port = 5010;
  _control_port = 5011;
  _host_mtu = 1024;
  _network_mtu = 1024;
  _max_window = 1000;
  _max_bucket = 20;
  _ssrc = 3;
  _tspec._min_rate = 1;
  _tspec._peak_rate = 1; 

  _tvg_duration = 30;

  _from_file="in.dat";
  _to_file="out.dat";

  while ((opt=getopt(argc, (char*const*)argv, 
                     "m:p:C:i:f:t:d:n:r:s:S:R:B:v:P:X:")) != -1){
    switch (opt){
    case 'm':
      /* Mode string */
      if (strcmp(optarg, "FILE2NET")==0) {
        _mode = FILE2NET;
      } else if (strcmp(optarg, "IN2NET")==0) {
        _mode==IN2NET;
      } else if (strcmp(optarg, "NET2FILE")==0) {
        _mode==NET2FILE;
      } else if (strcmp(optarg,"NET2OUT")==0) {
        _mode==NET2OUT;
      } else if (strcmp(optarg, "TVG2NET")==0) {
        _mode==TVG2NET;
      } else if (strcmp(optarg, "NET2TVG")==0) {
        _mode=NET2TVG;
      } else {
        usage(cerr);
      }
      break;
    case 'p':
      /* Data port */
      _data_port = atoi(optarg);
      break;
    case 'C':
      /* Control port */
      _control_port=atoi(optarg); 
      break;
    case 'i':
      /* Remote IP */
      _remote_ip=string(optarg);
      break;
    case 'f':
      /* From file name */
      _from_file=string(optarg);
      break;
    case 't':
      /* To file name */
      _to_file=string(optarg);
      break;
    case 'd':
      /* Debug level */
      _debug_level = atoi(optarg);
      break;
    case 's':
      /* Client side mtu */
      _host_mtu = atoi(optarg);
      break;
    case 'S':
      /* Network side mtu */
      _network_mtu = atoi(optarg);
      break;
    case 'R':
      /* Peak rate */
      _tspec._peak_rate=atoi(optarg);
      break;
    case 'v':
      /* Test Vector Generator duration */
      _tvg_duration=atof(optarg);
      break;
    case 'P':
      /* Protocol for transport */
      _protocol=atoi(optarg);
      break;
    case 'X':
      /* Read configuration from XML file */
      _xml_file=string(optarg); 
      break;
    case '?':
      cerr << "Unknown option" << endl;
      exit(1);
    }
  }    
#ifdef XMLPARSING
  if (!_xml_file.empty()) {
    SAXTree config;
    vector<SAXTree*>::iterator current_tree;
    // Use parse supplied xml file.
    XMLParser p(_xml_file, config);
    // config.print(0);
    for (current_tree=config._children.begin();
         current_tree!=config._children.end();
         ++current_tree) {
      if ((*current_tree)->_tag=="admin") {
        cout << "admin\n";
        parse_admin(**current_tree);
      } else if ((*current_tree)->_tag=="scan_list") {
        cout << "scan_list\n";
        parse_scan_list(**current_tree);
      } else if ((*current_tree)->_tag=="network") {
        cout << "network\n";
        parse_network(**current_tree);
      } else if ((*current_tree)->_tag=="vsie") {
        cout << "vsie\n";
        parse_vsie(**current_tree);
      } else if ((*current_tree)->_tag=="test") {
        cout << "test\n";
        parse_test(**current_tree);
      } else if ((*current_tree)->_tag=="profile") {
        cout << "profile\n";
        parse_profile(**current_tree);
      }
    }
  } else {
    // Use values from command line.
  }
#endif
}

Options::~Options()
{
    
}

#ifdef XMLPARSING
void Options::parse_admin(const SAXTree& s)
{
  vector<SAXTree*>::const_iterator current_tree;
  // Use parse supplied xml file.
  for (current_tree=s._children.begin();
       current_tree!=s._children.end();
       ++current_tree) {
    if ((*current_tree)->_tag=="host_name") {
      _host_name=(*current_tree)->_value;
    } else if ((*current_tree)->_tag=="contact") {
      _contact=(*current_tree)->_value;
    } else if ((*current_tree)->_tag=="mode") {
      if ((*current_tree)->_value == "client") {
        _mode=CLIENT;
      } else if ((*current_tree)->_value == "server") {
        _mode=SERVER;
      } else if ((*current_tree)->_value == "test") {
        _mode=TEST;
      }
    } else if ((*current_tree)->_tag=="log_file") {
      _log_file=(*current_tree)->_value.c_str();
    } else if ((*current_tree)->_tag=="debug_level") {
      _debug_level=atoi((*current_tree)->_value.c_str());
    }
  }
}

void Options::parse_scan_list(const SAXTree& s)
{
  vector<SAXTree*>::const_iterator current_tree;
  // Parse supplied xml file.
  for (current_tree=s._children.begin();
       current_tree!=s._children.end();
       ++current_tree) {
    if ((*current_tree)->_tag=="scan_file") {
      Scan new_scan;
      new_scan._scan_file=(*current_tree)->_value;
      new_scan._size=atol(((*current_tree)->_attributes["size"]).c_str());
      new_scan._date=(*current_tree)->_attributes["date"];
      new_scan._src=(*current_tree)->_attributes["src"];
      _scan_vector.push_back(new_scan);
    }
  }
}

void Options::parse_network(const SAXTree& s)
{
  vector<SAXTree*>::const_iterator current_tree;
  // Use parse supplied xml file.
  for (current_tree=s._children.begin();
       current_tree!=s._children.end();
       ++current_tree) {
    // TODO: Stricter type checking on incoming parameters.
    if ((*current_tree)->_tag=="local_ip") {
      _local_ip = (*current_tree)->_value;
    } else if ((*current_tree)->_tag=="remote_ip") {
      _remote_ip = (*current_tree)->_value;
    } else if ((*current_tree)->_tag=="remote_port") {
      _remote_port = atoi((*current_tree)->_value.c_str());
    } else if ((*current_tree)->_tag=="local_port") {
      _local_port = atoi((*current_tree)->_value.c_str());
    } else if ((*current_tree)->_tag=="control_port") {
      _control_port = atoi((*current_tree)->_value.c_str());
    } else if ((*current_tree)->_tag=="streams") {
      _streams = atoi((*current_tree)->_value.c_str());
    } else if ((*current_tree)->_tag=="protocol") {
      _protocol = atoi((*current_tree)->_value.c_str());
    } else if ((*current_tree)->_tag=="host_mtu") {
      _host_mtu = atoi((*current_tree)->_value.c_str());
    } else if ((*current_tree)->_tag=="network_mtu") {
      _network_mtu = atoi((*current_tree)->_value.c_str());
    } else if ((*current_tree)->_tag=="rtt") {
      _rtt = atoi((*current_tree)->_value.c_str());
    } else if ((*current_tree)->_tag=="path_bandwidth") {
      _path_bandwidth = atol((*current_tree)->_value.c_str());
    } else if ((*current_tree)->_tag=="ssrc") {
      _ssrc = atol((*current_tree)->_value.c_str());
    } else if ((*current_tree)->_tag=="peak_rate") {
      _tspec._peak_rate = atol((*current_tree)->_value.c_str());
    } else if ((*current_tree)->_tag=="min_rate") {
      _tspec._min_rate = atol((*current_tree)->_value.c_str());
    } else if ((*current_tree)->_tag=="max_send_window") {
      _max_window = atol((*current_tree)->_value.c_str());
    } else if ((*current_tree)->_tag=="max_bucket") {
      _max_bucket = atol((*current_tree)->_value.c_str());
    } else if ((*current_tree)->_tag=="cc") {
      if ((*current_tree)->_value=="OPENLOOP") {
        _tspec._cc = RCC_OPENLOOP;
      } else if ((*current_tree)->_value=="TFRC") {
        _tspec._cc = RCC_TFRC;
      } else if ((*current_tree)->_value=="DLRC") {
        _tspec._cc = RCC_DLRC;
      }
    }
  }
}

void Options::parse_vsie(const SAXTree& s)
{
  vector<SAXTree*>::const_iterator current_tree;
  // Use parse supplied xml file.
  for (current_tree=s._children.begin();
       current_tree!=s._children.end();
       ++current_tree) {
    if ((*current_tree)->_tag=="sampling_frequency") {
      _sampling_frequency = atol((*current_tree)->_value.c_str());
    } else if ((*current_tree)->_tag=="packet_size") {
      _packet_size = atol((*current_tree)->_value.c_str());
    } else if ((*current_tree)->_tag=="samples_per_packet") {
      _samples_per_packet = atol((*current_tree)->_value.c_str());
    } else if ((*current_tree)->_tag=="min_rate") {
      _bits_per_sample = atol((*current_tree)->_value.c_str());
    }
  }
}

void Options::parse_test(const SAXTree& s)
{
  vector<SAXTree*>::const_iterator current_tree;
  // Use parse supplied xml file.
  for (current_tree=s._children.begin();
       current_tree!=s._children.end();
       ++current_tree) {
    if ((*current_tree)->_tag=="tvg") {
      if ((*current_tree)->_value=="enabled") {
        _tvg = true;
      } else {
        _tvg = false;
      }
    } else if ((*current_tree)->_tag=="tvg_duration") {
      _tvg_duration = atoi((*current_tree)->_value.c_str());
    }
  }
}

void Options::parse_profile(const SAXTree& s)
{
}
#endif // XMLPARSING

void Options::usage(ostream& c) const {
  c << "Usage:    -m <FILE2NET|IN2NET|NET2FILE|NET2OUT|TVG>" << endl;
  c << "          -p <data port>" << endl;
  c << "          -C <control port>" << endl;
  c << "          -i <remote ip>" << endl;
  c << "          -f <from file>" << endl;
  c << "          -t <to file>" << endl;
  c << "          -d <debug level>" << endl;
  c << "          -s <client side mtu>" << endl; 
  c << "          -S <network side mtu>" << endl; 
  c << "          -R <Maximum transmission rate(bps)>" << endl; 
  c << "          -X <xml configuration file>" << endl; 
}

void Options::dump(ostream& c) const
{
  c << "<Configuration>\n";
  switch (_mode) {
  case FILE2NET:
    c << "    <_mode>FILE2NET</_mode>";
    break;
  case IN2NET:
    c << "    <_mode>IN2NET</_mode>";
    break;
  case NET2FILE:
    c << "    <_mode>NET2FILE</_mode>";
    break;  
  case NET2OUT:
    c << "    <_mode>NET2OUT</_mode>";
    break;
  case TVG:
    c << "    <_mode>TVG</_mode>";
    break;
  default:
    cerr << "Uknown mode\n";
  }
  c << endl;
  c << "    <_debug_level>" << _debug_level << "</_debug_level>" << endl;
  c << "    <_data_port>" << _data_port << "</_data_port>" << endl;
  c << "    <_control_port>" << _control_port << "</_control_port>" << endl;
  c << "    <_remote_ip>" << _remote_ip << "</_remote_ip>" << endl;
  c << "    <_host_mtu>" << _host_mtu << "</_host_mtu>" << endl;
  c << "    <_network_mtu>" << _network_mtu << "</_network_mtu>" << endl;
  c << "    <_max_window>" << _max_window << "</_max_window>" << endl;
  c << "    <_max_bucket>" << _max_bucket << "</_max_bucket>" << endl;
  c << "    <_ssrc>" << _ssrc << "</_ssrc>" << endl;
  c << "    <_peak_rate>" << _tspec._peak_rate << "</_peak_rate>" << endl;
  c << "    <_tvg_duration>" << _tvg_duration << "</_tvg_duration>" << endl;
  c << "    <_from_file>" << _from_file << "</_from_file>" << endl;
  c << "    <_to_file>" << _to_file << "</_to_file>" << endl;
  c << "    <_protocol>" << _protocol << "</_protocol>" << endl;
  c << "</Configuration>\n";
}

