/*----------------    FILE HEADER  ------------------------------------------

This file is part of deegree.
Copyright (C) 2001 by:
EXSE, Department of Geography, University of Bonn
http://www.giub.uni-bonn.de/exse/
lat/lon Fitzke/Fretter/Poth GbR
http://www.lat-lon.de

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

Contact:

Andreas Poth
lat/lon Fitzke/Fretter/Poth GbR
Meckenheimer Allee 176
53115 Bonn
Germany
E-Mail: poth@lat-lon.de

Jens Fitzke
Department of Geography
University of Bonn
Meckenheimer Allee 166
53115 Bonn
Germany
E-Mail: jens.fitzke@uni-bonn.de

                 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.clients.wcasclient;

/**
 *
 * @author  administrator
 */
public interface Constants {
    String COLLECTION = "Collection";
    
    String CONF_DATETO = "DATETO";
    String CONF_DATEFROM = "DATEFROM";
    String CONF_FILEIDENTIFIER = "FILEIDENTIFIER";    
    String CONF_DATASERIES = "DATASERIES";
    String CONF_FREESEARCH = "FREESEARCH";
    String CONF_TOPICCATEGORY = "TOPICCATEGORY";    
    String CONF_KEYWORDS = "KEYWORDS";        
    String CONF_GEOGRAPHICBOX = "GEOGRAPHICBOX";
    String CONF_SEARCHMAP = "SEARCHMAP";
    
    String RPC_AVAILABILITY = "availability";
    String RPC_BBOX = "boundingBox";
    String RPC_BBOXMAXX = "maxx";
    String RPC_BBOXMAXY = "maxy";
    String RPC_BBOXMINX = "minx";
    String RPC_BBOXMINY = "miny";
    String RPC_CATALOG = "catalog";
    String RPC_DATASERIES = "DATASERIES";
    String RPC_DATEFROM = "startTime";
    String RPC_DATETO = "endTime";        
    String RPC_DAY = "day";
    String RPC_FILEIDENTIFIER = "FILEIDENTIFIER";    
    String RPC_FREESEARCH = "freeSearch";
    String RPC_GEOSERVICES = "geoServices";
    String RPC_ID = "ID";    
    String RPC_IDENTIFIER = "Identifier";   
    String RPC_KEYWORDS = "thesaurus"; 
    String RPC_MAXRECORDS = "MAXRECORDS";
    String RPC_MONTH = "month";
    String RPC_OUTPUTFORMAT = "OUTPUTFORMAT";
    String RPC_OUTPUTRECTYPE = "OUTPUTRECTYPE";    
    String RPC_QUERYSCOPE = "QUERYSCOPE";    
    String RPC_REQUEST = "request";
    String RPC_SEARCHBOX = "SEARCHBOX";
    String RPC_SETNAME = "SETNAME";
    String RPC_STARTPOSITION = "STARTPOSITION";   
    String RPC_TITLE = "Title";    
    String RPC_TOPICCATEGORY = "topicCategory";
    String RPC_TYPENAME = "TYPENAME";    
    String RPC_YEAR = "year";    
    
    String MAP_GETMAPREQUEST = "MAP_GETMAPREQUEST";
    
    String RESULT_SEARCH = "RESULT_SEARCH";
    String SEARCHBOX = "SEARCHBOX";
    
    String SOURCE = "SOURCE";
    String MESSAGE = "MESSAGE";
    String MISSING = "MISSING";
    String PRODUCT = "Product";
    
    //String REQUEST_ERRORCATALOGS = "ERRORCATALOGS";
    String REQUEST_ISO19115BRIEF = "ISO19115BRIEF";
    String REQUEST_ISO19115FULL = "ISO19115FULL";
    //String REQUEST_ITERATORCOUNT = "ITERATORCOUNT";
    
    String SERVICE = "Service";
    
    String SHOPPINGCARD = "ShoppingCard";    
    String SHOPPING_DELETE = "DELETEENTRY.X";
    String SHOPPING_GETDATA = "GETDATA.X";
    String SHOPPING_SHOWMAP = "SHOWMAP.X";
    
    String TEXTCOMPONENT = "TEXTCOMPONENT";
    String THESAURUSRESULT = "THESAURUSRESULT";
    
    String USERNAME = "USERNAME";
    String WMSREQUESTS = "WMSREQUESTS";
    
    String SESSION_DETAILEDSEARCHPARAM = "DETAILEDSEARCHPARAM";
    String SESSION_SELECTION = "SESSION_SELECTION";
    String SESSION_METADATA = "SESSION_METADATA";
     
}