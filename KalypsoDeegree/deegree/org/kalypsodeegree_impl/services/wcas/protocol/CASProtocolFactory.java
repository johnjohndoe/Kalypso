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
package org.deegree_impl.services.wcas.protocol;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;

import org.deegree.gml.GMLFeature;
import org.deegree.services.OGCWebServiceException;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.WebServiceException;
import org.deegree.services.wcas.protocol.CASDescribeRecordTypeRequest;
import org.deegree.services.wcas.protocol.CASDescribeRecordTypeResponse;
import org.deegree.services.wcas.protocol.CASGetCapabilitiesRequest;
import org.deegree.services.wcas.protocol.CASGetRecordRequest;
import org.deegree.services.wcas.protocol.CASGetRecordResponse;
import org.deegree.services.wcas.protocol.CASInsertResult;
import org.deegree.services.wcas.protocol.CASOperation;
import org.deegree.services.wcas.protocol.CASQuery;
import org.deegree.services.wcas.protocol.CASRegisterServiceRequest;
import org.deegree.services.wcas.protocol.CASTransactionRequest;
import org.deegree.services.wcas.protocol.CASTransactionResponse;
import org.deegree.services.wfs.filterencoding.Filter;
import org.deegree.xml.Marshallable;
import org.deegree.xml.XMLTools;
import org.deegree_impl.gml.GMLFeature_Impl;
import org.deegree_impl.services.wfs.filterencoding.AbstractFilter;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * 
 * <p>
 * --------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon>Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class CASProtocolFactory
{
  private static String WFSNS = "http://www.opengis.net/wfs";

  //    private static String OGCNS = "http://www.opengis.net/ogc";

  /**
   * creates a WFS request from a reader that contains an XML encoded form of
   * the request
   */
  public static OGCWebServiceRequest createRequest( String id, Reader reader ) throws Exception
  {
    Debug.debugMethodBegin( "CASProtocolFactory", "createRequest(Document)" );

    OGCWebServiceRequest request = null;

    Document doc = XMLTools.parse( reader );

    String root = doc.getDocumentElement().getLocalName();

    if( root.equals( "GetRecord" ) )
    {
      request = createCASGetRecordRequest( id, doc );
    }
    else if( root.equals( "DescribeRecordType" ) )
    {
      request = createCASDescribeRecordTypeRequest( id, doc );
    }
    else if( root.equals( "Transaction" ) )
    {
      request = createCASTransactionRequest( id, doc );
    }
    else if( root.equals( "GetCapabilities" ) )
    {
      request = createCASGetCapabilitiesRequest( id, null );
    }
    else if( root.equals( "RegisterService" ) )
    {
      request = createCASRegisterServiceRequest( id, doc );
    }

    Debug.debugMethodEnd();
    return request;
  }

  /**
   * creates a <tt>CASGetRecordRequest</tt> from a KVP encoded request like it
   * is used by HTTP Get. The request is stored in a <tt>Map</tt>
   * 
   * @param id
   *          unique ID of the request
   * @param httpget
   *          KVP encoded request
   * @return
   */
  public static String createGetRecordRequest( String id, Map map ) throws WebServiceException,
      Exception
  {
    String version = (String)map.get( "VERSION" );
    if( version == null )
    {
      throw new WebServiceException( "version parameter must be set" );
    }
    String service = (String)map.get( "SERVICE" );
    if( service == null )
    {
      throw new WebServiceException( "service parameter must be set" );
    }
    if( !service.equals( "WCAS" ) && !service.equals( "CSW" ) )
    {
      throw new WebServiceException( "service must be WCAS or CSW" );
    }
    String queryLanguage = (String)map.get( "CONSTRAINTLANGUAGE" );

    if( !queryLanguage.equals( "Filter" ) )
    {
      throw new WebServiceException( "queryLanguage must be 'Filter'" );
    }
    int maxrec = 10;
    if( map.get( "MAXRECORDS" ) != null )
    {
      maxrec = Integer.parseInt( map.get( "MAXRECORDS" ).toString() );
    }
    String format = "text/XML";
    if( map.get( "OUTPUTFORMAT" ) != null )
    {
      format = map.get( "OUTPUTFORMAT" ).toString();
    }
    String rectype = "ISO19115";
    if( map.get( "OUTPUTSCHEMA" ) != null )
    {
      rectype = map.get( "OUTPUTSCHEMA" ).toString();
    }
    String setName = "Full";
    if( map.get( "ELEMENTSETNAME" ) != null )
    {
      setName = map.get( "ELEMENTSETNAME" ).toString();
    }
    if( !setName.equals( "Full" ) && !setName.equals( "Summary" ) && !setName.equals( "Brief" )
        && !setName.equals( "Hits" ) )
    {
      throw new WebServiceException( "ElementSetName must be either"
          + "'Full', 'Summary', 'Brief' or 'Hits'" );
    }
    int startpos = -1;
    if( map.get( "STARTPOSITION" ) != null )
    {
      startpos = Integer.parseInt( map.get( "STARTPOSITION" ).toString() );
    }
    String typeName = (String)map.get( "TYPENAMES" );
    if( rectype.equals( "ISO19119" ) && !typeName.equals( "Service" ) )
    {
      throw new WebServiceException( "invalid combination of typeNames and outputSchema" );
    }
    if( rectype.equals( "ISO19115" ) && typeName.equals( "Service" ) )
    {
      throw new WebServiceException( "invalid combination of typeNames and outputSchema" );
    }
    if( typeName == null )
    {
      throw new WebServiceException( "typeName parameter must be set" );
    }
    String querySpec = (String)map.get( "CONSTRAINT" );
    if( querySpec != null && queryLanguage == null )
    {
      throw new WebServiceException( "CONSTRAINTLANGUAGE parameter must be set" );
    }

    StringBuffer sb = new StringBuffer( 2000 );
    sb.append( "<GetRecord xmlns='http://www.opengis.net/wfs' " );
    sb.append( "xmlns:ogc='http://www.opengis.net/ogc' " );
    sb.append( "xmlns:gml='http://www.opengis.net/gml' " );
    sb.append( "maxRecords='" ).append( maxrec ).append( "' " );
    sb.append( "outputFormat='" ).append( format ).append( "' " );
    sb.append( "outputRecType='" ).append( rectype ).append( "' queryScope='0' " );
    sb.append( "startPosition='" ).append( startpos ).append( "'>" );
    sb.append( "<Query typeName='" ).append( typeName ).append( "'>" );
    sb.append( "<PropertySet setName='" ).append( setName ).append( "'/>" );

    if( querySpec != null )
    {
      sb.append( URLDecoder.decode( querySpec ) );
    }

    sb.append( "</Query>" );
    sb.append( "</GetRecord>" );

    return sb.toString();
  }

  /**
   * creates a DescribeRecordType request from a reader object that offers
   * access to a string resource that contains XML document that encodes the
   * request.
   */
  public static CASDescribeRecordTypeRequest createCASDescribeRecordTypeRequest( String id,
      Document doc ) throws IOException
  {
    Debug.debugMethodBegin( "CASProtocolFactory", "createCASDescribeRecordTypeRequest" );

    Element element = (Element)doc.getElementsByTagName( "DescribeRecordType" ).item( 0 );

    String outputFormat = XMLTools.getAttrValue( element, "outputFormat" );

    NodeList nl = element.getElementsByTagName( "TypeName" );

    String[] typeNames = null;
    String[] setNames = null;

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      typeNames = new String[nl.getLength()];
      setNames = new String[nl.getLength()];

      for( int i = 0; i < nl.getLength(); i++ )
      {
        typeNames[i] = nl.item( i ).getFirstChild().getNodeValue();
        setNames[i] = XMLTools.getAttrValue( nl.item( i ), "setName" );
      }
    }

    CASDescribeRecordTypeRequest request = createCASDescribeRecordTypeRequest( "1.0.0", id, null,
        typeNames, setNames, outputFormat );

    Debug.debugMethodEnd();
    return request;
  }

  /**
   * creates a <tt>CASDescribeRecordTypeRequest</tt> object.
   * 
   * @param id
   *          id of the request
   * @param vendorSpecificParameter
   *          none standadized parameters as name-value pairs
   * @param outputFormat
   *          indicates the format the result shall be formated
   * @param typeNames
   *          names of the feature types that shalle be described
   */
  public static CASDescribeRecordTypeRequest createCASDescribeRecordTypeRequest( String version,
      String id, HashMap vendorSpecificParameter, String[] typeNames, String[] setNames,
      String outputFormat )
  {
    Debug.debugMethodBegin( "CASProtocolFactory", "createCASDescribeRecordTypeRequest" );

    CASDescribeRecordTypeRequest req = new CASDescribeRecordTypeRequest_Impl( version, id,
        vendorSpecificParameter, typeNames, setNames, outputFormat );

    Debug.debugMethodEnd();
    return req;
  }

  /**
   * creates a <tt>CASDescribeRecordTypeResponse</tt> object
   * 
   * @param request
   *          a copy of the request that leads to this response
   * @param exception
   *          a describtion of an excetion (only if raised)
   * @param featureTypeSchema
   *          schemas of the feature types which describtion has been requested.
   */
  public static CASDescribeRecordTypeResponse createCASDescribeRecordTypeResponse(
      OGCWebServiceRequest request, OGCWebServiceException exception, String featureTypeSchema )
  {
    Debug.debugMethodBegin( "CASProtocolFactory", "createCASDescribeRecordTypeResponse" );

    Document doc = null;

    if( exception != null )
    {
      StringReader reader = new StringReader( ( (Marshallable)exception ).exportAsXML() );
      try
      {
        doc = XMLTools.parse( reader );
      }
      catch( Exception e )
      {
        System.out.println( e );
      }

    }

    CASDescribeRecordTypeResponse response = new CASDescribeRecordTypeResponse_Impl( request, doc,
        featureTypeSchema );

    Debug.debugMethodEnd();
    return response;
  }

  /**
   * creates a <tt>CASGetRecordRequest</tt> object.
   * 
   * @param id
   *          id of the request
   * @param vendorSpecificParameter
   *          none standadized parameters as name-value pairs
   * @param outputFormat
   *          indicates the format the result shall be formated
   * @param filter
   *          filter expression that describes the 'global' limitations of the
   *          query.
   * @param maxRecords
   *          maximal amout of featuers that shall be returned
   * @param startPosition
   *          index of the feature the query shall start
   * @param queries
   *          a set of Query objects that describes the query to perform
   * @param queryScope
   *          number that indicates if the request is part of a cascading
   *          request
   */
  public static CASGetRecordRequest createCASGetRecordRequest( String id,
      HashMap vendorSpecificParameter, int maxRecords, int startPosition, String outputFormat,
      String outputRecType, CASQuery[] queries, int queryScope, Filter filter )
  {
    Debug.debugMethodBegin( "CASProtocolFactory", "createCASGetRecordRequest" );

    CASGetRecordRequest req = new CASGetRecordRequest_Impl( "1.0.0", id, vendorSpecificParameter,
        maxRecords, startPosition, outputFormat, outputRecType, queries, queryScope, filter );
    Debug.debugMethodEnd();
    return req;
  }

  /**
   * creates a instance of a <tt>CASGetRecordRequest</tt> object from a reader
   * that contains the request-xml.
   * 
   * @param doc
   *          DOM object that contains the request
   * @param id
   *          id of the request
   */
  public static CASGetRecordRequest createCASGetRecordRequest( String id, Document doc )
      throws Exception
  {
    Debug.debugMethodBegin( "CASProtocolFactory", "createCASGetRecordRequest(Reader)" );

    CASGetRecordRequest req = null;

    Element element = element = (Element)doc.getElementsByTagName( "GetRecord" ).item( 0 );

    String outputFormat = XMLTools.getAttrValue( element, "outputFormat" );
    String handle = XMLTools.getAttrValue( element, "handle" );
    CASQuery[] queries = getQuery( element );
    Filter filter = getFilter( element );

    String tmp = null;
    int maxRecords = -1;

    try
    {
      tmp = XMLTools.getAttrValue( element, "maxRecords" ).trim();
      maxRecords = Integer.parseInt( tmp );
    }
    catch( Exception ex )
    {}

    int startPosition = -1;

    try
    {
      tmp = XMLTools.getAttrValue( element, "startPosition" ).trim();
      startPosition = Integer.parseInt( tmp );
    }
    catch( Exception ex )
    {}

    int queryScope = -1;

    try
    {
      tmp = XMLTools.getAttrValue( element, "queryScope" ).trim();
      queryScope = Integer.parseInt( tmp );
    }
    catch( Exception ex )
    {}

    String outputRecType = XMLTools.getAttrValue( element, "outputRecType" ).trim();

    req = createCASGetRecordRequest( id, null, maxRecords, startPosition, outputFormat,
        outputRecType, queries, queryScope, filter );

    Debug.debugMethodEnd();
    return req;
  }

  /**
   * The query defines which feature type to query, what properties to retrieve
   * and what constraints (spatial and non-spatial) to apply to those
   * properties.
   * <p>
   * only used for xml-coded requests
   */
  private static CASQuery[] getQuery( Element element ) throws Exception
  {
    Debug.debugMethodBegin( "CASProtocolFactory", "getQuery" );

    NodeList nl = element.getChildNodes();
    ArrayList list = new ArrayList();

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      for( int i = 0; i < nl.getLength(); i++ )
      {
        if( nl.item( i ).getNodeName().equals( "Query" ) )
        {
          Element elem = (Element)nl.item( i );
          String[] propertyNames = getPropertyNames( elem );
          String propertySetName = getPropertySetName( elem );
          String handle = XMLTools.getAttrValue( elem, "handle" );
          String version = XMLTools.getAttrValue( elem, "version" );
          String typeName = XMLTools.getAttrValue( elem, "typeName" );

          if( !typeName.equals( "Product" ) && !typeName.equals( "Service" )
              && !typeName.equals( "Collection" ) )
          {
            throw new Exception( "not known typeName used within " + "the query" );
          }

          Filter filter = getFilter( elem );
          list.add( new CASQuery_Impl( propertyNames, handle, version, typeName, filter,
              propertySetName ) );
        }
      }
    }

    Debug.debugMethodEnd();

    return (CASQuery[])list.toArray( new CASQuery[list.size()] );
  }

  /**
   * returns the name of the propertySetName of the catalog request from the
   * passed <tt>Element</tt>
   * 
   * @param element
   * @throws Exception
   * @return
   */
  private static String getPropertySetName( Element element ) throws Exception
  {
    Debug.debugMethodBegin( "CASProtocolFactory", "getPropertySetName" );

    String propertyset = null;

    NodeList nl = element.getElementsByTagName( "PropertySet" );

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      String s = XMLTools.getAttrValue( nl.item( 0 ), "setName" );

      if( !s.equals( "Full" ) && !s.equals( "Brief" ) && !s.equals( "Summary" )
          && !s.equals( "Hits" ) )
      {
        throw new Exception( "not supported porperty set type" + " used within the query." );
      }

      propertyset = s;
    }

    Debug.debugMethodEnd();
    return propertyset;
  }

  /**
   * The property names is used to enumerate the feature properties or
   * attributes that should be selected. If no property names are specified then
   * all properties should be fetched.
   */
  private static String[] getPropertyNames( Element element )
  {
    Debug.debugMethodBegin( "CASProtocolFactory", "getPropertyNames" );

    String[] propertynames = null;
    NodeList nl = element.getChildNodes();
    ArrayList list = new ArrayList();

    if( nl != null )
    {
      if( nl.getLength() > 0 )
      {
        for( int i = 0; i < nl.getLength(); i++ )
        {
          if( nl.item( i ).getNodeName().equals( "PropertyName" ) )
          {
            list.add( nl.item( i ).getFirstChild().getNodeValue() );
          }
        }
      }
    }

    propertynames = (String[])list.toArray( new String[list.size()] );

    Debug.debugMethodEnd();
    return propertynames;
  }

  /**
   * returns the filter that limits the query
   */
  public static Filter getFilter( Element element ) throws Exception
  {
    Debug.debugMethodBegin( "CASProtocolFactory", "getFilter" );

    Filter filter = null;

    NodeList nl = element.getChildNodes();

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      for( int i = 0; i < nl.getLength(); i++ )
      {
        if( nl.item( i ) instanceof Element && nl.item( i ).getLocalName().equals( "Filter" ) )
        {
          filter = AbstractFilter.buildFromDOM( (Element)nl.item( i ) );
          break;
        }
      }
    }

    Debug.debugMethodEnd();
    return filter;
  }

  /**
   * creates a <tt>CASGetRecordResponse</tt> object
   * 
   * @param request
   *          a copy of the request that leads to this response
   * @param exception
   *          a describtion of an excetion (only if raised)
   * @param response
   *          the response to the request
   */
  public static CASGetRecordResponse createCASGetRecordResponse( OGCWebServiceRequest request,
      OGCWebServiceException exception, Object response )
  {
    Debug.debugMethodBegin( "CASProtocolFactory", "createCASGetRecordResponse" );

    Document doc = null;

    if( exception != null )
    {
      StringReader reader = new StringReader( ( (Marshallable)exception ).exportAsXML() );
      try
      {
        doc = XMLTools.parse( reader );
      }
      catch( Exception e )
      {
        System.out.println( e );
      }
    }

    CASGetRecordResponse res = new CASGetRecordResponse_Impl( request, doc, response );

    Debug.debugMethodEnd();
    return res;
  }

  /**
   * creates a <tt>CASGetCapabilitiesRequest</tt> object.
   * 
   * @param id
   *          id of the request
   * @param vendorSpecificParameter
   *          none standadized parameters as name-value pairs
   */

  public static CASGetCapabilitiesRequest createCASGetCapabilitiesRequest( String id,
      HashMap vendorSpecificParameter )
  {
    Debug.debugMethodBegin( "CASProtocolFactory", "createCASGetCapabilitiesRequest" );

    CASGetCapabilitiesRequest req = new CASGetCapabilitiesRequest_Impl( "0.0.6", id,
        vendorSpecificParameter );

    Debug.debugMethodEnd();
    return req;
  }

  /**
   * creates a <tt>CASGetCapabilitiesResponse</tt> object
   * 
   * @param request
   *          a copy of the request that leads to this response
   * @param exception
   *          a describtion of an excetion (only if raised)
   * @param native_
   *          is intended to allow access to vendor specific capabilities
   * @param affectedRecordTypes
   *          names of the feature types affected by the response
   * @param response
   *          the response to the request
   */

  //    public CASGetCapabilitiesResponse createCASGetCapabilitiesResponse(
  //   													OGCWebServiceRequest request,
  //													OGCWebServiceException exception,
  //													String [] affectedRecordTypes,
  //													Document response )
  //	{
  //		Debug.debugMethodBegin("CASProtocolFactory",
  // "createCASGetCapabilitiesResponse" );
  //
  //		Document doc = null;
  //
  //		if ( exception != null ) {
  //			doc = exception.exportAsXML();
  //		}
  //
  //		CASGetCapabilitiesResponse res =
  //			new CASGetCapabilitiesResponse_Impl( request, doc, affectedRecordTypes,
  //												 response);
  //
  //		Debug.debugMethodEnd();
  //		return res;
  //	}
  /**
   * creates a CAS Query object from a dom element.
   * 
   * @param element
   *          dom element containing a CAS query
   */
  public CASQuery createQuery( Element element ) throws Exception
  {
    Debug.debugMethodBegin( "CASProtocolFactory", "createQuery" );

    String[] propertyNames = getPropertyNames( element );
    String propertySetName = getPropertySetName( element );
    String handle = XMLTools.getAttrValue( element, "handle" );
    String version = XMLTools.getAttrValue( element, "version" );
    String typeName = XMLTools.getAttrValue( element, "typeName" );
    Filter filter = getFilter( element );
    CASQuery query = new CASQuery_Impl( propertyNames, handle, version, typeName, filter,
        propertySetName );

    Debug.debugMethodEnd();
    return query;
  }

  /**
   * creates a <tt>CASTransactionRequest</tt> object.
   * 
   * @param id
   *          id of the request
   * @param lockId
   * @param operations
   * @param handle
   */
  public CASTransactionRequest createCASTransactionRequest( String id,
      HashMap vendorSpecificParameter, String lockId, String handle, CASOperation[] operations )
  {
    Debug.debugMethodBegin( "CASProtocolFactory", "createCASTransactionRequest" );

    CASTransactionRequest tr = new CASTransactionRequest_Impl( "1.0.0", id,
        vendorSpecificParameter, lockId, handle, operations );

    Debug.debugMethodEnd();
    return tr;
  }

  /**
   * creates a <tt>CASTransactionRequest</tt> object.
   * 
   * @param id
   *          id of the request
   * @param doc
   *          DOM objects that contains the request
   */
  public static CASTransactionRequest createCASTransactionRequest( String id, Document doc )
      throws Exception
  {
    Debug.debugMethodBegin( "CASProtocolFactory", "createCASTransactionRequest" );

    CASTransactionRequest tr = null;

    Element element = (Element)doc.getElementsByTagName( "Transaction" ).item( 0 );

    String handle = XMLTools.getAttrValue( element, "handle" );

    NodeList nl = element.getElementsByTagName( "Insert" );
    CASOperation[] ins = createInserts( nl );

    nl = element.getElementsByTagName( "Update" );

    CASOperation[] ups = createUpdates( nl );

    nl = element.getElementsByTagName( "Delete" );

    CASOperation[] dels = createDeletes( nl );

    CASOperation[] ops = new CASOperation[ins.length + ups.length + dels.length];
    int k = 0;

    for( int i = 0; i < ins.length; i++ )
    {
      ops[k++] = ins[i];
    }

    for( int i = 0; i < ups.length; i++ )
    {
      ops[k++] = ups[i];
    }

    for( int i = 0; i < dels.length; i++ )
    {
      ops[k++] = dels[i];
    }

    String lockId = null;

    Element elem = XMLTools.getNamedChild( element, WFSNS, "LockId" );

    if( elem != null )
    {
      lockId = elem.getFirstChild().getNodeValue();
    }

    tr = new CASTransactionRequest_Impl( "1.0.0", id, null, lockId, handle, ops );

    Debug.debugMethodEnd();
    return tr;
  }

  /**
   * creates insert operations object from a XML-element defined by the OGC CAS
   * specifications
   */
  private static CASOperation[] createInserts( NodeList nl )
  {
    Debug.debugMethodBegin( "CASProtocolFactory", "createInserts" );

    CASOperation[] operations = null;

    if( nl != null )
    {
      operations = new CASOperation[nl.getLength()];

      // for each insert operation defined within the element
      for( int i = 0; i < nl.getLength(); i++ )
      {
        Element element = (Element)nl.item( i );
        // get the operations handle (may be null)
        String handle = XMLTools.getAttrValue( element, "handle" );
        ArrayList list = new ArrayList();
        NodeList nl_ = element.getChildNodes();

        // create list of features that shall be inserted
        for( int j = 0; j < nl_.getLength(); j++ )
        {
          if( nl_.item( j ) instanceof Element )
          {
            list.add( new GMLFeature_Impl( (Element)nl_.item( j ) ) );
          }
        }

        GMLFeature[] feat = new GMLFeature[list.size()];
        feat = (GMLFeature[])list.toArray( feat );

        // create insert operation
        operations[i] = new CASInsert_Impl( handle, feat );
      }
    }
    else
    {
      operations = new CASOperation[0];
    }

    Debug.debugMethodEnd();
    return operations;
  }

  /**
   * creates update operations object from a XML-element defined by the OGC CAS
   * specifications
   */
  private static CASOperation[] createUpdates( NodeList nl )
  {
    Debug.debugMethodBegin( "CASProtocolFactory", "createUpdates" );

    CASOperation[] operations = new CASOperation[0];

    Debug.debugMethodEnd();
    return operations;
  }

  /**
   * creates delete operations object from a XML-element defined by the OGC CAS
   * specifications
   */
  private static CASOperation[] createDeletes( NodeList nl ) throws Exception
  {
    Debug.debugMethodBegin( "CASProtocolFactory", "createDeletes" );

    CASOperation[] operations = null;

    if( nl != null )
    {
      operations = new CASOperation[nl.getLength()];
      for( int i = 0; i < nl.getLength(); i++ )
      {
        Element element = (Element)nl.item( i );
        Filter filter = getFilter( element );
        String handle = XMLTools.getAttrValue( element, "handle" );
        String type = XMLTools.getAttrValue( element, "type" );
        operations[i] = new CASDelete_Impl( handle, filter, type );
      }
    }

    Debug.debugMethodEnd();
    return operations;
  }

  /**
   * creates nativ operations object from a XML-element defined by the OGC CAS
   * specifications
   */
  //    private static CASOperation[] createNatives(NodeList nl) {
  //        Debug.debugMethodBegin("CASProtocolFactory", "createNatives");
  //
  //        CASOperation[] operations = new CASOperation[0];
  //
  //        Debug.debugMethodEnd();
  //        return operations;
  //    }
  /**
   * creates a <tt>CASTransactionRequest</tt> object.
   * 
   * @param request
   *          request that lead to the response
   * @param exception
   *          exception if raised
   * @param status
   *          termination status of the transaction (Success|Partial|Failed)
   * @param handle
   *          something to identify the failure if one has occured
   */
  public static CASTransactionResponse createCASTransactionResponse( OGCWebServiceRequest request,
      OGCWebServiceException exception, CASInsertResult[] insertResults, String status,
      String handle )
  {
    Debug.debugMethodBegin();

    Document doc = null;

    if( exception != null )
    {
      StringReader reader = new StringReader( ( (Marshallable)exception ).exportAsXML() );
      try
      {
        doc = XMLTools.parse( reader );
      }
      catch( Exception e )
      {
        System.out.println( e );
      }
    }

    CASTransactionResponse response = new CASTransactionResponse_Impl( request, doc, insertResults,
        status, handle );

    Debug.debugMethodEnd();
    return response;
  }

  /**
   * creates a <tt>CASInsertResult</tt>
   */
  public static CASInsertResult createCASInsertResult( String handle, String[] featureIds )
  {
    Debug.debugMethodBegin( "CASProtocolFactory", "createCASTransactionResponse" );

    CASInsertResult result = new CASInsertResult_Impl( handle, featureIds );

    Debug.debugMethodEnd();
    return result;
  }

  /**
   * creates a <tt>CASRegisterServiceRequest</tt> object
   * 
   * @param id
   *          id of the request
   * @param vendorSpecificParameter
   * @param serviceAddress
   *          URL of (ISO19119) document that describes the service that shall
   *          be registered
   * @param serviceOwnerContactInfo
   *          information about who to contact to get additional informations
   *          about the service
   * @param harvestFrequency
   *          time interval in hours a catalog shall revisit the service
   */
  public static CASRegisterServiceRequest createCASRegisterServiceRequest( String id,
      HashMap vendorSpecificParameter, URL serviceAddress, String serviceOwnerContactInfo,
      int harvestFrequency )
  {
    Debug.debugMethodBegin( "CASProtocolFactory", "createCASRegisterServiceRequest" );

    CASRegisterServiceRequest rsr = new CASRegisterServiceRequest_Impl( "1.0.0", id,
        vendorSpecificParameter, serviceAddress, serviceOwnerContactInfo, harvestFrequency );

    Debug.debugMethodEnd();
    return rsr;
  }

  /**
   * creates a <tt>CASRegisterServiceRequest</tt> object
   * 
   * @param id
   *          id of the request
   * @param doc
   *          XML document encoding the request
   */
  public static CASRegisterServiceRequest createCASRegisterServiceRequest( String id, Document doc )
      throws MalformedURLException
  {
    Debug.debugMethodBegin( "CASProtocolFactory", "createCASRegisterServiceRequest" );

    Element element = (Element)doc.getElementsByTagName( "RegisterService" ).item( 0 );

    String serviceOwnerContactInfo = XMLTools.getAttrValue( element, "serviceOwnerContactInfo" );
    int harvestFrequency = -1;

    try
    {
      String s = XMLTools.getAttrValue( element, "harvestFrequency" );
      harvestFrequency = Integer.parseInt( s );
    }
    catch( Exception e )
    {}

    Element elem = (Element)element.getElementsByTagName( "ServiceAddr" ).item( 0 );
    String href = XMLTools.getAttrValue( elem, "href" );
    URL serviceAddress = new URL( href );

    CASRegisterServiceRequest rsr = new CASRegisterServiceRequest_Impl( "1.0.0", id, null,
        serviceAddress, serviceOwnerContactInfo, harvestFrequency );

    Debug.debugMethodEnd();
    return rsr;
  }

  /**
   * puts a http-GET request to a <tt>HashMap</tt>
   */
  private static Map toMap( String request )
  {
    StringTokenizer st = new StringTokenizer( request, "&?" );
    HashMap map = new HashMap();

    while( st.hasMoreTokens() )
    {
      String s = st.nextToken();

      if( s != null )
      {
        int pos = s.indexOf( '=' );

        if( pos > -1 )
        {
          String s1 = s.substring( 0, pos );
          String s2 = s.substring( pos + 1, s.length() );
          map.put( s1.toUpperCase(), s2 );
        }
      }
    }

    return map;
  }

}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.3  2004/10/07 14:09:04  doemming
 * *** empty log message ***
 *
 * Revision 1.1  2004/09/02 23:56:51  doemming
 * *** empty log message ***
 * Revision 1.3 2004/08/31 12:53:32 doemming
 * *** empty log message *** Revision 1.18 2004/08/13 07:01:46 poth HTTP Get
 * added for WCAS GetRecord
 * 
 * Revision 1.17 2004/08/10 16:19:31 poth no message
 * 
 * Revision 1.16 2004/03/29 10:39:04 poth no message
 * 
 * Revision 1.15 2004/02/19 10:08:58 poth no message
 * 
 * Revision 1.14 2004/02/18 15:08:54 mrsnyder Corrections to the WFS-Delete
 * functionality. Worked on WCAS, too.
 * 
 * Revision 1.13 2004/02/11 08:06:06 poth no message
 * 
 * Revision 1.12 2004/01/08 09:50:23 poth no message
 * 
 * Revision 1.5 2003/06/10 07:52:12 poth no message
 * 
 * Revision 1.4 2003/05/27 06:55:25 poth no message
 * 
 * Revision 1.3 2003/04/07 07:26:17 poth no message
 * 
 * Revision 1.2 2002/12/12 13:11:17 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:33 poth no message
 * 
 * Revision 1.1 2002/08/20 15:56:54 ap no message
 * 
 *  
 */
