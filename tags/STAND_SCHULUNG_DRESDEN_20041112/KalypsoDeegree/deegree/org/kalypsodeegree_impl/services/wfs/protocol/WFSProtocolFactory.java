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
package org.deegree_impl.services.wfs.protocol;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;

import org.deegree.gml.GMLFeature;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.services.InconsistentRequestException;
import org.deegree.services.OGCWebServiceException;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.wfs.capabilities.WFSCapabilities;
import org.deegree.services.wfs.filterencoding.Filter;
import org.deegree.services.wfs.filterencoding.FilterConstructionException;
import org.deegree.services.wfs.protocol.WFSDelete;
import org.deegree.services.wfs.protocol.WFSDescribeFeatureTypeRequest;
import org.deegree.services.wfs.protocol.WFSDescribeFeatureTypeResponse;
import org.deegree.services.wfs.protocol.WFSGetCapabilitiesRequest;
import org.deegree.services.wfs.protocol.WFSGetCapabilitiesResponse;
import org.deegree.services.wfs.protocol.WFSGetFeatureRequest;
import org.deegree.services.wfs.protocol.WFSGetFeatureResponse;
import org.deegree.services.wfs.protocol.WFSInsert;
import org.deegree.services.wfs.protocol.WFSInsertResult;
import org.deegree.services.wfs.protocol.WFSNative;
import org.deegree.services.wfs.protocol.WFSOperation;
import org.deegree.services.wfs.protocol.WFSQuery;
import org.deegree.services.wfs.protocol.WFSTransactionRequest;
import org.deegree.services.wfs.protocol.WFSTransactionResponse;
import org.deegree.services.wfs.protocol.WFSUpdate;
import org.deegree.xml.DOMPrinter;
import org.deegree.xml.ElementList;
import org.deegree.xml.Marshallable;
import org.deegree.xml.XMLParsingException;
import org.deegree.xml.XMLTools;
import org.deegree_impl.gml.GMLFeature_Impl;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.services.wfs.filterencoding.AbstractFilter;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.StringExtend;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Factory class for all mapped WFS-XML-Requests.
 * 
 * @author <a href="mailto:poth@lat-lon">Andreas Poth </a>
 * @author <a href="mailto:mschneider@lat-lon">Markus Schneider </a>
 * 
 * @version $Revision$ $Date$
 *  
 */
public class WFSProtocolFactory
{

  private static String wfsNS = "http://www.opengis.net/wfs";

  private static String ogcNS = "http://www.opengis.net/ogc";

  /**
   * creates a WFS request from a reader that contains an XML encoded form of
   * the request
   */
  public static OGCWebServiceRequest createRequest( String id, Reader reader ) throws Exception
  {
    Debug.debugMethodBegin();

    OGCWebServiceRequest request = null;

    Document doc = XMLTools.parse( reader );

    String root = doc.getDocumentElement().getLocalName();

    if( root.equals( "GetFeature" ) )
    {
      request = createWFSGetFeatureRequest( id, doc );
    }
    else if( root.equals( "DescribeFeatureType" ) )
    {
      request = createWFSDescribeFeatureTypeRequest( id, doc );
    }
    else if( root.equals( "Transaction" ) )
    {
      request = createWFSTransactionRequest( id, doc );
    }
    else if( root.equals( "GetCapabilities" ) )
    {
      request = createWFSGetCapabilitiesRequest( id, null, null );
    }

    Debug.debugMethodEnd();
    return request;
  }

  /**
   * creates a DescribeFeatureType request from a reader object that offers
   * access to a string resource that contains XML document that encodes the
   * request.
   */
  public static WFSDescribeFeatureTypeRequest createWFSDescribeFeatureTypeRequest( String id,
      Document doc ) throws IOException
  {
    Debug.debugMethodBegin();

    Element element = doc.getDocumentElement();

    String outputFormat = XMLTools.getAttrValue( element, "outputFormat" );

    ElementList nl = XMLTools.getChildElementsByName( "TypeName", wfsNS, element );

    String[] typeNames = null;

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      typeNames = new String[nl.getLength()];

      for( int i = 0; i < nl.getLength(); i++ )
      {
        typeNames[i] = nl.item( i ).getFirstChild().getNodeValue();
      }
    }

    WFSDescribeFeatureTypeRequest request = createWFSDescribeFeatureTypeRequest( "1.0.0", id, null,
        null, outputFormat, typeNames );

    Debug.debugMethodEnd();
    return request;
  }

  /**
   * creates a <tt>WFSDescribeFeatureTypeRequest</tt> object.
   * 
   * @param id
   *          id of the request
   * @param vendorSpecificParameter
   *          none standadized parameters as name-value pairs
   * @param native_
   *          is intended to allow access to vendor specific capabilities
   * @param outputFormat
   *          indicates the format the result shall be formated
   * @param typeNames
   *          names of the feature types that shalle be described
   */
  public static WFSDescribeFeatureTypeRequest createWFSDescribeFeatureTypeRequest( String version,
      String id, HashMap vendorSpecificParameter, WFSNative native_, String outputFormat,
      String[] typeNames )
  {
    Debug.debugMethodBegin();

    WFSDescribeFeatureTypeRequest req = new WFSDescribeFeatureTypeRequest_Impl( version, id,
        vendorSpecificParameter, native_, outputFormat, typeNames );

    Debug.debugMethodEnd();
    return req;
  }

  /**
   * creates a <tt>WFSDescribeFeatureTypeResponse</tt> object
   * 
   * @param request
   *          a copy of the request that leads to this response
   * @param exception
   *          a describtion of an excetion (only if raised)
   * @param affectedFeatureTypes
   *          names of the feature types affected by the response
   */
  public static WFSDescribeFeatureTypeResponse createWFSDescribeFeatureTypeResponse(
      OGCWebServiceRequest request, String[] affectedFeatureTypes,
      OGCWebServiceException exception, Document featureTypeSchema )
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

    WFSDescribeFeatureTypeResponse response = new WFSDescribeFeatureTypeResponse_Impl( request,
        affectedFeatureTypes, doc, featureTypeSchema );

    Debug.debugMethodEnd();
    return response;
  }

  /**
   * creates a GetFeature request from a key-value-pair encoding of the
   * parameters contained in the passed varialble 'request'
   * 
   * @param id
   *          id of the request
   * @param request
   *          key-value-pair encoded GetFeature request
   */
  public static WFSGetFeatureRequest createWFSGetFeatureRequest( String id, String request )
      throws InconsistentRequestException
  {
    Debug.debugMethodBegin();

    Map model = toMap( request );

    // WFS version
    String version = (String)model.remove( "VERSION" );
    if( version == null )
    {
      throw new InconsistentRequestException( "version parameter must be set" );
    }

    // requested feature types or featureIds
    String tmp = (String)model.remove( "TYPENAME" );
    String[] typenames = null;
    String[] featureIds = null;
    if( tmp == null && model.get( "FEATUREID" ) == null )
    {
      throw new InconsistentRequestException( "typename or featureid parameter must be set" );
    }
    else if( tmp != null )
    {
      typenames = StringExtend.toArray( tmp, ",", false );
    }
    else
    {
      tmp = (String)model.remove( "FEATUREID" );
      featureIds = StringExtend.toArray( tmp, ",", false );
    }

    tmp = (String)model.remove( "PROPERTYNAME" );
    Map typeProp = new HashMap();
    // assign properties to feature types
    if( tmp != null )
    {
      String[] tmpa = StringExtend.toArray( tmp, ")", false );
      if( typenames != null )
      {
        if( tmpa.length != typenames.length )
        {
          throw new InconsistentRequestException( "if properties are defined "
              + "it must be done for each featuretype" );
        }
        for( int i = 0; i < tmpa.length; i++ )
        {
          if( tmpa[i].length() > 1 )
          {
            typeProp.put( typenames[i], tmpa[i].substring( 1 ) );
          }
          else
          {
            typeProp.put( typenames[i], null );
          }
        }
      }
      else
      {
        if( tmpa.length != featureIds.length )
        {
          throw new InconsistentRequestException( "if properties are defined "
              + "it must be done for each featureID" );
        }
        for( int i = 0; i < tmpa.length; i++ )
        {
          if( tmpa[i].length() > 1 )
          {
            typeProp.put( featureIds[i], tmpa[i].substring( 1 ) );
          }
          else
          {
            typeProp.put( featureIds[i], null );
          }
        }
      }
    }
    else
    {
      if( typenames != null )
      {
        for( int i = 0; i < typenames.length; i++ )
        {
          typeProp.put( typenames[i], null );
        }
      }
      else
      {
        for( int i = 0; i < featureIds.length; i++ )
        {
          typeProp.put( featureIds[i], null );
        }
      }
    }

    // max features
    tmp = (String)model.remove( "MAXFEATURES" );
    int maxFeatures = -1;
    if( tmp != null )
    {
      try
      {
        maxFeatures = Integer.parseInt( tmp );
        if( maxFeatures < 1 )
          throw new Exception();
      }
      catch( Exception e )
      {
        throw new InconsistentRequestException( "if maxFeatures is defined "
            + "it must be an integer > 0" );
      }
    }

    // Filters
    Map typeFilter = new HashMap();
    tmp = (String)model.remove( "FILTER" );
    if( tmp != null && featureIds == null )
    {
      String[] tmpa = StringExtend.toArray( tmp, ")", false );
      if( tmpa.length != typenames.length )
      {
        throw new InconsistentRequestException( "if filters are defined "
            + "it must be done for each featuretype" );
      }
      for( int i = 0; i < tmpa.length; i++ )
      {
        if( tmpa[i].length() > 1 )
        {
          Filter filter = null;
          try
          {
            String s = URLDecoder.decode( tmpa[i].substring( 1 ), "UTF-8" );
            StringReader sr = new StringReader( s );
            Document doc = XMLTools.parse( sr );
            filter = AbstractFilter.buildFromDOM( doc.getDocumentElement() );
          }
          catch( Exception e )
          {}
          typeFilter.put( typenames[i], filter );
        }
        else
        {
          typeFilter.put( typenames[i], null );
        }
      }
    }
    else
    {
      throw new InconsistentRequestException( "A Filter can't be defined with a FeatureId" );
    }

    // BBOX
    tmp = (String)model.remove( "BBOX" );
    if( tmp != null && featureIds == null )
    {
      Filter filter = null;
      try
      {
        double[] tempa = StringExtend.toArrayDouble( tmp, "," );
        GM_Envelope bbox = GeometryFactory.createGM_Envelope( tempa[0], tempa[1], tempa[2],
            tempa[3] );
      }
      catch( Exception e )
      {}
    }
    else
    {
      throw new InconsistentRequestException( "A BBOX can't be defined with a FeatureId" );
    }

    // outputFormat
    String outputFormat = (String)model.remove( "OUTPUTFORMAT" );
    if( outputFormat == null )
    {
      outputFormat = "GML2";
    }

    WFSQuery[] queries = null;
    if( typenames != null )
    {
      queries = new WFSQuery[typenames.length];
      for( int i = 0; i < typenames.length; i++ )
      {
        queries[i] = createQuery( (String[])typeProp.get( typenames[i] ), null, version,
            typenames[i], (Filter)typeFilter.get( typenames[i] ) );
      }
    }
    else
    {}
    //createQuery(String[] propertyNames, String handle, String version, String
    // typeName, Filter filter)
    // TODO create WFSGetFeatureRequest
    WFSGetFeatureRequest gfRequest = null;

    Debug.debugMethodEnd();
    return gfRequest;
  }

  /**
   * creates a <tt>WFSGetFeatureRequest</tt> object.
   * 
   * @param id
   *          id of the request
   * @param vendorSpecificParameter
   *          none standadized parameters as name-value pairs
   * @param native_
   *          is intended to allow access to vendor specific capabilities
   * @param outputFormat
   *          indicates the format the result shall be formated
   * @param filter
   *          filter expression that describes the 'global' limitations of the
   *          query.
   * @param maxFeatures
   *          maximal amout of featuers that shall be returned
   * @param startPosition
   *          index of the feature the query shall start
   * @param query
   *          a set of Query objects that describes the query to perform
   */
  public static WFSGetFeatureRequest createWFSGetFeatureRequest( String version, String id,
      HashMap vendorSpecificParameter, WFSNative native_, String outputFormat, String handle,
      Filter filter, int maxFeatures, int startPosition, WFSQuery[] query )
  {
    Debug.debugMethodBegin();

    WFSGetFeatureRequest req = new WFSGetFeatureRequest_Impl( version, id, vendorSpecificParameter,
        native_, outputFormat, handle, filter, maxFeatures, startPosition, query, null, null, null );
    Debug.debugMethodEnd();
    return req;
  }

  /**
   * creates a instance of a <tt>WFSGetFeatureRequest</tt> object from a
   * reader that contains the request-xml.
   * 
   * @param doc
   *          DOM object that contains the request
   * @param id
   *          id of the request
   */
  public static WFSGetFeatureRequest createWFSGetFeatureRequest( String id, Document doc )
      throws Exception
  {
    Debug.debugMethodBegin();

    WFSGetFeatureRequest req = null;

    Element element = doc.getDocumentElement();

    String outputFormat = XMLTools.getAttrValue( element, "outputFormat" );
    String handle = XMLTools.getAttrValue( element, "handle" );
    WFSQuery[] queries = getQuery( element );
    Filter filter = getFilter( element );
    int maxFeatures = -1;

    try
    {
      maxFeatures = Integer.parseInt( XMLTools.getAttrValue( element, "maxFeatures" ).trim() );
    }
    catch( Exception ex )
    {}

    int startPosition = -1;

    try
    {
      startPosition = Integer.parseInt( XMLTools.getAttrValue( element, "startPosition" ).trim() );
    }
    catch( Exception ex )
    {}

    WFSNative native_ = getNative( element );

    req = createWFSGetFeatureRequest( "1.0.0", id, null, native_, outputFormat, handle, filter,
        maxFeatures, startPosition, queries );

    Debug.debugMethodEnd();
    return req;
  }

  /**
   * The <Native>element is intended to allow access to vendor specific
   * capabilities of any particular web feature server or datastore. The
   * <Native>tag simply delimits the vendor specific command or operation.
   */
  public static WFSNative getNative( Element element )
  {
    Debug.debugMethodBegin();
    NodeList nl = element.getChildNodes();

    WFSNative native_ = null;

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      for( int i = 0; i < nl.getLength(); i++ )
      {
        if( nl.item( i ) instanceof Element && nl.item( i ).getLocalName().equals( "Native" ) )
        {
          String vendorId = XMLTools.getAttrValue( element, "vendorId" );
          String s = XMLTools.getAttrValue( element, "safeToIgnore" );
          boolean safeToIgnore = s.equalsIgnoreCase( "true" );
          String n = nl.item( i ).getNodeValue();
          native_ = new WFSNative_Impl( n, vendorId, safeToIgnore );
        }
      }
    }
    Debug.debugMethodEnd();
    return native_;
  }

  /**
   * The query defines which feature type to query, what properties to retrieve
   * and what constraints (spatial and non-spatial) to apply to those
   * properties.
   * <p>
   * only used for xml-coded requests
   */
  private static WFSQuery[] getQuery( Element element ) throws Exception
  {
    Debug.debugMethodBegin();

    NodeList nl = element.getChildNodes();
    ArrayList list = new ArrayList();

    if( ( nl != null ) && ( nl.getLength() > 0 ) )
    {
      for( int i = 0; i < nl.getLength(); i++ )
      {
        if( nl.item( i ) instanceof Element
            && XMLTools.toLocalName( nl.item( i ).getNodeName() ).equals( "Query" ) )
        {
          //if (nl.item(i) instanceof Element &&
          // nl.item(i).getLocalName().equals("Query")) {
          Element elem = (Element)nl.item( i );
          String[] propertyNames = getPropertyNames( elem );
          String handle = XMLTools.getAttrValue( elem, "handle" );
          String version = XMLTools.getAttrValue( elem, "version" );
          String typeName = XMLTools.getAttrValue( elem, "typeName" );
          Filter filter = getFilter( elem );
          list.add( new WFSQuery_Impl( propertyNames, handle, version, typeName, filter ) );
        }
      }
    }

    Debug.debugMethodEnd();

    return (WFSQuery[])list.toArray( new WFSQuery[list.size()] );
  }

  /**
   * The property names is used to enumerate the feature properties or
   * attributes that should be selected. If no property names are specified then
   * all properties should be fetched.
   */
  private static String[] getPropertyNames( Element element )
  {
    Debug.debugMethodBegin();

    String[] propertynames = null;
    NodeList nl = element.getChildNodes();
    ArrayList list = new ArrayList();

    if( nl != null )
    {
      if( nl.getLength() > 0 )
      {
        for( int i = 0; i < nl.getLength(); i++ )
        {
          if( nl.item( i ) instanceof Element
              && nl.item( i ).getLocalName().equals( "PropertyName" ) )
          {
            String s = nl.item( i ).getFirstChild().getNodeValue();
            if( s.startsWith( "/" ) )
            {
              s = s.substring( 1, s.length() );
            }
            list.add( s );
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
  public static Filter getFilter( Element element ) throws FilterConstructionException
  {
    Debug.debugMethodBegin();

    Filter filter = null;

    Element el = XMLTools.getChildByName( "Filter", ogcNS, element );
    if( el != null )
    {
      filter = AbstractFilter.buildFromDOM( el );
    }

    Debug.debugMethodEnd();
    return filter;
  }

  /**
   * creates a <tt>WFSGetFeatureResponse</tt> object
   * 
   * @param request
   *          a copy of the request that leads to this response
   * @param exception
   *          a describtion of an excetion (only if raised)
   * @param affectedFeatureTypes
   *          names of the feature types affected by the response
   * @param response
   *          the response to the request
   */
  public static WFSGetFeatureResponse createWFSGetFeatureResponse( OGCWebServiceRequest request,
      String[] affectedFeatureTypes, OGCWebServiceException exception, Object response )
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

    WFSGetFeatureResponse res = new WFSGetFeatureResponse_Impl( request, affectedFeatureTypes, doc,
        response );

    Debug.debugMethodEnd();
    return res;
  }

  /**
   * creates a <tt>WFSGetCapabilitiesRequest</tt> object.
   * 
   * @param id
   *          id of the request
   * @param vendorSpecificParameter
   *          none standadized parameters as name-value pairs
   * @param native_
   *          is intended to allow access to vendor specific capabilities
   */
  public static WFSGetCapabilitiesRequest createWFSGetCapabilitiesRequest( String id,
      HashMap vendorSpecificParameter, WFSNative native_ )
  {
    Debug.debugMethodBegin();

    WFSGetCapabilitiesRequest req = new WFSGetCapabilitiesRequest_Impl( "1.0.0", id,
        vendorSpecificParameter, native_ );

    Debug.debugMethodEnd();
    return req;
  }

  /**
   * creates a <tt>WFSGetCapabilitiesResponse</tt> object
   * 
   * @param request
   *          a copy of the request that leads to this response
   * @param exception
   *          a describtion of an excetion (only if raised)
   * @param response
   *          the response to the request
   */
  public static WFSGetCapabilitiesResponse createWFSGetCapabilitiesResponse(
      OGCWebServiceRequest request, OGCWebServiceException exception, WFSCapabilities response )
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

    WFSGetCapabilitiesResponse res = new WFSGetCapabilitiesResponse_Impl( request, doc, response );

    Debug.debugMethodEnd();
    return res;
  }

  /**
   * creates a <tt>WFSQuery</tt> object from its property names, a handle the
   * used version, the feature type (typeName) targeted by the query and a
   * filter.
   */
  public static WFSQuery createQuery( String[] propertyNames, String handle, String version,
      String typeName, Filter filter )
  {
    return new WFSQuery_Impl( propertyNames, handle, version, typeName, filter );
  }

  /**
   * creates a WFS Query object from a dom element.
   * 
   * @param element
   *          dom element containing a WFS query
   */
  public static WFSQuery createQuery( Element element ) throws Exception
  {
    Debug.debugMethodBegin();

    String[] propertyNames = getPropertyNames( element );
    String handle = XMLTools.getAttrValue( element, "handle" );
    String version = XMLTools.getAttrValue( element, "version" );
    String typeName = XMLTools.getAttrValue( element, "typeName" );
    Filter filter = getFilter( element );
    WFSQuery query = new WFSQuery_Impl( propertyNames, handle, version, typeName, filter );

    Debug.debugMethodEnd();
    return query;
  }

  /**
   * creates a <tt>WFSTransactionRequest</tt> object.
   * 
   * @param id
   *          id of the request
   * @param lockId
   * @param operations
   * @param handle
   * @param releaseAction
   * @param version
   */
  public static WFSTransactionRequest createWFSTransactionRequest( String version, String id,
      String lockId, WFSOperation[] operations, String handle, String releaseAction )
  {
    Debug.debugMethodBegin();

    WFSTransactionRequest tr = new WFSTransactionRequest_Impl( version, id, lockId, operations,
        handle, releaseAction );

    Debug.debugMethodEnd();
    return tr;
  }

  /**
   * creates a <tt>WFSTransactionRequest</tt> object.
   * 
   * @param id
   *          id of the request
   * @param doc
   *          DOM objects that contains the request
   */
  public static WFSTransactionRequest createWFSTransactionRequest( String id, Document doc )
      throws XMLParsingException
  {
    Debug.debugMethodBegin();

    ArrayList ops = new ArrayList();
    WFSTransactionRequest tr = null;

    Element element = doc.getDocumentElement();

    String wfsNS = XMLTools.getAttrValue( element, "xmlns:wfs" );

    if( wfsNS == null )
    {
      wfsNS = "";
    }

    String releaseAction = XMLTools.getAttrValue( element, "releaseAction" );
    String handle = XMLTools.getAttrValue( element, "handle" );
    String version = XMLTools.getAttrValue( element, "version" );

    if( version == null )
    {
      version = "1.0.0";
    }

    ElementList el = XMLTools.getChildElements( element );
    for( int i = 0; i < el.getLength(); i++ )
    {
      Element child = el.item( i );
      String namespace = child.getNamespaceURI();
      if( namespace != null && !wfsNS.equals( namespace ) )
      {
        continue;
      }
      String childName = child.getLocalName();
      if( childName.equals( "Insert" ) )
      {
        ops.add( createInsert( child ) );
      }
      else if( childName.equals( "Update" ) )
      {
        ops.add( createUpdate( child ) );
      }
      else if( childName.equals( "Delete" ) )
      {
        ops.add( createDelete( child ) );
      }
      else if( childName.equals( "Native" ) )
      {
        ops.add( createNative( child ) );
      }
    }
    WFSOperation[] operations = (WFSOperation[])ops.toArray( new WFSOperation[ops.size()] );

    String lockId = null;
    Element elem = XMLTools.getNamedChild( element, wfsNS, "LockId" );

    if( elem != null )
    {
      lockId = elem.getFirstChild().getNodeValue();
    }

    tr = new WFSTransactionRequest_Impl( version, id, lockId, operations, handle, releaseAction );

    Debug.debugMethodEnd();
    return tr;
  }

  /**
   * creates a <tt>WFSInsert</tt> object from an array of <tt>GMLFeature</tt>
   * and a handle.
   */
  public static WFSInsert createInsert( GMLFeature[] feat, String handle )
  {
    return new WFSInsert_Impl( feat, handle );
  }

  /**
   * creates a <tt>WFSDelete</tt> object from a <tt>Filter</tt> and a
   * typeName specifying the feature type to perform the deletion on.
   */
  public static WFSDelete createDelete( Filter filter, String typeName )
  {
    return new WFSDelete_Impl( filter, typeName );
  }

  /**
   * Creates a <tt>WFSInsert</tt> object from a wfs:Update-element as defined
   * in the OGC-WFS specification.
   * <p>
   * 
   * @param element
   *          the 'Insert'- <tt>Element</tt>
   * @return the constructed <tt>WFSInsert</tt> -instance
   */
  private static WFSInsert createInsert( Element element ) throws FilterConstructionException
  {

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

    GMLFeature[] feat = (GMLFeature[])list.toArray( new GMLFeature[list.size()] );

    // create insert operation
    return new WFSInsert_Impl( feat, handle );
  }

  /**
   * Creates a <tt>WFSDelete</tt> object from a wfs:Delete-element as defined
   * in the OGC-WFS specification.
   * <p>
   * 
   * @param element
   *          the 'Delete'- <tt>Element</tt>
   * @return the constructed <tt>WFSDelete</tt> -instance
   */
  private static WFSDelete createDelete( Element element ) throws FilterConstructionException
  {
    Filter filter = getFilter( element );
    String typeName = XMLTools.getAttrValue( element, "typeName" );
    return new WFSDelete_Impl( filter, typeName );
  }

  /**
   * Creates a <tt>WFSUpdate</tt> object from a wfs:Update-element as defined
   * in the OGC-WFS specification.
   * <p>
   * 
   * @param element
   *          the 'Update'- <tt>Element</tt>
   * @return the constructed <tt>WFSUpdate</tt> -instance
   */
  private static WFSUpdate createUpdate( Element element ) throws XMLParsingException
  {

    // get the typeName (FeatureType) to be updated
    String typeName = XMLTools.getRequiredAttrValue( "typeName", element );

    // get the operations handle (may be null)
    Filter filter = getFilter( element );

    // collect the properties to be changed
    HashMap properties = new HashMap();
    NodeList nl = element.getElementsByTagNameNS( wfsNS, "Property" );

    for( int i = 0; i < nl.getLength(); i++ )
    {
      Node node = nl.item( i );
      String name = XMLTools.getRequiredStringValue( "Name", wfsNS, node );
      Element valueNode = XMLTools.getRequiredChildByName( "Value", wfsNS, node );

      String value = null;
      Element firstChild = XMLTools.getFirstElement( valueNode );
      if( firstChild != null )
      {
        value = DOMPrinter.nodeToString( firstChild, "UTF-8" );
      }
      else
      {
        value = valueNode.getFirstChild().getNodeValue();
      }
      //System.out.println ("Name: " + name + ", value: " + value);
      properties.put( name, value );
    }

    // create update operation
    return new WFSUpdate_Impl( typeName, properties, filter );
  }

  /**
   * creates nativ operations object from a XML-element defined by the OGC WFS
   * specifications
   */
  private static WFSNative createNative( Element element )
  {
    Debug.debugMethodBegin();

    Debug.debugMethodEnd();
    return null;
  }

  /**
   * creates a <tt>WFSTransactionRequest</tt> object.
   * 
   * @param request
   *          request that lead to the response
   * @param affectedFeatureTypes
   *          feature types that has been affected by the request
   * @param exception
   *          exception if raised
   * @param status
   *          termination status of the transaction (Success|Partial|Failed)
   * @param handle
   *          something to identify the failure if one has occured
   */
  public static WFSTransactionResponse createWFSTransactionResponse( OGCWebServiceRequest request,
      String[] affectedFeatureTypes, OGCWebServiceException exception,
      WFSInsertResult[] insertResults, String status, String handle )
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

    WFSTransactionResponse response = new WFSTransactionResponse_Impl( request,
        affectedFeatureTypes, doc, insertResults, status, handle );

    Debug.debugMethodEnd();
    return response;
  }

  /**
   * creates a <tt>WFSInsertResult</tt>
   */
  public static WFSInsertResult createWFSInsertResult( String handle, String[] featureIds )
  {
    Debug.debugMethodBegin();

    WFSInsertResult result = new WFSInsertResult_Impl( handle, featureIds );

    Debug.debugMethodEnd();
    return result;
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
 * Revision 1.3  2004/10/07 14:09:12  doemming
 * *** empty log message ***
 *
 * Revision 1.1  2004/09/02 23:56:58  doemming
 * *** empty log message ***
 * Revision 1.3 2004/08/31 12:53:32 doemming
 * *** empty log message *** Revision 1.29 2004/06/24 14:23:04 poth no message
 * 
 * Revision 1.28 2004/04/27 15:40:38 poth no message
 * 
 * Revision 1.27 2004/04/07 06:43:50 poth no message
 * 
 * Revision 1.26 2004/04/05 07:36:39 poth no message
 * 
 * Revision 1.25 2004/03/26 11:19:32 poth no message
 * 
 * Revision 1.24 2004/03/12 15:39:50 tfr fixed bug in method
 * createWFSDescribeFeatureTypeRequest()
 * 
 * Revision 1.21 2004/01/26 08:10:37 poth no message
 * 
 * Revision 1.20 2004/01/23 08:26:51 poth no message
 * 
 * Revision 1.19 2003/11/28 11:35:57 poth no message
 * 
 * Revision 1.18 2003/11/26 17:05:37 poth no message
 * 
 * Revision 1.17 2003/08/18 07:23:26 poth no message
 * 
 * Revision 1.16 2003/07/25 16:07:33 poth no message
 * 
 * Revision 1.15 2003/07/21 07:50:47 poth no message
 * 
 * Revision 1.14 2003/06/10 07:52:17 poth no message
 * 
 * Revision 1.13 2003/06/02 07:11:49 poth no message
 * 
 * Revision 1.12 2003/05/26 07:17:18 poth no message
 * 
 * Revision 1.11 2003/05/20 15:49:19 mrsnyder WFSUpdate/WFSDelete-Requests
 * should be working now. Added PointDBDataStore support for these requests,
 * too.
 * 
 * Revision 1.10 2003/05/15 15:55:12 mrsnyder Implemented WFSUpdate. Fixed some
 * WFSInsert problems.
 */
