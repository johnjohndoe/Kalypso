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
package org.deegree_impl.services.wms.capabilities;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.StringTokenizer;

import org.deegree.enterprise.Proxy;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.services.OGCWebService;
import org.deegree.services.capabilities.CException;
import org.deegree.services.capabilities.DCPType;
import org.deegree.services.capabilities.HTTP;
import org.deegree.services.capabilities.MetadataURL;
import org.deegree.services.capabilities.Service;
import org.deegree.services.wcs.capabilities.WCSCapabilities;
import org.deegree.services.wcs.protocol.WCSGetCoverageRequest;
import org.deegree.services.wfs.capabilities.WFSCapabilities;
import org.deegree.services.wfs.protocol.WFSQuery;
import org.deegree.services.wms.capabilities.Attribution;
import org.deegree.services.wms.capabilities.AuthorityURL;
import org.deegree.services.wms.capabilities.Capability;
import org.deegree.services.wms.capabilities.DataSource;
import org.deegree.services.wms.capabilities.DataURL;
import org.deegree.services.wms.capabilities.DeegreeParam;
import org.deegree.services.wms.capabilities.Dimension;
import org.deegree.services.wms.capabilities.Extent;
import org.deegree.services.wms.capabilities.FeatureListURL;
import org.deegree.services.wms.capabilities.Format;
import org.deegree.services.wms.capabilities.GazetteerParam;
import org.deegree.services.wms.capabilities.Identifier;
import org.deegree.services.wms.capabilities.Layer;
import org.deegree.services.wms.capabilities.LayerBoundingBox;
import org.deegree.services.wms.capabilities.LegendURL;
import org.deegree.services.wms.capabilities.Operation;
import org.deegree.services.wms.capabilities.Request;
import org.deegree.services.wms.capabilities.ScaleHint;
import org.deegree.services.wms.capabilities.Style;
import org.deegree.services.wms.capabilities.StyleSheetURL;
import org.deegree.services.wms.capabilities.StyleURL;
import org.deegree.services.wms.capabilities.UserDefinedSymbolization;
import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.deegree.services.wms.protocol.WMSGetMapRequest;
import org.deegree.xml.ElementList;
import org.deegree.xml.XMLParsingException;
import org.deegree.xml.XMLTools;
import org.deegree_impl.enterprise.Proxy_Impl;
import org.deegree_impl.model.ct.GeoTransformer;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.services.capabilities.CException_Impl;
import org.deegree_impl.services.capabilities.DCPType_Impl;
import org.deegree_impl.services.capabilities.HTTP_Impl;
import org.deegree_impl.services.wcs.WCSFactory;
import org.deegree_impl.services.wcs.capabilities.WCSCapabilitiesFactory;
import org.deegree_impl.services.wcs.protocol.WCSProtocolFactory;
import org.deegree_impl.services.wfs.RemoteWFService;
import org.deegree_impl.services.wfs.WFSFactory;
import org.deegree_impl.services.wfs.capabilities.WFSCapabilitiesFactory;
import org.deegree_impl.services.wfs.protocol.WFSProtocolFactory;
import org.deegree_impl.services.wms.RemoteWMService;
import org.deegree_impl.services.wms.protocol.WMSProtocolFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.IDGenerator;
import org.deegree_impl.tools.NetWorker;
import org.deegree_impl.tools.StringExtend;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Factory class for creating WMS capability classes from WMS capabilities XML
 * documents that comply to the deegree WMS 1.1.1 specification.
 * <p>
 * 
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public class DeegreeWMSCapabilitiesFactory extends OGCWMSCapabilitiesFactory
{
  private String deegreeRoot = "";

  private URL defaultOnlineResource = null;

  private HashMap owsMap = new HashMap();

  /**
   * Creates a <tt>WMSCapabilities</tt> -instance according to the contents of
   * the DOM-subtree starting at the given 'WMT_MS_Capabilities'-
   * <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'WMT_MS_Capabilities'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>WMSCapabilities</tt> -instance
   */
  protected WMSCapabilities createCapabilities( Element element ) throws XMLParsingException
  {

    // required: 'version'-attribute
    version = XMLTools.getRequiredAttrValue( "version", element );

    // optional: 'updateSequence'-attribute
    String updateSequence = XMLTools.getAttrValue( "updateSequence", element );

    // required: <DeegreeParam>
    DeegreeParam deegreeParam = createDeegreeParam( XMLTools.getRequiredChildByName(
        "DeegreeParam", wmsNS, element ) );

    // required: <Service>
    Service service = createService( XMLTools.getRequiredChildByName( "Service", wmsNS, element ) );

    // required: <Capability>
    Capability capability = createCapability( XMLTools.getRequiredChildByName( "Capability", wmsNS,
        element ) );

    return new WMSCapabilities_Impl( version, updateSequence, service, capability, deegreeParam );
  }

  /**
   * Creates a <tt>DeegreeParam</tt> -instance according to the contents of
   * the DOM-subtree starting at the given 'DeegreeParam'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'DeegreeParam'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>Service</tt> -instance
   */
  protected DeegreeParam createDeegreeParam( Element element ) throws XMLParsingException
  {
    // required: <RootDirectory>
    deegreeRoot = XMLTools.getRequiredStringValue( "RootDirectory", wmsNS, element );

    // check if the root directory really exists
    File file = new File( deegreeRoot );
    if( !file.exists() )
    {
      throw new XMLParsingException( "deegree root directory: " + deegreeRoot + " does not exist!" );
    }

    // required: <DefaultOnlineResource>
    defaultOnlineResource = createOnlineResource( XMLTools.getRequiredChildByName(
        "DefaultOnlineResource", wmsNS, element ) );

    // optional: <CacheSize>, default: 100 (MB)
    int cacheSize = (int)XMLTools.getDoubleValue( "CacheSize", wmsNS, element, 100 );

    // optional: <MaxLifeTime>, default: 3600 (sec)
    int maxLifeTime = (int)XMLTools.getDoubleValue( "MaxLifeTime", wmsNS, element, 3600 );

    // optional: <RequestTimeLimit>, default: 15 (sec)
    int requestTimeLimit = (int)XMLTools.getDoubleValue( "RequestTimeLimit", wmsNS, element, 15 );

    // optional: <MapQuality>, default: 0.95
    float mapQuality = (float)XMLTools.getDoubleValue( "MapQuality", wmsNS, element, 0.95 );

    // optional: <MaxMapWidth>, default: 1000
    int maxMapWidth = (int)XMLTools.getDoubleValue( "MaxMapWidth", wmsNS, element, 1000 );

    // optional: <MaxMapHeight>, default: 1000
    int maxMapHeight = (int)XMLTools.getDoubleValue( "MaxMapHeight", wmsNS, element, 1000 );

    // optional: <CopyRight>, default: ""
    String copyRight = XMLTools.getStringValue( "Copyright", wmsNS, element, "" );

    // optional: <Gazetteer>
    Element gazetteerElement = XMLTools.getChildByName( "Gazetteer", wmsNS, element );

    GazetteerParam gazetteer = createGazetteerParameter( gazetteerElement );

    // optional: <SchemaLocation>
    Element schemaLocationElement = XMLTools.getChildByName( "SchemaLocation", wmsNS, element );
    URL schemaLocation = null;
    if( schemaLocationElement != null )
    {
      schemaLocation = createOnlineResource( XMLTools.getRequiredChildByName( "OnlineResource",
          wmsNS, schemaLocationElement ) );
    }

    // optional: <DTDLocation>
    Element dtdLocationElement = XMLTools.getChildByName( "DTDLocation", wmsNS, element );
    URL dtdLocation = null;
    if( dtdLocationElement != null )
    {
      dtdLocation = createOnlineResource( XMLTools.getRequiredChildByName( "OnlineResource", wmsNS,
          dtdLocationElement ) );
    }
    else
    {
      // default dtd location
      try
      {
        dtdLocation = new URL( "http://schemas.opengis.net/wms/1.1.1/WMS_MS_Capabilities.dtd" );
      }
      catch( Exception e )
      {}
    }

    // optional: <Proxy>
    Element proxyElement = XMLTools.getChildByName( "Proxy", wmsNS, element );
    Proxy proxy = createProxy( proxyElement );

    return new DeegreeParam_Impl( cacheSize, maxLifeTime, requestTimeLimit, mapQuality,
        defaultOnlineResource, deegreeRoot, maxMapWidth, maxMapHeight, copyRight, gazetteer,
        schemaLocation, dtdLocation, proxy );
  }

  /**
   * creates an object that describes the access to a gazetteer if one have been
   * defined at the <DeegreeParam>section of the capabilities/configuration
   */
  private GazetteerParam createGazetteerParameter( Element element ) throws XMLParsingException
  {
    GazetteerParam gazetteer = null;

    if( element != null )
    {
      // required: <OnlineResource>
      URL onlineResource = createOnlineResource( XMLTools.getRequiredChildByName( "OnlineResource",
          wmsNS, element ) );
      // optional: <LocationRadius>, default: 10
      double radius = XMLTools.getDoubleValue( "LocationRadius", wmsNS, element, 10 );
      gazetteer = new GazetteerParam_Impl( onlineResource, radius );
    }

    return gazetteer;
  }

  /**
   * creates a <tt>Proxy</tt> instance from the passed <tt>Element</tt>
   * encapsulating the proxy address to be used with the WMS
   */
  private Proxy createProxy( Element proxyElement ) throws XMLParsingException
  {
    if( proxyElement != null )
    {
      String proxyHost = XMLTools.getRequiredAttrValue( "proxyHost", proxyElement );
      String proxyPort = XMLTools.getRequiredAttrValue( "proxyPort", proxyElement );
      Proxy proxy = new Proxy_Impl( proxyHost, proxyPort );
      // set the proxy address to the system properties
      proxy.setProxy( true );
      return proxy;
    }
    else
    {
      return null;
    }
  }

  /**
   * Creates a <tt>Capability</tt> -instance according to the contents of the
   * DOM-subtree starting at the given 'Capability'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'Capability'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>Capability</tt> -instance
   */
  protected Capability createCapability( Element element ) throws XMLParsingException
  {

    // optional: <Request>
    Request request = null;
    Element requestElement = XMLTools.getChildByName( "Request", wmsNS, element );
    if( requestElement != null )
    {
      request = createRequest( requestElement );
    }
    else
    {
      // create defaultOperations (getCapabilities, getMap, getFeatureInfo)
      Operation[] operations = new Operation[3];
      operations[0] = createGetCapabilities();
      operations[1] = createGetMap();
      operations[2] = createGetFeatureInfo();
      request = new Request_Impl( operations );
    }

    // optional: <Exception>
    CException exception = null;
    Element exceptionElement = XMLTools.getChildByName( "Exception", wmsNS, element );
    if( exceptionElement != null )
    {
      exception = createException( XMLTools.getRequiredChildByName( "Exception", wmsNS, element ) );
      exception.addFormat( "application/vnd.ogc.se_xml" );
    }
    else
    {
      String[] defaultFormats = new String[1];
      defaultFormats[0] = "application/vnd.ogc.se_xml";
      exception = new CException_Impl( defaultFormats );
    }

    // optional: <VendorSpecificCapabilities>
    Document vendor = null;
    Element vendorElement = XMLTools.getChildByName( "VendorSpecificCapabilities", wmsNS, element );
    if( vendorElement != null )
    {
      vendor = createVendorSpecificCapabilities( vendorElement );
    }

    // optional: <VendorSpecificCapabilities>
    UserDefinedSymbolization userSymbolization = null;
    Element userSymbolizationElement = XMLTools.getChildByName( "UserDefinedSymbolization", wmsNS,
        element );

    if( userSymbolizationElement != null )
    {
      userSymbolization = createUserDefinedSymbolization( userSymbolizationElement );
    }

    // optional: <Layer>
    Layer layer = null;
    Element layerElement = XMLTools.getChildByName( "Layer", wmsNS, element );

    if( layerElement != null )
    {
      layer = createLayer( layerElement, null );
    }

    return new Capability_Impl( request, exception, vendor, userSymbolization, layer );
  }

  /**
   * Creates a <tt>Request</tt> -instance according to the contents of the
   * DOM-subtree starting at the given 'Request'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'Request'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>Request</tt> -instance
   */
  protected Request createRequest( Element element ) throws XMLParsingException
  {
    ArrayList operationList = new ArrayList();
    ElementList elementList = XMLTools.getChildElements( element );

    for( int i = 0; i < elementList.getLength(); i++ )
    {
      Element operationElement = elementList.item( i );
      String name = operationElement.getLocalName();

      if( name.equals( "GetCapabilities" ) || name.equals( "GetMap" )
          || name.equals( "GetFeatureInfo" ) || name.equals( "DescribeLayer" )
          || name.equals( "GetLegendGraphic" ) || name.equals( "GetStyles" )
          || name.equals( "PutStyles" ) || name.equals( "GetScaleBar" ) )
      {
        operationList.add( createOperation( operationElement ) );
      }
      else
      {
        throw new XMLParsingException( "Element '" + name + "' in element 'Request' is not "
            + "a known Operation!" );
      }
    }

    Operation[] operations = (Operation[])operationList
        .toArray( new Operation[operationList.size()] );
    Request request = new Request_Impl( operations );

    // create default Operations if missing
    URL[] defaultURLs = new URL[1];
    defaultURLs[0] = defaultOnlineResource;

    // check / add getCapabilities-Operation
    Operation getCapabilities = request.getOperation( Operation.GETCAPABILITIES );
    if( getCapabilities == null )
    {
      request.addOperation( createGetCapabilities() );
    }
    else
    {
      Format fo = new Format_Impl( "application/vnd.ogc.wms_xml" );
      getCapabilities.addFormat( fo );

      DCPType[] dcpTypes = getCapabilities.getDCPTypes();

      if( dcpTypes.length != 0 )
      {
        HTTP http = (HTTP)dcpTypes[0].getProtocol();

        if( http.getGetOnlineResources().length == 0 )
        {
          http.addGetOnlineResource( defaultOnlineResource );
        }
      }
      else
      {
        getCapabilities.addDCPType( new DCPType_Impl( new HTTP_Impl( defaultURLs, new URL[0] ) ) );
      }
    }

    // check / add getMap-Operation
    Operation getMap = request.getOperation( Operation.GETMAP );
    if( getMap == null )
    {
      request.addOperation( createGetMap() );
    }
    else
    {
      getMap.addFormat( new Format_Impl( "image/gif" ) );
      getMap.addFormat( new Format_Impl( "image/png" ) );
      getMap.addFormat( new Format_Impl( "image/jpeg" ) );

      DCPType[] dcpTypes = getMap.getDCPTypes();

      if( dcpTypes.length != 0 )
      {
        HTTP http = (HTTP)dcpTypes[0].getProtocol();
        if( ( http.getPostOnlineResources().length != 0 )
            && ( http.getGetOnlineResources().length == 0 ) )
        {
          http.addGetOnlineResource( defaultOnlineResource );
        }
        else if( ( http.getPostOnlineResources().length == 0 )
            && ( http.getGetOnlineResources().length == 0 ) )
        {
          http.addGetOnlineResource( defaultOnlineResource );
          http.addPostOnlineResource( defaultOnlineResource );
        }
      }
      else
      {
        getMap.addDCPType( new DCPType_Impl( new HTTP_Impl( defaultURLs, defaultURLs ) ) );
      }
    }
    // check / add getFeatureInfo-Operation
    Operation getFeatureInfo = request.getOperation( Operation.GETFEATUREINFO );
    if( getFeatureInfo == null )
    {
      request.addOperation( createGetFeatureInfo() );
    }
    else
    {
      URL url = null;

      try
      {
        url = new URL( "file:///" + deegreeRoot + "/WEB-INF/xml/featureinfo2html.xsl" );
      }
      catch( MalformedURLException e )
      {
        throw new XMLParsingException( "Constructed URL for getFeatureInfo '" + "file:///"
            + deegreeRoot + "/WEB-INF/xml/featureinfo2html.xsl' does " + "not denote a valid URL!" );
      }

      getFeatureInfo.addFormat( new Format_Impl( "application/vnd.ogc.gml" ) );
      getFeatureInfo.addFormat( new Format_Impl( "text/html", url ) );

      DCPType[] dcpTypes = getFeatureInfo.getDCPTypes();

      if( dcpTypes.length != 0 )
      {
        HTTP http = (HTTP)dcpTypes[0].getProtocol();

        if( http.getGetOnlineResources().length == 0 )
        {
          http.addGetOnlineResource( defaultOnlineResource );
        }
      }
      else
      {
        getFeatureInfo.addDCPType( new DCPType_Impl( new HTTP_Impl( defaultURLs, new URL[0] ) ) );
      }
    }

    return request;
  }

  /**
   * Creates an <tt>Operation</tt> -instance according to the contents of the
   * DOM-subtree starting at the given <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the <tt>Element</tt> that describes an <tt>Operation</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>Operation</tt> -instance
   */
  protected Operation createOperation( Element element ) throws XMLParsingException
  {

    // use node name as name of the Operation to be defined
    String responsibleClass = XMLTools.getAttrValue( "responsibleClass", element );
    String name = element.getNodeName();

    // optional: <Format>(s)
    ElementList nodelist = XMLTools.getChildElementsByName( "Format", wmsNS, element );
    Format[] formats = new Format[nodelist.getLength()];

    for( int i = 0; i < nodelist.getLength(); i++ )
    {
      String s = XMLTools.getStringValue( nodelist.item( i ) );
      String tm = XMLTools.getAttrValue( nodelist.item( i ), "Filter" );
      URL url = null;
      if( tm != null )
      {
        try
        {
          url = new URL( tm );
        }
        catch( Exception e )
        {
          throw new XMLParsingException( e.toString() );
        }
      }
      else if( ( tm == null ) && s.equalsIgnoreCase( "text/html" ) )
      {
        try
        {
          url = new URL( "file:///" + deegreeRoot + "/WEB-INF/xml/featureinfo2html.xsl" );
        }
        catch( Exception e )
        {
          throw new XMLParsingException( e.toString() );
        }
      }

      formats[i] = new Format_Impl( s, url );
    }

    // optional: <DCPType>(s)
    nodelist = XMLTools.getChildElementsByName( "DCPType", wmsNS, element );

    DCPType[] dcpTypes = new DCPType[nodelist.getLength()];

    for( int i = 0; i < nodelist.getLength(); i++ )
    {
      dcpTypes[i] = createDCPType( nodelist.item( i ) );
    }

    if( responsibleClass == null )
    {
      return new Operation_Impl( name, formats, dcpTypes );
    }
    else
    {
      return new Operation_Impl( name, formats, dcpTypes, responsibleClass );
    }

  }

  /**
   * Creates an <tt>URL</tt> -instance according to the contents of the
   * DOM-subtree starting at the given 'OnlineResource'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'OnlineResource'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>URL</tt> -instance
   */
  protected URL createOnlineResource( Element element ) throws XMLParsingException
  {
    URL url = null;

    if( element != null )
    {
      // required: href-Attribute (in <OnlineResource>)
      String href = XMLTools.getRequiredAttrValue( "href", xlnNS, element );

      try
      {
        url = new URL( href );
      }
      catch( MalformedURLException e )
      {
        throw new XMLParsingException( "Value ('" + href + "') of attribute 'href' of "
            + "element 'OnlineResource' does not denote a valid URL: " + e.getMessage() );
      }
    }
    else
    {
      url = defaultOnlineResource;
    }

    return url;
  }

  /**
   * Creates a <tt>Layer</tt> -instance according to the contents of the
   * DOM-subtree starting at the given 'Layer'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'Layer'- <tt>Element</tt>
   * @param bbox
   *          latlon bounding box of the parent layer
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>Layer/tt>-instance
   */
  protected Layer createLayer( Element element, GM_Envelope bbox ) throws XMLParsingException
  {

    // optional: 'queryable'-attribute
    boolean queryable = false;
    String attr = XMLTools.getAttrValue( element, "queryable" );

    if( ( attr != null ) && attr.equals( "1" ) )
    {
      queryable = true;
    }

    // optional: 'cascaded'-attribute
    int cascaded = -1;
    attr = XMLTools.getAttrValue( element, "cascaded" );

    if( attr != null )
    {
      try
      {
        cascaded = Integer.parseInt( attr );
      }
      catch( NumberFormatException e )
      {
        throw new XMLParsingException( "Given value ('" + attr + "') for attribute 'cascaded' of "
            + "element 'Layer' does not denote " + "a valid integer value." );
      }
    }

    // optional: 'opaque'-attribute
    boolean opaque = false;
    attr = XMLTools.getAttrValue( element, "opaque" );

    if( ( attr != null ) && attr.equals( "1" ) )
    {
      opaque = true;
    }

    // optional: 'noSubsets'-attribute
    boolean noSubsets = false;
    attr = XMLTools.getAttrValue( element, "noSubsets" );

    if( ( attr != null ) && attr.equals( "1" ) )
    {
      noSubsets = true;
    }

    // optional: 'fixedWidth'-attribute
    int fixedWidth = -1;
    attr = XMLTools.getAttrValue( element, "fixedWidth" );

    if( attr != null )
    {
      try
      {
        fixedWidth = Integer.parseInt( attr );
      }
      catch( NumberFormatException e )
      {
        throw new XMLParsingException( "Given value ('" + attr
            + "') for attribute 'fixedWidth' of "
            + "element 'Layer' does not denote a valid integer value." );
      }
    }

    // optional: 'fixedHeight'-attribute
    int fixedHeight = -1;
    attr = XMLTools.getAttrValue( element, "fixedHeight" );

    if( attr != null )
    {
      try
      {
        fixedHeight = Integer.parseInt( attr );
      }
      catch( NumberFormatException e )
      {
        throw new XMLParsingException( "Given value ('" + attr
            + "') for attribute 'fixedHeight' of "
            + "element 'Layer' does not denote a valid integer value." );
      }
    }

    // optional: <Name>
    String name = XMLTools.getStringValue( "Name", wmsNS, element, null );

    // check if layer name has been used already
    if( name != null && !name.trim().equals( "" ) )
    {
      name = name.trim();
      if( collectedLayers.contains( name ) )
      {
        throw new XMLParsingException( "Layer with name '" + name + "' is defined more than once!" );
      }
      else
      {
        collectedLayers.add( name );
      }
    }

    // required: <Title>
    String title = XMLTools.getRequiredStringValue( "Title", wmsNS, element );
    title = title.trim();

    // optional: <Abstract>
    String abstractString = XMLTools.getStringValue( "Abstract", wmsNS, element, null );

    // optional: <KeywordList>
    String[] keywords = null;
    Element keywordListElement = XMLTools.getChildByName( "KeywordList", wmsNS, element );

    if( keywordListElement != null )
    {
      ElementList el = XMLTools.getChildElementsByName( "Keyword", wmsNS, keywordListElement );
      keywords = new String[el.getLength()];

      for( int i = 0; i < keywords.length; i++ )
      {
        keywords[i] = XMLTools.getStringValue( el.item( i ) );
      }
    }

    // optional: <SRS>(s)
    ElementList nodelist = XMLTools.getChildElementsByName( "SRS", wmsNS, element );
    ArrayList srsL = new ArrayList( 100 );
    for( int i = 0; i < nodelist.getLength(); i++ )
    {
      String tmp = XMLTools.getStringValue( nodelist.item( i ) );
      if( tmp != null && !tmp.trim().equals( "" ) )
      {
        String[] crs = StringExtend.toArray( tmp, " ,;", true );
        for( int j = 0; j < crs.length; j++ )
        {
          srsL.add( crs[j] );
        }
      }
    }
    String[] srs = (String[])srsL.toArray( new String[srsL.size()] );

    // conditional: <LatLonBoundingBox>; if <LatLonBoundingBox> is not set
    // at least one <BoundingBox> must be set or the <LatLonBoundingBox>
    // must be inherited from a higher layer
    //bbox = null;
    LayerBoundingBox[] bboxes = null;
    Element bboxElement = XMLTools.getChildByName( "LatLonBoundingBox", wmsNS, element );

    if( bboxElement != null )
    {
      bbox = createLatLonBoundingBox( bboxElement );
      bboxes = new LayerBoundingBox[srs.length];
      for( int i = 0; i < srs.length; i++ )
      {
        bboxes[i] = createBoundingBox( bbox, srs[i] );
      }
    }
    else if( bbox != null )
    {
      bboxes = new LayerBoundingBox[srs.length];
      for( int i = 0; i < srs.length; i++ )
      {
        bboxes[i] = createBoundingBox( bbox, srs[i] );
      }
    }
    else
    {
      // conditional: <BoundingBox>(s); if <LatLonBoundingBox> is not defined
      // and can't be inherited from a higher layer at least one <BoundingBox>
      // element must be defined
      nodelist = XMLTools.getChildElementsByName( "BoundingBox", wmsNS, element );

      bboxes = new LayerBoundingBox[nodelist.getLength()];

      for( int i = 0; i < nodelist.getLength(); i++ )
      {
        bboxes[i] = createBoundingBox( nodelist.item( i ) );
      }

      int k = -1;
      while( k + 1 < bboxes.length && !bboxes[++k].getSRS().equals( "EPSG:4326" ) )
        ;

      // ensure that at least bounding box exists
      if( k < bboxes.length && k >= 0 )
      {
        bbox = createLatLonBoundingBox( bboxes[k] );
      }
    }

    // optional: <Dimension>(s)
    nodelist = XMLTools.getChildElementsByName( "Dimension", wmsNS, element );

    Dimension[] dimensions = new Dimension[nodelist.getLength()];

    for( int i = 0; i < nodelist.getLength(); i++ )
    {
      dimensions[i] = createDimension( nodelist.item( i ) );
    }

    // optional: <Extent>(s)
    nodelist = XMLTools.getChildElementsByName( "Extent", wmsNS, element );

    Extent[] extents = new Extent[nodelist.getLength()];

    for( int i = 0; i < nodelist.getLength(); i++ )
    {
      extents[i] = createExtent( nodelist.item( i ) );
    }

    // optional: <Attribution>
    Attribution attribution = null;
    Element attributionElement = XMLTools.getChildByName( "Attribution", wmsNS, element );

    if( attributionElement != null )
    {
      attribution = createAttribution( attributionElement );
    }

    // optional: <AuthorityURL>(s)
    nodelist = XMLTools.getChildElementsByName( "AuthorityURL", wmsNS, element );

    AuthorityURL[] authorityURLs = new AuthorityURL[nodelist.getLength()];

    for( int i = 0; i < nodelist.getLength(); i++ )
    {
      authorityURLs[i] = createAuthorityURL( nodelist.item( i ) );
    }

    // optional: <Identifier>(s)
    nodelist = XMLTools.getChildElementsByName( "Identifier", wmsNS, element );

    Identifier[] identifiers = new Identifier[nodelist.getLength()];

    for( int i = 0; i < nodelist.getLength(); i++ )
    {
      identifiers[i] = createIdentifier( nodelist.item( i ) );
    }

    // optional: <MetadataURL>(s)
    nodelist = XMLTools.getChildElementsByName( "MetadataURL", wmsNS, element );

    MetadataURL[] metadataURLs = new MetadataURL[nodelist.getLength()];

    for( int i = 0; i < nodelist.getLength(); i++ )
    {
      metadataURLs[i] = createMetadataURL( nodelist.item( i ) );
    }

    // optional: <DataURL>(s)
    nodelist = XMLTools.getChildElementsByName( "DataURL", wmsNS, element );

    DataURL[] dataURLs = new DataURL[nodelist.getLength()];

    for( int i = 0; i < nodelist.getLength(); i++ )
    {
      dataURLs[i] = createDataURL( nodelist.item( i ) );
    }

    // optional: <FeatureListURL>(s)
    nodelist = XMLTools.getChildElementsByName( "FeatureListURL", wmsNS, element );

    FeatureListURL[] featureListURLs = new FeatureListURL[nodelist.getLength()];

    for( int i = 0; i < nodelist.getLength(); i++ )
    {
      featureListURLs[i] = createFeatureListURL( nodelist.item( i ) );
    }

    // optional: <Style>(s)
    nodelist = XMLTools.getChildElementsByName( "Style", wmsNS, element );

    ArrayList list = new ArrayList();

    boolean hasDefault = false;
    if( nodelist.getLength() > 0 )
    {
      for( int i = 0; i < nodelist.getLength(); i++ )
      {
        Style style = createStyle( nodelist.item( i ), name );
        if( style.getName().startsWith( "default:" ) )
        {
          hasDefault = true;
        }
        list.add( style );
      }
    }
    else
    {
      // create default style
      URL styleResourceURL = null;
      try
      {
        styleResourceURL = new URL( "file:///" + deegreeRoot + "/WEB-INF/xml/styles.xml" );
      }
      catch( MalformedURLException e )
      {
        throw new XMLParsingException( styleResourceURL + " does not denote a valid URL: "
            + e.getMessage() );
      }

      // create a reference to the default legend image
      LegendURL[] legendURLs = new LegendURL[1];
      URL url = null;
      String tmp = null;
      try
      {
        tmp = NetWorker.url2String( defaultOnlineResource );
        tmp = tmp + "/legend/default.png";
        url = new URL( tmp + "/legend/default.png" );
        legendURLs[0] = new LegendURL_Impl( 30, 30, "image/png", url );
      }
      catch( MalformedURLException e )
      {
        throw new XMLParsingException( tmp + " does not denote a valid URL: " + e.getMessage() );
      }

      Style style = new Style_Impl( "default", "default", "default", legendURLs, null, null,
          styleResourceURL );
      list.add( style );
    }

    // create layer specific default type named:
    // default:%layer_name% if no explict default style has been defined
    if( !hasDefault )
    {
      list.add( createDefaultStyle( name, null ) );
    }

    Style[] styles = (Style[])list.toArray( new Style[list.size()] );

    // optional: <ScaleHint>
    ScaleHint scaleHint = null;
    Element scaleHintElement = XMLTools.getChildByName( "ScaleHint", wmsNS, element );

    if( scaleHintElement != null )
    {
      scaleHint = createScaleHint( scaleHintElement );
    }
    else
    {
      scaleHint = new ScaleHint_Impl( 0, Double.MAX_VALUE );
    }

    // optional: <DataSource>s (this is a deegree specific parameter)
    nodelist = XMLTools.getChildElementsByName( "DataSource", wmsNS, element );

    DataSource[] dataSources = new DataSource[nodelist.getLength()];

    if( nodelist.getLength() > 0 )
    {
      for( int i = 0; i < nodelist.getLength(); i++ )
      {
        dataSources[i] = createDataSource( nodelist.item( i ), name, scaleHint );
      }
    }
    else
    {
      // create default DataSource
      try
      {
        dataSources = new DataSource[1];

        URL owsCapabilities = new URL( "file:///" + deegreeRoot
            + "/WEB-INF/xml/LOCALWFS_capabilities.xml" );
        OGCWebService ows = null;
        // check if the OWS has been initialized before
        if( owsMap.get( owsCapabilities.toExternalForm() ) == null )
        {
          WFSCapabilities capa = null;
          capa = WFSCapabilitiesFactory.createCapabilities( owsCapabilities );
          ows = WFSFactory.createWFSService( capa, null );
          owsMap.put( owsCapabilities.toExternalForm(), ows );
        }
        else
        {
          // get OWS from a global HashMap to ensure that a OWS will
          // only be initialized once
          ows = (OGCWebService)owsMap.get( owsCapabilities.toExternalForm() );
        }
        ScaleHint sh = scaleHint;

        if( sh == null )
        {
          sh = new ScaleHint_Impl( 0, Double.MAX_VALUE );
        }

        dataSources[0] = new DataSource_Impl( name, DataSource.LOCALWFS, "/GEOM", ows,
            owsCapabilities, sh, (WFSQuery)null );
      }
      catch( Exception e )
      {
        e.printStackTrace();
        throw new XMLParsingException( e.toString() );
      }
    }

    // optional: <Layer>(s)
    nodelist = XMLTools.getChildElementsByName( "Layer", wmsNS, element );
    Layer[] layers = new Layer[nodelist.getLength()];
    for( int i = 0; i < nodelist.getLength(); i++ )
    {
      layers[i] = createLayer( nodelist.item( i ), bbox );
    }
    Layer layer = new Layer_Impl( queryable, cascaded, opaque, noSubsets, fixedWidth, fixedHeight,
        name, title, abstractString, bbox, attribution, scaleHint, keywords, srs, bboxes,
        dimensions, extents, authorityURLs, identifiers, metadataURLs, dataURLs, featureListURLs,
        styles, layers, dataSources, null );

    // set the new layer as parent of its child layers
    if( layers != null )
    {
      for( int i = 0; i < layers.length; i++ )
      {
        ( (Layer_Impl)layers[i] ).setParent( layer );
      }
    }

    return layer;
  }

  /**
   * creates a <tt>GM_Envelope</tt> with lat/lon coordinates from the passed
   * <tt>LayerBoundingBox</tt> by transforming the corner coordinates to
   * EPSG:4326.
   * 
   * @param box
   *          <tt>LayerBoundingBox</tt>
   * @return <tt>GM_Envelope</tt> with lat/lon coordinates
   */
  protected GM_Envelope createLatLonBoundingBox( LayerBoundingBox box ) throws XMLParsingException
  {
    Debug.debugMethodBegin( this, "createLatLonBoundingBox(LayerBoundingBox)" );

    GM_Envelope llBBox = null;
    try
    {
      GeoTransformer transformer = new GeoTransformer( "EPSG:4326" );
      llBBox = transformer.transformEnvelope( box, box.getSRS() );
    }
    catch( Exception e )
    {
      Debug.debugException( e, "" );
      throw new XMLParsingException( e.toString() );
    }

    Debug.debugMethodEnd();
    return llBBox;
  }

  /**
   * creates a <tt>LayerBoundingBox</tt> with a CRS othe than EPSG:4326
   * instance from the passed LatLonBoundingBox and the desired CRS
   * 
   * @param llBBox
   *          latlon Bounding Box
   * @param crs
   *          desired target CRS for the returned <tt>LayerBoundingBox</tt>
   * @return <tt>LayerBoundingBox</tt> in the desired CRS
   */
  protected LayerBoundingBox createBoundingBox( GM_Envelope llBBox, String crs )
      throws XMLParsingException
  {
    Debug.debugMethodBegin( this, "createBoundingBox( GM_Envelope, String )" );

    LayerBoundingBox layerBBox = null;

    try
    {
      GeoTransformer transformer = new GeoTransformer( crs );
      GM_Envelope bbox = transformer.transformEnvelope( llBBox, "EPSG:4326" );
      layerBBox = new LayerBoundingBox_Impl( bbox.getMin(), bbox.getMax(), crs, -1, -1 );
    }
    catch( Exception e )
    {
      Debug.debugException( e, "" );
      throw new XMLParsingException( e.toString() );
    }

    Debug.debugMethodEnd();

    return layerBBox;
  }

  /**
   * creates a default style for the submitted layer name. If the submitted
   * LegendURL !=<tt>null</tt> it will be used in the style definition
   * 
   * @param layerName
   *          name of the layer to create a default style for
   * @param lURL
   *          <tt>LegendURL</tt> to be used for style if not <tt>null</tt>
   */
  private Style createDefaultStyle( String layerName, LegendURL[] lURL ) throws XMLParsingException
  {
    LegendURL[] lu = null;
    URL styleResourceURL = null;
    URL legendURL = null;
    try
    {
      styleResourceURL = new URL( "file:///" + deegreeRoot + "/WEB-INF/xml/styles.xml" );
      String s = NetWorker.url2String( defaultOnlineResource );
      if( layerName != null )
      {
        if( lURL == null )
        {
          legendURL = new URL( s + "/legend/default_" + layerName.replace( ':', '_' ) + ".png" );
          lu = new LegendURL[1];
          lu[0] = new LegendURL_Impl( 20, 20, "image/png", legendURL );
        }
        else
        {
          lu = lURL;
        }
      }
    }
    catch( MalformedURLException e )
    {
      throw new XMLParsingException( "not denote a valid URL: " + e.getMessage() );
    }

    return new Style_Impl( "default:" + layerName, "default:" + layerName, null, lu, null, null,
        styleResourceURL );
  }

  /**
   * Creates a <tt>Style</tt> -instance according to the contents of the
   * DOM-subtree starting at the given 'Style'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'Style'- <tt>Element</tt>
   * @param layerName
   *          name of the layer the style belongs to
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>Style/tt>-instance
   */
  protected Style createStyle( Element element, String layerName ) throws XMLParsingException
  {

    // required: <Name>
    String name = XMLTools.getRequiredStringValue( "Name", wmsNS, element );

    // optional: <Title> (defaults to name)
    String title = XMLTools.getStringValue( "Title", wmsNS, element, name );

    // optional: <Abstract>
    String abstractString = XMLTools.getStringValue( "Abstract", wmsNS, element, null );

    // optional: <LegendURL>(s)
    ElementList nodelist = XMLTools.getChildElementsByName( "LegendURL", wmsNS, element );
    LegendURL[] legendURLs = new LegendURL[nodelist.getLength()];

    if( nodelist.getLength() > 0 )
    {
      for( int i = 0; i < nodelist.getLength(); i++ )
      {
        legendURLs[i] = createLegendURL( nodelist.item( i ), name );
      }
    }
    else
    {
      legendURLs = new LegendURL[1];
      String tmp = NetWorker.url2String( defaultOnlineResource );
      tmp = tmp + "/legend/" + name.replace( ':', '_' ) + ".png";
      URL url = null;
      try
      {
        url = new URL( tmp );
      }
      catch( Exception e )
      {
        throw new XMLParsingException( "couldn't create LegendURL from " + tmp );
      }
      legendURLs[0] = new LegendURL_Impl( 25, 25, "image/png", url );
    }

    // optional: <StyleSheetURL>
    StyleSheetURL styleSheetURL = null;
    Element styleSheetURLElement = XMLTools.getChildByName( "StyleSheetURL", wmsNS, element );

    if( styleSheetURLElement != null )
    {
      styleSheetURL = createStyleSheetURL( styleSheetURLElement );
    }

    // optional: <StyleURL>
    StyleURL styleURL = null;
    Element styleURLElement = XMLTools.getChildByName( "StyleURL", wmsNS, element );

    if( styleURLElement != null )
    {
      styleURL = createStyleURL( styleURLElement );
    }

    // optional: <StyleResource> (this is a deegree specific parameter)
    URL styleResourceURL = null;
    String styleResource = XMLTools.getStringValue( "StyleResource", wmsNS, element, "file:///"
        + deegreeRoot + "/WEB-INF/xml/styles.xml" );
    try
    {
      styleResourceURL = new URL( styleResource );
    }
    catch( MalformedURLException e )
    {
      throw new XMLParsingException( "Value ('" + styleResource + "') of element 'styleResource'"
          + " does not denote a valid URL: " + e.getMessage() );
    }

    if( name.equalsIgnoreCase( "default" ) )
    {
      return createDefaultStyle( layerName, legendURLs );
    }
    else
    {
      return new Style_Impl( name, title, abstractString, legendURLs, styleSheetURL, styleURL,
          styleResourceURL );
    }
  }

  /**
   * Creates a <tt>DataSource</tt> -instance according to the contents of the
   * DOM-subtree starting at the given 'DataSource'- <tt>Element</tt>.
   * <p>
   * This is a deegree-specific parameter!
   * <p>
   * 
   * @param element
   *          the 'DataSource'- <tt>Element</tt>
   * @param dfName
   *          default name to be used if missing (same as name of layer)
   * @param dfScaleHint
   *          default value to be used if missing
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>DataSource/tt>-instance
   */
  protected DataSource createDataSource( Element element, String dfName, ScaleHint dfScaleHint )
      throws XMLParsingException
  {
    // optional: <Name>
    String name = XMLTools.getStringValue( "Name", wmsNS, element, dfName );

    // optional: <Type>
    String typeS = XMLTools.getStringValue( "Type", wmsNS, element, "LOCALWFS" );
    int type = DataSource.LOCALWFS;
    if( typeS.equals( "LOCALWCS" ) )
    {
      type = DataSource.LOCALWCS;
    }
    else if( typeS.equals( "REMOTEWCS" ) )
    {
      type = DataSource.REMOTEWCS;
    }
    else if( typeS.equals( "LOCALWFS" ) )
    {
      type = DataSource.LOCALWFS;
    }
    else if( typeS.equals( "REMOTEWFS" ) )
    {
      type = DataSource.REMOTEWFS;
    }
    else if( typeS.equals( "REMOTEWMS" ) )
    {
      type = DataSource.REMOTEWMS;
    }
    else
    {
      throw new XMLParsingException( "Invalid/unknown Service: " + typeS );
    }

    // optional: <GeometryProperty>
    String geometryProperty = XMLTools.getStringValue( "GeometryProperty", wmsNS, element, "/GEOM" );

    // optional: <ScaleHint>
    ScaleHint scaleHint = dfScaleHint;
    Element scaleHintElement = XMLTools.getChildByName( "ScaleHint", wmsNS, element );

    if( scaleHintElement != null )
    {
      scaleHint = createScaleHint( scaleHintElement );
    }

    // optional: <OWSCapabilities>
    URL owsCapabilities = null;
    Element owsCapabilitiesElement = XMLTools.getChildByName( "OWSCapabilities", wmsNS, element );
    if( owsCapabilitiesElement != null )
    {
      owsCapabilities = createOnlineResource( XMLTools.getChildByName( "OnlineResource", wmsNS,
          owsCapabilitiesElement ) );
    }
    else
    {
      if( type == DataSource.LOCALWFS )
      {
        try
        {
          owsCapabilities = new URL( "file:///" + deegreeRoot
              + "/WEB-INF/xml/LOCALWFS_capabilities.xml" );
        }
        catch( MalformedURLException e )
        {
          throw new XMLParsingException( "Constructed default URL: '" + "file:///" + deegreeRoot
              + "/WEB-INF/xml/LOCALWFS_capabilities.xml" + " does not denote a valid URL!" );
        }
      }
      else if( type == DataSource.LOCALWCS )
      {
        try
        {
          owsCapabilities = new URL( "file:///" + deegreeRoot
              + "/WEB-INF/xml/LOCALWCS_capabilities.xml" );
        }
        catch( MalformedURLException e )
        {
          throw new XMLParsingException( "Constructed default URL: '" + "file:///" + deegreeRoot
              + "/WEB-INF/xml/LOCALWCS_capabilities.xml" + " does not denote a valid URL!" );
        }
      }
      else
      {
        throw new XMLParsingException( "Given DataSource type '" + typeS + "' requires an "
            + "'OWSCapabilities'-element!" );
      }
    }

    // optional: <FilterCondition>
    WFSQuery query = null;
    WCSGetCoverageRequest gcr = null;
    WMSGetMapRequest gmr = null;
    Element filterConditionElement = XMLTools.getChildByName( "FilterCondition", wmsNS, element );

    if( filterConditionElement != null )
    {
      switch( type )
      {
      case DataSource.LOCALWFS:
      case DataSource.REMOTEWFS:
      {

        // create WFSQuery
        try
        {
          query = WFSProtocolFactory.createQuery( XMLTools.getRequiredChildByName( "Query", wfsNS,
              filterConditionElement ) );
        }
        catch( Exception e )
        {
          throw new XMLParsingException(
              "Exception occured while processing WFS-FilterCondition: '" + e.getMessage() + "'" );
        }
        break;
      }

      case DataSource.LOCALWCS:
      case DataSource.REMOTEWCS:
      {
        // create WCSGetCoverageRequest
        String requestValue = XMLTools.getRequiredStringValue( "WCSRequest", wmsNS,
            filterConditionElement );

        // set default values
        HashMap params = toMap( "ID=54&version=0.7&Layer=%default%&"
            + "SRS=EPSG:4326&BBOX=0,0,1,1&Width=1" + "&Height=1&Format=%default%", null );
        params.put( "BBOX", GeometryFactory.createGM_Envelope( 0, 0, 1, 1 ) );
        params = toMap( requestValue, params );

        try
        {
          gcr = WCSProtocolFactory.createWCSGetCoverageRequest( params );
        }
        catch( Exception e )
        {
          e.printStackTrace();
          throw new XMLParsingException(
              "Exception occured while processing WCS-FilterCondition: '" + e.getMessage() + "'" );
        }
        break;
      }
      case DataSource.REMOTEWMS:
      {
        // create WMSGetMapRequest
        String requestValue = XMLTools.getRequiredStringValue( "WMSRequest", wmsNS,
            filterConditionElement );

        HashMap params = toMap( "REQUEST=GetMap&LAYERS=%default%&"
            + "STYLES=&SRS=EPSG:4326&BBOX=0,0,1,1&WIDTH=1&" + "HEIGHT=1&FORMAT=%default%", null );

        params = toMap( requestValue, params );
        if( params.get( "VERSION" ) == null && params.get( "WMTVER" ) == null )
        {
          params.put( "VERSION", "1.1.1" );
        }
        // if no service is set use WMS as default
        if( params.get( "SERVICE" ) == null )
        {
          params.put( "SERVICE", "WMS" );
        }
        try
        {
          String id = IDGenerator.getInstance().generateUniqueID() + "";
          gmr = WMSProtocolFactory.createGetMapRequest( id, params );
        }
        catch( Exception e )
        {
          e.printStackTrace();
          throw new XMLParsingException(
              "Exception occured while processing WMS-FilterCondition: '"
                  + StringExtend.stackTraceToString( e.getStackTrace() ) + "'" );
        }

        break;
      }

      }
    }

    // create OWS
    OGCWebService ows = null;
    try
    {
      // check if the OWS has been initialized before
      if( owsMap.get( owsCapabilities.toExternalForm() ) == null )
      {
        switch( type )
        {
        case DataSource.LOCALWFS:
        {
          WFSCapabilities capa = null;
          capa = WFSCapabilitiesFactory.createCapabilities( owsCapabilities );
          ows = WFSFactory.createWFSService( capa, null );
          break;
        }
        case DataSource.REMOTEWFS:
        {
          WFSCapabilities capa = null;
          capa = WFSCapabilitiesFactory.createCapabilities( owsCapabilities );
          ows = new RemoteWFService( capa );
          break;
        }
        case DataSource.LOCALWCS:
        case DataSource.REMOTEWCS:
        {
          WCSCapabilities capa = null;
          capa = WCSCapabilitiesFactory.createCapabilities( owsCapabilities );
          ows = WCSFactory.createWCService( capa );
          break;
        }
        case DataSource.REMOTEWMS:
        {
          WMSCapabilities capa = null;
          capa = ( new OGCWMSCapabilitiesFactory() ).createCapabilities( owsCapabilities );
          ows = new RemoteWMService( capa );
          break;
        }
        }
        // store capabilites in a global HashMap
        owsMap.put( owsCapabilities.toExternalForm(), ows );
      }
      else
      {
        // get OWS from the HashMap to ensure that only oe instance of
        // one OWS will be used
        ows = (OGCWebService)owsMap.get( owsCapabilities.toExternalForm() );
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw new XMLParsingException( "Couldn't create data source OWS: " + e );
    }

    // create DataSource
    DataSource ds = null;

    switch( type )
    {
    case DataSource.LOCALWFS:
    case DataSource.REMOTEWFS:
      ds = new DataSource_Impl( name, type, geometryProperty, ows, owsCapabilities, scaleHint,
          query );
      break;
    case DataSource.LOCALWCS:
    case DataSource.REMOTEWCS:
      ds = new DataSource_Impl( name, type, geometryProperty, ows, owsCapabilities, scaleHint, gcr );
      break;
    case DataSource.REMOTEWMS:
      ds = new DataSource_Impl( name, geometryProperty, ows, owsCapabilities, scaleHint, gmr );
      break;
    }

    return ds;
  }

  /**
   * puts a http-GET request to a <tt>HashMap</tt>
   */
  private HashMap toMap( String request, HashMap map )
  {
    StringTokenizer st = new StringTokenizer( request, "&?" );
    if( map == null )
    {
      map = new HashMap();
    }
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

  /**
   * Creates a <tt>LegendURL</tt> -instance according to the contents of the
   * DOM-subtree starting at the given 'LegendURL'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'LegendURL'- <tt>Element</tt>
   * @param styleName
   *          name of the currently processed style
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>LegendURL/tt>-instance
   */
  protected LegendURL createLegendURL( Element element, String styleName )
      throws XMLParsingException
  {
    // required: 'width'-attribute
    int width = 0;

    try
    {
      width = Integer.parseInt( XMLTools.getRequiredAttrValue( "width", element ) );
    }
    catch( NumberFormatException e )
    {
      throw new XMLParsingException( "Attribute 'width' of Element 'LogoURL' does not "
          + "denote a valid integer value: " + e );
    }

    // required: 'height'-attribute
    int height = 0;

    try
    {
      height = Integer.parseInt( XMLTools.getRequiredAttrValue( "height", element ) );
    }
    catch( NumberFormatException e )
    {
      throw new XMLParsingException( "Attribute 'height' of Element 'LogoURL' does not "
          + "denote a valid integer value: " + e );
    }

    // required: <Format>
    String format = XMLTools.getRequiredStringValue( "Format", wmsNS, element );

    // optional: <OnlineResource>
    URL onlineResource = null;
    Element onlineResourceElement = XMLTools.getChildByName( "OnlineResource", wmsNS, element );

    if( onlineResourceElement != null )
    {
      onlineResource = createOnlineResource( XMLTools.getChildByName( "OnlineResource", wmsNS,
          element ) );
    }
    else
    {
      try
      {
        String s = NetWorker.url2String( defaultOnlineResource );
        onlineResource = new URL( s + "/legend/" + styleName + ".png" );
      }
      catch( MalformedURLException e )
      {
        String s = NetWorker.url2String( defaultOnlineResource );
        throw new XMLParsingException( "Constructed URL ('" + s + "/legend/" + styleName
            + ".png') does not denote a valid URL!" );
      }
    }

    return new LegendURL_Impl( width, height, format, onlineResource );
  }

  /**
   * Creates the default 'GetMap'-Operation to be used if it not defined.
   * defined in the capabilities-document
   * <p>
   * 
   * @return default 'GetMap'- <tt>Operation</tt>
   */
  private Operation createGetMap()
  {
    URL[] urls = new URL[1];
    urls[0] = defaultOnlineResource;
    HTTP http = new HTTP_Impl( urls, urls );
    DCPType[] dcpTypes = new DCPType[1];
    dcpTypes[0] = new DCPType_Impl( http );

    Format[] formats = new Format[3];
    formats[0] = new Format_Impl( "image/gif" );
    formats[1] = new Format_Impl( "image/png" );
    formats[2] = new Format_Impl( "image/jpeg" );

    return new Operation_Impl( "GetMap", formats, dcpTypes );
  }

  /**
   * Creates the default 'GetFeatureInfo'-Operation to be used if it not defined
   * in the capabilities-document.
   * <p>
   * 
   * @return default 'GetFeatureInfo'- <tt>Operation</tt>
   */
  private Operation createGetFeatureInfo() throws XMLParsingException
  {
    URL[] urls = new URL[1];
    urls[0] = defaultOnlineResource;
    HTTP defaultHTTP = new HTTP_Impl( urls, new URL[0] );
    DCPType[] defaultDCPTypes = new DCPType[1];
    defaultDCPTypes[0] = new DCPType_Impl( defaultHTTP );

    URL url = null;

    try
    {
      url = new URL( "file:///" + deegreeRoot + "/WEB-INF/xml/featureinfo2html.xsl" );
    }
    catch( MalformedURLException e )
    {
      throw new XMLParsingException( "Constructed URL for getFeatureInfo '" + "file:///"
          + deegreeRoot + "/WEB-INF/xml/featureinfo2html.xsl' does " + "not denote a valid URL!" );
    }

    Format[] defaultFormats = new Format[2];
    defaultFormats[0] = new Format_Impl( "application/vnd.ogc.wms_gml" );
    defaultFormats[1] = new Format_Impl( "text/html", url );

    return new Operation_Impl( "GetFeatureInfo", defaultFormats, defaultDCPTypes );
  }

  /**
   * Creates the default 'GetCapabilities'-Operation to be used if it not
   * defined in the capabilities-document.
   * <p>
   * 
   * @return default 'GetCapabilities'- <tt>Operation</tt>
   */
  private Operation createGetCapabilities()
  {
    URL[] urls = new URL[1];
    urls[0] = defaultOnlineResource;
    HTTP defaultHTTP = new HTTP_Impl( urls, new URL[0] );
    DCPType[] defaultDCPTypes = new DCPType[1];
    defaultDCPTypes[0] = new DCPType_Impl( defaultHTTP );

    Format[] defaultFormats = new Format[1];
    defaultFormats[0] = new Format_Impl( "application/vnd.ogc.wms_xml" );

    return new Operation_Impl( "GetCapabilities", defaultFormats, defaultDCPTypes );
  }

}