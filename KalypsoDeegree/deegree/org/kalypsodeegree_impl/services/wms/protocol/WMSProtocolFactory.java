// $Header:
// /var/lib/cvs/backupdeegree/deegree/org/deegree_impl/services/wms/protocol/WMSProtocolFactory.java,v
// 1.2 2004/06/21 13:40:57 doemming Exp $
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
package org.deegree_impl.services.wms.protocol;

import java.awt.Color;
import java.awt.Point;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;

import org.deegree.gml.GMLDocument;
import org.deegree.gml.GMLFeature;
import org.deegree.gml.GMLFeatureCollection;
import org.deegree.gml.GMLGeoProperty;
import org.deegree.gml.GMLGeometry;
import org.deegree.gml.GMLPoint;
import org.deegree.graphics.sld.StyledLayerDescriptor;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Point;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.geometry.GM_Surface;
import org.deegree.services.InconsistentRequestException;
import org.deegree.services.OGCWebServiceException;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.wms.InvalidFormatException;
import org.deegree.services.wms.capabilities.GazetteerParam;
import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.deegree.services.wms.protocol.WMSFeatureInfoRequest;
import org.deegree.services.wms.protocol.WMSFeatureInfoResponse;
import org.deegree.services.wms.protocol.WMSGetCapabilitiesRequest;
import org.deegree.services.wms.protocol.WMSGetCapabilitiesResponse;
import org.deegree.services.wms.protocol.WMSGetLegendGraphicRequest;
import org.deegree.services.wms.protocol.WMSGetMapRequest;
import org.deegree.services.wms.protocol.WMSGetMapResponse;
import org.deegree.services.wms.protocol.WMSGetScaleBarRequest;
import org.deegree.services.wms.protocol.WMSGetScaleBarResponse;
import org.deegree.xml.Marshallable;
import org.deegree.xml.XMLParsingException;
import org.deegree.xml.XMLTools;
import org.deegree_impl.gml.GMLDocument_Impl;
import org.deegree_impl.graphics.sld.SLDFactory;
import org.deegree_impl.model.ct.GeoTransformer;
import org.deegree_impl.model.geometry.GMLAdapter;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.MimeTypeMapper;
import org.deegree_impl.tools.NetWorker;
import org.deegree_impl.tools.StringExtend;
import org.w3c.dom.Document;

/**
 * Factory that builds the different types of WMS-Requests & Responses.
 * 
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author <a href="mailto:wanhoff@uni-bonn.de">Jeronimo Wanhoff </a>
 * @version $Revision$ $Date$
 */
public class WMSProtocolFactory
{
  /**
   * creates a request
   * 
   * @param id
   *          an unique id of the request
   * @param paramMap
   *          the request parameters
   * @return the request as an OGC WebService Request
   * @throws InconsistentRequestException
   *           if the Capabilities-document and the including configuration was
   *           invalid / inconsistent.
   */
  public static OGCWebServiceRequest createRequest( String id, HashMap paramMap )
      throws InconsistentRequestException, XMLParsingException, MalformedURLException
  {
    Debug.debugMethodBegin( "org.deegree_impl.enterprise.WMSProtocolFactory", "createRequest" );

    OGCWebServiceRequest request = null;

    String requestStr = (String)paramMap.remove( "REQUEST" );

    if( requestStr == null )
    {
      throw new InconsistentRequestException( "Required parameter 'REQUEST' is missing." );
    }

    if( requestStr.equalsIgnoreCase( "getCapabilities" ) )
    {
      request = createWMSGetCapabilitiesRequest( id, paramMap );
    }
    else if( requestStr.equalsIgnoreCase( "GetMap" ) || requestStr.equalsIgnoreCase( "map" ) )
    {
      request = createGetMapRequest( id, paramMap );
    }
    else if( requestStr.equalsIgnoreCase( "getFeatureInfo" )
        || requestStr.equalsIgnoreCase( "feature_info" ) )
    {
      request = createGetFeatureInfoRequest( id, paramMap );
    }
    else if( requestStr.equalsIgnoreCase( "getScaleBar" ) )
    {
      request = createWMSGetScaleBarRequest( id, paramMap );
    }/*
      * else if ( requestStr.equalsIgnoreCase( "getStyles" ) ) { } else if (
      * requestStr.equalsIgnoreCase( "putStyles" ) ) { } else if (
      * requestStr.equalsIgnoreCase( "describeLayer" ) ) { }
      */

    else
    {
      throw new InconsistentRequestException( "Parameter 'REQUEST' has an invalid value: '"
          + requestStr + "'. Supported values are: 'getCapabilities', "
          + "'getMap' and 'getFeatureInfo'" );
    }

    Debug.debugMethodEnd();
    return request;
  }

  /**
   * creates an WMS GetCapabilities Request
   * 
   * @param id
   *          an unique ID of the request
   * @param paramMap
   *          the parameters of the request
   * @return the GetCapabilities request
   * @throws InconsistentRequestException
   *           if the request is inconsistent
   */
  public static WMSGetCapabilitiesRequest createWMSGetCapabilitiesRequest( String id,
      HashMap paramMap ) throws InconsistentRequestException
  {
    String version = (String)paramMap.get( "VERSION" );

    if( version == null )
    {
      version = (String)paramMap.get( "WMTVER" );
    }

    String service = (String)paramMap.get( "SERVICE" );
    String updateSequence = (String)paramMap.get( "UPDATESEQUENCE" );

    if( service == null )
    {
      throw new InconsistentRequestException( "Required parameter 'SERVICE' is missing." );
    }

    if( !service.equals( "WMS" ) )
    {
      throw new InconsistentRequestException( "Parameter 'SERVICE' must be 'WMS'." );
    }

    return new WMSGetCapabilitiesRequest_Impl( id, version, updateSequence, null );
  }

  /**
   * creates an instance of a <tt>WMSGetCapabilitiesResponse</tt> object
   * 
   * @param request
   *          request that lead to the response
   * @param exception
   *          exception if one occuered
   * @param capabilities
   *          WMS capabilities
   * 
   * @return <tt>WMSGetCapabilitiesResponse</tt>
   */
  public static WMSGetCapabilitiesResponse createWMSGetCapabilitiesResponse(
      OGCWebServiceRequest request, OGCWebServiceException exception, WMSCapabilities capabilities )
  {
    Debug.debugMethodBegin();

    Document doc = null;

    if( exception != null )
    {
      StringReader reader = new StringReader( ( (Marshallable)exception ).exportAsXML() );

      try
      {
        doc = XMLTools.parse( reader );
        reader.close();
      }
      catch( Exception e )
      {
        System.out.println( e );
      }
    }

    WMSGetCapabilitiesResponse res = new WMSGetCapabilitiesResponse_Impl( request, doc,
        capabilities );

    Debug.debugMethodEnd();
    return res;
  }

  /**
   * creates a <tt>WMSGetMapRequest</tt> from a <tt>HashMap</tt> that
   * contains the request parameters as key-value-pairs. Keys are expected to be
   * in upper case notation.
   * 
   * @param id
   *          an unique id of the request
   * @param request
   * @return an instance of <tt>WMSGetMapRequest</tt>
   */
  public static WMSGetMapRequest createGetMapRequest( String id, String request )
      throws InconsistentRequestException, XMLParsingException, MalformedURLException
  {
    Debug.debugMethodBegin();

    Map map = toMap( request );
    WMSGetMapRequest req = createGetMapRequest( id, map );

    Debug.debugMethodEnd();
    return req;
  }

  /**
   * creates a <tt>WMSGetMapRequest</tt> from a <tt>HashMap</tt> that
   * contains the request parameters as key-value-pairs. Keys are expected to be
   * in upper case notation.
   * 
   * @param id
   *          an unique id of the request
   * @param model
   *          <tt>HashMap</tt> containing the request parameters
   * @return an instance of <tt>WMSGetMapRequest</tt>
   */
  public static WMSGetMapRequest createGetMapRequest( String id, Map model )
      throws InconsistentRequestException, XMLParsingException, MalformedURLException
  {
    Debug.debugMethodBegin();

    // use model.remove(..) so at the end of the method the vendor
    // specific parameters remains in the model HashMap
    model.remove( "REQUEST" );

    // wms capabilities
    // will not be available for the creation of requests against WMS in
    // a cascade, so have to be checked if it's null before it can be used
    WMSCapabilities capabilities = (WMSCapabilities)model.remove( "CAPABILITIES" );

    String service = (String)model.remove( "SERVICE" );

    if( !"WMS".equals( service ) )
    {
      throw new XMLParsingException( "Service parameter must be equal to 'WMS' " );
    }

    // Version
    String version = (String)model.remove( "VERSION" );

    if( version == null )
    {
      version = (String)model.remove( "WMTVER" );
    }

    if( version == null )
    {
      throw new InconsistentRequestException( "VERSION-value must be set" );
    }

    // LAYERS & STYLES & SLD (URL, XML)
    StyledLayerDescriptor sld = null;
    String sld_body = (String)model.remove( "SLD_BODY" );
    String sld_urlstring = (String)model.remove( "SLD" );

    // The SLD is complete in the Maprequest
    URL sLD_URL = null;

    if( sld_body != null )
    {
      try
      {
        sld = SLDFactory.createSLD( URLDecoder.decode( sld_body, "UTF-8" ) );
      }
      catch( XMLParsingException xmlpex )
      {
        throw new InconsistentRequestException( "\nInconsistentRequestException "
            + "encountered during parsing of SLD statement in WMS-Request:\n" + xmlpex.getMessage() );
      }
      catch( Exception ee )
      {
        throw new XMLParsingException( "Could not decode SLD_BODY: " + ee.toString() );
      }
    }
    else if( sld_urlstring != null )
    {
      // The SLD is as url in the Maprequest
      sLD_URL = new URL( sld_urlstring );

      try
      {
        InputStreamReader isr = new InputStreamReader( sLD_URL.openStream() );
        sld = SLDFactory.createSLD( isr );
        isr.close();
      }
      catch( IOException ioex )
      {
        throw new InconsistentRequestException( "IOException occured "
            + "during the access to the SLD-URL. Wrong URL? Server down?\n" + ioex.getMessage() );
      }
    }

    // LAYERS & STYLES
    // will only be evaluated if no SLD has been defined
    WMSGetMapRequest.Layer[] ls = null;

    if( sld == null )
    {
      String layersstring = (String)model.remove( "LAYERS" );
      String stylesstring = (String)model.remove( "STYLES" );

      // normalize styles parameter
      if( ( stylesstring == null ) || stylesstring.trim().equals( "" ) )
      {
        stylesstring = "default";
      }
      if( stylesstring.startsWith( "," ) )
      {
        stylesstring = "default" + stylesstring;
      }
      stylesstring = StringExtend.replace( stylesstring, ",,", ",default,", true );
      if( stylesstring.endsWith( "," ) )
      {
        stylesstring = stylesstring + "default";
      }
      ArrayList layers = new ArrayList();
      ArrayList styles = new ArrayList();
      if( ( layersstring != null ) && !layersstring.trim().equals( "" ) )
      {
        StringTokenizer st = new StringTokenizer( layersstring, "," );
        int a = 0;

        while( st.hasMoreTokens() )
        {
          String s = st.nextToken();
          if( s.equals( "%default%" ) )
          {}
          layers.add( s );
        }
        st = new StringTokenizer( stylesstring, "," );
        for( int i = 0; i < layers.size(); i++ )
        {
          styles.add( "default" );
        }
        a = 0;
        while( st.hasMoreTokens() )
        {
          String s = st.nextToken();
          styles.set( a++, s );
        }

        // At last, build up the Layer object
        ls = new WMSGetMapRequest.Layer[layers.size()];

        for( int i = 0; i < layers.size(); i++ )
        {
          ls[i] = WMSGetMapRequest_Impl.createLayer( (String)layers.get( i ), (String)styles
              .get( i ) );
        }
      }
      else
      {
        throw new InconsistentRequestException( "no layers defined in GetMap request" );
      }
    }

    // ELEVATION
    String[] el = (String[])model.remove( "ELEVATION" );
    double[] elevation = null;

    if( ( el != null ) && ( el.length > 0 ) )
    {
      elevation = new double[el.length];

      for( int i = 0; i < el.length; i++ )
      {
        elevation[i] = Double.parseDouble( el[i] );
      }
    }

    // SAMPLE DIMENSION
    String[] sampleDimension = null;
    String sd = (String)model.remove( "DIMENSION" );

    if( sd != null )
    {
      StringTokenizer dimst = new StringTokenizer( sd, "," );
      sampleDimension = new String[dimst.countTokens()];

      int i = 0;

      while( dimst.hasMoreTokens() )
      {
        sampleDimension[i++] = dimst.nextToken();
      }
    }

    // FORMAT
    String format = (String)model.remove( "FORMAT" );

    if( format == null )
    {
      throw new InconsistentRequestException( "FORMAT-value must be set" );
    }

    if( !MimeTypeMapper.isKnownImageType( format ) )
    {
      throw new InvalidFormatException( format + " is not a valid image/result format" );
    }

    // WIDTH
    if( model.get( "WIDTH" ) == null )
    {
      throw new InconsistentRequestException( "WIDTH-value must be set" );
    }

    int width = Integer.parseInt( (String)model.remove( "WIDTH" ) );

    // exceeds the max allowed map width ?
    if( capabilities != null )
    {
      if( width > capabilities.getDeegreeParam().getMaxMapWidth() )
      {
        throw new InconsistentRequestException( "requested map width exceeds "
            + "allowed maximum width of " + capabilities.getDeegreeParam().getMaxMapWidth()
            + " pixel" );
      }
    }

    // HEIGHT
    if( model.get( "HEIGHT" ) == null )
    {
      throw new InconsistentRequestException( "HEIGHT-value must be set" );
    }
    int height = Integer.parseInt( (String)model.remove( "HEIGHT" ) );

    // exceeds the max allowed map height ?
    if( capabilities != null )
    {
      if( height > capabilities.getDeegreeParam().getMaxMapHeight() )
      {
        throw new InconsistentRequestException( "requested map height exceeds "
            + "allowed maximum height of " + capabilities.getDeegreeParam().getMaxMapHeight()
            + " pixel" );
      }
    }

    // SRS
    String srs = (String)model.remove( "SRS" );
    if( srs == null )
    {
      throw new InconsistentRequestException( "SRS-value must be set" );
    }

    // BBOX
    String boxstring = (String)model.remove( "BBOX" );
    GM_Envelope boundingBox = null;
    if( boxstring == null )
    {
      throw new InconsistentRequestException( "BBOX-value must be set" );
    }
    else
    {
      StringTokenizer st = new StringTokenizer( boxstring, "," );

      String s = st.nextToken().replace( ' ', '+' );
      double minx = Double.parseDouble( s );
      s = st.nextToken().replace( ' ', '+' );
      double miny = Double.parseDouble( s );
      s = st.nextToken().replace( ' ', '+' );
      double maxx = Double.parseDouble( s );
      s = st.nextToken().replace( ' ', '+' );
      double maxy = Double.parseDouble( s );

      if( minx >= maxx )
      {
        throw new InvalidFormatException( "minx must be lesser than maxx" );
      }

      if( miny >= maxy )
      {
        throw new InvalidFormatException( "miny must be lesser than maxy" );
      }

      boundingBox = GeometryFactory.createGM_Envelope( minx, miny, maxx, maxy );
    }

    // the Gazetteer Operator is a vendorspecific parameter that replaces
    // the boundingbox of the map request with the one of the location
    // specified by the LOCATION parameter
    // this parameter should never be part of the request model for creating
    // a request against a WMS in a cascade!!!
    if( model.get( "LOCATION" ) != null )
    {
      String location = (String)model.remove( "LOCATION" );
      String locationType = (String)model.remove( "LOCATIONTYPE" );

      GazetteerParam gazetteer = capabilities.getDeegreeParam().getGazetteer();

      if( gazetteer == null )
      {
        throw new InconsistentRequestException( "Gazetteer parameter is requested "
            + "but no gazetteer is defined " + "for the WMS." );
      }

      // radius (bbox width/height) that will be used if the location
      // is a point with no extent
      double radius = 10;

      if( model.get( "LOCATIONRADIUS" ) != null )
      {
        String tmp = (String)model.remove( "LOCATIONRADIUS" );
        radius = Double.parseDouble( tmp );
      }
      else
      {
        radius = gazetteer.getRadius();
      }

      GM_Envelope bb = getBoundingBoxFromGazetteer( location, locationType, radius, gazetteer );

      if( bb != null )
      {
        boundingBox = bb;
      }
    }

    // TRANSPARENCY
    boolean transparency = false;
    String tp = (String)model.remove( "TRANSPARENT" );
    if( tp != null )
    {
      transparency = tp.toUpperCase().trim().equals( "TRUE" );
    }

    String mime = MimeTypeMapper.toMimeType( format );
    if( mime.equals( "image/jpg" ) || mime.equals( "image/jpeg" ) || mime.equals( "image/bmp" )
        || mime.equals( "image/tif" ) || mime.equals( "image/tiff" ) )
    {
      transparency = false;
    }

    // BGCOLOR
    String tmp = (String)model.remove( "BGCOLOR" );
    Color bgColor = Color.white;
    if( tmp != null )
    {
      bgColor = Color.decode( tmp );
    }

    // EXCEPTIONS
    String exceptions = (String)model.remove( "EXCEPTIONS" );
    if( exceptions == null )
    {
      exceptions = "application/vnd.ogc.se_xml";
    }

    // TIME
    String time = (String)model.remove( "TIME" );

    // WFS

    /*
     * URL wFS_URL = null; if ((String)model.get( "WFS" ) != null) { wFS_URL =
     * new URL((String)model.remove( "WFS" )); }
     */

    // ID
    if( id == null )
    {
      throw new InconsistentRequestException( "ID-value must be set" );
    }

    // VendorSpecificParameter; because all defined parameters has been removed
    // from the model the vendorSpecificParameters are what left
    HashMap vendorSpecificParameter = (HashMap)( (HashMap)model ).clone();

    Debug.debugMethodEnd();

    return createGetMapRequest( version, id, ls, elevation, sampleDimension, format, width, height,
        srs, boundingBox, transparency, bgColor, exceptions, time, sLD_URL, sld,
        vendorSpecificParameter );
  }

  /**
   * returns the bounding box of the submitted location. The vounding box will
   * be read from a gazetteer service. If the location represent a point
   * geometry that hasn't an extent the submitted radius will be used to create
   * a bbox the the radius' width and height.
   * 
   * @param location
   *          location from which to get the boundingbox
   * @param locationType
   *          type of the location. all locationTypes will be find if not set.
   * @param radius
   *          radius to use to create the boundingbox if location represents a
   *          point geometry
   */
  private static synchronized GM_Envelope getBoundingBoxFromGazetteer( String location,
      String locationType, double radius, GazetteerParam gazetteer )
      throws InconsistentRequestException
  {
    Debug.debugMethodBegin( "WMSProtocolFactory", "getBoundingBoxFromGazetteer" );

    StringBuffer req = new StringBuffer( "<GetFeature outputFormat='GML2' " );
    req.append( "xmlns='http://www.opengis.net/wfs' " );
    req.append( "xmlns:gml='http://www.opengis.net/gml' service='WFS' " );
    req.append( "version='1.0.0' ><Query typeName=" );
    req.append( "'SI_LOCATION_INSTANCE_SUMMARY'><Filter>" );

    if( locationType != null )
    {
      req.append( "<AND>" );
      req.append( "<PropertyIsEqualTo>" );
      req.append( "<PropertyName>SI_LocationType</PropertyName>" );
      req.append( "<Literal>" + locationType + "</Literal>" );
      req.append( "</PropertyIsEqualTo>" );
    }

    req.append( "<PropertyIsLike wildCard='*' singleChar='#' escape='!'>" );
    req.append( "<PropertyName>geographicIdentifier</PropertyName>" );
    req.append( "<Literal>" + location + "</Literal>" );
    req.append( "</PropertyIsLike>" );

    if( locationType != null )
    {
      req.append( "</AND>" );
    }

    req.append( "</Filter></Query></GetFeature>" );

    Document doc = null;

    try
    {
      NetWorker nw = new NetWorker( gazetteer.getGazetteer(), req.toString() );
      InputStreamReader isr = new InputStreamReader( nw.getInputStream() );
      doc = XMLTools.parse( isr );
      isr.close();
    }
    catch( Exception e )
    {
      throw new InconsistentRequestException( "could not perform gazetteer request\n" + e );
    }

    GM_Envelope bbox = null;

    try
    {
      // extract bounding box
      GMLDocument gml = new GMLDocument_Impl( doc );
      GMLFeatureCollection gmlFC = gml.getRoot();
      GMLFeature[] feature = gmlFC.getFeatures();

      if( ( feature != null ) && ( feature.length > 0 ) )
      {
        GMLGeoProperty[] geomProp = feature[0].getGeoProperties();
        GMLGeometry geom = geomProp[0].getGeoPropertyValue();

        if( geom instanceof GMLPoint )
        {
          GM_Point point = (GM_Point)GMLAdapter.wrap( geom );
          double minx = point.getX() - ( radius / 2d );
          double miny = point.getY() - ( radius / 2d );
          double maxx = point.getX() + ( radius / 2d );
          double maxy = point.getY() + ( radius / 2d );
          bbox = GeometryFactory.createGM_Envelope( minx, miny, maxx, maxy );
        }
        else
        {
          GM_Surface surface = (GM_Surface)GMLAdapter.wrap( geom );
          bbox = surface.getEnvelope();
        }
      }
      //bbox = transformBBoxToEPSG4326( bbox );
    }
    catch( Exception e )
    {
      throw new InconsistentRequestException( "Couldn't parse result to gazetteer request\n" + e );
    }

    Debug.debugMethodEnd();
    return bbox;
  }

  /**
   * creates a <tt>WTSGetViewRequest</tt> from a set of parameters and builds
   * up the complete SLD.
   * 
   * @return an instance of <tt>WMSGetMapRequest</tt>
   * @param version
   *          Request version.
   * @param layers
   *          list of one or more map layers. Optional if SLD parameter is
   *          present. Contains list of one rendering style per requested layer.
   *          Optional if SLD parameter is present.
   * @param elevation
   *          Elevation of layer desired.
   * @param sampleDimension
   *          Value of other dimensions as appropriate.
   * @param format
   *          Output format of map.
   * @param width
   *          Width in pixels of map picture.
   * @param height
   *          Height in pixels of map picture.
   * @param srs
   *          the requested Spatial Reference System.
   * @param boundingBox
   *          Bounding box corners (lower left, upper right) in SRS units.
   * @param transparency
   *          Background transparency of map.
   * @param bGColor
   *          Hexadecimal red-green-blue color value for the background color.
   * @param exceptions
   *          The format in which exceptions are to be reported by the WMS.
   * @param time
   *          Time value of layer desired
   * @param sld
   *          Styled Layer Descriptor
   * @param id
   *          an unique ID of the request
   * @param vendorSpecificParameter
   *          Vendor Specific Parameter
   */
  public static WMSGetMapRequest createGetMapRequest( String version, String id,
      org.deegree.services.wms.protocol.WMSGetMapRequest.Layer[] layers, double[] elevation,
      String[] sampleDimension, String format, int width, int height, String srs,
      GM_Envelope boundingBox, boolean transparency, Color bGColor, String exceptions, String time,
      URL sldURL, StyledLayerDescriptor sld, HashMap vendorSpecificParameter )
      throws XMLParsingException
  {
    Debug.debugMethodBegin();

    // Adds the content from the LAYERS and STYLES attribute to the SLD
    if( sld == null )
    {
      StringBuffer sb = new StringBuffer( 5000 );
      sb.append( "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" );
      sb.append( "<StyledLayerDescriptor version=\"1.0.0\" " );
      sb.append( "xmlns='http://www.opengis.net/sld'>" );

      for( int i = 0; i < layers.length; i++ )
      {
        sb.append( "<NamedLayer>" );
        sb.append( "<Name>" + layers[i].getName() + "</Name>" );
        sb.append( "<NamedStyle><Name>" + layers[i].getStyleName()
            + "</Name></NamedStyle></NamedLayer>" );
      }

      sb.append( "</StyledLayerDescriptor>" );

      sld = SLDFactory.createSLD( sb.toString() );
    }

    Debug.debugMethodEnd();

    return new WMSGetMapRequest_Impl( version, id, layers, elevation, sampleDimension, format,
        width, height, srs, boundingBox, transparency, bGColor, exceptions, time, sldURL, sld,
        vendorSpecificParameter );
  }

  /**
   * creates a <tt>WFSGetMapResponse</tt> object
   * 
   * @param request
   *          a copy of the request that leads to this response
   * @param exception
   *          a describtion of an excetion (only if raised)
   * @param response
   *          the response to the request
   */
  public static WMSGetMapResponse createWMSGetMapResponse( OGCWebServiceRequest request,
      OGCWebServiceException exception, Object response )
  {
    Debug.debugMethodBegin();

    Document doc = null;

    if( exception != null )
    {
      StringReader reader = new StringReader( ( (Marshallable)exception ).exportAsXML() );

      try
      {
        doc = XMLTools.parse( reader );
        reader.close();
      }
      catch( Exception e )
      {
        System.out.println( e );
      }
    }

    WMSGetMapResponse res = new WMSGetMapResponse_Impl( request, doc, response );

    Debug.debugMethodEnd();
    return res;
  }

  /**
   * creates a <tt>WMSFeatureInfoRequest</tt> from a <tt>HashMap</tt> that
   * contains the request parameters as key-value-pairs. Keys are expected to be
   * in upper case notation.
   * 
   * @param id
   *          an unique id of the request
   * @param model
   *          <tt>HashMap</tt> containing the request parameters
   * @return an instance of <tt>WMSFeatureInfoRequest</tt>
   */
  public static WMSFeatureInfoRequest createGetFeatureInfoRequest( String id, HashMap model )
      throws InconsistentRequestException
  {
    Debug.debugMethodBegin();

    // VERSION
    String version = (String)model.get( "VERSION" );
    if( version == null )
    {
      version = (String)model.get( "WMTVER" );
    }
    if( version == null )
    {
      throw new InconsistentRequestException(
          "VERSION-value must be set in the GetFeatureInfo request" );
    }

    // ID
    if( id == null )
    {
      throw new InconsistentRequestException( "ID-value must be set in the GetFeatureInfo request" );
    }

    // QUERY_LAYERS
    String layerlist = (String)model.remove( "QUERY_LAYERS" );
    String[] queryLayers = null;

    if( layerlist != null )
    {
      StringTokenizer st = new StringTokenizer( layerlist, "," );
      queryLayers = new String[st.countTokens()];
      int i = 0;
      while( st.hasMoreTokens() )
      {
        queryLayers[i++] = st.nextToken();
      }
    }
    else
    {
      throw new InconsistentRequestException(
          "QUERY_LAYERS-value must be set in the GetFeatureInfo request" );
    }

    // INFO_FORMAT (mime-type)
    String infoFormat = (String)model.remove( "INFO_FORMAT" );
    if( infoFormat == null )
    {
      infoFormat = "application/vnd.ogc.gml";
    }

    // FEATURE_COUNT (default=1)
    String feco = (String)model.remove( "FEATURE_COUNT" );
    int featureCount = 1;
    if( feco != null )
    {
      featureCount = Integer.parseInt( feco.trim() );
    }
    if( featureCount < 0 )
    {
      featureCount = 1;
    }

    // X, Y (measured from upper left corner=0)
    String xstring = (String)model.remove( "X" );
    String ystring = (String)model.remove( "Y" );
    Point clickPoint = null;
    if( ( xstring != null ) & ( ystring != null ) )
    {
      int x = Integer.parseInt( xstring.trim() );
      int y = Integer.parseInt( ystring.trim() );
      clickPoint = new Point( x, y );
    }
    else
    {
      throw new InconsistentRequestException(
          "X- and/or Y-value must be set in the GetFeatureInfo request" );
    }

    // EXCEPTIONS (default=application/vnd.ogc.se_xml)
    String exceptions = (String)model.get( "EXCEPTIONS" );
    if( exceptions == null )
    {
      exceptions = "application/vnd.ogc.se_xml";
    }

    //  <map_request_copy>
    WMSGetMapRequest getMapRequestCopy = null;

    try
    {
      getMapRequestCopy = createGetMapRequest( id, model );
    }
    catch( Exception ex )
    {
      throw new InconsistentRequestException( "\nAn Exception "
          + "occured in creating the GetMap request-copy included in the "
          + "GetFeatureInfo-Request:\n"
          + "--> Location: WMSProtocolFactory, createGetFeatureInfoRequest(int, HashMap)\n"
          + ex.getMessage() );
    }

    // VendorSpecificParameter; because all defined parameters has been removed
    // from the model the vendorSpecificParameters are what left
    HashMap vendorSpecificParameter = model;

    // StyledLayerDescriptor
    StyledLayerDescriptor sld = getMapRequestCopy.getStyledLayerDescriptor();

    Debug.debugMethodEnd();
    return createGetFeatureInfoRequest( version, id, queryLayers, getMapRequestCopy, infoFormat,
        featureCount, clickPoint, exceptions, sld, vendorSpecificParameter );
  }

  /**
   * creates a <tt>WMSFeatureInfoRequest</tt> from the request parameters.
   * 
   * @return an instance of <tt>WMSFeatureInfoRequest</tt>
   * @param version
   *          VERSION=version (R): Request version.
   * @param queryL,ayers
   *          QUERY_LAYERS=layer_list (R): Comma-separated list of one or more
   *          layers to be queried.
   * @param getMapRequestCopy
   *          &lt;map_request_copy&gt; (R): Partial copy of the Map request
   *          parameters that generated the map for which information is
   *          desired.
   * @param infoFormat
   *          INFO_FORMAT=output_format (O): Return format of feature
   *          information (MIME type).
   * @param featureCount
   *          FEATURE_COUNT=number (O): Number of features about which to return
   *          information (default=1).
   * @param clickPoint
   *          X=pixel_column (R): X coordinate in pixels of feature (measured
   *          from upper left corner=0) Y=pixel_row (R): Y coordinate in pixels
   *          of feature (measured from upper left corner=0)
   * @param exceptions
   *          EXCEPTIONS=exception_format (O): The format in which exceptions
   *          are to be reported by the WMS
   *          (default=application/vnd.ogc.se_xml).
   * @param sld
   *          StyledLayerDescriptor
   * @param vendorSpecificParameter
   *          Vendor-specific parameters (O): Optional experimental parameters.
   */
  public static WMSFeatureInfoRequest createGetFeatureInfoRequest( String version, String id,
      String[] queryLayers, WMSGetMapRequest getMapRequestCopy, String infoFormat,
      int featureCount, Point clickPoint, String exceptions, StyledLayerDescriptor sld,
      HashMap vendorSpecificParameter )
  {
    Debug.debugMethodBegin();

    WMSFeatureInfoRequest fir = new WMSFeatureInfoRequest_Impl( version, id, queryLayers,
        getMapRequestCopy, infoFormat, featureCount, clickPoint, exceptions, sld,
        vendorSpecificParameter );

    Debug.debugMethodEnd();
    return fir;
  }

  /**
   * creates a <tt>WFSGetFeatureInfoResponse</tt> object
   * 
   * @param request
   *          a copy of the request that leads to this response
   * @param exception
   *          a describtion of an excetion (only if raised)
   * @param featureInfo
   */
  public static WMSFeatureInfoResponse createWMSFeatureInfoResponse( OGCWebServiceRequest request,
      OGCWebServiceException exception, String featureInfo )
  {
    Debug.debugMethodBegin();

    Document doc = null;

    if( exception != null )
    {
      StringReader reader = new StringReader( ( (Marshallable)exception ).exportAsXML() );

      try
      {
        doc = XMLTools.parse( reader );
        reader.close();
      }
      catch( Exception e )
      {
        System.out.println( e );
      }
    }

    WMSFeatureInfoResponse res = new WMSFeatureInfoResponse_Impl( request, doc, featureInfo );

    Debug.debugMethodEnd();
    return res;
  }

  /**
   * 
   * 
   * @param id
   * @param model
   * 
   * @return @throws
   *         InconsistentRequestException
   */
  public static WMSGetScaleBarRequest createWMSGetScaleBarRequest( String id, HashMap model )
      throws InconsistentRequestException
  {
    //remove all ScaleBarRequest-related Parameter so that
    // VendorSpecificParameter remain
    // remove the getScaleBar-REQUEST
    model.remove( "REQUEST" );

    String version = (String)model.remove( "VERSION" );

    if( version == null )
    {
      version = (String)model.remove( "WMTVER" );
    }

    String service = (String)model.remove( "SERVICE" );

    // format
    String format = (String)model.remove( "FORMAT" );

    if( format == null )
    {
      throw new InconsistentRequestException( "Parameter FORMAT must be set." );
    }

    if( !MimeTypeMapper.isKnownImageType( format ) )
    {
      throw new InvalidFormatException( format + " is not a valid image/result format" );
    }

    // units
    String units = (String)model.remove( "UNITS" );

    if( !"meter".equals( units ) )
    {
      //TODO zus?tliche Ma?einheiten hinzuf?gen
      throw new InconsistentRequestException(
          "Parameter 'UNITS' must be one of 'meter', 'additional units "
              + "will be supported in future versions'." );
      //TODO bei zus?tzlichen Ma?einheiten anpassen
    }

    // top label content
    String tmp = (String)model.remove( "TOPLABEL" );
    int topLabel = WMSGetScaleBarRequest.L_NONE;

    if( "SCALE".equals( tmp ) )
    {
      topLabel = WMSGetScaleBarRequest.L_SCALE;
    }
    else if( "SIZE".equals( tmp ) )
    {
      topLabel = WMSGetScaleBarRequest.L_SIZE;
    }

    // bottom label content
    tmp = (String)model.remove( "BOTTOMLABEL" );
    int bottomLabel = WMSGetScaleBarRequest.L_SCALE;

    if( "NONE".equals( tmp ) )
    {
      topLabel = WMSGetScaleBarRequest.L_NONE;
    }
    else if( "SIZE".equals( tmp ) )
    {
      topLabel = WMSGetScaleBarRequest.L_SIZE;
    }

    // label color
    tmp = (String)model.remove( "LABELCOLOR" );
    Color labelColor = Color.BLACK;

    try
    {
      if( tmp != null )
      {
        labelColor = Color.decode( tmp );
      }
    }
    catch( Exception e )
    {
      throw new InconsistentRequestException( "Invalid label color definition." );
    }

    // font
    String labelFont = (String)model.remove( "FONT" );

    if( labelFont == null )
    {
      labelFont = "ARIAL";
    }

    tmp = (String)model.remove( "FONTSIZE" );

    if( tmp == null )
    {
      tmp = "10";
    }

    int labelFontSize = 10;

    try
    {
      labelFontSize = Integer.parseInt( tmp );
    }
    catch( Exception e )
    {
      throw new InconsistentRequestException( "Invalid label font size definition." );
    }

    java.awt.Font font = java.awt.Font.getFont( labelFont );

    //TODO @Andreas: hier beschwert sich meine Testklasse ?ber nullpointer
    //font = font.deriveFont( labelFontSize );
    //font = font.deriveFont ((float)10);
    String barStyle = (String)model.remove( "STYLE" );

    if( barStyle == null )
    {
      barStyle = "default";
    }

    // bar color
    tmp = (String)model.remove( "COLOR" );
    Color color = Color.BLACK;

    try
    {
      if( tmp != null )
      {
        color = Color.decode( tmp );
      }
    }
    catch( Exception e )
    {
      throw new InconsistentRequestException( "Invalid bar color definition." );
    }

    // background color
    tmp = (String)model.remove( "BGCOLOR" );
    Color bgColor = Color.WHITE;

    try
    {
      if( tmp != null )
      {
        color = Color.decode( tmp );
      }
    }
    catch( Exception e )
    {
      throw new InconsistentRequestException( "Invalid background color definition." );
    }

    // scale bar size
    tmp = (String)model.remove( "SIZE" );
    int size = 0;

    try
    {
      size = Integer.parseInt( tmp );
    }
    catch( Exception e )
    {
      throw new InconsistentRequestException( "Invalid or missing size definition." );
    }

    if( version == null )
    {
      throw new InconsistentRequestException( "Parameter 'VERSION' must be set." );
    }

    // Service = WMS
    if( !"WMS".equals( service ) )
    {
      throw new InconsistentRequestException( "Parameter 'SERVICE' must be 'WMS'." );
    }

    // BoundingBox
    GM_Envelope boundingBox;
    tmp = (String)model.remove( "BBOX" );

    if( tmp == null )
    {
      throw new InconsistentRequestException( "Parameter 'BBOX' must be set." );
    }
    else
    {
      StringTokenizer st = new StringTokenizer( tmp, "," );
      String s = st.nextToken().replace( ' ', '+' );
      double minx = Double.parseDouble( s );
      s = st.nextToken().replace( ' ', '+' );
      double miny = Double.parseDouble( s );
      s = st.nextToken().replace( ' ', '+' );
      double maxx = Double.parseDouble( s );
      s = st.nextToken().replace( ' ', '+' );
      double maxy = Double.parseDouble( s );

      if( minx >= maxx )
      {
        throw new InvalidFormatException( "minx must be lesser than maxx" );
      }

      if( miny >= maxy )
      {
        throw new InvalidFormatException( "miny must be lesser than maxy" );
      }

      boundingBox = GeometryFactory.createGM_Envelope( minx, miny, maxx, maxy );
    }

    // Width
    tmp = (String)model.remove( "WIDTH" );

    if( tmp == null )
    {
      throw new InconsistentRequestException( "Parameter 'WIDTH' must be set." );
    }

    int width = 0;

    try
    {
      width = Integer.parseInt( tmp );
    }
    catch( Exception e )
    {
      throw new InconsistentRequestException( "Invalid map width definition." );
    }

    //Height
    tmp = (String)model.remove( "HEIGHT" );

    if( tmp == null )
    {
      throw new InconsistentRequestException( "Parameter 'HEIGHT' must be set." );
    }

    int height = 0;

    try
    {
      height = Integer.parseInt( tmp );
    }
    catch( Exception e )
    {
      throw new InconsistentRequestException( "Invalid map width definition." );
    }

    // SRS
    String srs = (String)model.remove( "SRS" );

    if( tmp == null )
    {
      throw new InconsistentRequestException( "Parameter 'SRS' must be set." );
    }

    if( !srs.equalsIgnoreCase( "EPSG:4326" ) )
    {
      // transform the bounding box of the request to EPSG:4326
      try
      {
        GeoTransformer transformer = new GeoTransformer( "EPSG:4326" );
        boundingBox = transformer.transformEnvelope( boundingBox, srs );
      }
      catch( Exception e )
      {
        System.out.println( "Fehler beim umwandeln des Koordinatensystems:" );
        e.printStackTrace();
      }
    }

    //calculate the Scale
    double dx = boundingBox.getWidth() / width;
    double dy = boundingBox.getHeight() / height;

    // create a box on the central map pixel to determine its size in meter
    GM_Position min = GeometryFactory
        .createGM_Position( boundingBox.getMin().getX() + ( dx * ( width / 2d - 1d ) ), boundingBox
            .getMin().getY()
            + ( dy * ( height / 2d - 1d ) ) );
    GM_Position max = GeometryFactory.createGM_Position( boundingBox.getMin().getX()
        + ( dx * ( width / 2d ) ), boundingBox.getMin().getY() + ( dy * ( height / 2d ) ) );

    //calculate the Size of the Pixel in the middle of the BoundingBox
    double r = 6378.137;
    double rad = Math.PI / 180d;
    double cose = 0;
    cose = Math.sin( rad * min.getY() ) * Math.sin( rad * max.getY() );
    cose += ( Math.cos( rad * min.getY() ) * Math.cos( rad * max.getY() ) * Math.cos( rad
        * ( min.getX() - max.getX() ) ) );

    double scale = r * Math.acos( cose ) * 1000;
    int scaleDenominator = (int)( size / ( 0.00028 * scale ) );

    return new WMSGetScaleBarRequest_Impl( version, id, null, units, topLabel, bottomLabel,
        labelColor, font, barStyle, color, bgColor, size, scale, scaleDenominator, format );
  }

  /**
   * 
   * 
   * @param request
   * @param e
   * 
   * @return
   */
  public static WMSGetScaleBarResponse createWMSGetScaleBarResponse( WMSGetScaleBarRequest request,
      OGCWebServiceException e )
  {
    return new WMSGetScaleBarResponse_Impl( request, e );
  }

  /**
   * 
   * 
   * @param request
   * @param response
   * 
   * @return
   */
  public static WMSGetScaleBarResponse createWMSGetScaleBarResponse( WMSGetScaleBarRequest request,
      Object response )
  {
    return new WMSGetScaleBarResponse_Impl( request, response );
  }

  /**
   * @param id
   * @param request
   * @return @throws
   *         InconsistentRequestException
   */
  public static WMSGetLegendGraphicRequest createGetLegendGraphicRequest( String id, String request )
      throws InconsistentRequestException
  {
    Debug.debugMethodBegin();

    Map map = toMap( request );

    WMSGetLegendGraphicRequest req = createGetLegendGraphicRequest( id, map );

    Debug.debugMethodEnd();
    return req;
  }

  /**
   * @param id
   * @param model
   * @return @throws
   *         InconsistentRequestException
   */
  public static WMSGetLegendGraphicRequest createGetLegendGraphicRequest( String id, Map model )
      throws InconsistentRequestException
  {
    Debug.debugMethodBegin();

    // version
    String version = (String)model.remove( "VERSION" );

    if( version == null )
    {
      throw new InconsistentRequestException( "Parameter VERSION must be set." );
    }

    // request
    String request = (String)model.remove( "REQUEST" );

    if( !"GetLegendGraphic".equals( request ) )
    {
      throw new InconsistentRequestException( "Parameter REQUEST must be set."
          + " and must be equal to GetLegendGraphic" );
    }

    // format
    String format = (String)model.remove( "FORMAT" );

    if( format == null )
    {
      throw new InconsistentRequestException( "Parameter FORMAT must be set." );
    }

    if( !MimeTypeMapper.isKnownImageType( format ) )
    {
      throw new InvalidFormatException( format + " is not a valid image/result format" );
    }

    // request
    String layer = (String)model.remove( "LAYER" );

    if( layer == null )
    {
      throw new InconsistentRequestException( "Parameter LAYER must be set." );
    }

    Debug.debugMethodEnd();
    return null;
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