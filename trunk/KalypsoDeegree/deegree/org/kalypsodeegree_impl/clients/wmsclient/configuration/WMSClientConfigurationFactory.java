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
package org.deegree_impl.clients.wmsclient.configuration;

import java.io.InputStreamReader;
import java.net.URL;
import java.util.HashMap;
import java.util.StringTokenizer;

import org.deegree.services.wms.protocol.WMSGetMapRequest;
import org.deegree.xml.ElementList;
import org.deegree.xml.XMLParsingException;
import org.deegree.xml.XMLTools;
import org.deegree_impl.services.wms.capabilities.OGCWMSCapabilitiesFactory;
import org.deegree_impl.services.wms.protocol.WMSProtocolFactory;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * Factory class for creating instances of <tt>WMSClientConfiguration</tt>
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class WMSClientConfigurationFactory
{
  private String CNS = null;

  public WMSClientConfigurationFactory()
  {}

  public WMSClientConfigurationFactory( String namespace )
  {
    CNS = namespace;
  }

  /**
   * creates an instance of the <tt>WMSClientConfiguration</tt> from the
   * parameters contained in the configuration file referenced by the submitted
   * <tt>URL</tt>
   * 
   * @param url
   *          refernce to a wms client configuration document
   * @param defaultConfig
   *          default map client configuration
   * 
   * @return instance of <tt>WMSClientConfiguration</tt>
   */
  public WMSClientConfiguration createWMSClientConfiguration( URL url,
      WMSClientConfiguration defaultConfig ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    Document doc = null;

    try
    {
      InputStreamReader isr = new InputStreamReader( url.openStream() );
      doc = XMLTools.parse( isr );
    }
    catch( Exception e )
    {
      System.out.println( e );
    }

    Element root = doc.getDocumentElement();

    WMSClientConfiguration conf = createWMSClientConfiguration( root, defaultConfig );

    Debug.debugMethodEnd();
    return conf;
  }

  /**
   * creates an instance of the <tt>WMSClientConfiguration</tt> from the
   * parameters contained in the configuration file referenced by the passed
   * <tt>Element</tt>
   * 
   * @param root
   *          XML element containing the web map client configuration
   * @param defaultConfig
   *          default map client configuration
   * 
   * @return instance of <tt>WMSClientConfiguration</tt>
   */
  public WMSClientConfiguration createWMSClientConfiguration( Element root,
      WMSClientConfiguration defaultConfig ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    // get list of known WMS
    Element knownWMSE = XMLTools.getChildByName( "KnownWMS", CNS, root );
    HashMap knownWMS = createKnownWMS( knownWMSE );
    if( defaultConfig == null && knownWMS.size() == 0 )
    {
      throw new XMLParsingException( "KnownWMS must at least contain one WMS" );
    }

    //get offered image/map sizes
    Element mapSizes = XMLTools.getChildByName( "OfferedMapSize", CNS, root );
    MapSize[] offeredMapSizes = createOfferedMapSizes( mapSizes );
    if( defaultConfig == null && offeredMapSizes.length == 0 )
    {
      throw new XMLParsingException( "at least one mapsize must be defined" );
    }

    //get offered image/map formats
    Element mapFormats = XMLTools.getChildByName( "OfferedMapFormats", CNS, root );
    Format[] offeredMapFormats = createOfferedMapFormats( mapFormats );
    if( defaultConfig == null && offeredMapFormats.length == 0 )
    {
      throw new XMLParsingException( "at least one map format must be defined" );
    }

    //get offered feature info formats
    Element infoFormats = XMLTools.getChildByName( "OfferedInfoFormats", CNS, root );
    Format[] offeredInfoFormats = createOfferedMapFormats( infoFormats );
    if( defaultConfig == null && offeredInfoFormats.length == 0 )
    {
      throw new XMLParsingException( "at least one info format must be defined" );
    }

    // get offered map operations
    Element mapOperations = XMLTools.getChildByName( "OfferedMapOperations", CNS, root );
    MapOperation[] offeredMapOperations = createOfferedMapOperations( mapOperations );
    if( defaultConfig == null && offeredMapOperations.length == 0 )
    {
      throw new XMLParsingException( "at least one map operations must be defined" );
    }

    // get offered zoom factors
    Element zoomFactors = XMLTools.getChildByName( "OfferedZoomFactor", CNS, root );
    MapOperationFactor[] offeredZoomFactors = createOfferedMapOperationFactors( zoomFactors );
    if( defaultConfig == null && offeredZoomFactors.length == 0 )
    {
      throw new XMLParsingException( "at least one zoom factor must be defined" );
    }

    // get offered pan factors
    Element panFactors = XMLTools.getChildByName( "OfferedPanFactor", CNS, root );
    MapOperationFactor[] offeredPanFactors = createOfferedMapOperationFactors( panFactors );
    if( defaultConfig == null && offeredPanFactors.length == 0 )
    {
      throw new XMLParsingException( "at least one pan factor must be defined" );
    }

    // get that maps min and max scale (meter) offered by the client
    double minScale = XMLTools.getDoubleValue( "MinScale", CNS, root, 1 );
    double maxScale = XMLTools.getDoubleValue( "MaxScale", CNS, root, 100000 );

    // get initial GetMap request
    Element initMapReq = XMLTools.getChildByName( "InitialGetMapRequest", CNS, root );
    if( defaultConfig == null && initMapReq == null )
    {
      throw new XMLParsingException( "a initial GetMap request must be defined" );
    }
    WMSGetMapRequest gmr = createInitialGetMapRequest( initMapReq );

    Element projectsElem = XMLTools.getChildByName( "Projects", CNS, root );
    NodeList nl = projectsElem.getElementsByTagNameNS( CNS, "Project" );
    Project[] projects = new Project[nl.getLength()];
    for( int i = 0; i < nl.getLength(); i++ )
    {
      projects[i] = createProject( (Element)nl.item( i ) );
    }

    Debug.debugMethodEnd();
    return new WMSClientConfiguration( defaultConfig, knownWMS, offeredMapSizes, offeredMapFormats,
        offeredInfoFormats, offeredMapOperations, offeredZoomFactors, offeredPanFactors, minScale,
        maxScale, gmr, projects );
  }

  /**
   * creates a <tt>HashMap</tt> with the id of the WMS known by the client as
   * a key and the <tt>URL</tt> to its capabilities as value
   * 
   * @param knownWMSE
   *          <KnownWMS>element of the client configuration
   * 
   * @return <tt>HashMap</tt> of known WMS
   * 
   * @throws XMLParsingException
   */
  private HashMap createKnownWMS( Element knownWMSE ) throws XMLParsingException
  {
    Debug.debugMethodBegin( this, "createKnownWMS" );

    HashMap knownWMS = new HashMap();

    if( knownWMSE == null )
      return knownWMS;

    // get list of registered WMS
    ElementList el = XMLTools.getChildElementsByName( "WMS", CNS, knownWMSE );

    if( el.getLength() == 0 )
    {
      throw new XMLParsingException( "At least one WMS must be defined in the "
          + "WMSClientConfiguration." );
    }

    // get the ID and the capabilities for each registered WMS and put it
    // into the HashMap
    for( int i = 0; i < el.getLength(); i++ )
    {
      String id = XMLTools.getRequiredStringValue( "ID", CNS, el.item( i ) );
      String tmp = XMLTools.getRequiredStringValue( "CapabilitiesURL", CNS, el.item( i ) );

      try
      {
        URL url = new URL( tmp );
        OGCWMSCapabilitiesFactory fac = new OGCWMSCapabilitiesFactory();
        knownWMS.put( id, fac.createCapabilities( url ) );
      }
      catch( Exception e )
      {
        throw new XMLParsingException( "not a valid URL for WMS: " + id + "\n" + tmp + "\n"
            + e.toString() );
      }

    }

    Debug.debugMethodEnd();
    return knownWMS;
  }

  /**
   * creates a list of available map sizes for the client. If no offered map
   * size is defined in the client configuration document the default map size
   * of 600x400 will be created.
   * 
   * @param mapSizes
   *          <OfferedMapSize>element of the configuration
   * 
   * @return list of offered map sizes
   * 
   * @throws XMLParsingException
   */
  private MapSize[] createOfferedMapSizes( Element mapSizes ) throws XMLParsingException
  {
    Debug.debugMethodBegin( this, "createKnownWMS" );

    MapSize[] ms = null;

    if( mapSizes == null )
      return new MapSize[0];

    // get list of offered map sizes
    ElementList el = XMLTools.getChildElementsByName( "MapSize", CNS, mapSizes );

    if( el.getLength() == 0 )
    {
      // create available default mapsize if no map size was defined in
      // the configuration
      ms = new MapSize[1];
      ms[0] = new MapSize( 600, 400, true, false );
    }
    else
    {
      ms = new MapSize[el.getLength()];

      for( int i = 0; i < el.getLength(); i++ )
      {
        boolean free = true;
        String width = XMLTools.getRequiredAttrValue( "width", el.item( i ) );
        int w = -99;

        if( !width.equals( "*" ) )
        {
          free = false;
          w = Integer.parseInt( width );
        }

        String height = XMLTools.getRequiredAttrValue( "height", el.item( i ) );
        int h = -99;

        if( !height.equals( "*" ) )
        {
          h = Integer.parseInt( height );
        }

        String sel = XMLTools.getAttrValue( "selected", el.item( i ) );

        if( sel == null )
        {
          sel = "false";
        }

        boolean selected = sel.toUpperCase().equals( "TRUE" );
        ms[i] = new MapSize( w, h, selected, free );
      }
    }

    Debug.debugMethodEnd();
    return ms;
  }

  /**
   * Creates a list of the map/images formats offered by the client. If no
   * format is given in the configuration document 'image/jpg' will be used as
   * default
   * 
   * @param mapFormats
   *          <OfferedMapFormats>element of the configuration
   * 
   * @return list of offered map/image formats
   * 
   * @throws XMLParsingException
   */
  private Format[] createOfferedMapFormats( Element mapFormats ) throws XMLParsingException
  {
    Debug.debugMethodBegin( this, "createOfferedMapFormats" );

    if( mapFormats == null )
      return new Format[0];

    Format[] format = null;

    // get list of offered map formats
    ElementList el = XMLTools.getChildElementsByName( "Format", CNS, mapFormats );

    if( el.getLength() == 0 )
    {
      // create available default format if no one was defined in
      // the configuration
      format = new Format[1];
      format[0] = new Format( "image/jpg", true );
    }
    else
    {
      format = new Format[el.getLength()];

      for( int i = 0; i < el.getLength(); i++ )
      {
        String name = XMLTools.getStringValue( el.item( i ) );
        String sel = XMLTools.getAttrValue( "selected", el.item( i ) );

        if( sel == null )
        {
          sel = "false";
        }

        boolean selected = sel.toUpperCase().equals( "TRUE" );
        format[i] = new Format( name, selected );
      }
    }

    Debug.debugMethodEnd();
    return format;
  }

  /**
   * returns a list of the map operations (zoomin, zoomout, pan etc.) offered by
   * the client
   * 
   * @param mapOperations
   *          <OfferedMapOperations>element of the configuration
   * 
   * @return list of <tt>MapOperation</tt> s
   * 
   * @throws XMLParsingException
   */
  private MapOperation[] createOfferedMapOperations( Element mapOperations )
      throws XMLParsingException
  {
    Debug.debugMethodBegin( this, "createOfferedMapOperations" );

    if( mapOperations == null )
      return new MapOperation[0];

    MapOperation[] omo = null;

    // get list of offered map operations
    ElementList el = XMLTools.getChildElementsByName( "Operation", CNS, mapOperations );

    if( el.getLength() == 0 )
    {
      // create available default map operations if no one was defined in
      // the configuration
      omo = new MapOperation[6];
      omo[0] = new MapOperation( "ZOOMIN", true );
      omo[1] = new MapOperation( "ZOOMOUT", true );
      omo[2] = new MapOperation( "PAN", true );
      omo[3] = new MapOperation( "RECENTER", true );
      omo[4] = new MapOperation( "REFRESH", true );
      omo[5] = new MapOperation( "INFO", true );
    }
    else
    {
      omo = new MapOperation[el.getLength()];

      for( int i = 0; i < el.getLength(); i++ )
      {
        String name = XMLTools.getStringValue( el.item( i ) );
        String sel = XMLTools.getAttrValue( "selected", el.item( i ) );

        if( sel == null )
        {
          sel = "false";
        }

        boolean selected = sel.toUpperCase().equals( "TRUE" );
        omo[i] = new MapOperation( name, selected );
      }
    }

    Debug.debugMethodEnd();
    return omo;
  }

  /**
   * returns a list of offered numerical map operation factors that can be used
   * to determine zoom or pan levels
   * 
   * @param factors
   *          a <tt>Element</tt> that contains <Factor>elements as children
   * 
   * @return list of <tt>MapOperationFactor</tt> s
   * 
   * @throws XMLParsingException
   */
  private MapOperationFactor[] createOfferedMapOperationFactors( Element factors )
      throws XMLParsingException
  {
    Debug.debugMethodBegin( this, "createOfferedMapOperationFactors" );

    if( factors == null )
      return new MapOperationFactor[0];

    MapOperationFactor[] mof = null;

    // get list of offered factors
    ElementList el = XMLTools.getChildElementsByName( "Factor", CNS, factors );

    if( el.getLength() == 0 )
    {
      // create available default map operations factor if no one was
      // defined in the configuration
      mof = new MapOperationFactor[0];
      mof[0] = new MapOperationFactor( 10, true, false );
    }
    else
    {
      mof = new MapOperationFactor[el.getLength()];

      for( int i = 0; i < el.getLength(); i++ )
      {
        boolean free = true;
        String tmp = XMLTools.getStringValue( el.item( i ) );
        double fac = -99;

        if( !tmp.equals( "*" ) )
        {
          free = false;
          fac = Double.parseDouble( tmp );
        }

        String sel = XMLTools.getAttrValue( "selected", el.item( i ) );

        if( sel == null )
        {
          sel = "false";
        }

        boolean selected = sel.toUpperCase().equals( "TRUE" );
        mof[i] = new MapOperationFactor( fac, selected, free );
      }
    }

    Debug.debugMethodEnd();
    return mof;
  }

  /**
   * 
   * 
   * @param initMapReq
   * 
   * @return @throws
   *         XMLParsingException
   */
  private WMSGetMapRequest createInitialGetMapRequest( Element initMapReq )
      throws XMLParsingException
  {
    Debug.debugMethodBegin( this, "createInitialGetMapRequest" );

    if( initMapReq == null )
      return null;

    String params = XMLTools.getStringValue( initMapReq );

    StringTokenizer st = new StringTokenizer( params, "&" );
    HashMap map = new HashMap();
    while( st.hasMoreTokens() )
    {
      String s = st.nextToken();
      int pos = s.indexOf( '=' );
      String s1 = s.substring( 0, pos );
      String s2 = s.substring( pos + 1, s.length() );
      map.put( s1.toUpperCase(), s2 );
    }

    WMSGetMapRequest gmr = null;
    try
    {
      gmr = WMSProtocolFactory.createGetMapRequest( this.toString(), map );
    }
    catch( Exception e )
    {
      throw new XMLParsingException( "could not create initial get map " + "request\n" + e );
    }

    Debug.debugMethodEnd();
    return gmr;
  }

  /**
   * creates a predefined project that can be invoked by the client
   */
  private Project createProject( Element projectElement ) throws XMLParsingException
  {
    Debug.debugMethodBegin( this, "createProject" );

    String name = XMLTools.getRequiredAttrValue( "name", projectElement );

    Element initMapReq = XMLTools.getRequiredChildByName( "InitialGetMapRequest", CNS,
        projectElement );
    WMSGetMapRequest gmr = createInitialGetMapRequest( initMapReq );

    Project project = new Project( name, gmr );

    Debug.debugMethodEnd();
    return project;
  }

  public static void main( String[] args )
  {

    try
    {
      URL url = new URL( "file:///c:/temp/configuration.xml" );
      WMSClientConfigurationFactory fac = new WMSClientConfigurationFactory();
      WMSClientConfiguration conf = fac.createWMSClientConfiguration( url, null );
      conf.clone();
    }
    catch( Exception e )
    {
      System.out.println( e );
    }

  }

}