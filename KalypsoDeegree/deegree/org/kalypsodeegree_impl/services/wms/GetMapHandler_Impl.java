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
package org.deegree_impl.services.wms;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;

import javax.media.jai.JAI;
import javax.media.jai.RenderedOp;

import org.apache.batik.svggen.SVGGraphics2D;
import org.deegree.gml.GMLDocument;
import org.deegree.gml.GMLFeatureCollection;
import org.deegree.graphics.Theme;
import org.deegree.graphics.sld.FeatureTypeConstraint;
import org.deegree.graphics.sld.Layer;
import org.deegree.graphics.sld.LayerFeatureConstraints;
import org.deegree.graphics.sld.NamedLayer;
import org.deegree.graphics.sld.NamedStyle;
import org.deegree.graphics.sld.RemoteOWS;
import org.deegree.graphics.sld.Style;
import org.deegree.graphics.sld.StyledLayerDescriptor;
import org.deegree.graphics.sld.UserLayer;
import org.deegree.graphics.sld.UserStyle;
import org.deegree.model.feature.FeatureCollection;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Position;
import org.deegree.services.InconsistentRequestException;
import org.deegree.services.OGCWebServiceClient;
import org.deegree.services.OGCWebServiceEvent;
import org.deegree.services.OGCWebServiceException;
import org.deegree.services.OGCWebServiceResponse;
import org.deegree.services.RangeParamList;
import org.deegree.services.WebServiceException;
import org.deegree.services.wcs.protocol.WCSGetCoverageRequest;
import org.deegree.services.wcs.protocol.WCSGetCoverageResponse;
import org.deegree.services.wfs.capabilities.WFSCapabilities;
import org.deegree.services.wfs.filterencoding.Filter;
import org.deegree.services.wfs.protocol.WFSGetFeatureRequest;
import org.deegree.services.wfs.protocol.WFSGetFeatureResponse;
import org.deegree.services.wfs.protocol.WFSQuery;
import org.deegree.services.wms.GetMapHandler;
import org.deegree.services.wms.InvalidSRSException;
import org.deegree.services.wms.LayerNotDefinedException;
import org.deegree.services.wms.StyleNotDefinedException;
import org.deegree.services.wms.capabilities.DataSource;
import org.deegree.services.wms.capabilities.DeegreeParam;
import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.deegree.services.wms.protocol.WMSGetMapRequest;
import org.deegree.services.wms.protocol.WMSGetMapResponse;
import org.deegree.xml.DOMPrinter;
import org.deegree.xml.XMLParsingException;
import org.deegree.xml.XMLTools;
import org.deegree_impl.gml.GMLDocument_Impl;
import org.deegree_impl.graphics.MapFactory;
import org.deegree_impl.graphics.optimizers.LabelOptimizer;
import org.deegree_impl.model.cs.ConvenienceCSFactory;
import org.deegree_impl.model.cs.CoordinateSystem;
import org.deegree_impl.model.ct.GeoTransformer;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.gc.ImageGridCoverage;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.services.OGCWebServiceEvent_Impl;
import org.deegree_impl.services.OGCWebServiceException_Impl;
import org.deegree_impl.services.RangeParamList_Impl;
import org.deegree_impl.services.wcs.protocol.WCSProtocolFactory;
import org.deegree_impl.services.wfs.filterencoding.ComplexFilter;
import org.deegree_impl.services.wfs.filterencoding.FeatureFilter;
import org.deegree_impl.services.wfs.filterencoding.FeatureId;
import org.deegree_impl.services.wfs.protocol.WFSProtocolFactory;
import org.deegree_impl.services.wms.protocol.WMSGetMapRequest_Impl;
import org.deegree_impl.services.wms.protocol.WMSProtocolFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.IDGenerator;
import org.deegree_impl.tools.MimeTypeMapper;
import org.deegree_impl.tools.NetWorker;
import org.deegree_impl.tools.StringExtend;
import org.opengis.cs.CS_CoordinateSystem;
import org.opengis.gc.GC_GridCoverage;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

import com.sun.media.jai.codec.ByteArraySeekableStream;
import com.sun.media.jai.codec.MemoryCacheSeekableStream;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class GetMapHandler_Impl implements GetMapHandler
{
  protected WMSGetMapRequest request = null;

  private Object[] themes = null;

  protected double scale = 0;

  private int count = 0;

  protected CS_CoordinateSystem reqCRS = null;

  private WMSCapabilities capabilities = null;

  /**
   * Creates a new GetMapHandler object.
   * 
   * @param request
   *          request to perform
   */
  public GetMapHandler_Impl( WMSCapabilities capabilities, WMSGetMapRequest request )
      throws WebServiceException
  {
    this.request = request;
    this.capabilities = capabilities;

    try
    {
      reqCRS = createRequestCRS();
      if( reqCRS == null )
      {
        throw new InvalidSRSException( "SRS: " + request.getSrs()
            + "is nor known by the deegree WMS" );
      }
    }
    catch( Exception e )
    {
      throw new InvalidSRSException( "SRS: " + request.getSrs() + "is nor known by the deegree WMS" );
    }

  }

  /**
   * calculates the map scale as defined in the OGC WMS 1.1.1 specifications
   * 
   * @return scale of the map
   */
  private double calcScale() throws Exception
  {

    Debug.debugMethodBegin( this, "calcScale" );

    GM_Envelope bbox = request.getBoundingBox();

    double resx = bbox.getWidth() / request.getWidth();

    if( !request.getSrs().equalsIgnoreCase( "EPSG:4326" ) )
    {
      // transform the bounding box of the request to EPSG:4326
      GeoTransformer transformer = new GeoTransformer( "EPSG:4326" );
      bbox = transformer.transformEnvelope( bbox, reqCRS );
    }

    double dx = bbox.getWidth() / request.getWidth();
    double dy = bbox.getHeight() / request.getHeight();

    // create a box on the central map pixel to determine its size in meter
    GM_Position min = GeometryFactory.createGM_Position( bbox.getMin().getX() + dx
        * ( request.getWidth() / 2d - 1d ), bbox.getMin().getY() + dy
        * ( request.getHeight() / 2d - 1d ) );
    GM_Position max = GeometryFactory.createGM_Position( bbox.getMin().getX() + dx
        * ( request.getWidth() / 2d ), bbox.getMin().getY() + dy * ( request.getHeight() / 2d ) );

    double sc = calcDistance( min.getY(), min.getX(), max.getY(), max.getX() );
    System.out.println( "OGC WMS scale: " + sc );
    System.out.println( "OGC WCS resx (deegree WCS scale): " + resx );

    Debug.debugMethodEnd();

    return sc;
  }

  /**
   * calculates the distance between two points in EPSG:4326 coodinates.
   */
  private double calcDistance( double lon1, double lat1, double lon2, double lat2 )
  {
    double r = 6378.137;
    double rad = Math.PI / 180d;
    double cose = 0;

    cose = Math.sin( rad * lon1 ) * Math.sin( rad * lon2 );
    cose += Math.cos( rad * lon1 ) * Math.cos( rad * lon2 ) * Math.cos( rad * ( lat1 - lat2 ) );
    double dist = r * Math.acos( cose );
    return dist * 1000;
  }

  /**
   * creates an object that represents the CRS of the GetMap request
   */
  private CS_CoordinateSystem createRequestCRS()
  {

    Debug.debugMethodBegin( this, "createRequestCRS" );

    ConvenienceCSFactory cf = ConvenienceCSFactory.getInstance();
    CoordinateSystem csSource = cf.getCSByName( request.getSrs() );
    if( csSource == null )
      return null;
    org.deegree_impl.model.cs.Adapters adapter = org.deegree_impl.model.cs.Adapters.getDefault();
    CS_CoordinateSystem crs_ = adapter.export( csSource );

    Debug.debugMethodEnd();

    return crs_;
  }

  /**
   * increases the counter variable that holds the number of services that has
   * sent a response. All data are available if the counter value equals the
   * number of requested layers.
   */
  protected synchronized void increaseCounter()
  {
    count++;
    if( count == themes.length )
      notifyAll();
  }

  /**
   * performs a GetMap request and retruns the result encapsulated within a
   * <tt>WMSGetMapResponse</tt> object.
   * <p>
   * The method throws an WebServiceException that only shall be thrown if an
   * fatal error occurs that makes it imposible to return a result. If something
   * wents wrong performing the request (none fatal error) The exception shall
   * be encapsulated within the response object to be returned to the client as
   * requested (GetMap-Request EXCEPTION-Parameter).
   * 
   * @return response to the GetMap response
   */
  public OGCWebServiceResponse performGetMap() throws WebServiceException
  {
    Debug.debugMethodBegin( this, "performGetMap" );

    try
    {
      scale = calcScale();
    }
    catch( Exception e )
    {
      Debug.debugException( e, " - " );
      throw new WebServiceException( "Couldn't calculate scale! " + e );
    }

    StyledLayerDescriptor sld = request.getStyledLayerDescriptor();
    Layer[] layers = sld.getLayers();

    // there must be one theme for each requested layer
    themes = new Object[layers.length];

    // invokes the data supplyer for each layer in an independ thread
    for( int i = 0; i < layers.length; i++ )
    {

      if( layers[i] instanceof NamedLayer )
      {
        org.deegree.services.wms.capabilities.Layer lay = capabilities.getCapability().getLayer(
            layers[i].getName() );
        if( validate( lay, layers[i].getName() ) )
        {
          // get styles associated to the layer
          UserStyle[] us = getStyles( (NamedLayer)layers[i] );
          ServiceInvokerForNL si = new ServiceInvokerForNL( lay, us, i );
          si.start();
        }
        else
        {
          // set theme to null if no data are available for the requested
          // area and/or scale
          themes[i] = null;
          increaseCounter();
        }
      }
      else
      {
        ServiceInvokerForUL si = new ServiceInvokerForUL( (UserLayer)layers[i], i );
        si.start();
      }
    }

    if( count < themes.length )
    {
      // waits until the requested layers are available as
      // <tt>DisplayElements</tt>
      // or the time limit has been reached.
      // if count == themes.length then no request must be performed
      try
      {
        // must be synchronized to own the objects monitor
        synchronized( this )
        {
          long timeStamp = System.currentTimeMillis();
          // subtract 1 second for architecture overhead and image creation
          long timeLimit = 1000 * ( capabilities.getDeegreeParam().getRequestTimeLimit() - 1 );
          wait( timeLimit );
          if( System.currentTimeMillis() - timeStamp >= timeLimit )
          {
            throw new WebServiceException( "Processing of the GetMap request "
                + "exceeds timelimit" );
          }
        }
      }
      catch( Exception e )
      {
        Debug.debugException( e, " - " );
        return createResponse( e );
      }
    }

    WMSGetMapResponse res = renderMap();

    Debug.debugMethodEnd();

    return res;
  }

  /**
   * returns the <tt>UserStyle</tt> s assigned to a named layer
   * 
   * @param sldLayer
   *          layer to get the styles for
   */
  private UserStyle[] getStyles( NamedLayer sldLayer ) throws WebServiceException
  {

    org.deegree.graphics.sld.Style[] styles = sldLayer.getStyles();
    UserStyle[] us = new UserStyle[styles.length];

    // to avoid retrieving the layer again for each style
    org.deegree.services.wms.capabilities.Layer layer = null;
    layer = capabilities.getCapability().getLayer( sldLayer.getName() );

    for( int i = 0; i < styles.length; i++ )
    {
      if( styles[i] instanceof NamedStyle )
      {
        String s = styles[i].getName();
        if( s.equals( "default" ) )
        {
          s = "default:" + sldLayer.getName();
        }
        us[i] = layer.getStyle( s );
        if( us[i] == null && !( styles[i].getName().startsWith( "default" ) ) )
        {
          throw new StyleNotDefinedException( "StyleNotDefined:  " + styles[i].getName() );
        }
      }
      else
      {
        us[i] = (UserStyle)styles[i];
      }
    }

    return us;
  }

  /**
   * validates if the requested layer matches the conditions of the request if
   * not a <tt>WebServiceException</tt> will be thrown. If the layer matches
   * the request, but isn't able to deviever data for the requested area and/or
   * scale false will be returned. If the layer matches the request and contains
   * data for the requested area and/or scale true will be returned.
   * 
   * @param layer
   *          layer as defined at the capabilities/configuration
   * @param name
   *          name of the layer (must be submitted seperatly because the layer
   *          parameter can be <tt>null</tt>
   */
  private boolean validate( org.deegree.services.wms.capabilities.Layer layer, String name )
      throws WebServiceException
  {

    Debug.debugMethodBegin( this, "validate" );
    // check if layer is available
    if( layer == null )
    {
      throw new LayerNotDefinedException( "Layer: " + name + " is not known by the WMS" );
    }

    if( !layer.isSrsSupported( request.getSrs() ) )
    {
      throw new InvalidSRSException( "SRS: " + request.getSrs() + "is not known by layer: " + name );
    }

    // check for valid coordinated reference system
    String[] srs = layer.getSrs();
    boolean tmp = false;
    for( int i = 0; i < srs.length; i++ )
    {
      if( srs[i].equalsIgnoreCase( request.getSrs() ) )
      {
        tmp = true;
        break;
      }
    }

    if( !tmp )
    {
      throw new InvalidSRSException( "layer: " + name + " can't be " + "delievered in SRS: "
          + request.getSrs() );
    }
    // check scale
    if( layer.getDataSource( scale ) == null )
    {
      return false;
    }
    // check bounding box
    try
    {

      GM_Envelope bbox = request.getBoundingBox();
      GM_Envelope layerBbox = layer.getLatLonBoundingBox();
      if( !request.getSrs().equalsIgnoreCase( "EPSG:4326" ) )
      {
        // transform the bounding box of the request to EPSG:4326
        GeoTransformer transformer = new GeoTransformer( "EPSG:4326" );
        bbox = transformer.transformEnvelope( bbox, reqCRS );
      }
      if( !bbox.intersects( layerBbox ) )
      {
        return false;
      }

    }
    catch( Exception e )
    {
      Debug.debugException( e, " - " );
      throw new WebServiceException( "couldn't compare bounding boxes\n" + e.toString() );
    }

    Debug.debugMethodEnd();

    return true;
  }

  /**
   * creates a <tt>WMSGetMapResponse</tt> containing an
   * <tt>OGCWebServiceException</tt>
   * 
   * @param e
   *          exception to encapsulate into the response
   */
  private WMSGetMapResponse createResponse( Exception e )
  {
    String format = request.getExceptions();

    Object content = null;
    OGCWebServiceException exce = null;
    if( "application/vnd.ogc.se_inimage".equals( format ) )
    {
      BufferedImage bi = new BufferedImage( request.getWidth(), request.getHeight(),
          BufferedImage.TYPE_INT_ARGB );
      Graphics g = bi.getGraphics();
      g.setColor( Color.BLUE );
      g.drawString( e.toString(), 5, 20 );
      content = bi;
    }
    else if( "application/vnd.ogc.se_blank".equals( format ) )
    {
      content = new BufferedImage( request.getWidth(), request.getHeight(),
          BufferedImage.TYPE_INT_ARGB );
    }
    else
    {
      // default --> application/vnd.ogc.se_xml
      exce = new OGCWebServiceException_Impl( "GetMapHandler_Impl", e.getMessage() );
    }

    WMSGetMapResponse res = WMSProtocolFactory.createGetMapResponse( request, exce, content );

    return res;
  }

  /**
   * put a theme to the passed index of the themes array. The second param
   * passed is a <tt>Theme</tt> or an exception
   */
  protected synchronized void putTheme( int index, Object o )
  {
    themes[index] = o;
  }

  /**
   * renders the map from the <tt>DisplayElement</tt> s
   */
  private WMSGetMapResponse renderMap()
  {

    Debug.debugMethodBegin( this, "renderMap" );

    WMSGetMapResponse response = null;
    OGCWebServiceException exce = null;

    ArrayList list = new ArrayList( 50 );
    for( int i = 0; i < themes.length; i++ )
    {
      if( themes[i] instanceof Exception )
      {
        exce = new OGCWebServiceException_Impl( "GetMapHandler_Impl: renderMap", themes[i]
            .toString() );
        break;
      }
      if( themes[i] instanceof OGCWebServiceException )
      {
        exce = (OGCWebServiceException)themes[i];
        break;
      }
      if( themes[i] != null )
      {
        list.add( themes[i] );
      }
    }

    String mime = MimeTypeMapper.toMimeType( request.getFormat() );

    // get target object for rendering
    Object target = GraphicContextFactory.createGraphicTarget( mime, request.getWidth(), request
        .getHeight() );
    // get graphic context of the target
    Graphics g = GraphicContextFactory.createGraphicContext( mime, target );

    if( exce == null )
    {
      // only if no exception occured
      try
      {
        Theme[] th = (Theme[])list.toArray( new Theme[list.size()] );
        org.deegree.graphics.MapView map = null;
        if( th.length > 0 )
        {
          map = MapFactory.createMapView( "deegree WMS", request.getBoundingBox(), reqCRS, th );
        }
        g.setClip( 0, 0, request.getWidth(), request.getHeight() );
        if( !request.getTransparency() )
        {
          g.setColor( request.getBGColor() );
          g.fillRect( 0, 0, request.getWidth(), request.getHeight() );
        }
        if( map != null )
        {
          Theme[] themes = map.getAllThemes();
          map.addOptimizer( new LabelOptimizer( themes ) );
          // antialiasing must be switched of for gif output format
          // because the antialiasing may create more than 255 colors
          // in the map/image, even just a few colors are defined in
          // the styles
          if( !request.getFormat().equalsIgnoreCase( "image/gif" ) )
          {
            ( (Graphics2D)g ).setRenderingHint( RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON );
            ( (Graphics2D)g ).setRenderingHint( RenderingHints.KEY_TEXT_ANTIALIASING,
                RenderingHints.VALUE_TEXT_ANTIALIAS_ON );
          }
          map.paint( g );
        }
      }
      catch( Exception e )
      {
        Debug.debugException( e, " - " );
        exce = new OGCWebServiceException_Impl( "GetMapHandler_Impl: renderMap", e.toString() );
      }
    }

    // print a copyright note at the left lower corner of the map
    printCopyright( g, request.getHeight() );

    if( mime.equals( "image/svg+xml" ) || mime.equals( "image/svg xml" ) )
    {
      response = WMSProtocolFactory.createGetMapResponse( request, exce, ( (SVGGraphics2D)g )
          .getRoot() );
    }
    else
    {
      response = WMSProtocolFactory.createGetMapResponse( request, exce, target );
    }
    g.dispose();

    Debug.debugMethodEnd();
    return response;
  }

  /**
   * prints a copyright note at left side of the map bottom. The copyright note
   * will be extracted from the WMS capabilities/configuration
   * 
   * @param g
   *          graphic context of the map
   * @param heigth
   *          height of the map in pixel
   */
  private void printCopyright( Graphics g, int heigth )
  {
    DeegreeParam dp = capabilities.getDeegreeParam();
    String copyright = dp.getCopyRight();
    if( copyright != null )
    {
      g.setFont( new Font( "SANSSERIF", Font.PLAIN, 14 ) );
      g.setColor( Color.BLACK );
      g.drawString( copyright, 8, heigth - 15 );
      g.drawString( copyright, 10, heigth - 15 );
      g.drawString( copyright, 8, heigth - 13 );
      g.drawString( copyright, 10, heigth - 13 );
      g.setColor( Color.WHITE );
      g.setFont( new Font( "SANSSERIF", Font.PLAIN, 14 ) );
      g.drawString( copyright, 9, heigth - 14 );
      //g.dispose();
    }
  }

  ////////////////////////////////////////////////////////////////////////////
  //                          inner classes //
  ////////////////////////////////////////////////////////////////////////////

  /**
   * Inner class for accessing the data of one named layer and creating
   * <tt>DisplayElement</tt> s and a <tt>Thrme</tt> from it. The class
   * extends <tt>Thread</tt> and implements the run method, so that a parallel
   * data accessing from several layers is possible.
   * 
   * @version $Revision$
   * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
   */
  private class ServiceInvokerForNL extends Thread implements OGCWebServiceClient
  {
    private org.deegree.services.wms.capabilities.Layer layer = null;

    private UserStyle[] styles = null;

    private int index = 0;

    /**
     * Creates a new ServiceInvokerForNL object.
     * 
     * @param layer
     * @param index
     *          index of the requested layer
     */
    ServiceInvokerForNL( org.deegree.services.wms.capabilities.Layer layer, UserStyle[] styles,
        int index )
    {
      this.layer = layer;
      this.styles = styles;
      this.index = index;
    }

    /**
     * overrides the run-method of the parent class <tt>Thread</tt> for
     * enabling a multi-threaded access to the data.
     */
    public void run()
    {
      Debug.debugMethodBegin( this, "run" );

      DataSource ds = layer.getDataSource( scale );

      if( ds != null )
      {
        OGCWebServiceEvent event = null;
        try
        {
          int type = ds.getType();
          switch( type )
          {
          case DataSource.LOCALWFS:
          case DataSource.REMOTEWFS:
          {
            event = createGetFeatureRequest( ds );
            break;
          }
          case DataSource.LOCALWCS:
          case DataSource.REMOTEWCS:
          {
            event = createGetCoverageRequest( ds );
            break;
          }
          case DataSource.REMOTEWMS:
          {
            event = createGetMapRequest( ds );
            break;
          }
          }
        }
        catch( Exception e )
        {
          Debug.debugException( e, " - " );
          OGCWebServiceException exce = new OGCWebServiceException_Impl( "ServiceInvokerForNL: "
              + layer.getName(), "Couldn't create query!"
              + StringExtend.stackTraceToString( e.getStackTrace() ) );
          putTheme( index, exce );
          increaseCounter();
          Debug.debugMethodEnd();
          return;
        }

        try
        {
          ds.getOGCWebService().doService( event );
        }
        catch( Exception e )
        {
          Debug.debugException( e, " - " );
          OGCWebServiceException exce = new OGCWebServiceException_Impl( "ServiceInvokerForNL: "
              + layer.getName(), "Couldn't perform doService()!"
              + StringExtend.stackTraceToString( e.getStackTrace() ) );
          putTheme( index, exce );
          increaseCounter();
          Debug.debugMethodEnd();
          return;
        }

      }
      else
      {
        // increase counter because there is no service to call so it
        // is assumed that the request for the current layer if fullfilled
        increaseCounter();
      }

      Debug.debugMethodEnd();
    }

    /**
     * creates a getFeature request considering the getMap request and the
     * filterconditions defined in the submitted <tt>DataSource</tt> object.
     * The request will be encapsualted within a <tt>OGCWebServiceEvent</tt>.
     * 
     * @return envent object containing a GetFeature request
     */
    private OGCWebServiceEvent createGetFeatureRequest( DataSource ds ) throws Exception
    {
      Debug.debugMethodBegin( this, "createGetFeatureRequest" );

      GM_Envelope bbox = request.getBoundingBox();

      // transform request bounding box to the coordinate reference
      // system the WFS holds the data if requesting CRS and WFS-Data
      // crs are different
      WFSCapabilities capa = (WFSCapabilities)ds.getOGCWebService().getCapabilities();
      org.deegree.services.wfs.capabilities.FeatureType ft = capa.getFeatureTypeList()
          .getFeatureType( ds.getName() );
      if( ft == null )
      {
        throw new WebServiceException( "Feature Type:" + ds.getName() + " is not known by the WFS" );
      }

      // enable different formatations of the crs encoding for GML geometries
      String GML_SRS = "http://www.opengis.net/gml/srs/";
      String old_gml_srs = ft.getSrs();
      String old_srs;
      if( old_gml_srs.startsWith( GML_SRS ) )
      {
        old_srs = old_gml_srs.substring( 31 ).replace( '#', ':' ).toUpperCase();
      }
      else
      {
        old_srs = old_gml_srs;
      }

      String new_srs = request.getSrs();
      String new_gml_srs;
      if( old_gml_srs.startsWith( GML_SRS ) )
      {
        new_gml_srs = GML_SRS + new_srs.replace( ':', '#' ).toLowerCase();
      }
      else
      {
        new_gml_srs = new_srs;
      }

      if( !( old_srs.equalsIgnoreCase( new_srs ) ) )
      {
        GeoTransformer transformer = new GeoTransformer( old_srs );
        bbox = transformer.transformEnvelope( bbox, reqCRS );
      }

      // no filter condition has been defined
      StringBuffer sb = new StringBuffer( 5000 );
      sb.append( "<?xml version='1.0' encoding='UTF-8'?>" );
      sb.append( "<GetFeature xmlns='http://www.opengis.net/wfs' " );
      sb.append( "xmlns:ogc='http://www.opengis.net/ogc' " );
      sb.append( "xmlns:gml='http://www.opengis.net/gml' " );
      sb.append( "service='WFS' version='1.0.0' " );
      if( ds.getType() == DataSource.LOCALWFS )
      {
        sb.append( "outputFormat='FEATURECOLLECTION'>" );
      }
      else
      {
        sb.append( "outputFormat='GML2'>" );
      }
      sb.append( "<Query typeName='" + ds.getName() + "'>" );

      WFSQuery query = ds.getQuery();
      if( query == null )
      {
        sb.append( "<ogc:Filter><ogc:BBOX>" );
        sb.append( "<PropertyName>" ).append( ds.getGeometryProperty() );
        sb.append( "</PropertyName>" );
        sb.append( "<gml:Box srsName='" + new_gml_srs + "'>" );
        sb.append( "<gml:coordinates>" ).append( bbox.getMin().getX() );
        sb.append( ',' ).append( bbox.getMin().getY() );
        sb.append( ' ' ).append( bbox.getMax().getX() ).append( ',' );
        sb.append( bbox.getMax().getY() ).append( "</gml:coordinates >" );
        sb.append( "</gml:Box>" );
        sb.append( "</ogc:BBOX>" );
        sb.append( "</ogc:Filter></Query></GetFeature>" );
      }
      else
      {
        Filter filter = query.getFilter();

        sb.append( "<ogc:Filter>" );
        if( filter instanceof ComplexFilter )
        {
          sb.append( "<ogc:And>" );
          sb.append( "<ogc:BBOX><PropertyName>" ).append( ds.getGeometryProperty() );
          sb.append( "</PropertyName>" ).append( "<gml:Box srsName='" + new_gml_srs + "'>" );
          sb.append( "<gml:coordinates>" ).append( bbox.getMin().getX() );
          sb.append( ',' ).append( bbox.getMin().getY() );
          sb.append( ' ' ).append( bbox.getMax().getX() ).append( ',' );
          sb.append( bbox.getMax().getY() ).append( "</gml:coordinates >" );
          sb.append( "</gml:Box></ogc:BBOX>" );

          // add filter as defined in the layers datasource description
          // to the filter expression
          org.deegree.services.wfs.filterencoding.Operation op = ( (ComplexFilter)filter )
              .getOperation();
          sb.append( op.toXML() ).append( "</ogc:And>" );
        }
        else
        {
          ArrayList featureIds = ( (FeatureFilter)filter ).getFeatureIds();
          if( featureIds.size() > 1 )
          {
            sb.append( "<ogc:And>" );
          }
          for( int i = 0; i < featureIds.size(); i++ )
          {
            FeatureId fid = (FeatureId)featureIds.get( i );
            sb.append( fid.toXML() );
          }
          if( featureIds.size() > 1 )
          {
            sb.append( "</ogc:And>" );
          }
        }
        sb.append( "</ogc:Filter></Query></GetFeature>" );
      }

      // create dom representation of the request
      Document doc = XMLTools.parse( new StringReader( sb.toString() ) );

      // create OGCWebServiceEvent object
      IDGenerator idg = IDGenerator.getInstance();
      WFSGetFeatureRequest gfr = WFSProtocolFactory.createWFSGetFeatureRequest( ""
          + idg.generateUniqueID(), doc );

      OGCWebServiceEvent event = new OGCWebServiceEvent_Impl( this, gfr, "", this );

      Debug.debugMethodEnd();
      return event;
    }

    /**
     * creates a getCoverage request considering the getMap request and the
     * filterconditions defined in the submitted <tt>DataSource</tt> object
     * The request will be encapsualted within a <tt>OGCWebServiceEvent</tt>.
     * 
     * @return envent object containing a GetCoverage request
     */
    private OGCWebServiceEvent createGetCoverageRequest( DataSource ds )
        throws InconsistentRequestException
    {
      Debug.debugMethodBegin();

      GM_Envelope bbox = request.getBoundingBox();

      WCSGetCoverageRequest gcr = ds.getGetCoverageRequest();

      IDGenerator idg = IDGenerator.getInstance();
      String crs = request.getSrs();
      if( gcr != null && gcr.getSRS() != null )
      {
        crs = gcr.getSRS();
      }
      String format = request.getFormat();
      if( gcr != null && !"%default%".equals( gcr.getFormat() ) )
      {
        format = gcr.getFormat();
      }
      format = format.substring( format.indexOf( '/' ) + 1, format.length() );
      if( format.indexOf( "svg" ) > -1 )
      {
        format = "jpeg";
      }

      String version = "1.0.0";
      if( gcr != null && gcr.getVersion() != null )
      {
        version = gcr.getVersion();
      }
      String lay = ds.getName();
      if( gcr != null && !"%default%".equals( gcr.getLayer() ) )
      {
        lay = gcr.getLayer();
      }
      String ipm = null;
      if( gcr != null && gcr.getInterpolationMethod() != null )
      {
        ipm = gcr.getInterpolationMethod();
      }

      RangeParamList rpl = new RangeParamList_Impl();
      if( gcr != null )
      {
        rpl = gcr.getRangeList();
        System.out.println( "*** TODO: parsing TimeExtent from gcr!" );
        //TODO: parsing TimeExtent from gcr
      }
      else
      {
        //- Special case 1: time
        String stime = request.getTime(); // this is the time string to parse
        if( stime != null )
          rpl.addParameter( "time", stime );

        System.out.println( "ETJ:: WMS time is " + stime );

        //				//- TODO: Special case 2: elevation
        //				double[] delev = request.getElevation(); // this is the elevation
        // array
        //				if(delev != null)
        //					rpl.addParameter("elevation", delev); //dunno how to parse it

        //- Normal cases: parse all other dimensional parameters.
      }

      // png will be used as requesting format against the WCS because it
      // supports transparency so region without raster data won't cover
      // underlying layers
      gcr = WCSProtocolFactory.createWCSGetCoverageRequest( version, "" + idg.generateUniqueID(),
          null, lay, crs, request.getSrs(), bbox, rpl, request.getWidth(), request.getHeight(), 0,
          format, ipm, request.getExceptions() );

      OGCWebServiceEvent event = new OGCWebServiceEvent_Impl( this, gcr, "", this );

      Debug.debugMethodEnd();
      return event;
    }

    /**
     * creates a getMap request for requesting a cascaded remote WMS considering
     * the getMap request and the filterconditions defined in the submitted
     * <tt>DataSource</tt> object The request will be encapsualted within a
     * <tt>OGCWebServiceEvent</tt>.
     * 
     * @return envent object containing a GetMap request
     */
    private OGCWebServiceEvent createGetMapRequest( DataSource ds ) throws XMLParsingException
    {
      Debug.debugMethodBegin( this, "createGetMapRequest" );

      WMSGetMapRequest gmr = ds.getGetMapRequest();

      String format = request.getFormat();

      if( gmr != null && !"%default%".equals( gmr.getFormat() ) )
      {
        format = gmr.getFormat();
      }

      org.deegree.services.wms.protocol.WMSGetMapRequest.Layer[] lys = null;
      lys = new org.deegree.services.wms.protocol.WMSGetMapRequest.Layer[1];

      if( styles[0] != null )
      {
        lys[0] = WMSGetMapRequest_Impl.createLayer( layer.getName(), styles[0].getName() );
      }
      else
      {
        lys[0] = WMSGetMapRequest_Impl.createLayer( layer.getName(), "default" );
      }
      if( gmr != null && gmr.getLayers() != null
          && !( gmr.getLayers()[0].getName().equals( "%default%" ) ) )
      {
        lys = gmr.getLayers();
      }
      Color bgColor = request.getBGColor();
      if( gmr != null && gmr.getBGColor() != null )
      {
        bgColor = gmr.getBGColor();
      }
      String time = request.getTime();
      if( gmr != null && gmr.getTime() != null )
      {
        time = gmr.getTime();
      }
      HashMap vendorSpecificParameter = request.getVendorSpecificParameters();
      if( gmr != null && gmr.getVendorSpecificParameters() != null
          && gmr.getVendorSpecificParameters().size() > 0 )
      {
        vendorSpecificParameter = gmr.getVendorSpecificParameters();
      }
      String version = "1.1.0";
      if( gmr != null && gmr.getVersion() != null )
      {
        version = gmr.getVersion();
      }

      double[] elevation = request.getElevation();
      if( gmr != null && gmr.getElevation() != null )
      {
        elevation = gmr.getElevation();
      }
      String[] sampleDim = null;
      if( gmr != null && gmr.getSampleDimension() != null )
      {
        sampleDim = gmr.getSampleDimension();
      }

      boolean tranparency = false;
      if( gmr != null )
      {
        tranparency = gmr.getTransparency();
      }

      IDGenerator idg = IDGenerator.getInstance();
      gmr = WMSProtocolFactory.createGetMapRequest( version, "" + idg.generateUniqueID(), lys,
          elevation, sampleDim, format, request.getWidth(), request.getHeight(), request.getSrs(),
          request.getBoundingBox(), tranparency, bgColor, request.getExceptions(), time, null,
          null, vendorSpecificParameter );

      OGCWebServiceEvent event = new OGCWebServiceEvent_Impl( this, gmr, "", this );

      Debug.debugMethodEnd();
      return event;
    }

    /**
     * The method implements the <tt>OGCWebServiceClient</tt> interface. So a
     * deegree OWS implementation accessed by this class is able to return the
     * result of a request by calling the write-method.
     * 
     * @param result
     *          to a GetXXX request
     */
    public void write( Object result )
    {

      try
      {
        OGCWebServiceEvent event = (OGCWebServiceEvent)result;
        OGCWebServiceResponse res = event.getResponse();
        if( res.getException() != null )
        {
          Node message = res.getException().getElementsByTagName( "Message" ).item( 0 );
          Node location = res.getException().getElementsByTagName( "Locator" ).item( 0 );

          OGCWebServiceException exce = null;
          if( location != null )
          {
            exce = new OGCWebServiceException_Impl( "ServiceInvokerForNL: " + layer.getName()
                + "\n" + location.getFirstChild().getNodeValue(), message.getFirstChild()
                .getNodeValue() );
          }
          else
          {
            exce = new OGCWebServiceException_Impl( "ServiceInvokerForNL: " + layer.getName()
                + "\n" + "no location", message.getFirstChild().getNodeValue() );
          }

          putTheme( index, exce );
        }
        else
        {
          if( res instanceof WMSGetMapResponse )
          {
            handleGetMapResponse( (WMSGetMapResponse)res );
          }
          else if( res instanceof WFSGetFeatureResponse )
          {
            handleGetFeatureResponse( (WFSGetFeatureResponse)res );
          }
          else if( res instanceof WCSGetCoverageResponse )
          {
            handleGetCoverageResponse( (WCSGetCoverageResponse)res );
          }
          else
          {
            OGCWebServiceException exce = new OGCWebServiceException_Impl( "ServiceInvokerForNL: "
                + layer.getName(), "unknown response format!" );
            putTheme( index, exce );
          }
        }
      }
      catch( Exception e )
      {
        Debug.debugException( e, " - " );
        OGCWebServiceException exce = new OGCWebServiceException_Impl( "ServiceInvokerForNL: "
            + layer.getName(), e.toString() );
        putTheme( index, exce );
      }

      // increase counter to indicate that one more layers requesting is
      // completed
      increaseCounter();

    }

    /**
     * handles the response of a cascaded WMS and calls a factory to create
     * <tt>DisplayElement</tt> and a <tt>Theme</tt> from it
     */
    private void handleGetMapResponse( WMSGetMapResponse response ) throws Exception
    {
      Debug.debugMethodBegin();

      if( response.getException() != null )
      {
        Document doc = response.getException();
        String l = XMLTools.getStringValue( doc.getElementsByTagName( "Location" ).item( 0 ) );
        String m = XMLTools.getStringValue( doc.getElementsByTagName( "Message" ).item( 0 ) );
        putTheme( index, new OGCWebServiceException_Impl( l, m ) );
      }
      else
      {
        BufferedImage bi = (BufferedImage)response.getMap();
        GC_GridCoverage gc = new ImageGridCoverage( bi, request.getBoundingBox(), reqCRS, false );
        org.deegree.graphics.Layer rl = MapFactory.createRasterLayer( layer.getName(), gc );
        putTheme( index, MapFactory.createTheme( layer.getName(), rl ) );
      }

      Debug.debugMethodEnd();
    }

    /**
     * handles the response of a WFS and calls a factory to create
     * <tt>DisplayElement</tt> and a <tt>Theme</tt> from it
     */
    private void handleGetFeatureResponse( WFSGetFeatureResponse response ) throws Exception
    {
      Debug.debugMethodBegin();

      FeatureCollection fc = null;

      Object o = response.getResponse();
      if( o instanceof GMLFeatureCollection )
      {
        fc = FeatureFactory.createFeatureCollection( (GMLFeatureCollection)o );
      }
      else if( o instanceof FeatureCollection )
      {
        fc = (FeatureCollection)o;
      }
      else
      {
        throw new Exception( "unknown data format at a GetFeature response" );
      }

      org.deegree.graphics.Layer fl = MapFactory.createFeatureLayer( layer.getName(), reqCRS, fc );
      putTheme( index, MapFactory.createTheme( layer.getName(), fl, styles ) );

      Debug.debugMethodEnd();

    }

    /**
     * handles the response of a WCS and calls a factory to create
     * <tt>DisplayElement</tt> and a <tt>Theme</tt> from it
     */
    private void handleGetCoverageResponse( WCSGetCoverageResponse response ) throws Exception
    {
      Debug.debugMethodBegin();

      byte[] data = (byte[])response.getResponse();
      ByteArraySeekableStream bass = new ByteArraySeekableStream( data );
      RenderedOp rop = JAI.create( "stream", bass );

      GC_GridCoverage gc = new ImageGridCoverage( rop.getAsBufferedImage(), request
          .getBoundingBox(), reqCRS, false );
      org.deegree.graphics.Layer rl = MapFactory.createRasterLayer( layer.getName(), gc );

      putTheme( index, MapFactory.createTheme( layer.getName(), rl ) );
      bass.close();

      Debug.debugMethodEnd();
    }

  }

  /**
   * Inner class for accessing the data of one user layer and creating
   * <tt>DisplayElement</tt> s and a <tt>Thrme</tt> from it. The class
   * extends <tt>Thread</tt> and implements the run method, so that a parallel
   * data accessing from several layers is possible.
   * 
   * @version $Revision$
   * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
   */
  private class ServiceInvokerForUL extends Thread
  {
    private UserLayer layer = null;

    private UserStyle[] styles = null;

    private int index = 0;

    ServiceInvokerForUL( UserLayer layer, int index )
    {
      this.layer = layer;
      Style[] tmp = layer.getStyles();
      styles = new UserStyle[tmp.length];
      for( int i = 0; i < tmp.length; i++ )
      {
        styles[i] = (UserStyle)tmp[i];
      }

      this.index = index;
    }

    /**
     * overrides/implements the run-method of <tt>Thread</tt>
     */
    public void run()
    {
      Debug.debugMethodBegin();

      try
      {
        String service = layer.getRemoteOWS().getService();
        if( service.equals( RemoteOWS.WFS ) )
        {
          handleWFS();
        }
        else if( service.equals( RemoteOWS.WCS ) )
        {
          handleWCS();
        }
      }
      catch( Exception e )
      {
        Debug.debugException( e, " - " );
        OGCWebServiceException exce = new OGCWebServiceException_Impl( "ServiceInvokerForUL: "
            + layer.getName(), "Couldn't perform query!"
            + StringExtend.stackTraceToString( e.getStackTrace() ) );
        putTheme( index, exce );
        increaseCounter();
        Debug.debugMethodEnd();
        return;
      }

      Debug.debugMethodEnd();
    }

    /**
     * handles requests against a WFS
     */
    private void handleWFS() throws Exception
    {
      Debug.debugMethodBegin();

      String request = createGetFeatureRequest();
      RemoteOWS remoteOWS = layer.getRemoteOWS();
      URL url = remoteOWS.getOnlineResource();

      NetWorker nw = new NetWorker( url, request );
      InputStreamReader isr = new InputStreamReader( nw.getInputStream(), "UTF-8" );
      Document doc = XMLTools.parse( isr );
      if( doc.getDocumentElement().getNodeName().indexOf( "Exception" ) > -1 )
      {
        throw new Exception( DOMPrinter.nodeToString( doc, "UFT-8" ) );
      }
      GMLDocument gmlDoc = new GMLDocument_Impl( doc );
      FeatureCollection fc = FeatureFactory.createFeatureCollection( gmlDoc.getRoot() );
      org.deegree.graphics.Layer fl = MapFactory.createFeatureLayer( layer.getName(), reqCRS, fc );
      putTheme( index, MapFactory.createTheme( layer.getName(), fl, styles ) );
      increaseCounter();
      Debug.debugMethodEnd();
    }

    /**
     * creates a GetFeature request related to the UserLayer encapsulated in
     * this object
     */
    private String createGetFeatureRequest() throws Exception
    {
      Debug.debugMethodBegin();

      LayerFeatureConstraints lfc = layer.getLayerFeatureConstraints();
      FeatureTypeConstraint[] ftc = lfc.getFeatureTypeConstraint();

      // no filter condition has been defined
      StringBuffer sb = new StringBuffer( 5000 );
      sb.append( "<?xml version='1.0' encoding='UTF-8'?>" );
      sb.append( "<GetFeature xmlns='http://www.opengis.net/wfs' " );
      sb.append( "xmlns:ogc='http://www.opengis.net/ogc' " );
      sb.append( "xmlns:gml='http://www.opengis.net/gml' " );
      sb.append( "service='WFS' version='1.0.0' " );
      sb.append( "outputFormat='GML2'>" );
      for( int i = 0; i < ftc.length; i++ )
      {
        sb.append( "<Query typeName='" + ftc[i].getFeatureTypeName() + "'>" );
        Filter filter = ftc[i].getFilter();
        if( filter != null )
        {
          sb.append( filter.toXML() );
        }
        sb.append( "</Query>" );
      }
      sb.append( "</GetFeature>" );

      Debug.debugMethodEnd();
      return sb.toString();
    }

    /**
     * handles requests against a WCS
     */
    private void handleWCS() throws Exception
    {
      Debug.debugMethodBegin();

      RemoteOWS remoteOWS = layer.getRemoteOWS();
      URL url = remoteOWS.getOnlineResource();

      NetWorker nw = new NetWorker( url );
      MemoryCacheSeekableStream mcss = new MemoryCacheSeekableStream( nw.getInputStream() );

      RenderedOp rop = JAI.create( "stream", mcss );

      GC_GridCoverage gc = new ImageGridCoverage( rop.getAsBufferedImage(), request
          .getBoundingBox(), reqCRS, false );
      mcss.close();

      org.deegree.graphics.Layer rl = MapFactory.createRasterLayer( layer.getName(), gc );

      putTheme( index, MapFactory.createTheme( layer.getName(), rl ) );
      mcss.close();
      increaseCounter();
      Debug.debugMethodEnd();

    }
  }

}