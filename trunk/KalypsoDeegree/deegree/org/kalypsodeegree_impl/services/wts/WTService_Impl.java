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
package org.deegree_impl.services.wts;

import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ObjectInputStream;
import java.util.ArrayList;
import java.util.StringTokenizer;

import javax.media.j3d.Appearance;
import javax.media.j3d.Geometry;
import javax.media.j3d.Group;
import javax.media.j3d.Material;
import javax.media.j3d.PolygonAttributes;
import javax.media.j3d.Shape3D;
import javax.media.j3d.Texture;
import javax.media.j3d.TextureAttributes;
import javax.vecmath.Color3f;
import javax.vecmath.Point3d;

import org.deegree.gml.GMLFeature;
import org.deegree.gml.GMLFeatureCollection;
import org.deegree.gml.GMLProperty;
import org.deegree.model.feature.FeatureCollection;
import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.geometry.GM_Surface;
import org.deegree.services.OGCWebServiceClient;
import org.deegree.services.OGCWebServiceEvent;
import org.deegree.services.OGCWebServiceException;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.OGCWebServiceResponse;
import org.deegree.services.WebServiceException;
import org.deegree.services.wcs.protocol.WCSGetCoverageRequest;
import org.deegree.services.wcs.protocol.WCSGetCoverageResponse;
import org.deegree.services.wfs.protocol.WFSGetFeatureResponse;
import org.deegree.services.wms.protocol.WMSGetMapResponse;
import org.deegree.services.wts.ViewPoint;
import org.deegree.services.wts.WTSScene;
import org.deegree.services.wts.capabilities.WTSCapabilities;
import org.deegree.services.wts.configuration.WTSConfiguration;
import org.deegree.services.wts.protocol.WTSGetCapabilitiesRequest;
import org.deegree.services.wts.protocol.WTSGetViewRequest;
import org.deegree.xml.Marshallable;
import org.deegree_impl.services.OGCWebServiceEvent_Impl;
import org.deegree_impl.services.OGCWebServiceException_Impl;
import org.deegree_impl.services.OGCWebService_Impl;
import org.deegree_impl.services.wts.configuration.ConfigurationException;
import org.deegree_impl.services.wts.configuration.WTSConfiguration_Impl;
import org.deegree_impl.services.wts.protocol.WTSProtocolFactory;
import org.deegree_impl.services.wts.util.IMGtoTINArrayConverter;
import org.deegree_impl.services.wts.util.TINtoTINArrayConverter;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Document;

/**
 * @author Administrator
 * 
 * To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Generation - Code and Comments
 */
/**
 * 
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class WTService_Impl extends OGCWebService_Impl implements OGCWebServiceClient
{
  protected GM_Surface[][] boxes = null;

  private ArrayList feature = null;

  private OGCWebServiceClient client = null;

  private OGCWebServiceRequest request = null;

  private OffScreenWTSRenderer renderer = null;

  private WTSCapabilities capabilities = null;

  private Object[][] demTiles = null;

  private Object[][][] textureTiles = null;

  private int count = 0;

  /**
   * 
   * 
   * @param event
   * 
   * @throws WebServiceException
   */
  public void doService( OGCWebServiceEvent event ) throws WebServiceException
  {
    Debug.debugMethodBegin( this, "doService" );

    feature = new ArrayList( 1000 );
    request = event.getRequest();
    client = event.getDestination();

    if( request instanceof WTSGetCapabilitiesRequest )
    {
      handleGetCapabilities( (WTSGetCapabilitiesRequest)request );
    }
    else if( request instanceof WTSGetViewRequest )
    {
      try
      {
        handleGetView( (WTSGetViewRequest)request );
      }
      catch( Exception e )
      {
        // TODO
        // create error message
        System.out.println( e );
      }
    }
    else
    {
      // TODO
      // create error message
    }

    Debug.debugMethodEnd();
  }

  /**
   * returns the boxes the requested area has been splitted up to
   */
  GM_Surface[][] getBoxes()
  {
    return boxes;
  }

  synchronized void setDEMTile( Object tile, int x, int y )
  {
    demTiles[x][y] = tile;
  }

  /**
   * the method performs the handling of the passed OGCWebServiceEvent directly
   * and returns the result to the calling class/method
   * 
   * @param request
   *          request (WMS, WCS, WFS, WCAS, WCTS, WTS, Gazetter) to perform
   * 
   * @throws WebServiceException
   */
  public OGCWebServiceResponse doService( OGCWebServiceRequest request ) throws WebServiceException
  {
    Debug.debugMethodBegin();
    Debug.debugMethodEnd();
    throw new NoSuchMethodError( "doService(OGCWebServiceRequest)" );
  }

  /**
   * performs a get capabilities request by writing an XML representation of the
   * capabilities to the receiver (destination) of the response of the request.
   */
  private void handleGetCapabilities( WTSGetCapabilitiesRequest request )
  {
    Debug.debugMethodBegin();
    client.write( capabilities.exportAsXML() );
    Debug.debugMethodEnd();
  }

  /**
   * performs a get view request by initializing a <tt>WTSScene</tt>, forcing
   * an offscreen rendering of the scene and writing the resulting image
   * encapsulated into a <tt>GetViewResponse</tt> object to the receiver of
   * the response.
   */
  private void handleGetView( WTSGetViewRequest request ) throws Exception
  {
    Debug.debugMethodBegin();

    // create viewpoint object
    ViewPoint vp = createViewPoint( request );

    // split the footprint into several boxes that can be handled independly
    // to retrieve higher terrain resolutions for the boxes in front ...
    Splitter sp = new Splitter( request, vp );
    GM_Surface[] sur = sp.makeStripes( 6 );
    boxes = new GM_Surface[sur.length][];

    for( int i = 0; i < sur.length; i++ )
    {
      boxes[i] = sp.makeBoxes( sur[i], 2 );
    }

    int textCnt = request.getLayers().length;

    textureTiles = new Object[textCnt][boxes.length][boxes[0].length];

    demTiles = new Object[boxes.length][boxes[0].length];

    count = request.getElevationModels().length;
    count += request.getLayers().length;
    count *= ( boxes.length * boxes[0].length );

    if( ( request.getFeatureCollections() != null )
        && ( request.getFeatureCollections().length > 0 ) )
    {
      count++;
    }

    // perform dem requests in a seperate thread
    DEMLoader dl = new DEMLoader( request, this );
    dl.start();

    for( int i = 0; i < textCnt; i++ )
    {
      // perform texture requests in a seperate thread
      TextureLoader tl = new TextureLoader( request, i, this );
      tl.start();
    }

    // perform request for getting features superimposed onto the dem
    // in a seperate thread
    FeatureLoader fl = new FeatureLoader( request, this );
    fl.start();

    Debug.debugMethodEnd();
  }

  /**
   * creates a <tt>ViewPoint</tt> from a <tt>WTSGetViewRequest</tt>
   */
  private static ViewPoint createViewPoint( WTSGetViewRequest request )
  {
    Debug.debugMethodBegin( "WTSService_Impl", "handleGetView" );

    GM_Position poi = request.getPointOfInterest();
    Point3d poi_ = new Point3d( poi.getX(), poi.getY(), poi.getZ() );

    ViewPoint vp = new ViewPoint_Impl( request.getYAW(), request.getPitch(), request.getDistance(),
        poi_, request.getAOV() );

    Debug.debugMethodEnd();
    return vp;
  }

  /**
   * receives the results from the service calls and fills the texture and tiles
   * matrices
   * 
   * @param result
   *          result of a service call
   */
  public synchronized void write( Object result )
  {
    Debug.debugMethodBegin();

    count--;

    if( result instanceof String )
    {
      handleException( (String)result );
    }
    else
    {
      OGCWebServiceResponse response = null;

      if( result instanceof OGCWebServiceEvent )
      {
        response = ( (OGCWebServiceEvent)result ).getResponse();
      }
      else
      {
        response = (OGCWebServiceResponse)result;
      }

      Document doc = response.getException();
      if( doc == null )
      {
        try
        {
          if( response instanceof WCSGetCoverageResponse )
          {
            // handle WCS response; could be dem or texture
            handleWCSResponse( (WCSGetCoverageResponse)response );
          }
          else if( response instanceof WFSGetFeatureResponse )
          {
            // handle WFS response; could be dem or feature superimpose
            // on the dem
            handleWFSResponse( (WFSGetFeatureResponse)response );
          }
          else if( response instanceof WMSGetMapResponse )
          {
            // handle WMS response; could onl be texture
            handleWMSResponse( (WMSGetMapResponse)response );
          }
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
      }
      else
      {
        handleException( doc );
      }
    }

    // if count == 0 all sub-requests has been performed
    if( count == 0 )
    {
      forceGetViewResponseToClient();
    }

    Debug.debugMethodEnd();
  }

  /**
   * handles the case when the result returned to the <tt>WTService</tt>
   * through the write-method contains a error message string
   * 
   * @param error
   *          error message
   */
  protected void handleException( String error )
  {
    Debug.debugMethodBegin();
    Debug.debugMethodEnd();
  }

  /**
   * handles the case when the result returned to the <tt>WTService</tt>
   * through the write-method contains a error message DOM object
   * 
   * @param error
   *          error message containing Document
   */
  protected void handleException( Document error )
  {
    Debug.debugMethodBegin();
    Debug.debugMethodEnd();
  }

  /**
   * handles the response(s) from a WCS
   * 
   * @param response
   *          response to a GetCoverage request
   */
  private void handleWCSResponse( WCSGetCoverageResponse response ) throws Exception
  {
    Debug.debugMethodBegin();

    WCSGetCoverageRequest req = (WCSGetCoverageRequest)response.getRequest();
    String id = req.getId();
    // extracs the index position of the result from its ID
    int[] index = getIndexFromId( id );

    if( req.getFormat().equalsIgnoreCase( "IMG" ) )
    {
      // it's a dem tile
      ByteArrayInputStream bis = new ByteArrayInputStream( (byte[])response.getResponse() );
      ObjectInputStream ois = new ObjectInputStream( bis );
      float[][] values = (float[][])ois.readObject();

      if( ( values.length > 0 ) && ( values[0].length > 0 ) )
      {
        Geometry geom = IMGtoTINArrayConverter.convertToArray( values, true,
            boxes[index[0]][index[1]], (float)( (WTSGetViewRequest)request ).getScale() );
        Shape3D shape = new Shape3D();
        shape.setCapability( Shape3D.ALLOW_GEOMETRY_WRITE );
        shape.setGeometry( geom );
        demTiles[index[0]][index[1]] = shape;
      }
      else
      {
        demTiles[index[0]][index[1]] = null;
      }
    }
    else if( req.getFormat().equalsIgnoreCase( "GIF" ) || req.getFormat().equalsIgnoreCase( "PNG" )
        || req.getFormat().equalsIgnoreCase( "JPG" ) || req.getFormat().equalsIgnoreCase( "JPEG" )
        || req.getFormat().equalsIgnoreCase( "TIF" ) || req.getFormat().equalsIgnoreCase( "TIFF" )
        || req.getFormat().equalsIgnoreCase( "JAVAIMAGE" )
        || req.getFormat().equalsIgnoreCase( "BMP" ) )
    {
      // it's a texture tile
      try
      {
        //                SeekableStream sst = new ByteArraySeekableStream(
        // (byte[])response.getResponse() );
        //                RenderedOp rop = JAI.create( "stream", sst );
        //                textureTiles[index[0]][index[1]][index[2]] =
        // rop.getAsBufferedImage();
        textureTiles[index[0]][index[1]][index[2]] = response.getResponse();
        //                sst.close();
      }
      catch( Exception e )
      {
        textureTiles[index[0]][index[1]][index[2]] = new BufferedImage( 2, 2,
            BufferedImage.TYPE_INT_ARGB );
        System.out.println( e );
      }
    }

    Debug.debugMethodEnd();
  }

  /**
   * handles the response(s) from a WFS
   * 
   * @param response
   *          response to a GetFeature request
   */
  private void handleWFSResponse( WFSGetFeatureResponse response ) throws Exception
  {
    Debug.debugMethodBegin();

    double scale = ( (WTSGetViewRequest)request ).getScale();

    if( response.getException() != null )
    {
      handleException( response.getException() );
    }
    else
    {
      Object o = response.getResponse();
      String reqID = response.getRequest().getId();
      if( reqID.startsWith( "TIN" ) )
      {
        // extracs the index position of the result from its ID
        int[] index = getIndexFromId( reqID.substring( 3 ) );
        handleWFSTINs( (FeatureCollection)o, scale, index );
      }
      else if( reqID.startsWith( "ENTITY" ) )
      {
        handleWFSEntities( (GMLFeatureCollection)o, scale );
      }
    }

    Debug.debugMethodEnd();
  }

  /**
   * handles the response to a WFS GetFeature request containing buildings
   * 
   * @param gmlFc
   *          feature collection; each feature represents a building
   * @param scale
   *          scale of the buildings (1 = original scale)
   * @throws ConfigurationException
   */
  private void handleWFSEntities( GMLFeatureCollection gmlFc, double scale )
      throws ConfigurationException
  {
    Debug.debugMethodBegin();

    WTSConfiguration conf = WTSConfiguration_Impl.getInstance();

    double x = 0;
    double y = 0;
    double width = 0;
    double depth = 0;
    double height = 0;
    double direction = 0;
    double baseHeight = 0;

    GMLFeature[] feat = gmlFc.getFeatures();

    for( int i = 0; i < feat.length; i++ )
    {
      GMLProperty prop = feat[i].getProperty( "TAB_BUILDING.X" );
      x = Double.parseDouble( prop.getPropertyValue().toString() );
      prop = feat[i].getProperty( "TAB_BUILDING.Y" );
      y = Double.parseDouble( prop.getPropertyValue().toString() );
      prop = feat[i].getProperty( "TAB_BUILDING.WIDTH" );
      width = Double.parseDouble( prop.getPropertyValue().toString() );
      prop = feat[i].getProperty( "TAB_BUILDING.DEPTH" );
      depth = Double.parseDouble( prop.getPropertyValue().toString() );
      prop = feat[i].getProperty( "TAB_BUILDING.HEIGHT" );
      height = Double.parseDouble( prop.getPropertyValue().toString() );
      prop = feat[i].getProperty( "TAB_BUILDING.DIRECTION" );
      direction = Double.parseDouble( prop.getPropertyValue().toString() );
      prop = feat[i].getProperty( "TAB_BUILDING.BASE_HEIGHT" );
      baseHeight = Double.parseDouble( prop.getPropertyValue().toString() );
      prop = feat[i].getProperty( "TAB_BUILDING.NAME" );
      String name = prop.getPropertyValue().toString();
      prop = feat[i].getProperty( "TAB_BUILDING.TEXTURE" );
      String textureSource = prop.getPropertyValue().toString();
      BufferedImage bi = conf.getFeatureTexture( textureSource );

      if( bi == null )
      {
        bi = conf.getFeatureTexture( "DEFAULT" );
      }

      Group block = new Block( (int)-x, (int)y, (int)width / 2, (int)depth / 2,
          (int)( baseHeight * scale ), (int)( height * scale ), (int)direction, bi );
      feature.add( block );
    }

    Debug.debugMethodEnd();
  }

  /**
   * handles the response to a WFS GetFeature request containing DEM as TINs
   * 
   * @param fc
   *          feature collection containing the TINs
   * @param scale
   *          scale of the TINs (1 = original scale)
   * @param index
   *          index of the tiles the result will be assigned to
   * @throws ConfigurationException
   */
  private void handleWFSTINs( FeatureCollection fc, double scale, int[] index )
      throws ConfigurationException, GM_Exception
  {
    Debug.debugMethodBegin();
    Geometry geom = null;
    try
    {
      geom = TINtoTINArrayConverter.convertToArray( fc, true, (float)scale );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    Shape3D shape = new Shape3D();
    shape.setCapability( Shape3D.ALLOW_GEOMETRY_WRITE );
    shape.setGeometry( geom );
    demTiles[index[0]][index[1]] = shape;

    Debug.debugMethodEnd();
  }

  /**
   * handles the response(s) from a WMS
   * 
   * @param response
   *          response to a GetMap request
   */
  private void handleWMSResponse( WMSGetMapResponse response )
  {
    Debug.debugMethodBegin();
    Debug.debugMethodEnd();
  }

  /**
   * 
   * 
   * @param id
   * 
   * @return
   */
  private int[] getIndexFromId( String id )
  {
    StringTokenizer st = new StringTokenizer( id, "-" );
    int[] res = new int[st.countTokens()];

    for( int i = 0; i < res.length; i++ )
    {
      res[i] = Integer.parseInt( st.nextToken() );
    }

    return res;
  }

  /**
   * creates a response object and calls the write-method of the
   * <tt>OGCWebServiceClient</tt> that is registered as receiver of the
   * WTS-request.
   */
  private void forceGetViewResponseToClient()
  {
    Debug.debugMethodBegin( this, "forceResponseToClient" );

    WTSGetViewRequest req = (WTSGetViewRequest)request;
    OGCWebServiceResponse response = null;

    try
    {
      ArrayList list = new ArrayList( 100 );

      // assign appearence/texture to the shapes
      for( int i = 0; i < boxes.length; i++ )
      {
        for( int j = 0; j < boxes[0].length; j++ )
        {
          if( demTiles[i][j] != null )
          {
            if( textureTiles.length > 1 )
            {
              if( textureTiles[0][i][j] != null )
              {
                Graphics g = ( (BufferedImage)textureTiles[0][i][j] ).getGraphics();

                for( int k = 1; k < textureTiles.length; k++ )
                {
                  if( textureTiles[k][i][j] != null )
                  {
                    g.drawImage( (BufferedImage)textureTiles[k][i][j], 0, 0, null );
                  }
                }

                g.dispose();
              }
              else
              {
                textureTiles[0][i][j] = new BufferedImage( 2, 2, BufferedImage.TYPE_INT_ARGB );
              }
            }

            Appearance app = createAppearance( (BufferedImage)textureTiles[0][i][j] );
            ( (Shape3D)demTiles[i][j] ).setAppearance( app );
            list.add( demTiles[i][j] );
            demTiles[i][j] = null;

            for( int k = 0; k < textureTiles.length; k++ )
            {
              //textureTiles[k][i][j] = null;
            }
          }
        }
      }

      Shape3D[] shapes = (Shape3D[])list.toArray( new Shape3D[list.size()] );
      Group[] blocks = (Group[])feature.toArray( new Group[feature.size()] );

      ViewPoint vp = createViewPoint( req );

      // create 3D scene
      WTSScene scene = WTSFactory.createWTSScene( shapes, blocks, vp, req.getDate(), null, null,
          req.getBackground() );

      if( renderer == null )
      {
        renderer = new OffScreenWTSRenderer( scene );
      }

      renderer.setWidth( req.getWidth() );
      renderer.setHeight( req.getHeight() );
      renderer.setScene( scene );
      BufferedImage image = (BufferedImage)renderer.renderScene();

      //renderer = null;
      // create response object with rendered scene
      response = WTSProtocolFactory.createGetViewResponse( image, req );
      System.gc();
    }
    catch( Exception e )
    {
      e.printStackTrace();
      // create response object with error message
      OGCWebServiceException exc = new OGCWebServiceException_Impl( this.getClass().getName(), e
          .toString() );
      response = WTSProtocolFactory
          .createGetViewResponse( ( (Marshallable)exc ).exportAsXML(), req );
    }

    // create event and write it back to the client
    OGCWebServiceEvent event = new OGCWebServiceEvent_Impl( this, response, "" );
    client.write( event );

    Debug.debugMethodEnd();
  }

  /**
   * creates the appearence for the scenes material/dem
   */
  private Appearance createAppearance( BufferedImage image )
  {
    Material material = new Material();

    Color3f white = new Color3f( 0.7f, 0.7f, 0.7f );

    //The Material object defines the appearance of an object under
    // illumination.
    //If the Material object in an Appearance object is null, lighting is
    // disabled for all nodes
    //  that use that Appearance object.
    material.setAmbientColor( white );
    material.setDiffuseColor( white );
    material.setSpecularColor( white );
    material.setShininess( 1f );
    material.setLightingEnable( true );
    material.setEmissiveColor( 0.2f, 0.2f, 0.2f );

    Texture texture = new com.sun.j3d.utils.image.TextureLoader( image ).getTexture();

    //The Appearance object defines all rendering state that can be set as a
    // component object of a Shape3D node.
    Appearance app = new Appearance();
    app.setTexture( texture );
    app.setMaterial( material );

    //ALLOW_TEXTURE_WRITE: Specifies that this Appearance object allows writing
    // its texture component information.
    app.setCapability( Appearance.ALLOW_TEXTURE_WRITE );
    //TextureAttributes object defines attributes that apply to texture
    // mapping.
    TextureAttributes texAttr = new TextureAttributes();

    //MODULATE: Modulate the object color with the texture color.
    texAttr.setTextureMode( TextureAttributes.MODULATE );
    app.setTextureAttributes( texAttr );

    PolygonAttributes pa = new PolygonAttributes();
    pa.setBackFaceNormalFlip( true );

    //pa.setPolygonMode( PolygonAttributes.POLYGON_LINE );
    app.setPolygonAttributes( pa );

    return app;
  }

}