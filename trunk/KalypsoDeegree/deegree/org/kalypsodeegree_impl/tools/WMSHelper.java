package org.kalypsodeegree_impl.tools;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.media.jai.PlanarImage;
import javax.media.jai.TiledImage;

import org.deegree.services.wms.capabilities.Layer;
import org.deegree.services.wms.capabilities.LayerBoundingBox;
import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.coverage.GridRange;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Ring;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.kalypsodeegree_impl.model.ct.GeoTransformer;
import org.kalypsodeegree_impl.model.cv.GridRange_Impl;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/

/**
 * This class contains helper methods to handle wms specific tasks.
 * 
 * @author c.kuepferle@tuhh.de
 *  
 */
public class WMSHelper
{
  /**
   * This method tries to find a common spatial reference system (srs) for a
   * given set of layers. If all layers coorespond to the local crs the local
   * crs is returned, otherwise the srs of the top layer is returned and the
   * client must choose one to transform it to the local coordiante system
   * 
   * @param localCRS
   *          the local spatial reference system
   * @param capabilities
   *          the capabilites document of the web map service
   * @param layerNames
   *          the layers that have to be matched to the local srs
   * @return result an array of possible coordiante systems
   */
  public static CS_CoordinateSystem[] negotiateCRS( CS_CoordinateSystem localCRS,
      WMSCapabilities capabilities, String[] layerNames ) throws Exception
  {
    Layer topLayer = capabilities.getCapability().getLayer();
    CS_CoordinateSystem crs = matchCrs( topLayer, layerNames, localCRS );
    if( crs != null )
      return new CS_CoordinateSystem[]
      {
        localCRS
      };
    //get crs from top layer
    String[] topLayerSRS = topLayer.getSrs();
    List result = new ArrayList();
    try
    //try to create all coordinate systems
    {
      for( int i = 0; i < topLayerSRS.length; i++ )
      {
        result.add( ConvenienceCSFactory.getInstance().getOGCCSByName( topLayerSRS[i] ) );
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return null;
    }
    return (CS_CoordinateSystem[])result.toArray( new CS_CoordinateSystem[result.size()] );
  }

  /**
   * This method tries to match the local coordiante system to a given layer
   * selection.
   * 
   * @param topLayer
   *          the top layer of the layer structur of a web map service
   * @param layerSelection
   *          layers to be matched
   * @param localCRS
   *          the local coordinate system
   * @return returns null if one element of the layers to be matched is not
   *         available in the local coordinate system, otherwise it returns the
   *         local crs
   *  
   */

  private static CS_CoordinateSystem matchCrs( Layer topLayer, String[] layerSelection,
      CS_CoordinateSystem localCRS ) throws Exception
  {
    HashSet collector = new HashSet();

    collect( collector, topLayer, layerSelection );
    for( Iterator iter = collector.iterator(); iter.hasNext(); )
    {
      Layer layer = (Layer)iter.next();
      String[] layerSRS = layer.getSrs();
      if( contains( layerSRS, localCRS.getName() ) )
        continue;
      return null;

    }
    return localCRS;
  }

  /**
   * This method collects all layers from a capabilites document.
   * 
   * @param capabilites
   *          wms capabilites document
   * @param set
   *          the Set where the layers are collected in
   */
  public static void getAllLayers( WMSCapabilities capabilites, Set set )
  {
    try
    {
      Layer topLayer = capabilites.getCapability().getLayer();
      collect( set, topLayer, null );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * This method collects all layers (or the specified layers) from the top
   * layer of a WMSCapabilites document. If the parameter layerSeletion is empty
   * or null the method collects all layers, otherwise returns all layers with
   * the same name as in the layerSelection.
   * 
   * @param collector
   *          The set that collects the layers found.
   * @param layer
   *          the top layer of the wms capabilites document.
   * @param layerSelection
   *          an array of layer names to search for.
   */
  private static void collect( Set collector, Layer layer, String[] layerSelection )
  {

    Layer[] layerTree = layer.getLayer();
    for( int i = 0; i < layerTree.length; i++ )
    {
      Layer newLayer = layerTree[i];//.getLayer();
      if( newLayer.getLayer().length > 0 )
      {
        //recursive function call
        collect( collector, newLayer, layerSelection );
      }
      else
      {
        //System.out.println( layerTree[i].getName() );
        if( layerSelection != null )
        {

          if( contains( layerSelection, layerTree[i].getName() ) )
          {
            collector.add( layerTree[i] );
          }
        }
        else
          collector.add( layerTree[i] );
      }
      continue;
    }
  }

  /**
   * This method checks an array of Strings for a given String to match.
   * 
   * @param array
   *          strings to check for a match.
   * @param toMatch
   *          the string to match
   * @return boolean true if the String is the array, false otherwise
   */

  public static boolean contains( String[] array, String toMatch )
  {
    for( int i = 0; i < array.length; i++ )
    {
      if( array[i].equals( toMatch ) )
        return true;
    }
    return false;
  }

  /**
   * This method gets the max bounding box of a wms layer.
   * 
   * @param layers
   *          the layers in the map in an array
   *  
   */
  public static GM_Envelope getMaxExtend( String[] layers, WMSCapabilities capabilites,
      CS_CoordinateSystem srs ) throws Exception
  {
    Layer topLayer = capabilites.getCapability().getLayer();
    HashSet collector = new HashSet();
    collect( collector, topLayer, layers );
    GM_Envelope maxEnvelope = null;
    int counter = 0;
    for( Iterator iter = collector.iterator(); iter.hasNext(); )
    {
      Layer layer = (Layer)iter.next();
      LayerBoundingBox[] bbox = layer.getBoundingBox();
      for( int i = 0; i < bbox.length; i++ )
      {
        LayerBoundingBox env = bbox[i];
        if( env.getSRS().equals( srs.getName() ) )
        {
          //convert deegree Envelope to kalypsodeegree Envelope
          GM_Envelope kalypsoEnv = GeometryFactory.createGM_Envelope( env.getMin().getX(), env
              .getMin().getY(), env.getMax().getX(), env.getMax().getY() );
          if( counter < 1 )
            maxEnvelope = kalypsoEnv;
          else
          {
            if( !maxEnvelope.contains( kalypsoEnv ) )
            {
              GM_Envelope temp = maxEnvelope.getMerged( kalypsoEnv );
              maxEnvelope = temp;
            }//if !maxEnvelope
          }//else
        }//if
      }//for bbox
      counter++;
    }//for iter
    if( maxEnvelope != null )
      return maxEnvelope;
    org.deegree.model.geometry.GM_Envelope topLayerEnv = topLayer.getLatLonBoundingBox();
    return GeometryFactory.createGM_Envelope( topLayerEnv.getMin().getX(), topLayerEnv.getMin()
        .getY(), topLayerEnv.getMax().getX(), topLayerEnv.getMax().getY() );
  }

  /**
   * 
   * @param g2
   *          empty Graphics context
   * @param projection
   *          World to screen projection (passed from MapPanel)
   * @param rasterImage
   *          image from server
   * @param gridDomain
   *          image from server with geospatial ( real world ) context. CS from
   *          server and Envelope from server (all layers)
   * @param targetCS
   *          target coodriate system (local CS from client)
   */
  private static void internalTransformation( Graphics2D g2, GeoTransform projection,
      TiledImage rasterImage, RectifiedGridDomain gridDomain, CS_CoordinateSystem targetCS )
      throws Exception
  {

    //  get the Screen extent in real world coordiantes
    GM_Envelope sourceScreenRect = projection.getSourceRect();
    // create a surface and transform it in the coordinate system of the
    GM_Surface destScreenSurface = null;
    GM_Surface sourceScreenSurface = GeometryFactory.createGM_Surface( sourceScreenRect, targetCS );

    if( !targetCS.equals( gridDomain.getOrigin( null ).getCoordinateSystem() ) )
    {
      GeoTransformer geoTrans1 = new GeoTransformer( gridDomain.getOrigin( null )
          .getCoordinateSystem() );
      destScreenSurface = (GM_Surface)geoTrans1.transform( sourceScreenSurface );
    }
    else
      destScreenSurface = sourceScreenSurface;
    // get the gridExtent for the envelope of the surface
    int[] gridExtent = gridDomain.getGridExtent( destScreenSurface.getEnvelope(), gridDomain
        .getOrigin( null ).getCoordinateSystem() );
    int lowX = gridExtent[0];
    int lowY = gridExtent[1];
    int highX = gridExtent[2];
    int highY = gridExtent[3];

    // calculate imageExtent from gridExtent
    int minX = lowX;
    int minY = rasterImage.getHeight() - highY;
    int width = highX - lowX;
    int height = highY - lowY;
    // get the required subImage according to the gridExtent
    PlanarImage image = rasterImage.getSubImage( minX, minY, width, height );

    // get the destinationSurface in target coordinates
    GM_Surface destSurface = gridDomain.getGM_Surface( lowX, lowY, highX, highY, targetCS );
    GM_Ring destExtRing = destSurface.getSurfaceBoundary().getExteriorRing();
    GM_Position llCorner = destExtRing.getPositions()[0];
    GM_Position lrCorner = destExtRing.getPositions()[1];
    GM_Position urCorner = destExtRing.getPositions()[2];
    GM_Position ulCorner = destExtRing.getPositions()[3];
    // calculate the Corners in screen coordinates
    GM_Position pixel_llCorner = projection.getDestPoint( llCorner );
    GM_Position pixel_lrCorner = projection.getDestPoint( lrCorner );
    GM_Position pixel_urCorner = projection.getDestPoint( urCorner );
    GM_Position pixel_ulCorner = projection.getDestPoint( ulCorner );
    // calculate the height and width of the image on screen
    double destImageHeight = pixel_llCorner.getY() - pixel_ulCorner.getY();
    double destImageWidth = pixel_lrCorner.getX() - pixel_llCorner.getX();
    // calculate the scaling factors for the transformation
    double scaleX = destImageWidth / image.getWidth();
    double scaleY = destImageHeight / image.getHeight();
    // calculate the shear parameters for the transformation
    double shearX = pixel_llCorner.getX() - pixel_ulCorner.getX();
    double shearY = pixel_lrCorner.getY() - pixel_llCorner.getY();

    GM_Surface orgDestSurface = gridDomain.getGM_Surface( targetCS );
    GM_Position orgULCorner = orgDestSurface.getSurfaceBoundary().getExteriorRing().getPositions()[3];
    GM_Position pixel_orgULCorner = projection.getDestPoint( orgULCorner );

    AffineTransform trafo = new AffineTransform();
    // translate the image, so that the subImage is at the right position
    trafo.translate( pixel_orgULCorner.getX() - pixel_ulCorner.getX(), pixel_orgULCorner.getY()
        - pixel_ulCorner.getY() );
    // scale the image
    trafo.scale( scaleX, scaleY );
    // translate the image to compensate the shearing
    trafo.translate( Math.abs( shearX ) / Math.abs( scaleX ), Math.abs( shearY )
        / Math.abs( scaleY ) );
    // shear the image
    trafo.shear( shearX / destImageHeight, shearY / destImageWidth );

    // calculate the required extent of the bufferedImage
    GM_Position scaledImage_min = pixel_ulCorner;
    GM_Position scaledImage_max = GeometryFactory.createGM_Position( pixel_urCorner.getX(),
        pixel_llCorner.getY() );

    GM_Position buffImage_min = GeometryFactory.createGM_Position( scaledImage_min.getX()
        - Math.abs( shearX ), scaledImage_min.getY() - Math.abs( shearY ) );
    GM_Position buffImage_max = GeometryFactory.createGM_Position( scaledImage_max.getX()
        + Math.abs( shearX ), scaledImage_max.getY() + Math.abs( shearY ) );
    GM_Envelope buffImageEnv = GeometryFactory.createGM_Envelope( buffImage_min, buffImage_max );

    BufferedImage buffer = new BufferedImage( (int)buffImageEnv.getWidth(), (int)buffImageEnv
        .getHeight(), BufferedImage.TYPE_INT_ARGB );
    Graphics2D bufferGraphics = (Graphics2D)buffer.getGraphics();
    //bufferGraphics.setColor(Color.GREEN);
    // draw a transparent backround on the bufferedImage
    bufferGraphics.setColor( new Color( 255, 255, 255, 0 ) );
    bufferGraphics.fillRect( 0, 0, (int)buffImageEnv.getWidth(), (int)buffImageEnv.getHeight() );
    // draw the image with the given transformation
    bufferGraphics.drawRenderedImage( image, trafo );
    // draw bufferedImage on the screen
    g2.drawImage( buffer, (int)buffImageEnv.getMin().getX(),
     (int)buffImageEnv.getMin().getY(),
            null );
    //g2.drawImage( buffer, 0, 0, null );
  }

  public static GM_Envelope getTransformedEnvelope( GM_Envelope serverEnv,
      CS_CoordinateSystem serverCRS, CS_CoordinateSystem local )
  {
    try
    {
      GeoTransformer gt = new GeoTransformer( local );
      return gt.transformEnvelope( serverEnv, serverCRS );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  /**
   * This method transformes an image from a source to a target coordiante
   * system and paints it on the submitted graphic context g.
   * 
   * @param remoteMap
   *          image to be transformed
   * @param env
   *          bounding box of the remoteMap
   * @param localCSR
   *          target coodrdiante system
   * @param remoteCSR
   *          source coordiante system
   * @param worldToScreenTransformation
   *          transformation from target coordiante system to pixel unites
   * @param g
   *          graphics context to draw the transformed image to
   */

  public static void transformImage( Image remoteMap, GM_Envelope env,
      CS_CoordinateSystem localCSR, CS_CoordinateSystem remoteCSR,
      GeoTransform worldToScreenTransformation, Graphics g )
  {
    System.out.println("env: "+env);
    int height = remoteMap.getHeight( null );
    int width = remoteMap.getWidth( null );

    double[] offset = new double[]
    {
        ( env.getMax().getX() - env.getMin().getX() ) / width,
        ( env.getMax().getY() - env.getMin().getY() ) / height
    };

    GridRange range = new GridRange_Impl( new double[]
    {
        0,
        0
    }, new double[]
    {
        width,
        height
    } );

    try
    {
      RectifiedGridDomain gridDomain = new RectifiedGridDomain( GeometryFactory.createGM_Point( env.getMin().getX(), env
          .getMin().getY(), remoteCSR ), offset, range );
      
      TiledImage ti = new TiledImage( PlanarImage.wrapRenderedImage( (RenderedImage)remoteMap ), false );
      
      internalTransformation( (Graphics2D)g, worldToScreenTransformation, ti,
          gridDomain, localCSR );
      
      

    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  private static void internalTransformation2( Graphics2D g2, GeoTransform projection,
      TiledImage rasterImage, RectifiedGridDomain gridDomain, CS_CoordinateSystem targetCS )
  {

    try
    {
      PlanarImage image = rasterImage;

      RectifiedGridDomain rgDomain = gridDomain;

      CS_CoordinateSystem cs = targetCS;
      GM_Surface destSurface = rgDomain.getGM_Surface( cs );
      GM_Ring destExtRing = destSurface.getSurfaceBoundary().getExteriorRing();
      GM_Position llCorner = destExtRing.getPositions()[0];
      GM_Position lrCorner = destExtRing.getPositions()[1];
      GM_Position urCorner = destExtRing.getPositions()[2];
      GM_Position ulCorner = destExtRing.getPositions()[3];
      GM_Position pixel_llCorner = projection.getDestPoint( llCorner );
      GM_Position pixel_lrCorner = projection.getDestPoint( lrCorner );
      GM_Position pixel_urCorner = projection.getDestPoint( urCorner );
      GM_Position pixel_ulCorner = projection.getDestPoint( ulCorner );
      double destImageHeight = pixel_llCorner.getY() - pixel_ulCorner.getY();
      double destImageWidth = pixel_lrCorner.getX() - pixel_llCorner.getX();
      double scaleX = destImageWidth / image.getWidth();
      double scaleY = destImageHeight / image.getHeight();
      double shearX = pixel_llCorner.getX() - pixel_ulCorner.getX();
      double shearY = pixel_lrCorner.getY() - pixel_llCorner.getY();
      AffineTransform trafo = new AffineTransform();
      trafo.scale( scaleX, scaleY );
      trafo.translate( Math.abs( shearX ) / Math.abs( scaleX ), Math.abs( shearY )
          / Math.abs( scaleY ) );
      trafo.shear( shearX / destImageHeight, shearY / destImageWidth );

      GM_Position scaledImage_min = pixel_ulCorner;
      GM_Position scaledImage_max = GeometryFactory.createGM_Position( pixel_urCorner.getX(),
          pixel_llCorner.getY() );

      GM_Position buffImage_min = GeometryFactory.createGM_Position( scaledImage_min.getX()
          - Math.abs( shearX ), scaledImage_min.getY() - Math.abs( shearY ) );
      GM_Position buffImage_max = GeometryFactory.createGM_Position( scaledImage_max.getX()
          + Math.abs( shearX ), scaledImage_max.getY() + Math.abs( shearY ) );
      GM_Envelope buffImageEnv = GeometryFactory.createGM_Envelope( buffImage_min, buffImage_max );

      BufferedImage buffer = new BufferedImage( (int)buffImageEnv.getWidth(), (int)buffImageEnv
          .getHeight(), BufferedImage.TYPE_INT_ARGB );
      Graphics2D bufferGraphics = (Graphics2D)buffer.getGraphics();
      //bufferGraphics.setColor(Color.GREEN);
      bufferGraphics.setColor( new Color( 255, 255, 255, 0 ) );
      bufferGraphics.fillRect( 0, 0, (int)buffImageEnv.getWidth(), (int)buffImageEnv.getHeight() );
      bufferGraphics.drawRenderedImage( image, trafo );
      //      g2.drawImage( buffer, (int)buffImageEnv.getMin().getX(),
      // (int)buffImageEnv.getMin().getY(),
      //          null );
      g2.drawImage( buffer, 0, 0, null );
    }

    catch( Exception e )
    {
      // TODO: handle exception
    }

  }

}//class WMSHelper
