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
package org.kalypsodeegree_impl.tools;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;

import javax.media.jai.PlanarImage;
import javax.media.jai.TiledImage;

import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.coverage.GridRange;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Ring;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain.OffsetVector;
import org.kalypsodeegree_impl.model.ct.GeoTransformer;
import org.kalypsodeegree_impl.model.cv.GridRange_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * This class provides functions for transformation purposes.
 * 
 * @author Holger Albert
 */
public class TransformationUtilities
{
  /**
   * The constructor.
   */
  private TransformationUtilities( )
  {
  }

  /**
   * This method transforms an image from a source to a target coordinate system and paints it on the submitted graphic
   * context g.
   * 
   * @param remoteImage
   *            Image to be transformed.
   * @param env
   *            Bounding box of the remoteMap.
   * @param localCSR
   *            Target coordinate system.
   * @param remoteCSR
   *            Source coordinate system.
   * @param worldToScreenTransformation
   *            Transformation from target coordiante system to pixel unites.
   * @param g
   *            Graphics context to draw the transformed image to.
   * @throws Exception
   */

  public static void transformImage( TiledImage remoteImage, GM_Envelope env, CS_CoordinateSystem localCSR, CS_CoordinateSystem remoteCSR, GeoTransform worldToScreenTransformation, Graphics g ) throws Exception
  {
    int height = remoteImage.getHeight();
    int width = remoteImage.getWidth();

    OffsetVector offsetX = new OffsetVector( (env.getMax().getX() - env.getMin().getX()) / width, 0.0 );
    OffsetVector offsetY = new OffsetVector( 0.0, (env.getMax().getY() - env.getMin().getY()) / height );

    GridRange range = new GridRange_Impl( new double[] { 0, 0 }, new double[] { width, height } );

    GM_Point origin = GeometryFactory.createGM_Point( env.getMin().getX(), env.getMin().getY(), remoteCSR );
    RectifiedGridDomain gridDomain = new RectifiedGridDomain( origin, offsetX, offsetY, range );

    internalTransformation( (Graphics2D) g, worldToScreenTransformation, remoteImage, gridDomain, localCSR );
  }

  /**
   * Transforms.
   * 
   * @param g2d
   *            Empty graphics context.
   * @param projection
   *            World to screen projection (passed from MapPanel).
   * @param rasterImage
   *            Image from server.
   * @param gridDomain
   *            Image domain from server with geospatial ( real world ) context. CS from server and Envelope from server
   *            (all layers).
   * @param targetCS
   *            Target coordinate system (local CS from client).
   */
  private static void internalTransformation( Graphics2D g2d, GeoTransform projection, TiledImage rasterImage, RectifiedGridDomain gridDomain, CS_CoordinateSystem targetCS ) throws Exception
  {
    /* Get the Screen extent in real world coordiantes. */
    GM_Envelope sourceScreenRect = projection.getSourceRect();

    /* Create a surface and transform it in the coordinate system of the. */
    GM_Surface< ? > sourceScreenSurface = GeometryFactory.createGM_Surface( sourceScreenRect, targetCS );

    GM_Surface< ? > destScreenSurface;
    if( !targetCS.equals( gridDomain.getOrigin( null ).getCoordinateSystem() ) )
    {
      GeoTransformer geoTrans1 = new GeoTransformer( gridDomain.getOrigin( null ).getCoordinateSystem() );
      destScreenSurface = (GM_Surface< ? >) geoTrans1.transform( sourceScreenSurface );
    }
    else
      destScreenSurface = sourceScreenSurface;

    /* Get the gridExtent for the envelope of the surface. */
    int[] gridExtent = gridDomain.getGridExtent( destScreenSurface.getEnvelope(), gridDomain.getOrigin( null ).getCoordinateSystem() );
    int lowX = gridExtent[0];
    int lowY = gridExtent[1];
    int highX = gridExtent[2];
    int highY = gridExtent[3];

    /* Calculate imageExtent from gridExtent. */
    int minX = lowX;
    int minY = rasterImage.getHeight() - highY;
    int width = highX - lowX;
    int height = highY - lowY;

    /* Get the required subImage according to the gridExtent (size of the screen). */
    PlanarImage image = rasterImage.getSubImage( minX, minY, width, height );

    /* If the requested sub image is not on the screen (map panel) nothing to display. */
    if( image == null )
      return;

    /* Get the destinationSurface in target coordinates. */
    GM_Surface< ? > destSurface = gridDomain.getGM_Surface( lowX, lowY, highX, highY, targetCS );
    GM_Ring destExtRing = destSurface.getSurfaceBoundary().getExteriorRing();
    GM_Position llCorner = destExtRing.getPositions()[0];
    GM_Position lrCorner = destExtRing.getPositions()[1];
    GM_Position urCorner = destExtRing.getPositions()[2];
    GM_Position ulCorner = destExtRing.getPositions()[3];

    /* Calculate the Corners in screen coordinates. */
    GM_Position pixel_llCorner = projection.getDestPoint( llCorner );
    GM_Position pixel_lrCorner = projection.getDestPoint( lrCorner );
    GM_Position pixel_urCorner = projection.getDestPoint( urCorner );
    GM_Position pixel_ulCorner = projection.getDestPoint( ulCorner );

    /* Calculate the height and width of the image on screen. */
    double destImageHeight = pixel_llCorner.getY() - pixel_ulCorner.getY();
    double destImageWidth = pixel_lrCorner.getX() - pixel_llCorner.getX();

    /* Calculate the scaling factors for the transformation. */
    double scaleX = destImageWidth / image.getWidth();
    double scaleY = destImageHeight / image.getHeight();

    /* Calculate the shear parameters for the transformation. */
    double shearX = pixel_llCorner.getX() - pixel_ulCorner.getX();
    double shearY = pixel_lrCorner.getY() - pixel_llCorner.getY();

    GM_Surface< ? > orgDestSurface = gridDomain.getGM_Surface( targetCS );
    GM_Position orgULCorner = orgDestSurface.getSurfaceBoundary().getExteriorRing().getPositions()[3];
    GM_Position pixel_orgULCorner = projection.getDestPoint( orgULCorner );

    AffineTransform trafo = new AffineTransform();

    /* Translate the image, so that the subImage is at the right position. */
    trafo.translate( pixel_orgULCorner.getX() - pixel_ulCorner.getX(), pixel_orgULCorner.getY() - pixel_ulCorner.getY() );

    /* Scale the image. */
    trafo.scale( scaleX, scaleY );

    /* Translate the image to compensate the shearing. */
    trafo.translate( Math.abs( shearX ) / Math.abs( scaleX ), Math.abs( shearY ) / Math.abs( scaleY ) );

    /* Shear the image. */
    trafo.shear( shearX / destImageHeight, shearY / destImageWidth );

    /* Calculate the required extent of the bufferedImage. */
    GM_Position scaledImage_min = pixel_ulCorner;
    GM_Position scaledImage_max = GeometryFactory.createGM_Position( pixel_urCorner.getX(), pixel_llCorner.getY() );

    GM_Position buffImage_min = GeometryFactory.createGM_Position( scaledImage_min.getX() - Math.abs( shearX ), scaledImage_min.getY() - Math.abs( shearY ) );
    GM_Position buffImage_max = GeometryFactory.createGM_Position( scaledImage_max.getX() + Math.abs( shearX ), scaledImage_max.getY() + Math.abs( shearY ) );
    GM_Envelope buffImageEnv = GeometryFactory.createGM_Envelope( buffImage_min, buffImage_max );
    BufferedImage buffer = new BufferedImage( (int) buffImageEnv.getWidth(), (int) buffImageEnv.getHeight(), BufferedImage.TYPE_INT_ARGB );
    Graphics2D bufferGraphics = (Graphics2D) buffer.getGraphics();

    /* Draw a transparent backround on the bufferedImage. */
    bufferGraphics.setColor( new Color( 255, 255, 255, 0 ) );
    bufferGraphics.fillRect( 0, 0, (int) buffImageEnv.getWidth(), (int) buffImageEnv.getHeight() );

    /* Draw the image with the given transformation. */
    bufferGraphics.drawRenderedImage( image, trafo );

    /* Draw bufferedImage on the screen. */
    g2d.drawImage( buffer, (int) buffImageEnv.getMin().getX(), (int) buffImageEnv.getMin().getY(), null );
  }
}