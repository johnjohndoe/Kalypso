/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 *
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always.
 *
 * If you intend to use this software in other ways than in kalypso
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree,
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.tools;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.geom.AffineTransform;

import javax.media.jai.TiledImage;

import org.kalypso.transformation.GeoTransformer;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.coverage.GridRange;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Ring;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain.OffsetVector;
import org.kalypsodeegree_impl.model.cv.GridRange_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

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

  public static void transformImage( TiledImage remoteImage, GM_Envelope env, String localCSR, String remoteCSR, GeoTransform worldToScreenTransformation, Graphics g ) throws Exception
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
  private static void internalTransformation( Graphics2D g2d, GeoTransform projection, TiledImage rasterImage, RectifiedGridDomain gridDomain, String targetCS ) throws Exception
  {
    /* Get the Screen extent in real world coordiantes. */
    GM_Envelope sourceScreenRect = projection.getSourceRect();

    /* Create a surface and transform it in the coordinate system of the. */
    GM_Surface< ? > sourceScreenSurface = GeometryFactory.createGM_Surface( sourceScreenRect, targetCS );

    GM_Surface< ? > destScreenSurface;
    final String domainCrs = gridDomain.getOrigin( null ).getCoordinateSystem();
    if( !targetCS.equals( domainCrs ) )
    {
      GeoTransformer geoTrans1 = new GeoTransformer( domainCrs );
      destScreenSurface = (GM_Surface< ? >) geoTrans1.transform( sourceScreenSurface );
    }
    else
      destScreenSurface = sourceScreenSurface;

    /* Get the gridExtent for the envelope of the surface. */
    int[] gridExtent = gridDomain.getGridExtent( destScreenSurface.getEnvelope(), domainCrs );
    // Make it a bit larger in order to avoid undrawn border
    Point pLow = new Point( gridExtent[0] - 2, gridExtent[1] - 2 );
    Point pHigh = new Point( gridExtent[2] + 2, gridExtent[3] + 2 );

    /* Calculate imageExtent from gridExtent. */
    Point pMin = new Point( pLow.x, rasterImage.getHeight() - pHigh.y );
    int width = pHigh.x - pLow.x;
    int height = pHigh.y - pLow.y;

    /* Get the required subImage according to the gridExtent (size of the screen). */
    TiledImage image = rasterImage.getSubImage( pMin.x, pMin.y, width, height );

    /* If the requested sub image is not on the screen (map panel) nothing to display. */
    if( image == null )
      return;

    /* Get the destinationSurface in target coordinates. */
    GM_Surface< ? > destSurface = gridDomain.getGM_Surface( pLow.x, pLow.y, pHigh.x, pHigh.y, targetCS );
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
    double destImageWidth = pixel_lrCorner.getX() - pixel_llCorner.getX();
    double destImageHeight = pixel_llCorner.getY() - pixel_ulCorner.getY();

    /* If one of the values is <=0, there could nothing displayed. */
    if( destImageHeight <= 0 || destImageWidth <= 0 )
      return;

    /* Calculate the scaling factors for the transformation. */
    double scaleX = destImageWidth / image.getWidth();
    double scaleY = destImageHeight / image.getHeight();

    /* Calculate the shear parameters for the transformation. */
    double shearX = pixel_llCorner.getX() - pixel_ulCorner.getX();
    double shearY = pixel_lrCorner.getY() - pixel_llCorner.getY();

    /* Calculate the required extent of the bufferedImage. */
    GM_Position scaledImage_min = pixel_ulCorner;
    GM_Position scaledImage_max = GeometryFactory.createGM_Position( pixel_urCorner.getX(), pixel_llCorner.getY() );

    GM_Position buffImage_min = GeometryFactory.createGM_Position( scaledImage_min.getX() - Math.abs( shearX ), scaledImage_min.getY() - Math.abs( shearY ) );
    GM_Position buffImage_max = GeometryFactory.createGM_Position( scaledImage_max.getX() + Math.abs( shearX ), scaledImage_max.getY() + Math.abs( shearY ) );
    GM_Envelope buffImageEnv = GeometryFactory.createGM_Envelope( buffImage_min, buffImage_max, targetCS );

    AffineTransform trafo = new AffineTransform();
    trafo.translate( (int) buffImageEnv.getMin().getX(), (int) buffImageEnv.getMin().getY() );

    /* Translate the image, so that the subImage is at the right position. */
    trafo.translate( -image.getMinX() * scaleX, -image.getMinY() * scaleY );

    /* Scale the image. */
    trafo.scale( scaleX, scaleY );

    /* Translate the image to compensate the shearing. */
    trafo.translate( Math.abs( shearX ) / Math.abs( scaleX ), Math.abs( shearY ) / Math.abs( scaleY ) );

    /* Shear the image. */
    trafo.shear( shearX / destImageWidth, shearY / destImageHeight );

    /* We cannot draw, if the image would have one or both side with 0 pixels. */
    int width2 = (int) buffImageEnv.getWidth();
    int height2 = (int) buffImageEnv.getHeight();
    if( width2 <= 0 || height2 <= 0 )
      return;

    // TODO: maybe this code was used to support transparent tiffs (like TK); check it that worked before; if not leave
    // it or introduce flag
// BufferedImage buffer = new BufferedImage( width2, height2, BufferedImage.TYPE_INT_ARGB );
// Graphics2D bufferGraphics = (Graphics2D) buffer.getGraphics();
    /* Draw a transparent background on the bufferedImage. Why !? */
// bufferGraphics.setColor( new Color( 255, 255, 255, 0 ) );
// bufferGraphics.fillRect( 0, 0, width2, height2 );
// bufferGraphics.drawRenderedImage( image, trafo );
// g2d.drawImage( buffer, (int) buffImageEnv.getMin().getX(), (int) buffImageEnv.getMin().getY(), null );
    g2d.drawRenderedImage( image, trafo );
  }
}