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
package org.kalypso.ogc.gml.map.utilities;

import java.awt.Point;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.ImageLoader;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.jts.SnapUtilities;
import org.kalypso.jts.SnapUtilities.SNAP_TYPE;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.opengis.cs.CS_CoordinateSystem;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Utility class for map operations.
 * 
 * @author Holger Albert
 */
public class MapUtilities
{
  private MapUtilities( )
  {
  }

  /**
   * Snaps the given AWT-Point to a given geometry, if it lies into a specified radius.
   * 
   * @param mapPanel
   *            The MapPanel of the map.
   * @param p
   *            The AWT-Point which should be snapped.
   * @param radiusPx
   *            This radius will be converted to a world coord radius. Within this circle, the AWT-Point is beeing
   *            snapped.
   * @param type
   *            This type of snapping will be used. {@link SNAP_TYPE}
   * @return The GM_Point snapped on the geometry.
   */
  public static GM_Point snap( final MapPanel mapPanel, final GM_Object geometry, final Point p, final int radiusPx, final SNAP_TYPE type ) throws GM_Exception
  {
    /* Get the JTS geometry. */
    final Geometry geometryJTS = JTSAdapter.export( geometry );

    /* Transform the point to a GM_Point. */
    final GM_Point point = MapUtilities.transform( mapPanel, p );
    if( point == null )
      return null;

    final com.vividsolutions.jts.geom.Point pointJTS = (com.vividsolutions.jts.geom.Point) JTSAdapter.export( point );

    /* Buffer the point. */
    final Geometry pointBuffer = pointJTS.buffer( MapUtilities.calculateWorldDistance( mapPanel, point, radiusPx ) );

    if( !pointBuffer.intersects( geometryJTS ) )
      return null;

    if( geometryJTS instanceof com.vividsolutions.jts.geom.Point )
    {
      final com.vividsolutions.jts.geom.Point snapPoint = SnapUtilities.snapPoint( pointJTS );
      if( snapPoint != null )
        return (GM_Point) JTSAdapter.wrap( snapPoint );
    }
    else if( geometryJTS instanceof LineString )
    {
      final com.vividsolutions.jts.geom.Point snapPoint = SnapUtilities.snapLine( (LineString) geometryJTS, pointBuffer, type );
      if( snapPoint != null )
        return (GM_Point) JTSAdapter.wrap( snapPoint );
    }
    else if( geometryJTS instanceof Polygon )
    {
      final com.vividsolutions.jts.geom.Point snapPoint = SnapUtilities.snapPolygon( (Polygon) geometryJTS, pointBuffer, type );
      if( snapPoint != null )
        return (GM_Point) JTSAdapter.wrap( snapPoint );
    }

    return null;
  }

  /**
   * This method transforms the AWT-Point to a GM_Point.
   * 
   * @param mapPanel
   *            The MapPanel of the map.
   * @param p
   *            The AWT-Point.
   */
  public static GM_Point transform( final MapPanel mapPanel, final Point p )
  {
    final GeoTransform projection = mapPanel.getProjection();
    final IMapModell mapModell = mapPanel.getMapModell();
    if( mapModell == null )
      return null;

    final CS_CoordinateSystem coordinatesSystem = mapModell.getCoordinatesSystem();

    final double x = p.getX();
    final double y = p.getY();

    return GeometryFactory.createGM_Point( projection.getSourceX( x ), projection.getSourceY( y ), coordinatesSystem );
  }

  /**
   * This method transforms the GM_Point to a AWT-Point.
   * 
   * @param mapPanel
   *            The MapPanel of the map.
   * @param p
   *            The GM_Point.
   */
  public static Point retransform( final MapPanel mapPanel, final GM_Point p )
  {
    final GeoTransform projection = mapPanel.getProjection();

    final double x = p.getX();
    final double y = p.getY();

    return new Point( (int) projection.getDestX( x ), (int) projection.getDestY( y ) );
  }

  /**
   * This function transforms a distance in pixel to the world distance.
   * 
   * @param mapPanel
   *            The MapPanel of the map.
   * @param reference
   *            The reference point.
   * @param distancePx
   *            The distance to be calculated.
   * @return The distance in the world coords.
   */
  public static double calculateWorldDistance( final MapPanel mapPanel, final GM_Point reference, final int distancePx )
  {
    final Point point = MapUtilities.retransform( mapPanel, reference );
    point.x = point.x + distancePx;

    final GM_Point destination = MapUtilities.transform( mapPanel, point );
    return destination.getX() - reference.getX();
  }

  /**
   * This function transforms a distance in pixel to the world distance.
   * 
   * @param mapPanel
   *            The MapPanel of the map.
   * @param distancePx
   *            The distance in pixel to be calculated.
   * @return The distance in the world coordinates system.
   */
  public static double calculateWorldDistance( final MapPanel mapPanel, final int distancePx )
  {
    final GM_Position minPosition = mapPanel.getBoundingBox().getMin();
    final GM_Point reference = GeometryFactory.createGM_Point( minPosition.getX(), minPosition.getY(), mapPanel.getMapModell().getCoordinatesSystem() );

    return MapUtilities.calculateWorldDistance( mapPanel, reference, distancePx );
  }

  /**
   * This function returns the map scale from the given map panel.
   * 
   * @return The map scale.
   */
  public static double getMapScale( MapPanel mapPanel )
  {
    if( mapPanel == null )
      return 0.0;

    IMapModell mapModell = mapPanel.getMapModell();
    if( mapModell == null )
      return 0.0;

    GM_Envelope extent = mapPanel.getBoundingBox();
    int width = mapPanel.getWidth();
    int height = mapPanel.getHeight();

    return MapModellHelper.calcScale( mapModell, extent, width, height );
  }

  /**
   * This function sets the map scale, if different from the given map panel.
   * 
   * @param scale
   *            The new map scale.
   */
  public static void setMapScale( MapPanel mapPanel, double scale )
  {
    /* Get the current map scale. */
    double mapScale = getMapScale( mapPanel );

    /* If it is the same as before, don't change anything. */
    if( mapScale == scale )
      return;

    /* Get the current extent. */
    GM_Envelope extent = mapPanel.getBoundingBox();

    /* Get the current displayed distance (meter). */
    double width = extent.getWidth();
    double height = extent.getHeight();

    /* Calculate the center of the extent (coordinates). */
    double x = extent.getMin().getX();
    double y = extent.getMin().getY();
    x = x + width / 2;
    y = y + height / 2;

    /* Calculate the new extent. */
    double newWidth = (width / mapScale) * scale;
    double newHeight = (height / mapScale) * scale;

    double newX = x - newWidth / 2;
    double newY = y - newHeight / 2;

    /* Create the new extent. */
    GM_Envelope newExtent = GeometryFactory.createGM_Envelope( newX, newY, newX + newWidth, newY + newHeight );

    /* Set the new extent. */
    mapPanel.setBoundingBox( newExtent );
  }

  /**
   * This function exports the legend of the given themes to a file. It seems that it has to be run in an UI-Thread.
   * 
   * @param themes
   *            The list of the themes.
   * @param file
   *            The file, where it should save to.
   * @param format
   *            The image format (for example: SWT.IMAGE_PNG).
   * @param monitor
   *            A progress monitor.
   * @return A status, containing information about the process.
   */
  public static IStatus exportLegends( List<IKalypsoTheme> themes, File file, int format, IProgressMonitor monitor )
  {
    /* Monitor. */
    monitor.beginTask( "Exporting legend", themes.size() * 100 + 100 );

    try
    {
      /* This font will be used to generate the legend. */
      Font font = new Font( Display.getCurrent(), "Arial", 10, SWT.NORMAL );

      /* Memory for the legends. */
      ArrayList<Image> legends = new ArrayList<Image>();

      /* Collect the legends. */
      for( int i = 0; i < themes.size(); i++ )
      {
        /* Get the theme. */
        IKalypsoTheme theme = themes.get( i );

        /* Monitor. */
        monitor.subTask( "Creating legend from \"" + theme.getName() + "\" ..." );

        /* Get the legend. */
        Image legend = theme.getLegendGraphic( font );
        if( legend != null )
          legends.add( legend );

        /* Monitor. */
        monitor.worked( 100 );
      }

      /* No legends there. Perhaps all themes did not provide legends. */
      if( legends.size() == 0 )
        return StatusUtilities.createWarningStatus( "No legends available ..." );

      /* Calculate the size. */
      int width = 0;
      int height = 0;

      for( Image legend : legends )
      {
        Rectangle bounds = legend.getBounds();
        if( bounds.width > width )
          width = bounds.width;

        height = height + bounds.height;
      }

      /* Monitor. */
      monitor.worked( 25 );

      /* Now create the new image. */
      Image image = new Image( Display.getCurrent(), width, height );

      /* Need a GC. */
      GC gc = new GC( image );

      /* Draw on it. */
      int heightSoFar = 0;
      for( Image legend : legends )
      {
        gc.drawImage( legend, 0, heightSoFar );
        heightSoFar = heightSoFar + legend.getBounds().height;
      }

      /* Monitor. */
      monitor.worked( 50 );

      ImageLoader imageLoader = new ImageLoader();
      imageLoader.data = new ImageData[] { image.getImageData() };
      imageLoader.save( file.toString(), format );

      /* Monitor. */
      monitor.worked( 25 );

      return Status.OK_STATUS;
    }
    catch( Exception e )
    {
      return StatusUtilities.statusFromThrowable( e );
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }
}