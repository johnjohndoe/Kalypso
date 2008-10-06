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

import java.awt.Insets;
import java.awt.Point;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Device;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.ImageLoader;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.i18n.Messages;
import org.kalypso.jts.SnapUtilities;
import org.kalypso.jts.SnapUtilities.SNAP_TYPE;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

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
  public static GM_Point snap( final IMapPanel mapPanel, final GM_Object geometry, final Point p, final int radiusPx, final SNAP_TYPE type ) throws GM_Exception
  {
    /* Transform the point to a GM_Point. */
    final GM_Point point = MapUtilities.transform( mapPanel, p );
    if( point == null )
      return null;

    return snap( mapPanel, geometry, point, radiusPx, type );
  }

  /**
   * Snaps the given GM_Point to a given geometry, if it lies into a specified radius.
   * 
   * @param mapPanel
   *            The MapPanel of the map.
   * @param point
   *            The GM_Point which should be snapped.
   * @param radiusPx
   *            This radius will be converted to a world coord radius. Within this circle, the GM_Point is beeing
   *            snapped.
   * @param type
   *            This type of snapping will be used. {@link SNAP_TYPE}
   * @return The GM_Point snapped on the geometry.
   */
  public static GM_Point snap( final IMapPanel mapPanel, final GM_Object geometry, final GM_Point point, final int radiusPx, final SNAP_TYPE type ) throws GM_Exception
  {
    /* Get the JTS geometry. */
    final Geometry geometryJTS = JTSAdapter.export( geometry );
    final com.vividsolutions.jts.geom.Point pointJTS = (com.vividsolutions.jts.geom.Point) JTSAdapter.export( point );

    /* Buffer the point. */
    final Geometry pointBuffer = pointJTS.buffer( MapUtilities.calculateWorldDistance( mapPanel, point, radiusPx ) );

    if( !pointBuffer.intersects( geometryJTS ) )
      return null;

    if( geometryJTS instanceof com.vividsolutions.jts.geom.Point )
    {
      final com.vividsolutions.jts.geom.Point snapPoint = SnapUtilities.snapPoint( pointJTS );
      if( snapPoint != null )
      {
        final GM_Point myPoint = (GM_Point) JTSAdapter.wrap( snapPoint );
        /**
         * has no crs! see
         * 
         * @link{JTSAdapter}
         */
        myPoint.setCoordinateSystem( point.getCoordinateSystem() );

        return myPoint;
      }
    }
    else if( geometryJTS instanceof LineString )
    {
      final com.vividsolutions.jts.geom.Point snapPoint = SnapUtilities.snapLine( (LineString) geometryJTS, pointBuffer, type );
      if( snapPoint != null )
      {
        final GM_Point myPoint = (GM_Point) JTSAdapter.wrap( snapPoint );
        /**
         * has no crs! see
         * 
         * @link{JTSAdapter}
         */
        myPoint.setCoordinateSystem( point.getCoordinateSystem() );

        return myPoint;
      }
    }
    else if( geometryJTS instanceof Polygon )
    {
      final com.vividsolutions.jts.geom.Point snapPoint = SnapUtilities.snapPolygon( (Polygon) geometryJTS, pointBuffer, type );
      if( snapPoint != null )
      {
        final GM_Point myPoint = (GM_Point) JTSAdapter.wrap( snapPoint );
        /**
         * has no crs! see
         * 
         * @link{JTSAdapter}
         */
        myPoint.setCoordinateSystem( point.getCoordinateSystem() );

        return myPoint;
      }
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
  public static GM_Point transform( final IMapPanel mapPanel, final Point p )
  {
    final GeoTransform projection = mapPanel.getProjection();
    final IMapModell mapModell = mapPanel.getMapModell();
    if( mapModell == null )
      return null;

    String coordinatesSystem = mapModell.getCoordinatesSystem();
    if( coordinatesSystem == null )
      coordinatesSystem = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

    final double x = p.getX();
    final double y = p.getY();

    return GeometryFactory.createGM_Point( projection.getSourceX( x ), projection.getSourceY( y ), coordinatesSystem );
  }

  /**
   * This method transforms the GM_Point to an AWT-Point.
   * 
   * @param mapPanel
   *            The MapPanel of the map.
   * @param p
   *            The GM_Point.
   */
  public static Point retransform( final IMapPanel mapPanel, final GM_Point p )
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
  public static double calculateWorldDistance( final IMapPanel mapPanel, final GM_Point reference, final int distancePx )
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
  public static double calculateWorldDistance( final IMapPanel mapPanel, final int distancePx )
  {
    final GM_Position minPosition = mapPanel.getBoundingBox().getMin();
    final GM_Point reference = GeometryFactory.createGM_Point( minPosition.getX(), minPosition.getY(), mapPanel.getMapModell().getCoordinatesSystem() );

    return MapUtilities.calculateWorldDistance( mapPanel, reference, distancePx );
  }

  /**
   * This function sets the map scale, if different from the given map panel.
   * 
   * @param scale
   *            The new map scale.
   */
  public static void setMapScale( final IMapPanel mapPanel, final double scale )
  {
    /* Get the current map scale. */
    final double mapScale = mapPanel.getCurrentScale();

    /* If it is the same as before, don't change anything. */
    if( mapScale == scale )
      return;

    // TODO Transform to EPSG4XXX like in the function where the scale is calculated.
    // Then calculate the new BBOX.
    // Then retransform to the original CS.

    /* Get the current extent. */
    final GM_Envelope extent = mapPanel.getBoundingBox();

    /* Get the current displayed distance (meter). */
    final double width = extent.getWidth();
    final double height = extent.getHeight();

    /* Calculate the center of the extent (coordinates). */
    double x = extent.getMin().getX();
    double y = extent.getMin().getY();
    x = x + width / 2;
    y = y + height / 2;

    /* Calculate the new extent. */
    final double newWidth = (width / mapScale) * scale;
    final double newHeight = (height / mapScale) * scale;

    final double newX = x - newWidth / 2;
    final double newY = y - newHeight / 2;

    /* Create the new extent. */
    final GM_Envelope newExtent = GeometryFactory.createGM_Envelope( newX, newY, newX + newWidth, newY + newHeight, extent.getCoordinateSystem() );

    /* Set the new extent. */
    mapPanel.setBoundingBox( newExtent );
  }

  /**
   * This function exports the legend of the given themes to a file. It seems that it has to be run in an UI-Thread.
   * 
   * @param themes
   *          The themes to export.
   * @param file
   *          The file, where it should save to.
   * @param format
   *          The image format (for example: SWT.IMAGE_PNG).
   * @param monitor
   *          A progress monitor.
   * @return A status, containing information about the process.
   */
  public static IStatus exportLegends( final IKalypsoTheme[] themes, final File file, final int format, final IProgressMonitor monitor )
  {
    /* Monitor. */
    final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.ogc.gml.map.utilities.MapUtilities.0" ), 100 ); //$NON-NLS-1$

    Image image = null;
    try
    {
      // TODO: get both from outside
      final Display current = Display.getCurrent();
      final Insets insets = new Insets( 5, 5, 5, 5 );

      image = exportLegends( themes, current, insets, null, progress.newChild( 50 ) );
      ProgressUtilities.worked( progress, 50 );

      final ImageLoader imageLoader = new ImageLoader();
      imageLoader.data = new ImageData[] { image.getImageData() };
      imageLoader.save( file.toString(), format );

      /* Monitor. */
      ProgressUtilities.worked( monitor, 50 );

      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      return StatusUtilities.statusFromThrowable( e );
    }
    finally
    {
      if( image != null )
        image.dispose();

      progress.done();
    }
  }

  /**
   * This function exports the legend of the given themes as swt image. Has to run in an UI-Thread. (TODO change this!)<br>
   * 
   * @param themes
   *          The themes to export.
   * @param insets
   *          Defines the size of an empty border around the image. Must not be <code>null</code>.
   * @param backgroundRGB
   *          Defines the background color of the image. If <code>null</code>, an transparent image will be returned.
   * @param monitor
   *          A progress monitor.
   * @return The newly created image, must be disposed by the caller.
   */
  public static Image exportLegends( final IKalypsoTheme[] themes, final Device device, final Insets insets, final RGB backgroundRGB, final IProgressMonitor monitor ) throws CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.ogc.gml.map.utilities.MapUtilities.0" ), themes.length * 100 + 100 ); //$NON-NLS-1$

    /* This font will be used to generate the legend. */
    // TODO: this font does not exists necessarily on every platform... use one of eclipses standard font instead or
    // have at least a fallback
    final Font font = new Font( device, "Arial", 10, SWT.NORMAL ); //$NON-NLS-1$

    /* Memory for the legends. */
    final List<Image> legends = new ArrayList<Image>();

    /* Collect the legends. */
    for( final IKalypsoTheme theme : themes )
    {
      progress.subTask( Messages.getString( "org.kalypso.ogc.gml.map.utilities.MapUtilities.2" ) + theme.getName() + Messages.getString( "org.kalypso.ogc.gml.map.utilities.MapUtilities.3" ) ); //$NON-NLS-1$ //$NON-NLS-2$

      /* Get the legend. */
      final Image legend = theme.getLegendGraphic( font );
      if( legend != null )
        legends.add( legend );

      ProgressUtilities.worked( progress, 100 );
    }

    /* No legends there. Perhaps no theme did provide a legend. */
    if( legends.size() == 0 )
      throw new CoreException( StatusUtilities.createWarningStatus( Messages.getString( "org.kalypso.ogc.gml.map.utilities.MapUtilities.4" ) ) ); //$NON-NLS-1$

    /* Calculate the size. */
    int width = 0;
    int height = 0;

    for( final Image legend : legends )
    {
      final Rectangle bounds = legend.getBounds();
      if( bounds.width > width )
        width = bounds.width;

      height = height + bounds.height;
    }
    width += insets.left + insets.right;
    height += insets.top + insets.bottom;

    ProgressUtilities.worked( progress, 50 );

    /* Now create the new image. */
    final Image image = new Image( device, width, height );

    /* Need a GC. */
    final GC gc = new GC( image );

    final Color bgColor = new Color( device, backgroundRGB );
    gc.setBackground( bgColor );
    gc.fillRectangle( image.getBounds() );
    bgColor.dispose();

    /* Draw on it. */
    int heightSoFar = insets.top;
    for( final Image legend : legends )
    {
      gc.drawImage( legend, insets.left, heightSoFar );
      heightSoFar = heightSoFar + legend.getBounds().height;
      legend.dispose();
    }

    gc.dispose();

    ProgressUtilities.worked( progress, 50 );
    return image;
  }

}