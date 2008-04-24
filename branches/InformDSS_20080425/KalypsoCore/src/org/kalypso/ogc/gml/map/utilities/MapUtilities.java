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
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.jts.SnapUtilities;
import org.kalypso.jts.SnapUtilities.SNAP_TYPE;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.RuleTreeObject;
import org.kalypso.ogc.gml.ThemeStyleTreeObject;
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
  public static GM_Point snap( MapPanel mapPanel, GM_Object geometry, Point p, int radiusPx, SNAP_TYPE type ) throws GM_Exception
  {
    /* Transform the point to a GM_Point. */
    GM_Point point = MapUtilities.transform( mapPanel, p );
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
  public static GM_Point snap( MapPanel mapPanel, GM_Object geometry, GM_Point point, int radiusPx, SNAP_TYPE type ) throws GM_Exception
  {
    /* Get the JTS geometry. */
    Geometry geometryJTS = JTSAdapter.export( geometry );
    com.vividsolutions.jts.geom.Point pointJTS = (com.vividsolutions.jts.geom.Point) JTSAdapter.export( point );

    /* Buffer the point. */
    Geometry pointBuffer = pointJTS.buffer( MapUtilities.calculateWorldDistance( mapPanel, point, radiusPx ) );

    if( !pointBuffer.intersects( geometryJTS ) )
      return null;

    if( geometryJTS instanceof com.vividsolutions.jts.geom.Point )
    {
      com.vividsolutions.jts.geom.Point snapPoint = SnapUtilities.snapPoint( pointJTS );
      if( snapPoint != null )
      {
        GM_Point myPoint = (GM_Point) JTSAdapter.wrap( snapPoint );
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
      com.vividsolutions.jts.geom.Point snapPoint = SnapUtilities.snapLine( (LineString) geometryJTS, pointBuffer, type );
      if( snapPoint != null )
      {
        GM_Point myPoint = (GM_Point) JTSAdapter.wrap( snapPoint );
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
      com.vividsolutions.jts.geom.Point snapPoint = SnapUtilities.snapPolygon( (Polygon) geometryJTS, pointBuffer, type );
      if( snapPoint != null )
      {
        GM_Point myPoint = (GM_Point) JTSAdapter.wrap( snapPoint );
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
  public static GM_Point transform( MapPanel mapPanel, Point p )
  {
    GeoTransform projection = mapPanel.getProjection();
    IMapModell mapModell = mapPanel.getMapModell();
    if( mapModell == null )
      return null;

    String coordinatesSystem = mapModell.getCoordinatesSystem();
    if( coordinatesSystem == null )
      coordinatesSystem = KalypsoCorePlugin.getDefault().getCoordinatesSystem();

    double x = p.getX();
    double y = p.getY();

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
  public static Point retransform( MapPanel mapPanel, GM_Point p )
  {
    GeoTransform projection = mapPanel.getProjection();

    double x = p.getX();
    double y = p.getY();

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
  public static double calculateWorldDistance( MapPanel mapPanel, GM_Point reference, int distancePx )
  {
    Point point = MapUtilities.retransform( mapPanel, reference );
    point.x = point.x + distancePx;

    GM_Point destination = MapUtilities.transform( mapPanel, point );
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
  public static double calculateWorldDistance( MapPanel mapPanel, int distancePx )
  {
    GM_Position minPosition = mapPanel.getBoundingBox().getMin();
    GM_Point reference = GeometryFactory.createGM_Point( minPosition.getX(), minPosition.getY(), mapPanel.getMapModell().getCoordinatesSystem() );

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

    // TODO Transform to EPSG4XXX like in the function where the scale is calculated.
    // Then calculate the new BBOX.
    // Then retransform to the original CS.

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
    GM_Envelope newExtent = GeometryFactory.createGM_Envelope( newX, newY, newX + newWidth, newY + newHeight, extent.getCoordinateSystem() );

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

  /**
   * This function checks, if a theme is out of scale.
   * 
   * @param mapPanel
   *            The map panel.
   * @param theme
   *            The theme.
   * @return True, if the theme is out of scale.
   */
  public static boolean isOutOfScale( MapPanel mapPanel, IKalypsoTheme theme )
  {
    /* Get all children. */
    Object[] styles = theme.getChildren( theme );
    for( int i = 0; i < styles.length; i++ )
    {
      /* Get the object. */
      Object object = styles[i];

      if( object instanceof IKalypsoTheme )
      {
        /* This is a theme. */
        IKalypsoTheme kalypsoTheme = (IKalypsoTheme) object;

        /* If one is visible, return false. */
        if( isOutOfScale( mapPanel, kalypsoTheme ) == false )
          return false;
      }

      /* Only styles can be checked. */
      if( !(object instanceof ThemeStyleTreeObject) )
        return false;

      /* This is a style. */
      ThemeStyleTreeObject style = (ThemeStyleTreeObject) object;

      /* Get the rules. */
      Object[] rules = style.getChildren( style );
      for( Object object2 : rules )
      {
        /* Only rules can be checked. */
        if( !(object2 instanceof RuleTreeObject) )
          return false;

        /* This is a rule. */
        RuleTreeObject rule = (RuleTreeObject) object2;

        /* Get the scale constraints. */
        double minScaleDenominator = rule.getRule().getMinScaleDenominator();
        double maxScaleDenominator = rule.getRule().getMaxScaleDenominator();

        /* Get the current scale. */
        double mapScale = MapUtilities.getMapScale( mapPanel );

        System.out.println( "Min:" + minScaleDenominator );
        System.out.println( "Scale:" + mapScale );
        System.out.println( "Max:" + maxScaleDenominator );

        /* Is one rule is lying between the constraint, the theme is still visible. */
        if( minScaleDenominator <= mapScale && mapScale <= maxScaleDenominator )
          return false;
      }
    }

    /* No theme rule is visible, due to the scale constraints. */
    return true;
  }
}