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
package org.kalypso.ogc.gml.mapmodel;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.java.awt.HighlightGraphics;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.IKalypsoThemeFilter;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeLoadStatusVisitor;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.graphics.transformation.WorldToScreenTransform;
import org.kalypsodeegree_impl.model.ct.GeoTransformer;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.Debug;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Utility class for {@link IMapModell} associated functions.
 * 
 * @author Gernot Belger
 */
public class MapModellHelper
{
  public MapModellHelper( )
  {
    throw new UnsupportedOperationException( "Do not instantiate this helper class" );
  }

  /**
   * Waits for a {@link MapPanel} to be completely loaded. A progress dialog opens if this operation takes long.<br>
   * If an error occurs, an error dialog will be shown.
   * 
   * @return <code>false</code> if any error happened, the map is not garantueed to be loaded in this case.
   * @see ProgressUtilities#busyCursorWhile(ICoreRunnableWithProgress)
   * @see #createWaitForMapOperation(MapPanel)
   */
  public static boolean waitForAndErrorDialog( final Shell shell, final MapPanel mapPanel, final String windowTitle, final String message )
  {
    final ICoreRunnableWithProgress operation = createWaitForMapOperation( mapPanel );
    final IStatus waitErrorStatus = ProgressUtilities.busyCursorWhile( operation );
    ErrorDialog.openError( shell, windowTitle, message, waitErrorStatus );
    return waitErrorStatus.isOK();
  }

  /**
   * Creates an {@link ICoreRunnableWithProgress} which waits for a {@link MapPanel} to be loaded.<br>
   * Uses the {@link IMapModell#isLoaded()} and {@link IKalypsoTheme#isLoaded()} methods.
   */
  public static ICoreRunnableWithProgress createWaitForMapOperation( final MapPanel mapPanel )
  {
    final ICoreRunnableWithProgress waitForMapOperation = new ICoreRunnableWithProgress()
    {
      public IStatus execute( IProgressMonitor monitor ) throws InterruptedException
      {
        monitor.beginTask( "Karte wird geladen...", IProgressMonitor.UNKNOWN );

        Thread.sleep( 250 );

        while( true )
        {
          if( monitor.isCanceled() )
            return Status.CANCEL_STATUS;

          try
          {
            final IMapModell modell = mapPanel.getMapModell();
            if( modell != null && modell.isLoaded() )
            {
              final KalypsoThemeLoadStatusVisitor visitor = new KalypsoThemeLoadStatusVisitor();
              modell.accept( visitor, FeatureVisitor.DEPTH_INFINITE );

              if( visitor.isLoaded() == true )
                return Status.OK_STATUS;
            }

            Thread.sleep( 250 );

            monitor.worked( 10 );
          }
          catch( final InterruptedException e )
          {
            return StatusUtilities.statusFromThrowable( e );
          }
        }
      }
    };
    return waitForMapOperation;
  }

  /**
   * calculates the map scale (denominator) as defined in the OGC SLD 1.0.0 specification
   * 
   * @return scale of the map
   */
  public static double calcScale( final IMapModell model, final GM_Envelope bbox, final int mapWidth, final int mapHeight )
  {
    try
    {
      final CS_CoordinateSystem crs = model.getCoordinatesSystem();

      if( bbox == null )
        return 0.0;

      final GM_Envelope box;
      if( !crs.getName().equalsIgnoreCase( "EPSG:4326" ) )
      {
        // transform the bounding box of the request to EPSG:4326
        final GeoTransformer transformer = new GeoTransformer( "EPSG:4326", true );
        box = transformer.transformEnvelope( bbox, crs );
      }
      else
        box = bbox;

      if( box == null )
        return 0.0;

      final double dx = box.getWidth() / mapWidth;
      final double dy = box.getHeight() / mapHeight;

      // create a box on the central map pixel to determine its size in meters
      final GM_Position min = GeometryFactory.createGM_Position( box.getMin().getX() + dx * (mapWidth / 2d - 1), box.getMin().getY() + dy * (mapHeight / 2d - 1) );
      final GM_Position max = GeometryFactory.createGM_Position( box.getMin().getX() + dx * (mapWidth / 2d), box.getMin().getY() + dy * (mapHeight / 2d) );
      final double distance = MapModellHelper.calcDistance( min.getY(), min.getX(), max.getY(), max.getX() );

      // default pixel size defined in SLD specs is 28mm
      final double scale = distance / 0.00028;
      return scale;
    }
    catch( final Exception e )
    {
      Debug.debugException( e, "Exception occured when calculating scale!" );
    }

    return 0.0;
  }

  /**
   * Create an image of a map model and keep aspection ration of displayed map and its extend
   */
  public static BufferedImage createWellFormedImageFromModel( final MapPanel panel, final int width, final int height )
  {
    final IMapModell mapModell = panel.getMapModell();
    final GM_Envelope bbox = panel.getBoundingBox();

    final double ratio = (double) height / (double) width;
    final GM_Envelope boundingBox = MapModellHelper.adjustBoundingBox( mapModell, bbox, ratio );

    final GeoTransform transform = new WorldToScreenTransform();
    transform.setSourceRect( boundingBox );

    final Rectangle bounds = new Rectangle( width, height );

    return MapModellHelper.createImageFromModell( transform, boundingBox, bounds, bounds.width, bounds.height, mapModell );
  }

  /**
   * Is used to create an image of a map model. Does not wait until all themes are loaded. Is used from the map panel as
   * well, where the drawing is done every refresh of the map. So it does not matter, when some themes finish, if they
   * finish at last.
   */
  public static BufferedImage createImageFromModell( final GeoTransform p, final GM_Envelope bbox, final Rectangle bounds, final int width, final int height, final IMapModell model )
  {
    final BufferedImage image = new BufferedImage( width, height, BufferedImage.TYPE_INT_ARGB );
    final Graphics2D gr = (Graphics2D) image.getGraphics();
    try
    {
      gr.setColor( Color.white );
      gr.fillRect( 0, 0, width, height );
      gr.setColor( Color.black );
      gr.setClip( 0, 0, width, height );

      gr.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON );
      gr.setRenderingHint( RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON );

      final int x = bounds.x;
      final int y = bounds.y;
      final int w = bounds.width;
      final int h = bounds.height;

      p.setDestRect( x - 2, y - 2, w + x, h + y );

      final double scale = MapModellHelper.calcScale( model, bbox, bounds.width, bounds.height );
      try
      {
        model.paint( gr, p, bbox, scale, false );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        System.out.println( e.getMessage() );
      }

      final HighlightGraphics highlightGraphics = new HighlightGraphics( gr );
      model.paint( highlightGraphics, p, bbox, scale, true );
    }
    finally
    {
      gr.dispose();
    }

    return image;
  }

  /**
   * calculates the distance in meters between two points in EPSG:4326 coodinates .
   */
  private static double calcDistance( final double lon1, final double lat1, final double lon2, final double lat2 )
  {
    final double r = 6378.137;
    final double rad = Math.PI / 180d;
    double cose = 0;

    cose = Math.sin( rad * lon1 ) * Math.sin( rad * lon2 ) + Math.cos( rad * lon1 ) * Math.cos( rad * lon2 ) * Math.cos( rad * (lat1 - lat2) );
    final double dist = r * Math.acos( cose );

    return dist * 1000;
  }

  public static IKalypsoTheme[] filterThemes( final IMapModell modell, final IKalypsoThemeFilter filter )
  {
    final IKalypsoTheme[] allThemes = modell.getAllThemes();
    final List<IKalypsoTheme> themes = new ArrayList<IKalypsoTheme>( allThemes.length );
    for( final IKalypsoTheme theme : allThemes )
      if( filter.accept( theme ) )
        themes.add( theme );

    return themes.toArray( new IKalypsoTheme[themes.size()] );
  }

  /**
   * Calculates the common extent o fall given themes.
   * 
   * @param predicate
   *            If not <code>null</code>, only themes applying to the predicate are considered.
   * @return <code>null</code>, if the array of themes is empty or null.
   */
  public static GM_Envelope calculateExtent( final IKalypsoTheme[] themes, final IKalypsoThemePredicate predicate )
  {
    if( themes == null )
      return null;

    GM_Envelope result = null;
    for( final IKalypsoTheme kalypsoTheme : themes )
      if( (predicate == null) || predicate.decide( kalypsoTheme ) )
      {
        final GM_Envelope boundingBox = kalypsoTheme.getFullExtent();
        if( result == null )
          result = boundingBox;
        else
          result = result.getMerged( boundingBox );
      }

    return result;
  }

  /**
   * Adjust an given bounding box (env) to an new ratio
   */
  public static GM_Envelope adjustBoundingBox( final IMapModell model, GM_Envelope env, final double ratio )
  {
    if( env == null )
      env = model.getFullExtentBoundingBox();
    if( env == null )
      return null;

    // TODO search for a better solution
    if( Double.isNaN( ratio ) )
      return env;

    final double minX = env.getMin().getX();
    final double minY = env.getMin().getY();

    final double maxX = env.getMax().getX();
    final double maxY = env.getMax().getY();

    double dx = (maxX - minX) / 2d;
    double dy = (maxY - minY) / 2d;

    if( dx * ratio > dy )
      dy = dx * ratio;
    else
      dx = dy / ratio;

    final double mx = (maxX + minX) / 2d;
    final double my = (maxY + minY) / 2d;

    return GeometryFactory.createGM_Envelope( mx - dx, my - dy, mx + dx, my + dy );
  }

}
