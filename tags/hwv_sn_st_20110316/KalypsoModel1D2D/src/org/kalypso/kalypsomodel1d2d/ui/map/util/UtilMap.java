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
package org.kalypso.kalypsomodel1d2d.ui.map.util;

import java.awt.Graphics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemePredicate;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemeVisitor;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeVisitor;
import org.kalypso.ui.views.map.MapView;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * Provides map oriented utility methods.
 *
 * @author Patrice Congo
 * @author Dejan Antanaskovic
 */
public class UtilMap
{

  /* predicate for kalypso feature themes */
  private static final IKalypsoThemePredicate PREDICATE = new IKalypsoThemePredicate()
  {
    @Override
    public boolean decide( final IKalypsoTheme theme )
    {
      if( !(theme instanceof IKalypsoFeatureTheme) )
        return false;

      return true;
    }
  };

  /**
   * To get the map view. The view with the ID {@link MapView#ID} in the active workbench page is returned.
   *
   * @return a {@link IViewPart} representing the map view in the active workbench
   */
  public static final IViewPart getMapView( )
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    if( workbench == null )
      return null;
    final IWorkbenchWindow activeWorkbenchWindow = workbench.getActiveWorkbenchWindow();
    if( activeWorkbenchWindow == null )
      return null;
    final IWorkbenchPage activePage = activeWorkbenchWindow.getActivePage();
    if( activePage == null )
      return null;
    final IViewPart findView = activePage.findView( MapView.ID );
    return findView;
  }

  /**
   * Get First Theme which is showing elements substitutable to the specified QName (i.e. substituting it).
   */
  static public IKalypsoFeatureTheme findEditableTheme( final IMapPanel panel, final QName editElementQName )
  {
    final List<IKalypsoFeatureTheme> loadedKalypsoFeatureThemes = loadKalypsoFeatureThemes( panel );
    for( final IKalypsoFeatureTheme theme : loadedKalypsoFeatureThemes )
    {
      final IFeatureType featureType = theme.getFeatureType();
      if( featureType != null && GMLSchemaUtilities.substitutes( featureType, editElementQName ) )
        return theme;
    }
    return null;
  }

  /**
   * Find a discretisation model within the mapModel themes.
   *
   * @return The first discretisation model encountered in the list of themes.
   * @throws RuntimeException
   *           if model cannot be found
   */
  static public IFEDiscretisationModel1d2d findFEModelTheme( final IMapPanel panel ) throws RuntimeException
  {
    final List<IKalypsoFeatureTheme> loadedKalypsoFeatureThemes = loadKalypsoFeatureThemes( panel );
    for( final IKalypsoFeatureTheme theme : loadedKalypsoFeatureThemes )
    {
      final FeatureList featureList = theme.getFeatureList();
      final Feature modelFeature = featureList == null ? null : featureList.getParentFeature();
      if( modelFeature != null )
      {
        final IFEDiscretisationModel1d2d model = (IFEDiscretisationModel1d2d) modelFeature.getAdapter( IFEDiscretisationModel1d2d.class );
        if( model != null )
          return model;
      }
    }
    throw new RuntimeException( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap.4" ) ); //$NON-NLS-1$
  }

  private static List<IKalypsoFeatureTheme> loadKalypsoFeatureThemes( final IMapPanel panel )
  {
    final List<IKalypsoFeatureTheme> result = new ArrayList<IKalypsoFeatureTheme>();

    // PROBLEM: block the ui and freezes the application
    // PlatformUI.getWorkbench().getDisplay().syncExec( waitForFeaturesLoading( mapModel ) );

    // TODO: check if always works
    final ICoreRunnableWithProgress operation = MapModellHelper.createWaitForMapOperation( panel );
    IStatus waitErrorStatus;
    try
    {
      waitErrorStatus = operation.execute( new NullProgressMonitor() );
    }
    catch( final Exception e )
    {
      waitErrorStatus = StatusUtilities.statusFromThrowable( e );
    }

    if( !waitErrorStatus.isOK() )
    {
      KalypsoModel1D2DPlugin.getDefault().getLog().log( waitErrorStatus );
      return null;
    }

    final KalypsoThemeVisitor kalypsoThemeVisitor = new KalypsoThemeVisitor( PREDICATE );
    panel.getMapModell().accept( kalypsoThemeVisitor, IKalypsoThemeVisitor.DEPTH_INFINITE );
    final IKalypsoTheme[] foundThemes = kalypsoThemeVisitor.getFoundThemes();
    for( final IKalypsoTheme kalypsoTheme2 : foundThemes )
      result.add( (IKalypsoFeatureTheme) kalypsoTheme2 );

    return result;
  }

  public static void drawHandles( final Graphics g, final int[] x, final int[] y )
  {
    final int sizeOuter = 6;
    for( int i = 0; i < y.length; i++ )
      g.drawRect( x[i] - sizeOuter / 2, y[i] - sizeOuter / 2, sizeOuter, sizeOuter );
  }

  /**
   * returns a point array for a given {@link IFE1D2DNode} and a {@link GM_Point}
   */
  public static int[][] getPointArrays( final Point currentPoint )
  {
    return getPointArrays( currentPoint, null, null );
  }

  public static int[][] getPointArrays( final Point currentPoint, final List<GM_Point> nodes, final GeoTransform projection )
  {
    final List<Integer> xArray = new ArrayList<Integer>();
    final List<Integer> yArray = new ArrayList<Integer>();

    if( nodes != null && projection != null )
    {
      for( final GM_Point point : nodes )
      {
        final int x = (int) projection.getDestX( point.getX() );
        final int y = (int) projection.getDestY( point.getY() );

        xArray.add( new Integer( x ) );
        yArray.add( new Integer( y ) );
      }
    }

    if( currentPoint != null )
    {
      xArray.add( currentPoint.x );
      yArray.add( currentPoint.y );
    }

    final int[] xs = ArrayUtils.toPrimitive( xArray.toArray( new Integer[xArray.size()] ) );
    final int[] ys = ArrayUtils.toPrimitive( yArray.toArray( new Integer[yArray.size()] ) );

    return new int[][] { xs, ys };
  }
}
