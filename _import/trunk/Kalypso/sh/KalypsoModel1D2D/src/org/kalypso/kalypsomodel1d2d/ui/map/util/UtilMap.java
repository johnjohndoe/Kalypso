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

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ui.views.map.MapView;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Provides map oriented utility methods.
 * 
 * @author Patrice Congo
 * @author Dejan Antanaskovic
 */
public class UtilMap
{

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
   * Get First Theme which is showing elements substituable to the specified QName (i.e. substituting it).
   */
  static public IKalypsoFeatureTheme findEditableTheme( final IMapModell mapModel, final QName editElementQName )
  {
    final List<IKalypsoFeatureTheme> loadedKalypsoFeatureThemes = loadKalypsoFeatureThemes( mapModel );
    for( final IKalypsoFeatureTheme theme : loadedKalypsoFeatureThemes )
    {
      final IFeatureType featureType = theme.getFeatureType();
      if( GMLSchemaUtilities.substitutes( featureType, editElementQName ) )
        return theme;
    }
    return null;
  }

  /**
   * Find a discretisation model within the mapModel themes.
   * 
   * @return The first discretisation model encountered in the list of themes.
   * 
   * @throws RuntimeException
   *             if model cannot be found
   */
  static public IFEDiscretisationModel1d2d findFEModelTheme( final IMapModell mapModel ) throws RuntimeException
  {
    final List<IKalypsoFeatureTheme> loadedKalypsoFeatureThemes = loadKalypsoFeatureThemes( mapModel );
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
    throw new RuntimeException( Messages.getString( "UtilMap.4" ) );
  }

  private static List<IKalypsoFeatureTheme> loadKalypsoFeatureThemes( final IMapModell mapModel )
  {
    final List<IKalypsoFeatureTheme> result = new ArrayList<IKalypsoFeatureTheme>();
    final ICoreRunnableWithProgress waitForFeaturesLoadingRunnable = waitForFeaturesLoading( mapModel );
    final IStatus waitForLoadingStatus = ProgressUtilities.busyCursorWhile( waitForFeaturesLoadingRunnable );
    if( waitForLoadingStatus.isOK() )
    {
      final IKalypsoTheme[] allThemes = mapModel.getAllThemes();
      for( int i = 0; i < allThemes.length; i++ )
      {
        final IKalypsoTheme kalypsoTheme = allThemes[i];
        if( kalypsoTheme instanceof IKalypsoFeatureTheme )
          result.add( (IKalypsoFeatureTheme) kalypsoTheme );
      }
    }
    return result;
  }

  /**
   * Method waits for all <code>IKalypsoFeatureTheme</code> objects from <code>mapModel</code> to be fully loaded
   * (not only themes to be assigned to the map, but also features to be loaded)
   * 
   * @param mapModel
   *            map model from which the themes should be loaded
   */
  private static ICoreRunnableWithProgress waitForFeaturesLoading( final IMapModell mapModel )
  {
    final List<IKalypsoFeatureTheme> result = new ArrayList<IKalypsoFeatureTheme>();
    final ICoreRunnableWithProgress waitForFeatureLoadingOperation = new ICoreRunnableWithProgress()
    {
      public IStatus execute( final IProgressMonitor monitor )
      {
        monitor.beginTask( "Warte auf Elemente laden...", IProgressMonitor.UNKNOWN );
        boolean loadingNotFinished = true;
        while( loadingNotFinished )
        {
          try
          {
            if( monitor.isCanceled() )
              return Status.CANCEL_STATUS;
            loadingNotFinished = false;
            Thread.sleep( 200 );
            monitor.worked( 10 );
            final IKalypsoTheme[] allThemes = mapModel.getAllThemes();
            for( final IKalypsoTheme theme : allThemes )
            {
              if( theme instanceof IKalypsoFeatureTheme )
              {
                final IKalypsoFeatureTheme ftheme = (IKalypsoFeatureTheme) theme;
                if( !result.contains( ftheme ) )
                  loadingNotFinished = true;
                final IFeatureType featureType = ftheme.getFeatureType();
                if( featureType != null )
                  result.add( ftheme );
              }
            }
          }
          catch( final InterruptedException e )
          {
            return StatusUtilities.statusFromThrowable( e );
          }
        }
        return Status.OK_STATUS;
      }
    };
    return waitForFeatureLoadingOperation;
  }

  /**
   * Convert the given bounding box into a {@link GM_Curve}
   */
  public static final GM_Curve toGM_Curve( final GM_Envelope bBox, final CS_CoordinateSystem crs )
  {
    Assert.throwIAEOnNullParam( bBox, "bBox" ); //$NON-NLS-1$
    Assert.throwIAEOnNullParam( crs, "crs" ); //$NON-NLS-1$

    // System.out.println("getting shape:"+feature);
    try
    {
      final GM_Position min = bBox.getMin();
      final GM_Position max = bBox.getMax();

      final double minx = min.getX();
      final double miny = min.getY();

      final double maxx = max.getX();
      final double maxy = max.getY();

      final double[] coords = new double[] { minx, miny, maxx, miny, maxx, maxy, minx, maxy, minx, miny, };
      final GM_Curve curve = GeometryFactory.createGM_Curve( coords, 2, crs );
      return curve;
    }
    catch( final Throwable e )
    {
      throw new RuntimeException( Messages.getString( "UtilMap.3" ), e ); //$NON-NLS-1$
    }
  }
}
