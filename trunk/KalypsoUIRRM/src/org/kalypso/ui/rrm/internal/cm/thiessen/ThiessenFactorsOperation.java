/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ui.rrm.internal.cm.thiessen;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.wizard.IWizardPage;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.core.layoutwizard.ILayoutWizardPage;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.cm.view.CatchmentBean;
import org.kalypso.ui.rrm.internal.cm.view.FactorizedTimeseriesBean;
import org.kalypso.ui.rrm.internal.cm.view.LinearSumBean;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Calculates the thiessen factors from the chosen timeseries and the available catchments.
 *
 * @author Gernot Belger
 */
public class ThiessenFactorsOperation implements ICoreRunnableWithProgress
{
  private final IStatusCollector m_log = new StatusCollector( KalypsoUIRRMPlugin.getID() );

  private final IWizardPage[] m_pages;

  private final LinearSumBean m_generator;

  public ThiessenFactorsOperation( final IWizardPage[] pages, final LinearSumBean generator )
  {
    m_pages = pages;
    m_generator = generator;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    final CatchmentBean[] catchments = m_generator.getCatchments();

    monitor.beginTask( "Calculate thiessen factors", m_pages.length + catchments.length + 1 );

    /* Save pages */
    monitor.subTask( "saving wizard data..." );
    for( final IWizardPage wizardPage : m_pages )
    {
      if( wizardPage instanceof ILayoutWizardPage )
        ((ILayoutWizardPage) wizardPage).saveData( true, new SubProgressMonitor( monitor, 1 ) );
      else
        monitor.worked( 1 );
    }

    /* Get thiessen polgons */
    monitor.subTask( "loading data for thiessen factors..." );
    final TimeseriesThiessenPolygons thiessenPolygons = new TimeseriesThiessenPolygons();
    thiessenPolygons.loadData();
    monitor.worked( 1 );

    /* calculate factors */
    for( final CatchmentBean catchment : catchments )
    {
      calculateThiessenFactors( catchment, thiessenPolygons );
      monitor.worked( 1 );
    }

    return Status.OK_STATUS;
  }

  private void calculateThiessenFactors( final CatchmentBean bean, final TimeseriesThiessenPolygons thiessenPolygons )
  {
    final Polygon[] polygons = thiessenPolygons.getThiessenPolygons();
    final String[] timeseries = thiessenPolygons.getTimeseries();

    try
    {
      bean.clearAllWeights();

      /* calculate weights */
      final GM_Surface< ? > catchmentArea = bean.getCatchmentArea();
      if( catchmentArea == null )
      {
        System.out.println( "sososo" );
      }

      final Geometry catchmentPolygon = JTSAdapter.export( catchmentArea );

      final double[] weights = JTSUtilities.fractionAreasOf( catchmentPolygon, polygons );

      /* apply weights to bean */
      for( int i = 0; i < weights.length; i++ )
      {
        final double weight = weights[i];
        final int factor = (int) Math.round( weight * 100.0 );

        final String href = timeseries[i];

        final FactorizedTimeseriesBean factorizedTimeseries = bean.getTimeseries( href );
        if( factorizedTimeseries != null )
          factorizedTimeseries.setFactor( factor );
      }
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      final String name = bean.getCatchmentName();
      final String description = bean.getCatchmentDescription();
      m_log.add( IStatus.ERROR, "Failed to calculate thiessen weights for catchment '%s (%s)'", e, name, description );
    }
  }
}