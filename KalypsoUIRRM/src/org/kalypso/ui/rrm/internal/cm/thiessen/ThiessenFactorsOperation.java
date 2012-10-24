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
import org.kalypso.ui.rrm.internal.cm.LinearSumHelper;
import org.kalypso.ui.rrm.internal.cm.view.CatchmentBean;
import org.kalypso.ui.rrm.internal.cm.view.LinearSumBean;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Polygon;
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

    monitor.beginTask( Messages.getString( "ThiessenFactorsOperation_0" ), m_pages.length + catchments.length + 1 ); //$NON-NLS-1$

    /* Save pages */
    monitor.subTask( Messages.getString( "ThiessenFactorsOperation_1" ) ); //$NON-NLS-1$
    for( final IWizardPage wizardPage : m_pages )
    {
      if( wizardPage instanceof ILayoutWizardPage )
        ((ILayoutWizardPage) wizardPage).saveData( true, new SubProgressMonitor( monitor, 1 ) );
      else
        monitor.worked( 1 );
    }

    /* Get thiessen polgons. */
    monitor.subTask( Messages.getString( "ThiessenFactorsOperation_2" ) ); //$NON-NLS-1$
    final TimeseriesThiessenPolygons thiessenPolygons = new TimeseriesThiessenPolygons();
    thiessenPolygons.loadData();
    monitor.worked( 1 );

    /* Calculate factors. */
    for( final CatchmentBean catchment : catchments )
    {
      calculateThiessenFactors( catchment, thiessenPolygons );
      monitor.worked( 1 );
    }

    return Status.OK_STATUS;
  }

  private void calculateThiessenFactors( final CatchmentBean catchment, final TimeseriesThiessenPolygons thiessenPolygons )
  {
    /* Get the polygons and the timeseries. */
    final Polygon[] polygons = thiessenPolygons.getThiessenPolygons();
    final String[] timeseries = thiessenPolygons.getTimeseries();

    try
    {
      /* Clear all old weights. */
      catchment.clearAllWeights();

      /* Get the area of the catchment. */
      final GM_Polygon catchmentArea = catchment.getCatchmentArea();
      if( catchmentArea == null )
        System.out.println( "sososo" ); //$NON-NLS-1$

      /* Convert to JTS geometry. */
      final Geometry catchmentPolygon = JTSAdapter.export( catchmentArea );

      /* Calculate weights. */
      final double[] weights = JTSUtilities.fractionAreasOf( catchmentPolygon, polygons );

      /* Apply the weights to the bean. */
      LinearSumHelper.apply( catchment, timeseries, weights );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      final String name = catchment.getCatchmentName();
      final String description = catchment.getCatchmentDescription();
      m_log.add( IStatus.ERROR, Messages.getString( "ThiessenFactorsOperation_4" ), e, name, description ); //$NON-NLS-1$
    }
  }
}