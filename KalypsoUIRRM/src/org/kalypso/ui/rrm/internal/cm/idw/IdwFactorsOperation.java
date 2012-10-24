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
package org.kalypso.ui.rrm.internal.cm.idw;

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
import org.kalypso.model.rcm.util.InverseDistanceUtilities;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.cm.LinearSumHelper;
import org.kalypso.ui.rrm.internal.cm.view.CatchmentBean;
import org.kalypso.ui.rrm.internal.cm.view.LinearSumBean;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;

/**
 * Calculates the idw factors from the chosen timeseries and the available catchments.
 * 
 * @author Holger Albert
 */
public class IdwFactorsOperation implements ICoreRunnableWithProgress
{
  /**
   * The status log.
   */
  private final IStatusCollector m_log = new StatusCollector( KalypsoUIRRMPlugin.getID() );

  private final IWizardPage[] m_pages;

  private final LinearSumBean m_generator;

  private final Integer m_maxNumberStations;

  public IdwFactorsOperation( final Integer maxNumberStations, final IWizardPage[] pages, final LinearSumBean generator )
  {
    m_maxNumberStations = maxNumberStations;
    m_pages = pages;
    m_generator = generator;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    if( m_maxNumberStations == null || m_maxNumberStations.intValue() == 0 )
      throw new CoreException( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString( "IdwFactorsOperation_6" ) ) ); //$NON-NLS-1$

    /* Get the catchments. */
    final CatchmentBean[] catchments = m_generator.getCatchments();

    /* Monitor. */
    monitor.beginTask( Messages.getString( "IdwFactorsOperation_1" ), m_pages.length + catchments.length + 1 ); //$NON-NLS-1$
    monitor.subTask( Messages.getString( "IdwFactorsOperation_2" ) ); //$NON-NLS-1$

    /* Save pages. */
    for( final IWizardPage wizardPage : m_pages )
    {
      if( wizardPage instanceof ILayoutWizardPage )
        ((ILayoutWizardPage) wizardPage).saveData( true, new SubProgressMonitor( monitor, 1 ) );
      else
        monitor.worked( 1 );
    }

    /* Monitor. */
    monitor.subTask( Messages.getString( "IdwFactorsOperation_3" ) ); //$NON-NLS-1$

    /* Load the idw data. */
    final TimeseriesIdwStations idwStations = new TimeseriesIdwStations();
    idwStations.loadData();

    /* Monitor. */
    monitor.worked( 1 );
    monitor.subTask( Messages.getString( "IdwFactorsOperation_4" ) ); //$NON-NLS-1$

    /* Calculate the factors. */
    for( final CatchmentBean catchment : catchments )
    {
      /* Calculate the idw factors. */
      calculateIdwFactors( catchment, idwStations, m_maxNumberStations );

      /* Monitor. */
      monitor.worked( 1 );
    }

    return Status.OK_STATUS;
  }

  /**
   * This function calculates the idw factors for the given catchment.
   * 
   * @param catchment
   *          The catchment.
   * @param idwStations
   *          The idw stations.
   * @param maxNumberStations
   *          The maximal number of stations to use.
   */
  private void calculateIdwFactors( final CatchmentBean catchment, final TimeseriesIdwStations idwStations, final Integer maxNumberStations )
  {
    /* Get the stations and the timeseries. */
    final Point[] points = idwStations.getIdwStations();
    final String[] timeseries = idwStations.getTimeseries();

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
      final double[] weights = InverseDistanceUtilities.getWeights( catchmentPolygon, points, maxNumberStations );

      /* Apply the weights to the bean. */
      LinearSumHelper.apply( catchment, timeseries, weights );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      final String name = catchment.getCatchmentName();
      final String description = catchment.getCatchmentDescription();
      m_log.add( IStatus.ERROR, Messages.getString( "IdwFactorsOperation_5" ), e, name, description ); //$NON-NLS-1$
    }
  }
}