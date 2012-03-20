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

import java.util.Locale;

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
import org.kalypso.model.rcm.binding.ILinearSumGenerator;
import org.kalypso.model.rcm.util.InverseDistanceUtilities;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.cm.view.CatchmentBean;
import org.kalypso.ui.rrm.internal.cm.view.FactorizedTimeseriesBean;
import org.kalypso.ui.rrm.internal.cm.view.LinearSumBean;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Surface;
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

  /**
   * The wizard pages.
   */
  private final IWizardPage[] m_pages;

  /**
   * The linear sum bean.
   */
  private final LinearSumBean m_generator;

  /**
   * The constructor.
   * 
   * @param pages
   *          The wizard pages.
   * @param generator
   *          The linear sum bean.
   */
  public IdwFactorsOperation( final IWizardPage[] pages, final LinearSumBean generator )
  {
    m_pages = pages;
    m_generator = generator;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    /* Get the catchments. */
    final CatchmentBean[] catchments = m_generator.getCatchments();

    /* Monitor. */
    monitor.beginTask( Messages.getString( "IdwFactorsOperation_0" ), m_pages.length + catchments.length + 1 ); //$NON-NLS-1$
    monitor.subTask( Messages.getString( "IdwFactorsOperation_1" ) ); //$NON-NLS-1$

    /* Save pages. */
    for( final IWizardPage wizardPage : m_pages )
    {
      if( wizardPage instanceof ILayoutWizardPage )
        ((ILayoutWizardPage) wizardPage).saveData( true, new SubProgressMonitor( monitor, 1 ) );
      else
        monitor.worked( 1 );
    }

    /* Monitor. */
    monitor.subTask( Messages.getString( "IdwFactorsOperation_2" ) ); //$NON-NLS-1$

    /* Load the idw data. */
    final TimeseriesIdwStations idwStations = new TimeseriesIdwStations();
    idwStations.loadData();

    /* Monitor. */
    monitor.worked( 1 );
    monitor.subTask( Messages.getString( "IdwFactorsOperation_3" ) ); //$NON-NLS-1$

    /* Calculate the factors. */
    for( final CatchmentBean catchment : catchments )
    {
      /* Calculate the idw factors. */
      calculateIdwFactors( catchment, idwStations );

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
   */
  private void calculateIdwFactors( final CatchmentBean catchment, final TimeseriesIdwStations idwStations ) throws CoreException
  {
    /* The maximal number of stations to use. */
    final Integer maxNumberStations = getMaxNumberStations();

    /* Get the stations and the timeseries. */
    final Point[] points = idwStations.getIdwStations();
    final String[] timeseries = idwStations.getTimeseries();

    try
    {
      /* Clear all old weights. */
      catchment.clearAllWeights();

      /* calculate weights */
      final GM_Surface< ? > catchmentArea = catchment.getCatchmentArea();
      if( catchmentArea == null )
        System.out.println( "sososo" ); //$NON-NLS-1$

      /* Convert to JTS geometry. */
      final Geometry catchmentPolygon = JTSAdapter.export( catchmentArea );

      /* Calculate weights. */
      final double[] weights = InverseDistanceUtilities.getWeights( catchmentPolygon, points, maxNumberStations );

      /* Apply the weights to the bean. */
      for( int i = 0; i < weights.length; i++ )
      {
        /* Get the factor. */
        final double weight = weights[i];
        final int factor = (int) Math.round( weight * 100.0 );

        /* Determine the href. */
        final String href = timeseries[i];

        /* Set the factor. */
        final FactorizedTimeseriesBean factorizedTimeseries = catchment.getTimeseries( href );
        if( factorizedTimeseries != null )
          factorizedTimeseries.setFactor( factor );
      }

      /* Set the comment. */
      m_generator.getFeature().setProperty( ILinearSumGenerator.PROPERTY_COMMENT, String.format( Locale.PRC, "Created by Inverse Distance Weighting Method (%d)", maxNumberStations ) );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      final String name = catchment.getCatchmentName();
      final String description = catchment.getCatchmentDescription();
      m_log.add( IStatus.ERROR, Messages.getString( "IdwFactorsOperation_4" ), e, name, description ); //$NON-NLS-1$
    }
  }

  private Integer getMaxNumberStations( ) throws CoreException
  {
    final ILayoutWizardPage wizardPage = (ILayoutWizardPage) m_pages[0];
    final IdwOptionsLayoutPart optionsPart = (IdwOptionsLayoutPart) wizardPage.findLayoutPart( "idwOptionsPart.1" );
    final Integer maxStations = optionsPart.getMaxStations();
    if( maxStations == null || maxStations.intValue() == 0 )
      throw new CoreException( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString( "IdwFactorsOperation_5" ) ) );

    return maxStations;
  }
}