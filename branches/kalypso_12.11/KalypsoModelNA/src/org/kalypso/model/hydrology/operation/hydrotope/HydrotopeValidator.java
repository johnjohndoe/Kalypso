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
package org.kalypso.model.hydrology.operation.hydrotope;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.binding.HydrotopeCollection;
import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.IXLinkedFeature;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree_impl.model.sort.SpatialIndexExt;

/**
 * @author Gernot Belger
 */
public class HydrotopeValidator implements ICoreRunnableWithProgress
{
  private final String m_logLabel;

  private final HydrotopeCollection m_hydrotopes;

  private final NaModell m_naModel;

  public HydrotopeValidator( final HydrotopeCollection hydrotopes, final NaModell naModel, final String logLabel )
  {
    m_hydrotopes = hydrotopes;
    m_naModel = naModel;
    m_logLabel = logLabel;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final IStatusCollector log = new StatusCollector( ModelNA.PLUGIN_ID );

    final String taskName = Messages.getString("HydrotopeValidator_0"); //$NON-NLS-1$

    final SubMonitor progress = SubMonitor.convert( monitor, taskName, 110 );

    progress.subTask( Messages.getString("HydrotopeValidator_1") ); //$NON-NLS-1$
    final SpatialIndexExt hydrotopeIndex = buildOutputIndex();
    progress.worked( 10 );

    final HydrotopeCreationGeometryValidator validator = new HydrotopeCreationGeometryValidator( Messages.getString("HydrotopeValidator_2"), hydrotopeIndex ); //$NON-NLS-1$

    log.add( validator.checkGeometryCorrectness( progress.newChild( 33 ) ) );

    log.add( validator.checkSelfIntersection( progress.newChild( 33 ) ) );

    progress.subTask( Messages.getString("HydrotopeValidator_3") ); //$NON-NLS-1$
    log.add( checkAgainstCatchments() );
    progress.done();

    return log.asMultiStatus( m_logLabel );
  }

  private SpatialIndexExt buildOutputIndex( )
  {
    final IStatusCollector log = new StatusCollector( ModelNA.PLUGIN_ID );

    final IFeatureBindingCollection<IHydrotope> hydrotopes = m_hydrotopes.getHydrotopes();
    final SpatialIndexExt index = AbstractHydrotopeInput.buildIndex( hydrotopes, log );

    final IStatus status = log.asMultiStatus( Messages.getString("HydrotopeValidator_4") ); //$NON-NLS-1$
    ModelNA.getDefault().getLog().log( status );

    return index;
  }

  private IStatus checkAgainstCatchments( )
  {
    final IStatusCollector log = new StatusCollector( ModelNA.PLUGIN_ID );

    final Map<Catchment, Double> areaSums = new HashMap<>();

    /* Build sums */
    final IFeatureBindingCollection<IHydrotope> hydrotopes = m_hydrotopes.getHydrotopes();

    for( final IHydrotope hydrotope : hydrotopes )
    {
      final GM_MultiSurface geometry = hydrotope.getGeometry();
      final double area = geometry.getArea();
      final IXLinkedFeature catchmentLink = hydrotope.getCatchmentLink();
      final Catchment catchment = (Catchment) catchmentLink.getFeature();

      final Double oldSum = areaSums.get( catchment );
      if( oldSum == null )
        areaSums.put( catchment, area );
      else
        areaSums.put( catchment, oldSum + area );
    }

    /* Check sums */
    final IFeatureBindingCollection<Catchment> catchments = m_naModel.getCatchments();
    for( final Catchment catchment : catchments )
    {
      final Double sum = areaSums.get( catchment );
      if( sum == null )
        log.add( IStatus.WARNING, Messages.getString("HydrotopeValidator_5"), null, catchment.getName() ); //$NON-NLS-1$
      else
      {
        final double catchmentArea = catchment.getGeometry().getArea();
        final double difference = catchmentArea  - sum;

        final double differencePercent = (Math.abs( difference ) / catchmentArea) * 100;

        if( difference > FeatureListGeometryIntersector.MIN_AREA && differencePercent > 0.1 )
        {
          /* Catchment bigger than hydrotopes */
          log.add( IStatus.WARNING, Messages.getString("HydrotopeValidator_6"), null, catchment.getName(), differencePercent ); //$NON-NLS-1$
        }
        else if( difference < -FeatureListGeometryIntersector.MIN_AREA && differencePercent > 0.1 )
        {
          /* Catchment smaller than hydrotopes */
          log.add( IStatus.WARNING, Messages.getString("HydrotopeValidator_7"), null, catchment.getName(), differencePercent ); //$NON-NLS-1$
        }
      }
    }

    return log.asMultiStatus( Messages.getString("HydrotopeValidator_8") ); //$NON-NLS-1$
  }
}