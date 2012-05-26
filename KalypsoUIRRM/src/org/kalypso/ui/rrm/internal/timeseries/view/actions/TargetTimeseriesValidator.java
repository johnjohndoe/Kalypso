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
package org.kalypso.ui.rrm.internal.timeseries.view.actions;

import java.util.LinkedHashSet;
import java.util.Set;

import org.apache.commons.io.IOCase;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.joda.time.Period;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.operations.IImportTimeseriesOperationValidator;
import org.kalypso.ui.rrm.internal.timeseries.view.TimeseriesBean;
import org.kalypso.zml.ui.imports.ImportObservationData;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

import com.google.common.base.Strings;

/**
 * @author Dirk Kuch
 */
public class TargetTimeseriesValidator implements IImportTimeseriesOperationValidator
{

  private Period m_timestep;

  private final IStation m_station;

  private final ImportObservationData m_data;

  private final TimeseriesBean m_bean;

  public TargetTimeseriesValidator( final IStation station, final ImportObservationData data, final TimeseriesBean bean )
  {
    m_station = station;
    m_data = data;
    m_bean = bean;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final ITimeseries[] parameter = filterByParameterType( m_data.getParameterType() );
    final ITimeseries[] timestep = filterByTimeStep( parameter );
    final ITimeseries[] quality = filterByQuality( timestep );

    if( ArrayUtils.isNotEmpty( quality ) )
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString("TargetTimeseriesValidator_0") ); //$NON-NLS-1$

    return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), Messages.getString("TargetTimeseriesValidator_1") ); //$NON-NLS-1$
  }

  private ITimeseries[] filterByQuality( final ITimeseries[] timeserieses )
  {
    final String quality = getQuality();

    final Set<ITimeseries> found = new LinkedHashSet<>();
    for( final ITimeseries timeseries : timeserieses )
    {
      final String q = timeseries.getQuality();
      if( Strings.isNullOrEmpty( quality ) && Strings.isNullOrEmpty( q ) )
        found.add( timeseries );
      else if( !Strings.isNullOrEmpty( quality ) && Strings.isNullOrEmpty( q ) )
        continue;
      else if( Strings.isNullOrEmpty( quality ) && !Strings.isNullOrEmpty( q ) )
        continue;
      else if( IOCase.SYSTEM.checkEquals( quality, q ) )
        found.add( timeseries );

    }

    return found.toArray( new ITimeseries[] {} );
  }

  private String getQuality( )
  {
    final Object quality = m_bean.getProperty( ITimeseries.PROPERTY_QUALITY );

    return (String) quality;
  }

  private ITimeseries[] filterByTimeStep( final ITimeseries[] timeserieses )
  {
    final Set<ITimeseries> found = new LinkedHashSet<>();
    for( final ITimeseries timeseries : timeserieses )
    {
      if( m_timestep.equals( timeseries.getTimestep() ) )
        found.add( timeseries );

    }

    return found.toArray( new ITimeseries[] {} );
  }

  private ITimeseries[] filterByParameterType( final String parameterType )
  {
    final Set<ITimeseries> found = new LinkedHashSet<>();
    final IFeatureBindingCollection<ITimeseries> timeserieses = m_station.getTimeseries();
    for( final ITimeseries timeseries : timeserieses )
    {
      if( StringUtils.equals( timeseries.getParameterType(), parameterType ) )
        found.add( timeseries );
    }

    return found.toArray( new ITimeseries[] {} );
  }

  @Override
  public void setTimestep( final Period timestep )
  {
    m_timestep = timestep;

  }

}
