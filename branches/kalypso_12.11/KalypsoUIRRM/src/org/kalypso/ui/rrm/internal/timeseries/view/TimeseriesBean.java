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
package org.kalypso.ui.rrm.internal.timeseries.view;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.joda.time.Period;
import org.kalypso.commons.time.PeriodUtils;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;

/**
 * @author Gernot Belger
 */
public class TimeseriesBean extends FeatureBean<ITimeseries>
{
  private static final Status STATUS_DATA_FILE_MISSING = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString( "TimeseriesBean_0" ) ); //$NON-NLS-1$

  private static final Status STATUS_LINK_NOT_SET = new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), Messages.getString( "TimeseriesBean_1" ) ); //$NON-NLS-1$

  static final String PROPERTY_DATA_STATUS = "dataStatus"; //$NON-NLS-1$

  static String PROPERTY_PERIOD_TEXT = "periodText"; //$NON-NLS-1$

  public TimeseriesBean( )
  {
    super( ITimeseries.FEATURE_TIMESERIES );
  }

  public TimeseriesBean( final ITimeseries timeseries )
  {
    super( timeseries );
  }

  public String getPeriodText( )
  {
    final ITimeseries timeseries = getFeature();
    if( timeseries == null )
      return null;

    final Period timestep = timeseries.getTimestep();
    return PeriodUtils.formatDefault( timestep );
  }

  public IStatus getDataStatus( )
  {
    final ITimeseries timeseries = getFeature();
    if( timeseries == null )
      return STATUS_LINK_NOT_SET;

    final ZmlLink dataLink = timeseries.getDataLink();

    if( !dataLink.isLinkSet() )
      return STATUS_LINK_NOT_SET;

    if( !dataLink.isLinkExisting() )
      return STATUS_DATA_FILE_MISSING;

// try
// {
    return Status.OK_STATUS;

// final IObservation observation = dataLink.loadObservation();
// final IAxis[] axes = observation.getAxes();
//
// /* Parameter type */
// final String parameterType = timeseries.getParameterType();
// final IAxis parameterAxis = AxisUtils.findAxis( axes, parameterType );
// if( parameterAxis == null )
// {
// final String axisMessage = String.format( "Value axis for parameter type '%s' is missing.", parameterType );
// return new StatusWithEquals( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), axisMessage );
// }
//
// /* Timestep */
// final Period timestep = timeseries.getTimestep();
// final Period obsTimestep = MetadataHelper.getTimestep( observation.getMetadataList() );
//
// if( !ObjectUtils.equals( timestep, obsTimestep ) )
// {
// final PeriodFormatter formatter = PeriodFormat.wordBased( Locale.getDefault() );
// final String timestepText = formatter.print( timestep );
// final String obsTimestepText = formatter.print( obsTimestep );
//
// final String timestepMessage = String.format(
// "Inconsistent timestep definition: data file: '%s', timeseries definition: '%s'", obsTimestepText, timestepText );
// return new StatusWithEquals( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), timestepMessage );
// }
//
// return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), "Valid" );
// }
// catch( final SensorException e )
// {
// e.printStackTrace();
// final String eText = e.toString();
//
// // REMARK: we do NOT give the exception into the status, as exception do not compare with equals.
// // Else, we get a StackOverflow here
// final String message = String.format( "Failed to access data file: %s", eText );
//
// return new StatusWithEquals( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), message );
// }
  }

}