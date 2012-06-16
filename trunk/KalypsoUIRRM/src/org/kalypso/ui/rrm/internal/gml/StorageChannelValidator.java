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
package org.kalypso.ui.rrm.internal.gml;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.binding.model.channels.StorageChannel;
import org.kalypso.model.hydrology.binding.model.nodes.INode;
import org.kalypso.model.hydrology.gml.ZmlWQVInlineTypeHandler;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class StorageChannelValidator
{
  private final StorageChannel m_channel;

  private final IObservation m_wvqObservation;

  private final IAxis[] m_axes;

  private final IStatusCollector m_log;

  public StorageChannelValidator( final StorageChannel channel )
  {
    m_channel = channel;
    m_wvqObservation = channel.getWVQObservation();
    m_axes = m_wvqObservation == null ? null : m_wvqObservation.getAxes();
    m_log = new StatusCollector( KalypsoUIRRMPlugin.getID() );
  }

  public IStatus validate( )
  {
    if( m_wvqObservation == null )
      m_log.add( IStatus.ERROR, Messages.getString( "StorageChannelValidator.0" ) ); //$NON-NLS-1$
    else
    {
      validateWaterlevel();
      validateVolume();
      validateDischarge();
      validateOutlet( m_channel.getOutletNode1(), ZmlWQVInlineTypeHandler.AXIS_NAME_ABGABE_1, Messages.getString( "StorageChannelValidator_0" ) ); //$NON-NLS-1$
      validateOutlet( m_channel.getOutletNode2(), ZmlWQVInlineTypeHandler.AXIS_NAME_ABGABE_2, Messages.getString( "StorageChannelValidator_1" ) ); //$NON-NLS-1$
    }

    return m_log.asMultiStatusOrOK( Messages.getString( "StorageChannelValidator_2" ) ); //$NON-NLS-1$
  }

  private void validateWaterlevel( )
  {
    final IAxis axisNN = ObservationUtilities.findAxisByTypeNoEx( m_axes, ITimeseriesConstants.TYPE_NORMNULL );
    validateRequiredAxis( axisNN, ITimeseriesConstants.TYPE_NORMNULL );
  }

  private void validateVolume( )
  {
    final IAxis axisV = ObservationUtilities.findAxisByTypeNoEx( m_axes, ITimeseriesConstants.TYPE_VOLUME );
    validateRequiredAxis( axisV, ITimeseriesConstants.TYPE_VOLUME );
  }

  private void validateDischarge( )
  {
    final IAxis axisQ = ObservationUtilities.findAxisByNameNoEx( m_axes, ZmlWQVInlineTypeHandler.AXIS_NAME_ABFLUSS );
    validateRequiredAxis( axisQ, ZmlWQVInlineTypeHandler.AXIS_NAME_ABFLUSS );
  }

  private void validateRequiredAxis( final IAxis axis, final String axisName )
  {
    final boolean hasWaterlevelValues = findValues( m_wvqObservation, axis );
    if( !hasWaterlevelValues || axis == null )
    {
      final String msg = String.format( Messages.getString( "StorageChannelValidator_3" ), axisName ); //$NON-NLS-1$
      m_log.add( IStatus.ERROR, msg );
    }
  }

  private void validateOutlet( final INode outletNode, final String axisName, final String label )
  {
    final boolean hasOutletNode1 = outletNode != null;
    final IAxis axisQ2 = ObservationUtilities.findAxisByNameNoEx( m_axes, axisName );
    final boolean hasOutletValues1 = findValues( m_wvqObservation, axisQ2 );

    if( hasOutletNode1 && !hasOutletValues1 )
    {
      final String message = String.format( Messages.getString( "StorageChannelValidator_4" ), label ); //$NON-NLS-1$
      m_log.add( IStatus.WARNING, message );
    }

    if( !hasOutletNode1 && hasOutletValues1 )
    {
      final String message = String.format( Messages.getString( "StorageChannelValidator_5" ), label ); //$NON-NLS-1$
      m_log.add( IStatus.WARNING, message );
    }
  }

  private boolean findValues( final IObservation wvqObservation, final IAxis axis )
  {
    try
    {
      final HasValuesVisitor visitor = new HasValuesVisitor( axis );
      wvqObservation.accept( visitor, null, 1 );
      return visitor.hasValues();
    }
    catch( final SensorException e )
    {
      e.printStackTrace();
      return false;
    }
  }
}