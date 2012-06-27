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

import java.util.Calendar;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.model.hydrology.gml.ZmlWtKcLaiInlineTypeHandler;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTupleModel;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.view.observationDialog.AbstractObservationAction;
import org.kalypso.ogc.sensor.view.observationDialog.ObservationViewer;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class NewIdealLanduseAction extends AbstractObservationAction
{
  /**
   * We produce 13 values instead of 12....
   */
  private static final int MONTH_IN_YEAR = 13;

  private final ZmlWtKcLaiInlineTypeHandler m_typeHandler;

  public NewIdealLanduseAction( final ZmlWtKcLaiInlineTypeHandler typeHandler )
  {
    m_typeHandler = typeHandler;
  }

  @Override
  protected String getLabel( )
  {
    return Messages.getString( "org.kalypso.ogc.sensor.view.ObservationViewerDialog.6" ); //$NON-NLS-1$ 
  }

  @Override
  protected String getTooltip( )
  {
    return Messages.getString( "org.kalypso.ogc.sensor.view.ObservationViewerDialog.7" ); //$NON-NLS-1$ 
  }

  @Override
  protected IStatus execute( )
  {
    final ObservationViewer viewer = getViewer();
    final IAxis[] axis = m_typeHandler.createAxes();

    final String name = Messages.getString( "org.kalypso.ogc.sensor.view.ObservationViewerDialog.5" ); //$NON-NLS-1$

    // FIXME: validate this!

    final Calendar startDate = Calendar.getInstance();
    startDate.setTimeZone( KalypsoCorePlugin.getDefault().getTimeZone() );
    startDate.clear();
    startDate.set( 2000, 10, 15, 12, 0, 0 );

    final Object[][] values = new Object[MONTH_IN_YEAR][axis.length];

    for( int row = 0; row < MONTH_IN_YEAR; row++ )
    {
      values[row][0] = startDate.getTime();

      for( int ax = 1; ax < axis.length; ax++ )
        values[row][ax] = 0.0;

      startDate.add( Calendar.MONTH, 1 );
    }

    final ITupleModel model = new SimpleTupleModel( axis, values );
    viewer.setInput( new SimpleObservation( null, name, new MetadataList(), model ), viewer.getShow() );

    return Status.OK_STATUS;
  }
}