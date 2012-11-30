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
package org.kalypso.ui.rrm.internal.timeseries.view.imports;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.TimeZone;

import org.apache.commons.lang3.Range;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.metadata.MetadataHelper;
import org.kalypso.ogc.sensor.timeseries.AxisUtils;
import org.kalypso.ogc.sensor.visitor.IObservationValueContainer;
import org.kalypso.ogc.sensor.visitor.IObservationVisitor;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;

public class ValidateRuecksprungVisitor implements IObservationVisitor
{
  private final Set<Range<Integer>> m_rueckspruenge = new LinkedHashSet<>();

  private final StatusCollector m_stati = new StatusCollector( KalypsoUIRRMPlugin.getID() );

  private Date m_lastDate;

  private int m_ptrRuecksprung = -1;

  @Override
  public void visit( final IObservationValueContainer container ) throws SensorException
  {
    final IAxis dateAxis = AxisUtils.findDateAxis( container.getAxes() );
    final Date date = (Date) container.get( dateAxis );

    if( Objects.isNotNull( m_lastDate ) )
    {
      if( date.before( m_lastDate ) )
      {
        if( m_ptrRuecksprung == -1 )
          m_ptrRuecksprung = container.getIndex();
      }
      else
      {
        if( m_ptrRuecksprung != -1 )
        {
          m_rueckspruenge.add( Range.between( m_ptrRuecksprung, container.getIndex() ) );
          m_ptrRuecksprung = -1;

          final SimpleDateFormat sdf = new SimpleDateFormat( Messages.getString( Messages.getString("ValidateRuecksprungVisitor.0") ) ); //$NON-NLS-1$
          final TimeZone timezone = MetadataHelper.getTimeZone( container.getMetaData(), KalypsoCorePlugin.getDefault().getTimeZone().getID() );
          sdf.setTimeZone( timezone );

          final String msg = String.format( Messages.getString("ValidateRuecksprungVisitor.1"), sdf.format( m_lastDate ), sdf.format( date ) ); //$NON-NLS-1$
          m_stati.add( IStatus.WARNING, msg );
        }

        m_lastDate = date;
      }

    }
    else
      m_lastDate = date;

  }

  public boolean hasRuecksprung( )
  {
    return !m_rueckspruenge.isEmpty();
  }

  public Date getLastDate( )
  {
    return m_lastDate;
  }

  public Set<Range<Integer>> getRueckspruenge( )
  {
    return m_rueckspruenge;
  }

  public IStatus getStatus( )
  {
    return m_stati.asMultiStatus( Messages.getString("ValidateRuecksprungVisitor.2") ); //$NON-NLS-1$
  }
}
