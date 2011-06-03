/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.pdb.ui.internal.content;

import java.text.DateFormat;
import java.util.Date;

import org.apache.commons.lang.StringUtils;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.model.wspm.pdb.db.mapping.State;

/**
 * @author Gernot Belger
 */
public class PdbMeasurementLabelProvider extends ColumnLabelProvider
{
  private final DateFormat m_format;

  public PdbMeasurementLabelProvider( )
  {
    m_format = DateFormat.getDateInstance( DateFormat.MEDIUM );
    m_format.setTimeZone( KalypsoCorePlugin.getDefault().getTimeZone() );
  }

  @Override
  public String getText( final Object element )
  {
    if( element instanceof State )
    {
      final State state = (State) element;
      final Date measurementDate = state.getMeasurementDate();
      if( measurementDate == null )
        return null;

      return m_format.format( measurementDate );
    }

    return StringUtils.EMPTY;
  }
}