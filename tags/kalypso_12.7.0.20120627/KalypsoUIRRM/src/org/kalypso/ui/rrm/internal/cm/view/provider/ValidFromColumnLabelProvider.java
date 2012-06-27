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
package org.kalypso.ui.rrm.internal.cm.view.provider;

import java.text.DateFormat;
import java.util.Date;

import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.model.rcm.binding.IRainfallGenerator;

/**
 * @author Holger Albert
 */
public class ValidFromColumnLabelProvider extends ColumnLabelProvider
{
  /**
   * The date format.
   */
  private final DateFormat m_df;

  /**
   * The constructor.
   */
  public ValidFromColumnLabelProvider( )
  {
    m_df = DateFormat.getDateTimeInstance( DateFormat.MEDIUM, DateFormat.SHORT );
    m_df.setTimeZone( KalypsoCorePlugin.getDefault().getTimeZone() );
  }

  /**
   * @see org.eclipse.jface.viewers.ColumnLabelProvider#getText(java.lang.Object)
   */
  @Override
  public String getText( final Object element )
  {
    if( element instanceof IRainfallGenerator )
    {
      final IRainfallGenerator generator = (IRainfallGenerator) element;
      final Date validFrom = generator.getValidFrom();
      if( validFrom == null )
        return null;

      return String.format( "%s", m_df.format( validFrom ) ); //$NON-NLS-1$
    }

    return super.getText( element );
  }
}