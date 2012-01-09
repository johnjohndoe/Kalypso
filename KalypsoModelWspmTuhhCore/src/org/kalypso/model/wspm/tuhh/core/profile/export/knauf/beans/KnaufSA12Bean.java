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
package org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;

import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.KnaufCalculation;

/**
 * Header
 * 
 * @author Dirk Kuch
 */
public class KnaufSA12Bean extends AbstractKnaufProjectBean
{
  private final KnaufCalculation m_calculation;

  public KnaufSA12Bean( final KnaufCalculation calculation )
  {
    m_calculation = calculation;
  }

  @Override
  public Integer getSatzart( )
  {
    return 12;
  }

  protected String getText( )
  {
    final DateFormat sdf = new SimpleDateFormat( "dd.MM.yyyy HH:mm" );
    final Calendar calendar = Calendar.getInstance( KalypsoCorePlugin.getDefault().getTimeZone() );

    return String.format( "Exportiert am: %s", sdf.format( calendar.getTime() ) );
  }

}
