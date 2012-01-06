/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.tuhh.core.profile.export.knauf.printer;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;

import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.beans.KnaufSA12Bean;

/**
 * @author Dirk Kuch
 */
public class KnaufSA12Printer extends AbstractKnaufPrinter
{

  public KnaufSA12Printer( final KnaufSA12Bean bean )
  {
    super( bean );
  }

  @Override
  protected KnaufSA12Bean getBean( )
  {
    return (KnaufSA12Bean) super.getBean();
  }

  @Override
  protected int getMaxRowSize( )
  {
    return 62;
  }

  @Override
  protected String getContent( )
  {
    final DateFormat sdf = new SimpleDateFormat( "dd.MM.yyyy HH:mm" );
    final Calendar calendar = Calendar.getInstance( KalypsoCorePlugin.getDefault().getTimeZone() );

    return String.format( "Exportiert am: %s", sdf.format( calendar.getTime() ) );
  }

}
