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
package org.kalypso.model.wspm.tuhh.core.wprof;

import org.kalypso.model.wspm.tuhh.core.i18n.Messages;

/**
 * @author Gernot Belger
 */
public enum WProfProfileType
{
  Gewässerprofil(2, Messages.getString( "WProfProfileType_0" )), //$NON-NLS-1$
  Brückenprofil(3, Messages.getString( "WProfProfileType_1" )), //$NON-NLS-1$
  Absturzprofil(4, Messages.getString( "WProfProfileType_2" )), //$NON-NLS-1$
  Wehrprofil(5, Messages.getString( "WProfProfileType_3" )), //$NON-NLS-1$
  Verdohlungsprofil(6, Messages.getString( "WProfProfileType_4" )), //$NON-NLS-1$
  LängsbegleitendeStruktur(7, Messages.getString( "WProfProfileType_5" )), //$NON-NLS-1$
  EinzelpunkteAusserhalbVonBauwerken(9, Messages.getString( "WProfProfileType_6" )), //$NON-NLS-1$
  SonstigesProfil(0, Messages.getString( "WProfProfileType_7" )); //$NON-NLS-1$

  private final int m_wspmId;

  private final String m_label;

  private WProfProfileType( final int wspmId, final String label )
  {
    m_wspmId = wspmId;
    m_label = label;
  }

  public String getLabel( )
  {
    return m_label;
  }

  public static WProfProfileType valueOf( final int type )
  {
    final WProfProfileType[] values = WProfProfileType.values();
    for( final WProfProfileType wprofType : values )
    {
      if( wprofType.m_wspmId == type )
        return wprofType;
    }

    return null;
  }

}
