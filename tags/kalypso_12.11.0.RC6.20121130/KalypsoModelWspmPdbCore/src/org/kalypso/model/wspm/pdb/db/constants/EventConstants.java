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
package org.kalypso.model.wspm.pdb.db.constants;

import org.kalypso.model.wspm.pdb.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public interface EventConstants
{
  enum TYPE
  {
    /**
     * Default
     */
    Measurement( Messages.getString( "EventConstants.0" ) ), //$NON-NLS-1$
    Simulation( Messages.getString( "EventConstants.1" ) ); //$NON-NLS-1$

    private final String m_label;

    TYPE( final String label )
    {
      m_label = label;
    }

    @Override
    public String toString( )
    {
      return m_label;
    }
  }

  enum WL_TYPE
  {
    WL_1D( Messages.getString( "EventConstants.2" ) ), //$NON-NLS-1$
    WL_2D( Messages.getString( "EventConstants.3" ) ); //$NON-NLS-1$

    private final String m_label;

    WL_TYPE( final String label )
    {
      m_label = label;
    }

    @Override
    public String toString( )
    {
      return m_label;
    }
  }

  // FIXME
  int DESCRIPTION_LIMIT = 256;

  String PROPERTY_ID = "id"; //$NON-NLS-1$

  String PROPERTY_NAME = "name"; //$NON-NLS-1$

  String PROPERTY_DESCRIPTION = "description"; //$NON-NLS-1$

  String PROPERTY_SOURCE = "source"; //$NON-NLS-1$

  String PROPERTY_TYPE = "type"; //$NON-NLS-1$

  String PROPERTY_WL_TYPE = "wlType"; //$NON-NLS-1$

  String PROPERTY_WATER_BODY = "waterBody"; //$NON-NLS-1$

  String PROPERTY_STATE = "state"; //$NON-NLS-1$

  String PROPERTY_STYLE_ARRAY = "styleArray"; //$NON-NLS-1$
}