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
 * @author Monika Thuel
 */
public interface CategoryConstants
{

  String PROPERTY_CATEGORY = "category"; //$NON-NLS-1$

  enum CATEGORY
  {
    // TODO i18n?
    // HACK: introduced Category.None, do not insert none into DB
    P(Messages.getString( "CategoryConstants.0" )), //$NON-NLS-1$
    S(Messages.getString( "CategoryConstants.1" )), //$NON-NLS-1$
    W(Messages.getString( "CategoryConstants.2" )), //$NON-NLS-1$
    A(Messages.getString( "CategoryConstants.3" )), //$NON-NLS-1$
    UK(Messages.getString( "CategoryConstants.4" )), //$NON-NLS-1$
    K(Messages.getString( "CategoryConstants.5" )), //$NON-NLS-1$
    EI(Messages.getString( "CategoryConstants.6" )), //$NON-NLS-1$
    MA(Messages.getString( "CategoryConstants.7" )), //$NON-NLS-1$
    AR(Messages.getString( "CategoryConstants.8" )), //$NON-NLS-1$
    HA(Messages.getString( "CategoryConstants.9" )), //$NON-NLS-1$
    OK(Messages.getString( "CategoryConstants.10" )), //$NON-NLS-1$
    NONE(Messages.getString( "CategoryConstants.11" )); //$NON-NLS-1$

    private final String m_label;

    CATEGORY( final String label )
    {
      m_label = label;
    }

    @Override
    public String toString( )
    {
      return m_label;
    }
  }

}
