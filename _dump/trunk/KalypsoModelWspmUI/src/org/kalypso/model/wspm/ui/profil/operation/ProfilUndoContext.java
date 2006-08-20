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
package org.kalypso.model.wspm.ui.profil.operation;

import org.eclipse.core.commands.operations.IUndoContext;
import org.kalypso.model.wspm.core.profil.IProfil;


/**
 * UndoContext f�r Profil-Operationen.
 * Ein ProfilUndoContext passt (matches) einen anderen, wenn
 * beide das gleiche Profil repr�sentieren.
 * 
 * @author Belger
 *
 */
public class ProfilUndoContext implements IUndoContext
{
  private final IProfil m_profil;

  public ProfilUndoContext( final IProfil profil )
  {
    m_profil = profil;
  }

  /**
   * @see org.eclipse.core.commands.operations.IUndoContext#getLabel()
   */
  public String getLabel( )
  {
    return "ProfilUndoContext";
  }

  /**
   * @see org.eclipse.core.commands.operations.IUndoContext#matches(org.eclipse.core.commands.operations.IUndoContext)
   */
  public boolean matches( final IUndoContext context )
  {
    return context instanceof ProfilUndoContext && m_profil == ((ProfilUndoContext)context).m_profil;
  }
}
