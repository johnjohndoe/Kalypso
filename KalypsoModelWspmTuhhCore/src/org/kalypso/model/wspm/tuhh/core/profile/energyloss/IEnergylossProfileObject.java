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
package org.kalypso.model.wspm.tuhh.core.profile.energyloss;

import java.util.List;

import org.kalypso.model.wspm.core.profil.IProfileObject;

/**
 * @author Kim Werner
 * @author Holger Albert
 */
public interface IEnergylossProfileObject extends IProfileObject
{
  public static final String KEY_TYPE = "ENERGYLOSS_TYPE"; //$NON-NLS-1$

  public static final String KEY_DESCRIPTION = "ENERGYLOSS_DESCRIPTION"; //$NON-NLS-1$

  public static final String KEY_VALUE = "ENERGYLOSS_VALUE"; //$NON-NLS-1$

  public Energyloss[] getEnergylosses( );

  public int getSize( );

  public Energyloss getEnergyloss( final int index );

  public void addEnergyloss( final Energyloss energyloss );

  public void addEnergylosses( final List<Energyloss> energyloss );

  public void setEnergyloss( final int index, final Energyloss energyloss );

  public void removeEnergyloss( final int index );
}