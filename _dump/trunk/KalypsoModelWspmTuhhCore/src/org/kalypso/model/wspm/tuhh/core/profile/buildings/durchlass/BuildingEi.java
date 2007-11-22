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
package org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass;

import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;

/**
 * @author kimwerner
 */
public class BuildingEi extends AbstractProfilDurchlass
{
  public BuildingEi( )
  {
    super(IWspmTuhhConstants.BUILDING_TYP_EI,"Ei", new String[]{ IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X,
        IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y, IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE,
        IWspmTuhhConstants.BUILDING_PROPERTY_BREITE, IWspmTuhhConstants.BUILDING_PROPERTY_SOHLGEFAELLE,
        IWspmTuhhConstants.BUILDING_PROPERTY_RAUHEIT },new String[]{"Bezugspunkt X","Bezugspunkt Y","H�he","Breite","Sohlgef�lle","Rauheit"} );
  }

  
 
}
