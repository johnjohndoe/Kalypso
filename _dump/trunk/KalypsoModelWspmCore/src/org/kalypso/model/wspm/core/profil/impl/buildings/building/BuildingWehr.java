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
package org.kalypso.model.wspm.core.profil.impl.buildings.building;

import java.util.Arrays;
import java.util.LinkedList;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_TYP;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.impl.PlainProfil;


public class BuildingWehr extends AbstractProfilBuilding
{

  public BuildingWehr( )
  {
    super( BUILDING_TYP.WEHR, Arrays.asList( BUILDING_PROPERTY.WEHRART, BUILDING_PROPERTY.FORMBEIWERT ), new POINT_PROPERTY[] { POINT_PROPERTY.OBERKANTEWEHR } );
    m_buildingValues.put( BUILDING_PROPERTY.WEHRART, IProfil.WEHR_TYP.Beiwert );
  }

  @Override
  public void addProfilProperties( PlainProfil profil ) throws ProfilDataException
  {
    super.addProfilProperties( profil );
    final LinkedList<IProfilPoint> points = profil.getPoints();
    for( IProfilPoint pt : points )
    {

      final double h = pt.getValueFor( POINT_PROPERTY.HOEHE );
      pt.setValueFor( POINT_PROPERTY.OBERKANTEWEHR, h );

    }
  }

  @Override
  public void removeProfilProperties( PlainProfil profil ) throws ProfilDataException
  {
    super.removeProfilProperties( profil );
    final IProfilDevider[] devider = profil.getDevider( DEVIDER_TYP.WEHR );
    final int deviderCount = devider == null ? 0 : devider.length;
    for( int i = 0; i < deviderCount; i++ )
    {
      profil.removeDevider( devider[i] );
    }
  }

}
