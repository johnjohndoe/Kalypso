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
package org.kalypso.model.wspm.tuhh.core.profile.importer.wprof;

import org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint;

/**
 * @author Gernot Belger
 */
class ProfileDataAdderFactory
{
  private final ProfilePolygones m_polygones;

  public ProfileDataAdderFactory( final ProfilePolygones polygones )
  {
    m_polygones = polygones;
  }

  public IProfileDataAdder createDataAdder( )
  {
    final IWProfPoint[][] v0103Polies = m_polygones.findPoints( "V01", "V02", "V03", "V99" );
    if( v0103Polies != null )
      return new BridgeAdder( v0103Polies[0], v0103Polies[1], v0103Polies[2], v0103Polies[3] );

    final IWProfPoint[][] d0103Polies = m_polygones.findPoints( "D01", "D02", "D03" );
    if( v0103Polies != null )
      return new BridgeAdder( d0103Polies[0], d0103Polies[1], d0103Polies[2], null );

    final IWProfPoint[][] d0105Polies = m_polygones.findPoints( "D01", "D02", "D05" );
    if( d0105Polies != null )
      return new BridgeAdder( d0105Polies[0], d0105Polies[1], d0105Polies[2], null );

    final IWProfPoint[][] d9193Polies = m_polygones.findPoints( "D91", "D92", "D93" );
    if( d9193Polies != null )
      return new BridgeAdder( d9193Polies[0], d9193Polies[1], d9193Polies[2], null );

    final IWProfPoint[][] kreisPolies = m_polygones.findPoints( "V01", "K1", "V03", "V99" );
    if( kreisPolies != null )
      return new KreisAdder( kreisPolies[0], kreisPolies[1], kreisPolies[2], kreisPolies[3] );

    final IWProfPoint[][] gelaendePolies = m_polygones.findPoints( "21" );
    if( gelaendePolies != null )
      return new GelaendeAdder( gelaendePolies[0] );

    return null;
  }

}
