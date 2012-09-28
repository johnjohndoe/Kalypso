/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.core.profile.energyloss;

import java.util.List;

/**
 * Data container for maintaining the metadata of {@link EnergylossProfileObject}s.
 * 
 * @author Holger Albert
 */
public class EnergylossData
{
  private final List<Energyloss> m_energylosses;

  public EnergylossData( final List<Energyloss> energylosses )
  {
    m_energylosses = energylosses;
  }

  public Energyloss[] getEnergylosses( )
  {
    return m_energylosses.toArray( new Energyloss[] {} );
  }

  public int getSize( )
  {
    return m_energylosses.size();
  }

  public Energyloss getEnergyloss( final int index )
  {
    if( index < 0 || index >= m_energylosses.size() )
      return null;

    return m_energylosses.get( index );
  }

  public void addEnergyloss( final Energyloss energyloss )
  {
    m_energylosses.add( energyloss );
  }

  public void addEnergylosses( final List<Energyloss> energyloss )
  {
    m_energylosses.addAll( energyloss );
  }

  public void setEnergyloss( final int index, final Energyloss energyloss )
  {
    if( index < 0 || index >= m_energylosses.size() )
      return;

    m_energylosses.set( index, energyloss );
  }

  public void removeEnergyloss( final int index )
  {
    if( index < 0 || index >= m_energylosses.size() )
      return;

    m_energylosses.remove( index );
  }
}