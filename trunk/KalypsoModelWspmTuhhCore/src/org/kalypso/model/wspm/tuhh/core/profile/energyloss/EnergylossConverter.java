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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.core.profil.IProfileMetadata;

/**
 * @author Holger Albert
 */
public class EnergylossConverter
{
  private final IProfileMetadata m_metadata;

  public EnergylossConverter( final IProfileMetadata metadata )
  {
    m_metadata = metadata;
  }

  public EnergylossData createEnergylossData( )
  {
    final Map<Integer, Energyloss> energylossesMap = new TreeMap<>();

    final String[] keys = m_metadata.getKeys();
    for( final String key : keys )
    {
      if( key.startsWith( EnergylossProfileObject.KEY_TYPE ) )
      {
        final int index = findIndex( key );
        if( index == -1 )
          continue;

        final Integer bigIndex = new Integer( index );
        final String type = m_metadata.getMetadata( key );

        final Energyloss energyloss = energylossesMap.get( bigIndex );
        if( energyloss != null )
          energylossesMap.put( bigIndex, energyloss.setType( type ) );
        else
          energylossesMap.put( bigIndex, new Energyloss( type, null, null ) );

        continue;
      }

      if( key.startsWith( EnergylossProfileObject.KEY_DESCRIPTION ) )
      {
        final int index = findIndex( key );
        if( index == -1 )
          continue;

        final Integer bigIndex = new Integer( index );
        final String description = m_metadata.getMetadata( key );

        final Energyloss energyloss = energylossesMap.get( bigIndex );
        if( energyloss != null )
          energylossesMap.put( bigIndex, energyloss.setDescription( description ) );
        else
          energylossesMap.put( bigIndex, new Energyloss( null, description, null ) );

        continue;
      }

      if( key.startsWith( EnergylossProfileObject.KEY_VALUE ) )
      {
        final int index = findIndex( key );
        if( index == -1 )
          continue;

        final Integer bigIndex = new Integer( index );
        final BigDecimal value = getBigDecimalValue( m_metadata, key );

        final Energyloss energyloss = energylossesMap.get( bigIndex );
        if( energyloss != null )
          energylossesMap.put( bigIndex, energyloss.setValue( value ) );
        else
          energylossesMap.put( bigIndex, new Energyloss( null, null, value ) );

        continue;
      }
    }

    final List<Energyloss> energylossesList = new ArrayList<>();

    final Integer[] indexes = energylossesMap.keySet().toArray( new Integer[] {} );
    for( final Integer index : indexes )
    {
      final Energyloss energyloss = energylossesMap.get( index );
      energylossesList.add( energyloss );
    }

    return new EnergylossData( energylossesList );
  }

  private int findIndex( final String key )
  {
    final String[] split = key.split( "_" ); //$NON-NLS-1$
    final String lastSplit = split[split.length - 1];
    final int index = NumberUtils.parseQuietInt( lastSplit, -1 );
    return index;
  }

  private BigDecimal getBigDecimalValue( final IProfileMetadata metadata, final String key )
  {
    final String value = metadata.getMetadata( key );
    if( value == null )
      return null;

    try
    {
      return NumberUtils.parseBigDecimal( value );
    }
    catch( final NumberFormatException e )
    {
      // Ignore
      return null;
    }
  }
}