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
package org.kalypso.model.wspm.tuhh.core.wspwin.prf;

import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.HashMap;

import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSource;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

import au.com.bytecode.opencsv.CSVReader;

/**
 * @author kimwerner
 */
public class CsvSource implements IProfilSource
{
  private IProfilPointPropertyProvider m_provider = null;

  private final HashMap<String, TupleResult> m_ProfilesTable = new HashMap<String, TupleResult>();

  private String[] m_columns = null;

  private int getColumnIndex( final String id )
  {
    for( int i = 0; i < m_columns.length; i++ )
      if( id.equalsIgnoreCase( m_columns[i] ) ) //$NON-NLS-1$
        return i;
    return -1;
  }

  private IComponent getComponent( final String key )
  {
    if( "BREITE".equalsIgnoreCase( key ) ) //$NON-NLS-1$
      return m_provider.getPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    if( "HOEHE".equalsIgnoreCase( key ) ) //$NON-NLS-1$
      return m_provider.getPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    else
      return null;
  }

  private TupleResult getResult( final String id )
  {
    if( m_ProfilesTable.containsKey( id ) )
      return m_ProfilesTable.get( id );
    final TupleResult result = new TupleResult();
    for( final String column : m_columns )
    {
      final IComponent component = getComponent( column );
      if( component != null )
        result.addComponent( component );
    }
    m_ProfilesTable.put( id, result );
    return result;
  }

  private void extractDataBlocks( final CSVReader tableReader ) throws IOException
  {

    m_columns = tableReader.readNext();

    final int m_station = getColumnIndex( "STATION" ); //$NON-NLS-1$
    String[] values = tableReader.readNext();
    while( values != null )
    {
      if( values.length == m_columns.length )
      {
        final String key = m_station < 0 ? "-" : values[m_station]; //$NON-NLS-1$
        final TupleResult result = getResult( key );
        final IRecord record = result.createRecord();

        for( int i = 0; i < values.length; i++ )
        {
          final int index = result.indexOfComponent( getComponent( m_columns[i] ) );
          if( index < 0 )
            continue;
          final Double dbl = NumberUtils.parseQuietDouble( values[i] );
          if( dbl.isNaN() )
            record.setValue( index, values[i] );
          else
            record.setValue( index, dbl );
        }
        result.add( record );
      }
      values = tableReader.readNext();
    }
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.serializer.IProfilSource#read(org.kalypso.model.wspm.core.profil.IProfil)
   */

  @Override
  public IProfil[] read( final String profileTyp, final Reader reader ) throws IOException
  {
    m_provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profileTyp );
    extractDataBlocks( new CSVReader( reader, ';' ) );
    final ArrayList<IProfil> profiles = new ArrayList<IProfil>();
    for( final String key : m_ProfilesTable.keySet() )
    {
      final IProfil profil = ProfilFactory.createProfil( profileTyp );
      if( profil == null )
        throw new IOException( Messages.getString("CsvSource_0") ); //$NON-NLS-1$

      final TupleResult result = m_ProfilesTable.get( key );
      profil.setStation( NumberUtils.parseQuietDouble( key ) );
      profil.setResult( result );
      profiles.add( profil );
    }
    return profiles.toArray( new IProfil[] {} );
  }
}