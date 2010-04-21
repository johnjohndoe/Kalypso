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
package org.kalypso.model.wspm.tuhh.core.profile;

import java.io.IOException;
import java.io.Writer;
import java.math.BigDecimal;
import java.util.Formatter;
import java.util.HashSet;
import java.util.Set;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSink;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultLengthSection;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * @author kimwerner
 */
public class CsvSink implements IProfilSink
{
  private final static String DOUBLE_FORMAT = "%.4f";

  private final static String TAB_DOUBLE_FORMAT = "\t%.4f";

  private final String lineSeparator = System.getProperty( "line.separator" );

  private final WspmResultLengthSection[] m_lengthSections;

  public CsvSink( final WspmResultLengthSection[] lengthSections )
  {
    m_lengthSections = lengthSections;
  }

  private final void writeData( final Formatter formatter, final IComponent[] comps, final IProfil[] profiles ) throws IOException
  {
    // get data from profiles
    for( final IProfil profil : profiles )
    {
      final double station = profil.getStation();
      final BigDecimal bigStation = ProfilUtil.stationToBigDecimal( station );

      // get metadata from profile
      final String metaString = String.format( DOUBLE_FORMAT + "\t'%s'\t'%s'\t'%s'", bigStation, profil.getName(), profil.getDescription(), profil.getComment() );

      final int[] componentIndices = new int[comps.length];
      for( int i = 0; i < componentIndices.length; i++ )
        componentIndices[i] = profil.indexOfProperty( comps[i] );

      // get point data
      for( final IRecord point : profil.getPoints() )
      {
        formatter.format( metaString );

        for( final IComponent component : comps )
        {
          final int index = profil.indexOfProperty( component );
          if( index < 0 )
            formatter.format( "\tnull" );
          else
          {
            final Object value = point.getValue( index );
            // TODO: we need a more sophisticated handling of types here...
            if( profil.isPointMarker( component.getId() ) || !(value instanceof Number) )
              formatter.format( "\t%s", value.toString() );
            else
              formatter.format( TAB_DOUBLE_FORMAT, value );
          }
        }

        writeResults( bigStation, formatter );

        formatter.format( lineSeparator );

        final IOException ioException = formatter.ioException();
        if( ioException != null )
          throw ioException;
      }
    }
  }

  private void writeResults( final BigDecimal station, final Formatter formatter )
  {
    for( final WspmResultLengthSection ls : m_lengthSections )
    {
      final BigDecimal waterlevel = ls.getWaterlevel( station );
      formatter.format( TAB_DOUBLE_FORMAT, waterlevel );
    }
  }

  private final void writeHeader( final Formatter formatter, final IComponent[] comps ) throws IOException
  {
    formatter.format( "Station\tName\tBeschreibung\tKommentar" );

    for( final IComponent comp : comps )
      formatter.format( "\t'%s'", comp.getName() );

    for( final WspmResultLengthSection ls : m_lengthSections )
      formatter.format( "\t'%s'", ls.getLabel() );

    formatter.format( lineSeparator );
  }

  private final IComponent[] getComponents( final IProfil[] profiles )
  {
    final Set<IComponent> profCompSet = new HashSet<IComponent>();
    for( final IProfil profil : profiles )
    {
      for( final IComponent component : profil.getPointProperties() )
      {
        profCompSet.add( component );
      }
    }
    return profCompSet.toArray( new IComponent[] {} );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.serializer.IProfilSink#write(java.lang.Object, java.io.Writer)
   */
  @Override
  public boolean write( final IProfil[] profiles, final Writer writer ) throws IOException
  {
    // get all components of the profiles in order to create table header
    // that fits
    final IComponent[] comps = getComponents( profiles );

    final Formatter formatter = new Formatter( writer );

    // write table header including all components
    writeHeader( formatter, comps );

    // write table header including all components
    writeData( formatter, comps, profiles );

    formatter.flush();

    return true;
  }

}
