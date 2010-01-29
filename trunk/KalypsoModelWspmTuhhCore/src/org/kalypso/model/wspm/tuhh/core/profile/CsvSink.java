/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
import java.util.HashSet;
import java.util.Set;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSink;
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

  private final void writeData( final Writer writer, final IComponent[] comps, final IProfil[] profiles ) throws IOException
  {
    // get data from profiles
    for( final IProfil profil : profiles )
    {
      // get metadata from profile
      final String metaString = String.format( DOUBLE_FORMAT + "\t'%s'\t'%s'\t'%s'", profil.getStation(), profil.getName(), profil.getDescription(), profil.getComment() );

      // get point data
      for( final IRecord point : profil.getPoints() )
      {
        writer.write( metaString );
        for( final IComponent component : comps )
        {
          final int index = profil.indexOfProperty( component );
          if( index < 0 )
            writer.write( "\tnull" );
          else if( profil.isPointMarker( component.getId() ) )
            writer.write( String.format( "\t%s", point.getValue( index ).toString() ) );
          else
            writer.write( String.format( TAB_DOUBLE_FORMAT, point.getValue( index ) ) );
        }
        writer.write( lineSeparator );
      }
    }
  }

  private final void writeHeader( final Writer writer, final IComponent[] comps ) throws IOException
  {
    writer.write( "Station\tName\tBeschreibung\tKommentar" );

    for( final IComponent comp : comps )
      writer.write( String.format( "\t'%s'", comp.getName() ) );

    writer.write( lineSeparator );
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

  public boolean internalWrite( IProfil[] profiles, Writer writer ) throws IOException
  {

    // get all components of the profiles in order to create table header
    // that fits
    final IComponent[] comps = getComponents( profiles );

    // write table header including all components
    writeHeader( writer, comps );

    // write table header including all components
    writeData( writer, comps, profiles );

    return true;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.serializer.IProfilSink#write(java.lang.Object, java.io.Writer)
   */
  @Override
  public boolean write( Object source, Writer writer ) throws IOException
  {
    if( source instanceof IProfil[] )
      return internalWrite( (IProfil[]) source, writer );

    throw new IOException( "illegal Argument", new IllegalArgumentException( source.toString() ) );
  }
}
