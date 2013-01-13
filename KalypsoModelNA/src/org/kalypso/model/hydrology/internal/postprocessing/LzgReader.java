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
package org.kalypso.model.hydrology.internal.postprocessing;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.text.DateFormat;
import java.util.Date;

import org.apache.commons.lang3.tuple.Pair;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.java.lang.Doubles;
import org.kalypso.model.hydrology.binding.model.channels.Channel;
import org.kalypso.model.hydrology.binding.model.channels.VirtualChannel;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
class LzgReader
{
  private final DateFormat m_dateFormat;

  private final Date m_initialDate;

  private final Channel m_channel;

  private static enum ChannelStatus
  {
    SEARCH_HEADER,
    READ_QGS;
  }

  public LzgReader( final DateFormat dateFormat, final Date initialDate, final Channel channel )
  {
    m_dateFormat = dateFormat;
    m_initialDate = initialDate;
    m_channel = channel;
  }

  public Pair<Double, IStatus> read( final File lzgFile )
  {
    // FIXME: warum haben virtuell channel keine anfangswerte?! dubios!
    if( m_channel instanceof VirtualChannel )
      return Pair.of( null, Status.OK_STATUS );

    // FIXME: warum haben storage und km channel die gleiche art von anfangswerten?

    try( FileReader fileReader = new FileReader( lzgFile ) )
    {
      final Double value = readLzgFile( fileReader );
      if( Doubles.isNullOrInfinite( value ) )
      {
        final String message = String.format( Messages.getString("LzgReader.0"), m_channel.getName(), lzgFile.getName() ); //$NON-NLS-1$
        final IStatus noValueStatus = new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, message );
        return Pair.of( null, noValueStatus );
      }

      return Pair.of( value, Status.OK_STATUS );
    }
    catch( final FileNotFoundException e )
    {
      final String message = String.format( Messages.getString("LzgReader.1"), m_channel.getName() ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.WARNING, ModelNA.PLUGIN_ID, message, e );
      return Pair.of( null, status );
    }
    catch( final Exception e )
    {
      final String message = String.format( Messages.getString( "LzsToGml.0" ), lzgFile.getName() ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, message, e );
      return Pair.of( null, status );
    }
  }

  private Double readLzgFile( final FileReader fileReader ) throws NumberFormatException, IOException
  {
    final LineNumberReader reader = new LineNumberReader( fileReader );

    ChannelStatus status = ChannelStatus.SEARCH_HEADER;

    final String iniDate = m_dateFormat.format( m_initialDate );

    while( reader.ready() )
    {
      final String line = reader.readLine();
      if( line == null )
        break;

      final String cleanLine = line.trim().replaceAll( "\\s+", " " ); //$NON-NLS-1$ //$NON-NLS-2$
      switch( status )
      {
        case SEARCH_HEADER:
          if( cleanLine.endsWith( "qgs" ) && cleanLine.startsWith( iniDate ) ) //$NON-NLS-1$
            // 19960521 00 h 1 qgs
            // 1 0.000
            status = ChannelStatus.READ_QGS;
          break;

        case READ_QGS: // Gesamtabfluss
          final String[] strings = cleanLine.split( " " ); //$NON-NLS-1$
          return Double.valueOf( strings[1] );
      }
    }

    return null;
  }
}
