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
package org.kalypso.model.wspm.ewawi.data.reader;

import java.io.File;
import java.io.IOException;
import java.text.ParseException;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.io.FilenameUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.wspm.ewawi.WspmEwawiPlugin;
import org.kalypso.model.wspm.ewawi.data.EwawiPlus;
import org.kalypso.model.wspm.ewawi.utils.EwawiKey;
import org.kalypso.model.wspm.ewawi.utils.EwawiKeyUtilities;

/**
 * Reads several ewawi files from one input dir.
 * 
 * @author Gernot Belger
 */
public class EwawiDirReader
{
  /**
   * Key to data.
   */
  private final Map<EwawiKey, EwawiPlus> m_data = new HashMap<>();

  public EwawiPlus[] getData( )
  {
    return m_data.values().toArray( new EwawiPlus[m_data.size()] );
  }

  /**
   * Reads all ewawi files from the input dir. Each set of files with the same generated key are put together.
   */
  public IStatus read( final File inputDir )
  {
    final IStatusCollector log = new StatusCollector( WspmEwawiPlugin.PLUGIN_ID );

    final File[] files = inputDir.listFiles();
    for( final File file : files )
    {
      final IStatus status = readFile( file );
      log.add( status );

      System.out.println( String.format( "%d: %s", status.getSeverity(), status.getMessage() ) ); //$NON-NLS-1$
    }

    return log.asMultiStatus( Messages.EwawiDirReader_1 );
  }

  private IStatus readFile( final File file )
  {
    final String fileName = file.getName();
    final String extension = FilenameUtils.getExtension( fileName ).toLowerCase();
    switch( extension )
    {
      case "pro": //$NON-NLS-1$
        return readProFile( file );

      case "sta": //$NON-NLS-1$
        return readStaFile( file );

      case "epl": //$NON-NLS-1$
        return readEplFile( file );

        // TODO: Read additional files (foto locations, river names)...

      default:
        return new Status( IStatus.INFO, Messages.EwawiDirReader_5, fileName );
    }
  }

  private IStatus readProFile( final File file )
  {
    final EwawiPlus data = getData( file );
    if( data == null )
      return new Status( Status.WARNING, WspmEwawiPlugin.PLUGIN_ID, String.format( Messages.EwawiDirReader_6, file.getName(), file.getParent() ) );

    final EwawiProReader reader = new EwawiProReader( data );
    return read( reader, file );
  }

  private IStatus readStaFile( final File file )
  {
    final EwawiPlus data = getData( file );
    if( data == null )
      return new Status( Status.WARNING, WspmEwawiPlugin.PLUGIN_ID, String.format( Messages.EwawiDirReader_7, file.getName(), file.getParent() ) );

    final EwawiStaReader reader = new EwawiStaReader( data );
    return read( reader, file );
  }

  private IStatus readEplFile( final File file )
  {
    final EwawiPlus data = getData( file );
    if( data == null )
      return new Status( Status.WARNING, WspmEwawiPlugin.PLUGIN_ID, String.format( Messages.EwawiDirReader_8, file.getName(), file.getParent() ) );

    final EwawiEplReader reader = new EwawiEplReader( data );
    return read( reader, file );
  }

  private static IStatus read( final AbstractEwawiReader reader, final File file )
  {
    try
    {
      reader.read( file );
      return new Status( IStatus.OK, WspmEwawiPlugin.PLUGIN_ID, file.getName() );
    }
    catch( final IOException | ParseException e )
    {
      final String message = String.format( Messages.EwawiDirReader_9, file.getName() );
      return new Status( IStatus.WARNING, WspmEwawiPlugin.PLUGIN_ID, message, e );
    }
  }

  private EwawiPlus getData( final File file )
  {
    /* Generate the key for this file. */
    final EwawiKey key = EwawiKeyUtilities.generateKey( file );

    /* The parent path must contain the alias contained in the filename. */
    if( !file.getParent().contains( key.getAlias() ) )
      return null;

    /* Add new data object if necessary. */
    if( !m_data.containsKey( key ) )
      m_data.put( key, new EwawiPlus( key ) );

    return m_data.get( key );
  }
}