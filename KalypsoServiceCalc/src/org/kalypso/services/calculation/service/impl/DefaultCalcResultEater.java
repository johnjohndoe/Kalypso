/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.services.calculation.service.impl;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;
import java.util.zip.ZipOutputStream;

import javax.activation.DataHandler;
import javax.activation.FileDataSource;

import org.apache.commons.io.IOUtils;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.java.util.zip.ZipFileVisitor;
import org.kalypso.services.calculation.job.ICalcResultPacker;
import org.kalypso.services.calculation.service.CalcJobClientBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;

/**
 * @author belger
 */
public class DefaultCalcResultEater implements ICalcResultPacker
{
  private final Vector m_files = new Vector();

  private final ModelspecData m_modelspec;

  /** Should be synchronized */
  private final Vector m_results = new Vector();

  private final Map m_clientOutputMap;

  public DefaultCalcResultEater( final ModelspecData modelspec, final CalcJobClientBean[] clientOutput )
  {
    m_modelspec = modelspec;

    m_clientOutputMap = new HashMap( clientOutput.length );
    for( int i = 0; i < clientOutput.length; i++ )
    {
      final CalcJobClientBean bean = clientOutput[i];
      m_clientOutputMap.put( bean.getId(), bean );
    }
  }

  /**
   * @param id
   *          Die ID für die Bean
   * @param file
   *          Diese Datei oder dieses Verzeichnis werden zurück an den Server gegeben.
   * 
   * @throws CalcJobServiceException
   * @see org.kalypso.services.calculation.job.ICalcResultEater#addResult(java.lang.String, java.io.File)
   */
  public void addResult( final String id, final File file ) throws CalcJobServiceException
  {
    if( !m_modelspec.hasOutput( id ) )
      throw new CalcJobServiceException( "Vom Server unerwartete Ausgabe mit ID: " + id, null );

    final CalcJobClientBean clientBean = (CalcJobClientBean)m_clientOutputMap.get( id );
    if( clientBean == null )
      throw new CalcJobServiceException( "Vom Client unerwartete Ausgabe mit ID: " + id, null );

    m_results.add( new CalcResult( id, clientBean.getPath(), file ) );
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcResultPacker#getCurrentResults()
   */
  public String[] getCurrentResults()
  {
    final String[] results = new String[m_results.size()];
    for( int i = 0; i < results.length; i++ )
    {
      final CalcResult result = (CalcResult)m_results.get( i );
      results[i] = result.getID();
    }

    return results;
  }

  /**
   * @throws CalcJobServiceException
   * @see org.kalypso.services.calculation.job.ICalcResultPacker#packCurrentResults()
   */
  public DataHandler packCurrentResults() throws CalcJobServiceException
  {

    ZipOutputStream zos = null;
    try
    {
      final File zipFile = File.createTempFile( "CalcJobResult_", ".zip" );
      zipFile.deleteOnExit();
      addFile( zipFile );

      zos = new ZipOutputStream( new BufferedOutputStream( new FileOutputStream( zipFile ) ) );
      final ZipFileVisitor zipper = new ZipFileVisitor( zos );

      for( final Iterator iter = m_results.iterator(); iter.hasNext(); )
      {
        final CalcResult result = (CalcResult)iter.next();

        final File file = result.getFile();
        final String path = result.getPath();

        zipper.setBasePattern( file.getAbsolutePath() );
        zipper.setBaseReplace( path );

        FileUtilities.accept( file, zipper, true );
      }

      zos.close();

      return new DataHandler( new FileDataSource( zipFile ) );
    }
    catch( IOException e )
    {
      throw new CalcJobServiceException( "Ergebnisse konnten nicht übertragen werden.", e );
    }
    finally
    {
      IOUtils.closeQuietly( zos );
    }
  }

  public void addFile( final File file )
  {
    m_files.add( file );
  }

  /**
   * Löscht alle bisher hinzugefügten Dateien. Wenns Verzeichnisse sind, wird auch der Inhalt rekursiv gelöscht.
   */
  public void disposeFiles()
  {
    for( final Iterator iter = m_files.iterator(); iter.hasNext(); )
    {
      final File file = (File)iter.next();
      FileUtilities.deleteRecursive( file );
    }
  }
}
