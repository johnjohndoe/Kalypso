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
package org.kalypso.model.wspm.pdb.ui.internal.admin.attachments;

import java.io.File;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.Set;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.db.mapping.State;

/**
 * @author Gernot Belger
 */
public class ImportAttachmentsData extends AbstractModelObject
{
  public static final String PROPERTY_IMPORT_DIR = "importDir"; //$NON-NLS-1$

  public static final String PROPERTY_IMPORT_DIR_HISTORY = "importHistory"; //$NON-NLS-1$

  public static final String PROPERTY_IMPORT_PATTERN = "importPattern"; //$NON-NLS-1$

  public static final String PROPERTY_ZIP_FILE = "zipFile"; //$NON-NLS-1$

  public static final String PROPERTY_ZIP_HISTORY = "zipHistory"; //$NON-NLS-1$

  private final IPdbConnection m_connection;

  private State m_state;

  private String m_importPattern = String.format( "*<%s>*", AttachmentStationPattern.TOKEN ); //$NON-NLS-1$

  private File m_importDir;

  private String[] m_importHistory = new String[0];

  private File m_zipFile;

  private String[] m_zipHistory = new String[0];

  public ImportAttachmentsData( final IPdbConnection connection )
  {
    m_connection = connection;
  }

  public IPdbConnection getConnection( )
  {
    return m_connection;
  }

  public void init( final IStructuredSelection selection, final IDialogSettings settings )
  {
    load( settings );

    m_state = findState( selection );

    /* Propose a zip file name, based on state name */
    final File zipDir = m_zipFile != null ? m_zipFile.getParentFile() : FileUtils.getUserDirectory();
    final String zipName = m_state.getName() + ".zip"; //$NON-NLS-1$
    final File zipFile = new File( zipDir, zipName );
    setZipFile( zipFile );

    Assert.isNotNull( m_state );
  }

  private State findState( final IStructuredSelection selection )
  {
    if( selection.isEmpty() )
      return null;

    final Object firstElement = selection.getFirstElement();
    if( firstElement instanceof State )
      return (State) firstElement;

    return null;
  }

  private void load( final IDialogSettings settings )
  {
    if( settings == null )
      return;

    /* Import history */
    final String[] importHistory = settings.getArray( PROPERTY_IMPORT_DIR_HISTORY );
    if( importHistory != null )
    {
      setImportHistory( importHistory );

      if( importHistory.length > 0 )
      {
        final String importPath = importHistory[0];
        if( importPath != null )
          setImportDir( new File( importPath ) );
      }
    }

    /* zip history */
    final String[] zipHistory = settings.getArray( PROPERTY_ZIP_HISTORY );
    if( zipHistory != null )
    {
      setZipHistory( zipHistory );

      if( zipHistory.length > 0 )
      {
        final String zipPath = zipHistory[0];
        if( zipPath != null )
          setZipFile( new File( zipPath ) );
      }
    }

    /* Pattern */
    final String pattern = settings.get( PROPERTY_IMPORT_PATTERN );
    if( !StringUtils.isBlank( pattern ) )
      m_importPattern = pattern;
  }

  public void store( final IDialogSettings settings )
  {
    if( settings == null )
      return;

    /* Update import history */
    final String[] importHistory = getImportHistory();
    final File importDir = getImportDir();
    final String[] newImportHistory = updateHistory( importHistory, importDir );
    settings.put( PROPERTY_IMPORT_DIR_HISTORY, newImportHistory );

    /* Update zip history */
    final String[] zipHistory = getZipHistory();
    final File zipFile = getZipFile();
    final String[] newZipHistory = updateHistory( zipHistory, zipFile );
    settings.put( PROPERTY_ZIP_HISTORY, newZipHistory );

    /* Import pattern */
    settings.put( PROPERTY_IMPORT_PATTERN, m_importPattern );
  }

  private String[] updateHistory( final String[] history, final File file )
  {
    final Set<String> historySet = new LinkedHashSet<String>();
    // New entry on, top; avoid duplicate entries
    if( file != null )
    {
      final String absolutePath = file.getAbsolutePath();
      if( !StringUtils.isBlank( absolutePath ) )
        historySet.add( absolutePath );
    }

    historySet.addAll( Arrays.asList( history ) );

    final String[] newHistory = historySet.toArray( new String[historySet.size()] );
    return (String[]) ArrayUtils.subarray( newHistory, 0, 20 );
  }

  public State getState( )
  {
    return m_state;
  }

  public File getImportDir( )
  {
    return m_importDir;
  }

  public void setImportDir( final File importDir )
  {
    final Object oldValue = m_importDir;

    m_importDir = importDir;

    firePropertyChange( PROPERTY_IMPORT_DIR, oldValue, importDir );
  }

  public String[] getImportHistory( )
  {
    return m_importHistory;
  }

  public void setImportHistory( final String[] importHistory )
  {
    final Object oldValue = m_importDir;

    m_importHistory = importHistory;

    firePropertyChange( PROPERTY_IMPORT_DIR_HISTORY, oldValue, importHistory );
  }

  public String getImportPattern( )
  {
    return m_importPattern;
  }

  public void setImportPattern( final String importPattern )
  {
    final Object oldValue = m_importPattern;

    m_importPattern = importPattern;

    firePropertyChange( PROPERTY_IMPORT_PATTERN, oldValue, importPattern );
  }

  public File getZipFile( )
  {
    return m_zipFile;
  }

  public void setZipFile( final File zipFile )
  {
    final Object oldValue = m_zipFile;

    m_zipFile = zipFile;

    firePropertyChange( PROPERTY_ZIP_FILE, oldValue, zipFile );
  }

  public String[] getZipHistory( )
  {
    return m_zipHistory;
  }

  public void setZipHistory( final String[] zipHistory )
  {
    final Object oldValue = m_zipHistory;

    m_zipHistory = zipHistory;

    firePropertyChange( PROPERTY_ZIP_HISTORY, oldValue, zipHistory );
  }
}