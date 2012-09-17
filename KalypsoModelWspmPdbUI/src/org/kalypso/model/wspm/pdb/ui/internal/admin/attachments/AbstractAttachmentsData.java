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
package org.kalypso.model.wspm.pdb.ui.internal.admin.attachments;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Set;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.db.mapping.Document;

/**
 * @author Holger Albert
 */
public abstract class AbstractAttachmentsData extends AbstractModelObject
{
  public static final String PROPERTY_IMPORT_DIR = "importDir"; //$NON-NLS-1$

  public static final String PROPERTY_IMPORT_DIR_HISTORY = "importHistory"; //$NON-NLS-1$

  public static final String PROPERTY_IMPORT_MODE = "importMode"; //$NON-NLS-1$

  public static final String PROPERTY_ZIP_FILE = "zipFile"; //$NON-NLS-1$

  public static final String PROPERTY_ZIP_HISTORY = "zipHistory"; //$NON-NLS-1$

  public static final String PROPERTY_SELECTION_COUNT = "selectionCount"; //$NON-NLS-1$

  private final IPdbConnection m_connection;

  private AbstractAttachmentsDocumentsData m_documentData;

  private File m_importDir;

  private String[] m_importHistory;

  private ImportMode m_importMode = ImportMode.skip;

  private File m_zipFile;

  private String[] m_zipHistory;

  private int m_selectionCount;

  private final Collection<Document> m_selectedDocuments;

  public AbstractAttachmentsData( final IPdbConnection connection )
  {
    m_connection = connection;

    m_documentData = null;
    m_importDir = null;
    m_importHistory = new String[] {};
    m_importMode = ImportMode.skip;
    m_zipFile = null;
    m_zipHistory = new String[] {};
    m_selectionCount = 0;
    m_selectedDocuments = new HashSet<>();
  }

  public void load( final IDialogSettings settings )
  {
    if( settings == null )
      return;

    /* Import mode. */
    final String importMode = settings.get( PROPERTY_IMPORT_MODE );
    if( !StringUtils.isBlank( importMode ) )
      m_importMode = ImportMode.valueOf( importMode );

    /* Import history. */
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

    /* Zip history. */
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
  }

  public void store( final IDialogSettings settings )
  {
    if( settings == null )
      return;

    settings.put( PROPERTY_IMPORT_MODE, m_importMode.name() );

    /* Update import history. */
    final String[] importHistory = getImportHistory();
    final File importDir = getImportDir();
    final String[] newImportHistory = updateHistory( importHistory, importDir );
    settings.put( PROPERTY_IMPORT_DIR_HISTORY, newImportHistory );

    /* Update zip history. */
    final String[] zipHistory = getZipHistory();
    final File zipFile = getZipFile();
    final String[] newZipHistory = updateHistory( zipHistory, zipFile );
    settings.put( PROPERTY_ZIP_HISTORY, newZipHistory );
  }

  private String[] updateHistory( final String[] history, final File file )
  {
    /* New entry on, top; avoid duplicate entries. */
    final Set<String> historySet = new LinkedHashSet<>();

    if( file != null )
    {
      final String absolutePath = file.getAbsolutePath();
      if( !StringUtils.isBlank( absolutePath ) )
        historySet.add( absolutePath );
    }

    historySet.addAll( Arrays.asList( history ) );

    final String[] newHistory = historySet.toArray( new String[historySet.size()] );
    return ArrayUtils.subarray( newHistory, 0, 20 );
  }

  public IPdbConnection getConnection( )
  {
    return m_connection;
  }

  public synchronized AbstractAttachmentsDocumentsData getDocumentData( )
  {
    if( m_documentData == null )
      m_documentData = createDocumentData();

    return m_documentData;
  }

  protected abstract AbstractAttachmentsDocumentsData createDocumentData( );

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

  public ImportMode getImportMode( )
  {
    return m_importMode;
  }

  public void setImportMode( final ImportMode importMode )
  {
    final Object oldValue = m_importMode;
    m_importMode = importMode;
    firePropertyChange( PROPERTY_IMPORT_MODE, oldValue, importMode );
    updateSelectionCount();
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

  public int getSelectionCount( )
  {
    return m_selectionCount;
  }

  public void setSelectionCount( final int selectionCount )
  {
    final Object oldValue = m_selectionCount;
    m_selectionCount = selectionCount;
    firePropertyChange( PROPERTY_SELECTION_COUNT, oldValue, m_selectionCount );
  }

  public boolean isSelected( final Document doc )
  {
    return m_selectedDocuments.contains( doc );
  }

  public void selectDocument( final Document doc )
  {
    m_selectedDocuments.add( doc );
    updateSelectionCount();
  }

  public void unselectDocument( final Document doc )
  {
    m_selectedDocuments.remove( doc );
    updateSelectionCount();
  }

  public void clearSelection( )
  {
    m_selectedDocuments.clear();
    updateSelectionCount();
  }

  private void updateSelectionCount( )
  {
    setSelectionCount( getImportDocuments().length );
  }

  public String getUsername( )
  {
    return m_connection.getSettings().getUsername();
  }

  public Document[] getImportDocuments( )
  {
    final Collection<Document> selected = new ArrayList<>();

    for( final Document document : m_selectedDocuments )
    {
      final boolean exists = m_documentData.isExisting( document );
      if( m_importMode == ImportMode.overwrite || !exists )
        selected.add( document );
    }

    return selected.toArray( new Document[selected.size()] );
  }
}