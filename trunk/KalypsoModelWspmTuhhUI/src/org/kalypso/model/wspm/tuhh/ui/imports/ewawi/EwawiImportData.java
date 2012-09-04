/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Bj�rnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universit�t Hamburg-Harburg, Institut f�r Wasserbau, Hamburg, Germany
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
package org.kalypso.model.wspm.tuhh.ui.imports.ewawi;

import java.io.File;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.Set;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.kalypso.commons.databinding.swt.FileAndHistoryData;
import org.kalypso.commons.java.util.AbstractModelObject;

/**
 * @author Holger Albert
 */
public class EwawiImportData extends AbstractModelObject
{
  public static final String PROPERTY_PRO_FILE = "proFile"; //$NON-NLS-1$

  public static final String PROPERTY_STA_FILE = "staFile"; //$NON-NLS-1$

  public static final String PROPERTY_FOTO_DIRECTORY = "fotoDirectory"; //$NON-NLS-1$

  public static final String PROPERTY_FOTO_DIRECTORY_HISTORY = "fotoDirectoryHistory"; //$NON-NLS-1$

  public static final String PROPERTY_DOCUMENT_DIRECTORY = "documentDirectory"; //$NON-NLS-1$

  public static final String PROPERTY_DOCUMENT_DIRECTORY_HISTORY = "documentDirectoryHistory"; //$NON-NLS-1$

  public static final String PROPERTY_RIVER_SHAPE_DATA = "riverShapeData"; //$NON-NLS-1$

  private final FileAndHistoryData m_proFile;

  private final FileAndHistoryData m_staFile;

  private File m_fotoDirectory;

  private String[] m_fotoDirectoryHistory;

  private File m_documentDirectory;

  private String[] m_documentDirectoryHistory;

  private final EwawiRiverShapeData m_riverShapeData;

  public EwawiImportData( )
  {
    m_proFile = new FileAndHistoryData( "proFile" ); //$NON-NLS-1$
    m_staFile = new FileAndHistoryData( "staFile" ); //$NON-NLS-1$
    m_fotoDirectory = null;
    m_fotoDirectoryHistory = new String[0];
    m_documentDirectory = null;
    m_documentDirectoryHistory = new String[0];
    m_riverShapeData = new EwawiRiverShapeData();
  }

  public void init( final IDialogSettings settings )
  {
    if( settings == null )
      return;

    m_proFile.init( settings );
    m_staFile.init( settings );

    initFotoDirectoryHistory( settings );
    initDocumentDirectoryHistory( settings );

    m_riverShapeData.init( settings );
  }

  private void initFotoDirectoryHistory( final IDialogSettings settings )
  {
    final String[] fotoDirectoryHistory = settings.getArray( PROPERTY_FOTO_DIRECTORY_HISTORY );
    if( fotoDirectoryHistory != null )
    {
      setFotoDirectoryHistory( fotoDirectoryHistory );
      if( fotoDirectoryHistory.length > 0 )
        setFotoDirectory( new File( fotoDirectoryHistory[0] ) );
    }
  }

  private void initDocumentDirectoryHistory( final IDialogSettings settings )
  {
    final String[] documentDirectoryHistory = settings.getArray( PROPERTY_DOCUMENT_DIRECTORY_HISTORY );
    if( documentDirectoryHistory != null )
    {
      setDocumentDirectoryHistory( documentDirectoryHistory );
      if( documentDirectoryHistory.length > 0 )
        setDocumentDirectory( new File( documentDirectoryHistory[0] ) );
    }
  }

  public void storeSettings( final IDialogSettings settings )
  {
    if( settings == null )
      return;

    m_proFile.storeSettings( settings );
    m_staFile.storeSettings( settings );

    storeFotoDirectoryHistory( settings );
    storeDocumentDirectoryHistory( settings );

    m_riverShapeData.storeSettings( settings );
  }

  private void storeFotoDirectoryHistory( final IDialogSettings settings )
  {
    final Set<String> fotoDirectoryhistory = new LinkedHashSet<>();
    if( m_fotoDirectory != null )
      fotoDirectoryhistory.add( m_fotoDirectory.getAbsolutePath() );
    fotoDirectoryhistory.addAll( Arrays.asList( m_fotoDirectoryHistory ) );

    settings.put( PROPERTY_FOTO_DIRECTORY_HISTORY, fotoDirectoryhistory.toArray( new String[] {} ) );
  }

  private void storeDocumentDirectoryHistory( final IDialogSettings settings )
  {
    final Set<String> documentDirectoryhistory = new LinkedHashSet<>();
    if( m_documentDirectory != null )
      documentDirectoryhistory.add( m_documentDirectory.getAbsolutePath() );
    documentDirectoryhistory.addAll( Arrays.asList( m_documentDirectoryHistory ) );

    settings.put( PROPERTY_DOCUMENT_DIRECTORY_HISTORY, documentDirectoryhistory.toArray( new String[] {} ) );
  }

  public FileAndHistoryData getProFile( )
  {
    return m_proFile;
  }

  public FileAndHistoryData getStaFile( )
  {
    return m_staFile;
  }

  public File getFotoDirectory( )
  {
    return m_fotoDirectory;
  }

  public String[] getFotoDirectoryHistory( )
  {
    return m_fotoDirectoryHistory;
  }

  public File getDocumentDirectory( )
  {
    return m_documentDirectory;
  }

  public String[] getDocumentDirectoryHistory( )
  {
    return m_documentDirectoryHistory;
  }

  public EwawiRiverShapeData getRiverShapeData( )
  {
    return m_riverShapeData;
  }

  public void setFotoDirectory( final File fotoDirectory )
  {
    final File oldValue = m_fotoDirectory;
    m_fotoDirectory = fotoDirectory;
    firePropertyChange( PROPERTY_FOTO_DIRECTORY, oldValue, m_fotoDirectory );
  }

  public void setFotoDirectoryHistory( final String[] fotoDirectoryHistory )
  {
    final String[] oldValue = m_fotoDirectoryHistory;
    m_fotoDirectoryHistory = fotoDirectoryHistory;
    firePropertyChange( PROPERTY_FOTO_DIRECTORY_HISTORY, oldValue, m_fotoDirectoryHistory );
  }

  public void setDocumentDirectory( final File documentDirectory )
  {
    final File oldValue = m_documentDirectory;
    m_documentDirectory = documentDirectory;
    firePropertyChange( PROPERTY_DOCUMENT_DIRECTORY, oldValue, m_documentDirectory );
  }

  public void setDocumentDirectoryHistory( final String[] documentDirectoryHistory )
  {
    final String[] oldValue = m_documentDirectoryHistory;
    m_documentDirectoryHistory = documentDirectoryHistory;
    firePropertyChange( PROPERTY_DOCUMENT_DIRECTORY_HISTORY, oldValue, m_documentDirectoryHistory );
  }
}