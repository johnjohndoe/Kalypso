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
package org.kalypso.model.wspm.tuhh.ui.imports.ctripple;

import java.io.File;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.Set;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.kalypso.commons.databinding.swt.FileAndHistoryData;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.wspm.tuhh.core.ctripple.CodedTripple;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Holger Albert
 */
public class CodedTrippleImportData extends AbstractModelObject
{
  public static final String PROPERTY_SOURCE_FILE = "sourceFile"; //$NON-NLS-1$

  public static final String PROPERTY_FOTO_DIRECTORY = "fotoDirectory"; //$NON-NLS-1$

  public static final String PROPERTY_FOTO_DIRECTORY_HISTORY = "fotoDirectoryHistory"; //$NON-NLS-1$

  public static final String PROPERTY_COORDINATE_SYSTEM = "coordinateSystem"; //$NON-NLS-1$

  public static final String PROPERTY_DIRECTION_UPSTREAMS = "directionUpstreams"; //$NON-NLS-1$

  public static final String PROPERTY_CODED_TRIPPLE_DATA = "codedTrippleData"; //$NON-NLS-1$

  public static final String PROPERTY_CODED_TRIPPLE_DATA_STATUS = "codedTrippleDataStatus"; //$NON-NLS-1$

  private final FileAndHistoryData m_sourceFile;

  private File m_fotoDirectory;

  private String[] m_fotoDirectoryHistory;

  private String m_coordinateSystem;

  private boolean m_directionUpstreams;

  private CodedTripple m_codedTrippleData;

  private IStatus m_codedTrippleDataStatus;

  public CodedTrippleImportData( )
  {
    m_sourceFile = new FileAndHistoryData( "sourceFile" ); //$NON-NLS-1$
    m_fotoDirectory = null;
    m_fotoDirectoryHistory = new String[0];
    m_coordinateSystem = null;
    m_directionUpstreams = false;
    m_codedTrippleData = null;
    m_codedTrippleDataStatus = new Status( IStatus.INFO, KalypsoModelWspmTuhhUIPlugin.getID(), Messages.getString( "CodedTrippleImportData.0" ) ); //$NON-NLS-1$
  }

  public void init( final IDialogSettings settings )
  {
    if( settings == null )
      return;

    m_sourceFile.init( settings );

    initFotoDirectoryHistory( settings );

    m_coordinateSystem = settings.get( PROPERTY_COORDINATE_SYSTEM );
    m_directionUpstreams = settings.getBoolean( PROPERTY_DIRECTION_UPSTREAMS );
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

  public void storeSettings( final IDialogSettings settings )
  {
    if( settings == null )
      return;

    m_sourceFile.storeSettings( settings );

    storeFotoDirectoryHistory( settings );

    settings.put( PROPERTY_COORDINATE_SYSTEM, m_coordinateSystem );
    settings.put( PROPERTY_DIRECTION_UPSTREAMS, m_directionUpstreams );
  }

  private void storeFotoDirectoryHistory( final IDialogSettings settings )
  {
    final Set<String> fotoDirectoryhistory = new LinkedHashSet<>();
    if( m_fotoDirectory != null )
      fotoDirectoryhistory.add( m_fotoDirectory.getAbsolutePath() );
    fotoDirectoryhistory.addAll( Arrays.asList( m_fotoDirectoryHistory ) );

    settings.put( PROPERTY_FOTO_DIRECTORY_HISTORY, fotoDirectoryhistory.toArray( new String[] {} ) );
  }

  public FileAndHistoryData getSourceFile( )
  {
    return m_sourceFile;
  }

  public File getFotoDirectory( )
  {
    return m_fotoDirectory;
  }

  public String[] getFotoDirectoryHistory( )
  {
    return m_fotoDirectoryHistory;
  }

  public String getCoordinateSystem( )
  {
    return m_coordinateSystem;
  }

  public boolean isDirectionUpstreams( )
  {
    return m_directionUpstreams;
  }

  public CodedTripple getCodedTrippleData( )
  {
    return m_codedTrippleData;
  }

  public IStatus getCodedTrippleDataStatus( )
  {
    return m_codedTrippleDataStatus;
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

  public void setCoordinateSystem( final String coordinateSystem )
  {
    final String oldValue = m_coordinateSystem;
    m_coordinateSystem = coordinateSystem;
    firePropertyChange( PROPERTY_COORDINATE_SYSTEM, oldValue, m_coordinateSystem );
  }

  public void setDirectionUpstreams( final boolean directionUpstreams )
  {
    final boolean oldValue = m_directionUpstreams;
    m_directionUpstreams = directionUpstreams;
    firePropertyChange( PROPERTY_DIRECTION_UPSTREAMS, oldValue, m_directionUpstreams );
  }

  public void setCodedTrippleData( final CodedTripple codedTrippleData )
  {
    final CodedTripple oldValue = m_codedTrippleData;
    m_codedTrippleData = codedTrippleData;
    firePropertyChange( PROPERTY_CODED_TRIPPLE_DATA, oldValue, m_codedTrippleData );
  }

  public void setCodedTrippleDataStatus( final IStatus codedTrippleDataStatus )
  {
    final IStatus oldValue = m_codedTrippleDataStatus;
    m_codedTrippleDataStatus = codedTrippleDataStatus;
    firePropertyChange( PROPERTY_CODED_TRIPPLE_DATA_STATUS, oldValue, m_codedTrippleDataStatus );
  }
}