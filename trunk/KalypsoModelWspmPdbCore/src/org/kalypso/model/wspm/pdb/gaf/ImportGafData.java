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
package org.kalypso.model.wspm.pdb.gaf;

import java.io.File;
import java.util.ArrayList;
import java.util.Date;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import org.eclipse.core.databinding.observable.list.WritableList;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypsodeegree.KalypsoDeegreePlugin;

/**
 * @author Gernot Belger
 */
public class ImportGafData extends AbstractModelObject
{
  public static final String PROPERTY_OPEN_LOG = "openLog"; //$NON-NLS-1$

  public static final String PROPERTY_SRS = "srs"; //$NON-NLS-1$

  public static final String PROPERTY_GAF_FILE = "gafFile"; //$NON-NLS-1$

  public static final String PROPERTY_WATER_BODY = "waterBody"; //$NON-NLS-1$

  private String m_srs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

  private File m_gafFile = null;

  /** We always create a new state when importing a gaf file */
  private final State m_state = new State();

  private WaterBody m_waterBody;

  private final IPdbConnection m_connection;

  private final WritableList m_gafProfiles = new WritableList( new ArrayList<GafProfile>(), GafProfile.class );

  public ImportGafData( final IPdbConnection connection )
  {
    m_connection = connection;

    /* Pre init measurement date to now */
    m_state.setMeasurementDate( new Date() );
    m_state.setIsstatezero( State.ZERO_STATE_ON );
    m_state.setDescription( StringUtils.EMPTY );
    m_state.setEditingUser( connection.getSettings().getUsername() );
  }

  public void init( final IDialogSettings settings )
  {
    if( settings == null )
      return;

    final String gafPath = settings.get( PROPERTY_GAF_FILE );
    if( !StringUtils.isBlank( gafPath ) )
      setGafFile( new File( gafPath ) );

    final String srs = settings.get( PROPERTY_SRS );
    if( srs != null )
      setSrs( srs );
  }

  public void store( final IDialogSettings settings )
  {
    if( settings == null )
      return;

    final String gafPath = m_gafFile == null ? null : m_gafFile.getAbsolutePath();

    settings.put( PROPERTY_SRS, m_srs );
    settings.put( PROPERTY_GAF_FILE, gafPath );
  }

  public String getSrs( )
  {
    return m_srs;
  }

  public void setSrs( final String srs )
  {
    final String oldValue = m_srs;

    m_srs = srs;

    firePropertyChange( PROPERTY_SRS, oldValue, m_srs );
  }

  public WaterBody getWaterBody( )
  {
    return m_waterBody;
  }

  public void setWaterBody( final WaterBody waterBody )
  {
    final WaterBody oldValue = m_waterBody;

    m_waterBody = waterBody;

    firePropertyChange( PROPERTY_WATER_BODY, oldValue, m_waterBody );
  }

  public File getGafFile( )
  {
    return m_gafFile;
  }

  public void setGafFile( final File gafFile )
  {
    final File oldValue = m_gafFile;

    m_gafFile = gafFile;

    firePropertyChange( PROPERTY_GAF_FILE, oldValue, m_gafFile );

    if( m_gafFile != null )
    {
      final String filename = m_gafFile.getName();

      final String stateSource = String.format( "Importiert aus: %s", filename );
      m_state.setSource( stateSource );

      m_state.setName( FilenameUtils.removeExtension( filename ) );
    }
  }

  public State getState( )
  {
    return m_state;
  }

  public File getLogFile( )
  {
    final File gafFile = getGafFile();
    if( gafFile == null )
      return null;

    return new File( gafFile.getAbsolutePath() + ".log" );
  }

  public int getSrid( )
  {
    // FIXME: find srid from pdb by srs set here...

    // TODO Auto-generated method stub
    return 31468;
  }

  public IPdbConnection getConnection( )
  {
    return m_connection;
  }

  public WritableList getGafProfiles( )
  {
    return m_gafProfiles;
  }
}