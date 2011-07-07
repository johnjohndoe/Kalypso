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
import java.io.IOException;
import java.util.Date;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.hibernate.Session;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.wspm.pdb.PdbUtils;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.PdbInfo;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.internal.gaf.Coefficients;
import org.kalypso.model.wspm.pdb.internal.gaf.GafCodes;
import org.kalypso.model.wspm.pdb.internal.gaf.GafLine;
import org.kalypso.transformation.transformer.JTSTransformer;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.PrecisionModel;

/**
 * @author Gernot Belger
 */
public class ImportGafData extends AbstractModelObject
{
  public static final String PROPERTY_OPEN_LOG = "openLog"; //$NON-NLS-1$

  public static final String PROPERTY_SRS = "srs"; //$NON-NLS-1$

  public static final String PROPERTY_GAF_FILE = "gafFile"; //$NON-NLS-1$

  public static final String PROPERTY_WATER_BODY = "waterBody"; //$NON-NLS-1$

  public static final String PROPERTY_LOG_FILE = "logFile"; //$NON-NLS-1$

  private String m_srs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

  private File m_gafFile = null;

  /** We always create a new state when importing a gaf file */
  private final State m_state = new State();

  private WaterBody m_waterBody;

  private final IPdbConnection m_connection;

  private Coefficients m_coefficients;

  private PdbInfo m_info;

  private GeometryFactory m_geometryFactory;

  private GafLine[] m_lines;

  private GafProfiles m_gafProfiles;

  private IStatus m_readGafStatus;

  private GafPointCheck m_pointChecker;

  private File m_logFile;

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

  public void initFromDb( ) throws PdbConnectException, IOException
  {
    Session session = null;
    try
    {
      session = m_connection.openSession();

      m_coefficients = new Coefficients( session, IGafConstants.POINT_KIND_GAF );
      m_info = new PdbInfo( session );

      session.close();
    }
    finally
    {
      PdbUtils.closeSessionQuietly( session );
    }

    final GafCodes gafCodes = new GafCodes();
    final Coefficients coefficients = getCoefficients();
    m_pointChecker = new GafPointCheck( gafCodes, coefficients );
  }

  public String getSrs( )
  {
    return m_srs;
  }

  public void setSrs( final String srs )
  {
    final String oldValue = m_srs;

    m_srs = srs;

    final int targetSRID = m_info.getSRID();
    m_geometryFactory = new GeometryFactory( new PrecisionModel(), targetSRID );

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

    File logFile;
    if( m_gafFile == null )
      logFile = null;
    else
    {
      final String filename = m_gafFile.getName();

      final String stateSource = String.format( "Importiert aus: %s", filename );
      m_state.setSource( stateSource );

      m_state.setName( filename );

      final String logFilename = FilenameUtils.removeExtension( filename ) + ".log";//$NON-NLS-1$
      logFile = new File( m_gafFile.getParent(), logFilename );
    }

    setLogFile( logFile );
  }

  public State getState( )
  {
    return m_state;
  }

  public IPdbConnection getConnection( )
  {
    return m_connection;
  }

  public Coefficients getCoefficients( )
  {
    return m_coefficients;
  }

  public JTSTransformer getTransformer( )
  {
    try
    {
      final int sourceSRID = JTSAdapter.toSrid( m_srs );
      return new JTSTransformer( sourceSRID, m_info.getSRID() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  public void createProfiles( )
  {
    if( m_lines == null )
    {
      m_gafProfiles = null;
      return;
    }

    final LineString riverline = m_waterBody.getRiverlineAsLine();

    final IStatus readGafStatus = getReadGafStatus();

    m_gafProfiles = new GafProfiles( m_pointChecker, getTransformer(), riverline, readGafStatus );
    m_gafProfiles.addLines( m_lines );
  }

  public GeometryFactory getGeometryFactory( )
  {
    return m_geometryFactory;
  }

  public void setLines( final GafLine[] lines )
  {
    m_lines = lines;
    if( m_lines == null )
      return;

    m_pointChecker.cleanup();

    for( final GafLine line : lines )
    {
      if( line.getStatus().isOK() )
        m_pointChecker.check( line );
    }
  }

  public GafPointCheck getPointChecker( )
  {
    return m_pointChecker;
  }

  public void setReadGafStatus( final IStatus readGafStatus )
  {
    m_readGafStatus = readGafStatus;
  }

  public IStatus getReadGafStatus( )
  {
    return m_readGafStatus;
  }

  public GafProfiles getGafProfiles( )
  {
    return m_gafProfiles;
  }

  public File getLogFile( )
  {
    return m_logFile;
  }

  public void setLogFile( final File logFile )
  {
    m_logFile = logFile;
  }
}