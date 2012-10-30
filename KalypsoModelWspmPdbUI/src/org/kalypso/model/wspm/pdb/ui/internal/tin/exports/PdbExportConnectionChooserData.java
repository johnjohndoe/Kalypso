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
package org.kalypso.model.wspm.pdb.ui.internal.tin.exports;

import java.io.File;
import java.util.Date;
import java.util.List;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.SystemUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.hibernate.Session;
import org.kalypso.gml.ui.coverage.ImportCoverageData;
import org.kalypso.model.wspm.pdb.PdbUtils;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.GetPdbList;
import org.kalypso.model.wspm.pdb.db.PdbInfo;
import org.kalypso.model.wspm.pdb.db.mapping.DhmIndex;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.ui.internal.wspm.ConnectionChooserData;
import org.kalypso.transformation.transformer.GeoTransformerFactory;
import org.kalypso.transformation.transformer.IGeoTransformer;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.PrecisionModel;

/**
 * @author Holger Albert
 */
public class PdbExportConnectionChooserData extends ConnectionChooserData
{
  public static final String PROPERTY_DB_COORDINATE_SYSTEM = "dbCoordinateSystem"; //$NON-NLS-1$

  public static final String PROPERTY_DEM_SERVER_PATH = "demServerPath"; //$NON-NLS-1$

  public static final String PROPERTY_DHM_INDEX = "dhmIndex"; //$NON-NLS-1$

  private String m_dbCoordinateSystem;

  private IPath m_demServerPath;

  private DhmIndex m_dhmIndex;

  private final ImportCoverageData m_data;

  public PdbExportConnectionChooserData( final ImportCoverageData data )
  {
    m_dbCoordinateSystem = null;
    m_demServerPath = null;
    m_dhmIndex = null;
    m_data = data;
  }

  @Override
  public void setConnection( final IPdbConnection connection )
  {
    super.setConnection( connection );

    updateData( connection );
  }

  public void setDbCoordinateSystem( final String dbCoordinateSystem )
  {
    final String oldValue = m_dbCoordinateSystem;
    m_dbCoordinateSystem = dbCoordinateSystem;
    firePropertyChange( PROPERTY_DB_COORDINATE_SYSTEM, oldValue, m_dbCoordinateSystem );
  }

  public String getDbCoordinateSystem( )
  {
    return m_dbCoordinateSystem;
  }

  public void setDemServerPath( final IPath demServerPath )
  {
    final IPath oldValue = m_demServerPath;
    m_demServerPath = demServerPath;
    firePropertyChange( PROPERTY_DEM_SERVER_PATH, oldValue, m_demServerPath );
  }

  public IPath getDemServerPath( )
  {
    return m_demServerPath;
  }

  public void setDhmIndex( final DhmIndex dhmIndex )
  {
    final DhmIndex oldValue = m_dhmIndex;
    m_dhmIndex = dhmIndex;
    firePropertyChange( PROPERTY_DHM_INDEX, oldValue, m_dhmIndex );
  }

  public DhmIndex getDhmIndex( )
  {
    return m_dhmIndex;
  }

  public File getSourceFile( )
  {
    return m_data.getSelectedFiles()[0];
  }

  public ICoverage getNewCoverage( )
  {
    return m_data.getNewCoverages()[0];
  }

  private IStatus updateData( final IPdbConnection connection )
  {
    setDbCoordinateSystem( null );
    setDemServerPath( null );
    setDhmIndex( null );

    if( connection == null )
      return Status.OK_STATUS;

    Session session = null;
    try
    {
      session = connection.openSession();

      final PdbInfo info = new PdbInfo( session );
      final int srid = info.getSRID();
      final IPath demServerPath = info.getDemServerPath();
      final DhmIndex dhmIndex = findDhmIndex( GetPdbList.getList( session, DhmIndex.class ) );

      session.close();

      final String sourceSRS = m_data.getSourceSRS();
      dhmIndex.setMimeType( findMimeType() );
      dhmIndex.setEditingDate( new Date() );
      dhmIndex.setEditingUser( SystemUtils.USER_NAME );
      dhmIndex.setSrid( String.format( "%d", JTSAdapter.toSrid( sourceSRS ) ) ); //$NON-NLS-1$
      dhmIndex.setLocation( findLocation( srid ) );

      setDbCoordinateSystem( JTSAdapter.toSrs( srid ) );
      setDemServerPath( demServerPath );
      setDhmIndex( dhmIndex );

      return Status.OK_STATUS;
    }
    catch( final CoreException e )
    {
      return e.getStatus();
    }
    catch( final PdbConnectException ex )
    {
      return new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, Messages.getString("PdbExportConnectionChooserData_1"), ex ); //$NON-NLS-1$
    }
    finally
    {
      PdbUtils.closeSessionQuietly( session );
    }
  }

  private DhmIndex findDhmIndex( final List<DhmIndex> dhmIndexes )
  {
    final File file = getSourceFile();

    /* Check, if among the dhm indexes is already one, that must be updated. */
    for( final DhmIndex dhmIndex : dhmIndexes )
    {
      if( dhmIndex.getFilename().equalsIgnoreCase( file.getName() ) )
        return dhmIndex;
    }

    /* If not, create a new one. */
    final DhmIndex dhmIndex = new DhmIndex();
    dhmIndex.setFilename( file.getName() );
    dhmIndex.setCreationDate( new Date() );
    dhmIndex.setMeasurementDate( new Date() );

    return dhmIndex;
  }

  private String findMimeType( )
  {
    final File sourceFile = getSourceFile();
    final String extension = FilenameUtils.getExtension( sourceFile.getName() );

    if( "hmo".equals( extension ) ) //$NON-NLS-1$
      return "tin/hmo"; //$NON-NLS-1$

    if( "gml".equals( extension ) ) //$NON-NLS-1$
      return "tin/gml"; //$NON-NLS-1$

    if( "shp".equals( extension ) ) //$NON-NLS-1$
      return "tin/shp"; //$NON-NLS-1$

    if( "2dm".equals( extension ) ) //$NON-NLS-1$
      return "tin/2dm"; //$NON-NLS-1$

    if( "bin".equals( extension ) ) //$NON-NLS-1$
      return "image/bin"; //$NON-NLS-1$

    if( "tif".equals( extension ) || "tiff".equals( extension ) ) //$NON-NLS-1$ //$NON-NLS-2$
      return "image/tiff"; //$NON-NLS-1$

    return Messages.getString("PdbExportConnectionChooserData_15"); //$NON-NLS-1$
  }

  private Polygon findLocation( final int srid ) throws CoreException
  {
    try
    {
      final ICoverage coverage = getNewCoverage();
      final GM_Envelope boundedBy = coverage.getBoundedBy();

      final String srs = JTSAdapter.toSrs( srid );
      final IGeoTransformer geoTransformer = GeoTransformerFactory.getGeoTransformer( srs );
      final GM_Envelope transformedBoundedBy = geoTransformer.transform( boundedBy );
      final Envelope envelope = JTSAdapter.export( transformedBoundedBy );

      final GeometryFactory factory = new GeometryFactory( new PrecisionModel(), srid );
      final Geometry geometry = factory.toGeometry( envelope );

      return (Polygon) geometry;
    }
    catch( final Exception e )
    {
      throw new CoreException( new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, e.getLocalizedMessage(), e ) );
    }
  }
}