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
package org.kalypso.model.wspm.tuhh.ui.export.sobek.test;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.charset.Charset;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.java.net.UrlUtilities;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.ISobekProfileExportOperation;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.SobekExportInfo;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.SobekFrictionDatExportOperation;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.SobekProfileDatExportOperation;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.SobekProfileDefExportOperation;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.SobekStructDatExportOperation;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.SobekStructDefExportOperation;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Gernot Belger
 */
public class SobekExportTest extends Assert
{
  private String m_platformEncoding;

  private TuhhWspmProject m_project;

  private SobekExportInfo m_info;

  private File m_targetDir;

  private IProfileFeature[] m_profiles;

  @Before
  public void setup( ) throws Exception
  {
    m_platformEncoding = Charset.defaultCharset().name();

    m_targetDir = FileUtilities.createNewTempDir( "sobekExportText" );

    m_info = new SobekExportInfo( null, null );
    m_info.setIdPattern( "<Name>" ); //$NON-NLS-1$
    m_info.setIdSuffix( "_building" ); //$NON-NLS-1$
    m_info.setNamePattern( "<Name>" ); //$NON-NLS-1$
    m_info.setFlowZone( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    m_info.setExportBridges( true );
    m_info.setTargetDir( m_targetDir );
    m_info.setRoughnessID( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS );
    m_info.setRoughnessZoneTypes( m_info.getAllRoughnessZones() );

    final URL dataLocation = getClass().getResource( "resources/modell.gml.gz" ); //$NON-NLS-1$

    final GMLWorkspace wspmWorkspace = GmlSerializer.createGMLWorkspace( dataLocation, null );
    m_project = (TuhhWspmProject) wspmWorkspace.getRootFeature();

    final WspmWaterBody[] waterBodies = m_project.getWaterBodies();
    final IFeatureBindingCollection<IProfileFeature> profiles = waterBodies[0].getProfiles();
    m_profiles = profiles.toArray( new IProfileFeature[profiles.size()] );

    m_info.setProfiles( m_profiles );
  }

  @After
  public void tearDown( ) throws IOException
  {
    if( m_targetDir != null )
      FileUtils.deleteDirectory( m_targetDir );
  }

  private void testOperation( final ISobekProfileExportOperation exportOperation, final String filename ) throws IOException, CoreException
  {
    final File targetFile = new File( m_targetDir, filename );

    exportOperation.execute( new NullProgressMonitor() );

    final String actualContent = FileUtils.readFileToString( targetFile, m_platformEncoding );

    final URL expectedContentLocation = getClass().getResource( "resources/" + filename ); //$NON-NLS-1$
    final String expectedContent = UrlUtilities.readUrlToString( expectedContentLocation, m_platformEncoding );

    assertEquals( expectedContent, actualContent );
  }

  @Test
  public void exportProfileDef( ) throws IOException, CoreException
  {
    final ISobekProfileExportOperation exportOperation = new SobekProfileDefExportOperation( m_info );
    testOperation( exportOperation, SobekProfileDefExportOperation.PROFILE_DEF );
  }

  @Test
  public void exportProfileDat( ) throws IOException, CoreException
  {
    final ISobekProfileExportOperation exportOperation = new SobekProfileDatExportOperation( m_info );
    testOperation( exportOperation, SobekProfileDatExportOperation.PROFILE_DAT );
  }

  @Test
  public void exportStructDef( ) throws IOException, CoreException
  {
    final ISobekProfileExportOperation exportOperation = new SobekStructDefExportOperation( m_info );
    testOperation( exportOperation, SobekStructDefExportOperation.STRUCT_DEF );
  }

  @Test
  public void exportStructDat( ) throws IOException, CoreException
  {
    final ISobekProfileExportOperation exportOperation = new SobekStructDatExportOperation( m_info );
    testOperation( exportOperation, SobekStructDatExportOperation.STRUCT_DAT );
  }

  @Test
  public void exportFrictionDef( ) throws IOException, CoreException
  {
    final ISobekProfileExportOperation exportOperation = new SobekFrictionDatExportOperation( m_info );
    testOperation( exportOperation, SobekFrictionDatExportOperation.FRICTION_DAT );
  }

}
