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
import org.junit.Assert;
import org.junit.Test;
import org.kalypso.contribs.java.net.UrlUtilities;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.SobekExportInfo;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.SobekProfileDefExportOperation;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.SobekStructDefExportOperation;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Gernot Belger
 */
public class SobekExportTest extends Assert
{
  private final String m_platformEncoding;

  private final TuhhWspmProject m_project;

  private final SobekExportInfo m_info;

  public SobekExportTest( ) throws Exception
  {
    m_platformEncoding = Charset.defaultCharset().name();

    m_info = new SobekExportInfo( null, null );
    m_info.setIdPattern( "<Name>" ); //$NON-NLS-1$
    m_info.setIdSuffix( "_building" ); //$NON-NLS-1$
    m_info.setNamePattern( "<Name>" ); //$NON-NLS-1$
    m_info.setFlowZone( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    m_info.setExportBridges( true );

    final URL dataLocation = getClass().getResource( "resources/modell.gml.gz" ); //$NON-NLS-1$

    final GMLWorkspace wspmWorkspace = GmlSerializer.createGMLWorkspace( dataLocation, null );
    m_project = (TuhhWspmProject) wspmWorkspace.getRootFeature();
  }

  @Test
  public void exportProfileDef( ) throws IOException, CoreException
  {
    final URL expectedContentLocation = getClass().getResource( "resources/profile.def" ); //$NON-NLS-1$
    final String expectedContent = UrlUtilities.readUrlToString( expectedContentLocation, m_platformEncoding );

    final WspmWaterBody[] waterBodies = m_project.getWaterBodies();
    final IFeatureBindingCollection<IProfileFeature> profiles = waterBodies[0].getProfiles();
    final IProfileFeature[] allProfiles = profiles.toArray( new IProfileFeature[profiles.size()] );

    final File targetFile = File.createTempFile( "profile", ".def" ); //$NON-NLS-1$ //$NON-NLS-2$

    m_info.setProfiles( allProfiles );

    final SobekProfileDefExportOperation exportOperation = new SobekProfileDefExportOperation( m_info );
    exportOperation.execute( new NullProgressMonitor() );

    final String actualContent = FileUtils.readFileToString( targetFile, m_platformEncoding );

    assertEquals( expectedContent, actualContent );

    targetFile.delete();
  }

  @Test
  public void exportStructDef( ) throws IOException, CoreException
  {
    final URL expectedContentLocation = getClass().getResource( "resources/struct.def" ); //$NON-NLS-1$
    final String expectedContent = UrlUtilities.readUrlToString( expectedContentLocation, m_platformEncoding );

    final WspmWaterBody[] waterBodies = m_project.getWaterBodies();
    final IFeatureBindingCollection<IProfileFeature> profiles = waterBodies[0].getProfiles();
    final IProfileFeature[] allProfiles = profiles.toArray( new IProfileFeature[profiles.size()] );

    final File targetFile = File.createTempFile( "struct", ".def" ); //$NON-NLS-1$ //$NON-NLS-2$

    m_info.setProfiles( allProfiles );

    final SobekStructDefExportOperation exportOperation = new SobekStructDefExportOperation( m_info );
    exportOperation.execute( new NullProgressMonitor() );

    final String actualContent = FileUtils.readFileToString( targetFile, m_platformEncoding );

    assertEquals( expectedContent, actualContent );

    targetFile.delete();
  }

}
