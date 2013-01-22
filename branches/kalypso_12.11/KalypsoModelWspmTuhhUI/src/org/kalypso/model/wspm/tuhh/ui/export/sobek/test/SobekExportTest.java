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

import org.apache.commons.io.FileUtils;
import org.eclipse.compare.structuremergeviewer.Differencer;
import org.eclipse.compare.structuremergeviewer.ICompareInput;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.kalypso.commons.compare.DifferenceDumper;
import org.kalypso.commons.compare.FileContentAssertDumper;
import org.kalypso.commons.compare.IElementDumper;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.compare.FileStructureComparator;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.sobek.SobekModel;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.ISobekProfileExportOperation;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.SobekExportInfo;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.SobekExportOperation;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.SobekProfileExportOperation;
import org.kalypso.model.wspm.tuhh.ui.export.sobek.SobekProfileWriterOperation;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Gernot Belger
 */
public class SobekExportTest extends Assert
{
  private TuhhWspmProject m_project;

  private SobekExportInfo m_info;

  private File m_targetDir;

  private IProfileFeature[] m_profiles;

  @Before
  public void setup( ) throws Exception
  {
    m_targetDir = FileUtilities.createNewTempDir( "sobekExportTest" ); //$NON-NLS-1$

    m_info = new SobekExportInfo( null, null );
    m_info.setIdPattern( "<Station>_<Name>" ); //$NON-NLS-1$
    m_info.setIdSuffix( "_building" ); //$NON-NLS-1$
    m_info.setNamePattern( "<Name>" ); //$NON-NLS-1$
    m_info.setFlowZone( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    m_info.setExportBridges( true );
    m_info.setTargetDir( m_targetDir );
    m_info.setRoughnessID( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS );
    m_info.setRoughnessZoneTypes( m_info.getAllRoughnessZones() );

    final URL dataLocation = getClass().getResource( "/etc/test/resources/sobekexport/modell.gml.gz" ); //$NON-NLS-1$

    final GMLWorkspace wspmWorkspace = GmlSerializer.createGMLWorkspace( dataLocation, null );
    m_project = (TuhhWspmProject)wspmWorkspace.getRootFeature();

    final IFeatureBindingCollection<WspmWaterBody> waterBodies = m_project.getWaterBodies();
    final IFeatureBindingCollection<IProfileFeature> profiles = waterBodies.get( 0 ).getProfiles();
    m_profiles = profiles.toArray( new IProfileFeature[profiles.size()] );

    m_info.setProfiles( m_profiles );
  }

  @After
  public void tearDown( ) throws IOException
  {
    if( m_targetDir != null )
    {
      FileUtils.deleteDirectory( m_targetDir );
    }
  }

  @Test
  public void exportAll( ) throws IOException, CoreException
  {
    final SobekModel sobekModel = new SobekModel();

    final SobekProfileExportOperation converterOp = new SobekProfileExportOperation( m_info, sobekModel );
    final SobekProfileWriterOperation writerOp = new SobekProfileWriterOperation( m_info, sobekModel );

    final SobekExportOperation exportOperation = new SobekExportOperation( new ISobekProfileExportOperation[] { converterOp, writerOp } );

    exportOperation.execute( new NullProgressMonitor() );

    /* unzip expected results */
    final File expectedDir = FileUtilities.createNewTempDir( "sobekExportTestExpected" ); //$NON-NLS-1$
    final URL expectedResource = getClass().getResource( "/etc/test/resources/sobekexport/expectedResults.zip" ); //$NON-NLS-1$

//    final File xx = new File( "D:\\Temp\\expectedResults.zip" );
//    ZipUtilities.zip( xx, m_info.getTargetDir() );

    ZipUtilities.unzip( expectedResource, expectedDir );

    /* compare with expected results */
    final Differencer differencer = new Differencer();

    final FileStructureComparator actualComparator = new FileStructureComparator( m_info.getTargetDir() );
    final FileStructureComparator expectedComparator = new FileStructureComparator( expectedDir );

    final Object differences = differencer.findDifferences( false, new NullProgressMonitor(), null, null, expectedComparator, actualComparator );

    final IElementDumper elementDumper = new IElementDumper()
    {
      @Override
      public void dumpElement( final ICompareInput input )
      {
        new FileContentAssertDumper().dumpElement( input );
      }
    };

    final DifferenceDumper dumper = new DifferenceDumper( differences, elementDumper );
    dumper.dumpDifferences();

    /* clean up */
    FileUtils.deleteDirectory( expectedDir );
  }
}