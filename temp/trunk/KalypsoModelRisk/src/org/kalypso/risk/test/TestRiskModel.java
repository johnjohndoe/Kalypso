/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.risk.test;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;

import junit.framework.TestCase;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.risk.model.operation.RiskImportLanduseRunnable;
import org.kalypso.risk.model.operation.RiskLanduseRasterizationRunnable;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IVectorDataModel;
import org.kalypso.risk.plugin.KalypsoRiskDebug;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * Run this test as plug-in test.
 * 
 * @author Thomas Jung
 * 
 */
public class TestRiskModel extends TestCase
{
  public void testRiskModel( ) throws MalformedURLException, Exception
  {
    // unzip test project into workspace
    IWorkspace workspace = ResourcesPlugin.getWorkspace();
    IProject project = workspace.getRoot().getProject( "RiskTest" );
    project.create( new NullProgressMonitor() );

    final URL zipLocation = getClass().getResource( "resources/testProject.zip" );
    ZipUtilities.unzipToContainer( zipLocation, project, new NullProgressMonitor() );

    // run risk model
    final IFolder folder = project.getFolder( "testScenario" );

    final IFile riskFile = folder.getFile( new Path( "models/risk.gml" ) );
    GMLWorkspace riskWorkspace = GmlSerializer.createGMLWorkspace( ResourceUtilities.createURL( riskFile ), null );
    riskWorkspace.getRootFeature().getAdapter( IModel.class );

    KalypsoRiskDebug.OPERATION.printf( "%.4f", 0, 12345 );
    // TODO: start operation on scenario folder

    final IVectorDataModel vectorDataModel = (IVectorDataModel) riskWorkspace.getRootFeature().getAdapter( IVectorDataModel.class );
    final IRasterDataModel rasterDataModel = (IRasterDataModel) riskWorkspace.getRootFeature().getAdapter( IRasterDataModel.class );
    final IRasterizationControlModel rasterControlDataModel = (IRasterizationControlModel) riskWorkspace.getRootFeature().getAdapter( IRasterizationControlModel.class );

    final int selectDatabaseOption = 2; // 0: new database, 1: from other project, 2: import from template

    String landUseProperty = "LANDUSE"; // name of the shape file field that represents the landuse classes
    String damageFunctionsCollectionName = "IKSE, Regionalisierung Schleswig-Holstein"; // name of the template
    String assetValuesCollectionName = "Regionalisierungsmethode Schleswig-Holstein"; // name of the template

    final String crs = "EPSG:31467"; // the coordinate system of the shape file
    String sourceShapeFilePath = null; // path to the landuse shapefile

    String externalProjectName = ""; // name of the external project from which the database will be taken
    // (selectDatabaseOption = 1)

    /* pre-definitions from template */
    List<Feature> predefinedAssetValueClassesCollection = null;
    List<Feature> predefinedDamageValueClassesCollection = null;
    List<Feature> predefinedLanduseColorsCollection = null;

    final boolean wrongLandUseselectedStatus = false; // status of landuse selection

    /* IMPORT DATA */
    RiskImportLanduseRunnable importLanduseRunnable = new RiskImportLanduseRunnable( rasterControlDataModel, vectorDataModel, crs, folder, selectDatabaseOption, assetValuesCollectionName, landUseProperty, damageFunctionsCollectionName, sourceShapeFilePath, externalProjectName, predefinedAssetValueClassesCollection, predefinedDamageValueClassesCollection, predefinedLanduseColorsCollection, wrongLandUseselectedStatus );

    /* raster landuse classes */
    final IFolder outputFolder = folder;

    RiskLanduseRasterizationRunnable landuseRasterRunnable = new RiskLanduseRasterizationRunnable( vectorDataModel, rasterDataModel, outputFolder );

    // check results?
    // TODO?

  }
}
