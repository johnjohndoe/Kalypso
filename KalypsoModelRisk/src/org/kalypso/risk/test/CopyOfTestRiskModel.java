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
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.ui.internal.Workbench;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.risk.model.operation.RiskImportDBLanduseRunnable;
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
public class CopyOfTestRiskModel extends TestCase
{
  public void testRiskModel( ) throws MalformedURLException, Exception
  {
    // unzip test project into workspace
    IWorkspace workspace = ResourcesPlugin.getWorkspace();
    IProject project = workspace.getRoot().getProject( "RiskTest" );
    project.create( new NullProgressMonitor() );

    final URL zipLocation = getClass().getResource( "resources/testProject.zip" );
    ZipUtilities.unzip( zipLocation, project, new NullProgressMonitor() );

    // run risk model
    final IFolder folder = project.getFolder( "testScenario" );

    KalypsoRiskDebug.OPERATION.printf( "%s", "Lade VectorDataModel.gml...\n" );
    final IFile riskVectorFile = folder.getFile( new Path( "models/VectorDataModel.gml" ) );
    GMLWorkspace riskVectorWorkspace = GmlSerializer.createGMLWorkspace( ResourceUtilities.createURL( riskVectorFile ), null );

    KalypsoRiskDebug.OPERATION.printf( "%s", "Lade RasterizationControlModel.gml...\n" );
    final IFile riskControlFile = folder.getFile( new Path( "models/RasterizationControlModel.gml" ) );
    GMLWorkspace riskControlWorkspace = GmlSerializer.createGMLWorkspace( ResourceUtilities.createURL( riskControlFile ), null );

    KalypsoRiskDebug.OPERATION.printf( "%s", "Lade RasterDataModel.gml...\n" );
    final IFile riskRasterFile = folder.getFile( new Path( "models/RasterDataModel.gml" ) );
    GMLWorkspace riskRasterWorkspace = GmlSerializer.createGMLWorkspace( ResourceUtilities.createURL( riskRasterFile ), null );

    KalypsoRiskDebug.OPERATION.printf( "%s", "Modelle geladen..." );
    // TODO: start operation on scenario folder

    final IVectorDataModel vectorDataModel = (IVectorDataModel) riskVectorWorkspace.getRootFeature().getAdapter( IVectorDataModel.class );
    final IRasterDataModel rasterDataModel = (IRasterDataModel) riskRasterWorkspace.getRootFeature().getAdapter( IRasterDataModel.class );
    final IRasterizationControlModel rasterControlDataModel = (IRasterizationControlModel) riskControlWorkspace.getRootFeature().getAdapter( IRasterizationControlModel.class );

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

    final GMLWorkspace landuseShapeWS = ShapeSerializer.deserialize( sourceShapeFilePath, crs );
    final Feature shapeRootFeature = landuseShapeWS.getRootFeature();
    final List shapeFeatureList = (List) shapeRootFeature.getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );

    /* IMPORT DATA */
    RiskImportDBLanduseRunnable importLanduseRunnable = new RiskImportDBLanduseRunnable( rasterControlDataModel, vectorDataModel, shapeFeatureList, folder, landUseProperty, externalProjectName, predefinedLanduseColorsCollection, wrongLandUseselectedStatus );

    RunnableContextHelper.execute( new ProgressMonitorDialog( Workbench.getInstance().getActiveWorkbenchWindow().getShell() ), true, false, importLanduseRunnable );

    /* raster landuse classes */
    final IFolder outputFolder = folder;

    RiskLanduseRasterizationRunnable landuseRasterRunnable = new RiskLanduseRasterizationRunnable( rasterDataModel, vectorDataModel, outputFolder );

    // check results?
    // TODO?

  }
}
