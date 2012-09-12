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
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import junit.framework.TestCase;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.risk.model.actions.dataImport.waterdepth.AsciiRasterInfo;
import org.kalypso.risk.model.operation.RiskImportPredefinedLanduseRunnable;
import org.kalypso.risk.model.operation.RiskImportWaterdepthRunnable;
import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IVectorDataModel;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypso.risk.plugin.KalypsoRiskDebug;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.binding.shape.ShapeCollection;

/**
 * JUnit Test Case for the Kalypso Risk Model.<br>
 * This test extracts demo input data (landuse shape and waterdepth grids) from resources and converts them into risk
 * data format. <br>
 * This test only checks, if the input raster will be converted, but will not save the altered gml files.<br>
 *
 * As a next step, the test uses a pre-defined set of risk-gmls in order to create the output raster data for: <li>
 * specific damage <li>risk zones <br>
 * by using the converted input data (see above).
 *
 * <br>
 * <br>
 * Run this test as plug-in test.
 *
 * @author Thomas Jung
 *
 */
public class TestRiskModel extends TestCase
{
  private static final QName PROP_LANDUSE_COLORS_COLLECTION = new QName( KalypsoRiskSchemaCatalog.NS_PREDEFINED_DATASET, "landuseClassesDefaultColorsCollection" ); //$NON-NLS-1$

  private static final QName PROP_DAMAGE_FUNCTION_COLLECTION = new QName( KalypsoRiskSchemaCatalog.NS_PREDEFINED_DATASET, "damageFunctionsCollection" ); //$NON-NLS-1$

  private static final QName PROP_ASSET_VALUES_CLASSES_COLLECTION = new QName( KalypsoRiskSchemaCatalog.NS_PREDEFINED_DATASET, "assetValueClassesCollection" ); //$NON-NLS-1$

  @SuppressWarnings({ "unchecked" })
  public void testRiskModel( ) throws MalformedURLException, Exception
  {
    // unzip test project into workspace
    final IWorkspace workspace = ResourcesPlugin.getWorkspace();
    final IProject project = workspace.getRoot().getProject( "RiskTest" ); //$NON-NLS-1$
    project.create( new NullProgressMonitor() );

    final Display display = Display.getCurrent() != null ? Display.getCurrent() : Display.getDefault();
    final Shell shell = new Shell( display );

    final URL zipLocation = getClass().getResource( "resources/testProject.zip" ); //$NON-NLS-1$
    ZipUtilities.unzip( zipLocation, project, new NullProgressMonitor() );

    // run risk model
    final IFolder folder = project.getFolder( "testScenario" ); //$NON-NLS-1$
    final IFolder importDataFolder = project.getFolder( "Kelling_Stadt" ); //$NON-NLS-1$

    // load models
    KalypsoRiskDebug.OPERATION.printf( "%s", "Lade VectorDataModel.gml...\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    final IFile riskVectorFile = folder.getFile( new Path( "models/VectorDataModel.gml" ) ); //$NON-NLS-1$
    final GMLWorkspace riskVectorWorkspace = GmlSerializer.createGMLWorkspace( ResourceUtilities.createURL( riskVectorFile ), null );

    KalypsoRiskDebug.OPERATION.printf( "%s", "Lade RasterizationControlModel.gml...\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    final IFile riskControlFile = folder.getFile( new Path( "models/RasterizationControlModel.gml" ) ); //$NON-NLS-1$
    final GMLWorkspace riskControlWorkspace = GmlSerializer.createGMLWorkspace( ResourceUtilities.createURL( riskControlFile ), null );

    KalypsoRiskDebug.OPERATION.printf( "%s", "Lade RasterDataModel.gml...\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    final IFile riskRasterFile = folder.getFile( new Path( "models/RasterDataModel.gml" ) ); //$NON-NLS-1$
    final GMLWorkspace riskRasterWorkspace = GmlSerializer.createGMLWorkspace( ResourceUtilities.createURL( riskRasterFile ), null );

    KalypsoRiskDebug.OPERATION.printf( "%s", "Modelle geladen...\n\n" ); //$NON-NLS-1$ //$NON-NLS-2$

    final IVectorDataModel vectorDataModel = (IVectorDataModel) riskVectorWorkspace.getRootFeature().getAdapter( IVectorDataModel.class );
    final IRasterDataModel rasterDataModel = (IRasterDataModel) riskRasterWorkspace.getRootFeature().getAdapter( IRasterDataModel.class );
    final IRasterizationControlModel rasterControlDataModel = (IRasterizationControlModel) riskControlWorkspace.getRootFeature().getAdapter( IRasterizationControlModel.class );

    /* LANDUSE SHAPE */
    KalypsoRiskDebug.OPERATION.printf( "%s", "Lade Landnutzungs Shape...\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    final IFile shapeFile = importDataFolder.getFile( new Path( "Landuse/landuse" ) ); //$NON-NLS-1$

    final String sourceShapeFilePath = shapeFile.getLocation().toFile().toString();
    final String crs = "EPSG:31467"; // the coordinate system of the shape file //$NON-NLS-1$

    final ShapeCollection shapeCollection = ShapeSerializer.deserialize( sourceShapeFilePath, crs );

    // name of the shape file field that represents the landuse classes
    final String landUseProperty = "LANDUSE"; //$NON-NLS-1$

    /* pre-definitions from template */
    final IFile predefDataFile = folder.getFile( new Path( "models/PredefinedDataset.gml" ) ); //$NON-NLS-1$
    final GMLWorkspace predefinedDataWorkspace = GmlSerializer.createGMLWorkspace( ResourceUtilities.createURL( predefDataFile ), null );

    final List<Feature> predefinedLanduseColorsCollection = (FeatureList) predefinedDataWorkspace.getRootFeature().getProperty( PROP_LANDUSE_COLORS_COLLECTION );
    final List<Feature> predefinedDamageFunctionsCollection = (FeatureList) predefinedDataWorkspace.getRootFeature().getProperty( PROP_DAMAGE_FUNCTION_COLLECTION );
    final List<Feature> predefinedAssetValueClassesCollection = (FeatureList) predefinedDataWorkspace.getRootFeature().getProperty( PROP_ASSET_VALUES_CLASSES_COLLECTION );

    final String damageFunctionsCollectionName = "IKSE, Regionalisierung Schleswig-Holstein"; // name of the template //$NON-NLS-1$
    // //$NON-NLS-1$
    final String assetValuesCollectionName = "Regionalisierungsmethode Schleswig-Holstein"; // name of the template //$NON-NLS-1$
    // //$NON-NLS-1$

    /* IMPORT PREDEFINED DATA */
    KalypsoRiskDebug.OPERATION.printf( "%s", "Konvertiere Landnutzung in GML...\n" ); //$NON-NLS-1$ //$NON-NLS-2$

    final ICoreRunnableWithProgress importLanduseRunnable = new RiskImportPredefinedLanduseRunnable( rasterControlDataModel, vectorDataModel, shapeCollection, landUseProperty, assetValuesCollectionName, damageFunctionsCollectionName, predefinedAssetValueClassesCollection, predefinedDamageFunctionsCollection, predefinedLanduseColorsCollection );
    RunnableContextHelper.execute( new ProgressMonitorDialog( shell ), true, false, importLanduseRunnable );

    /* IMPORT WATERDEPTH */
    KalypsoRiskDebug.OPERATION.printf( "%s", "Importiere Fliesstiefenraster...\n" ); //$NON-NLS-1$ //$NON-NLS-2$

    // get the file locations as string
    final IFile rasterFile2 = importDataFolder.getFile( new Path( "Waterdepth/wsp_hq2.asc" ) ); //$NON-NLS-1$
    final IFile rasterFile5 = importDataFolder.getFile( new Path( "Waterdepth/wsp_hq5.asc" ) ); //$NON-NLS-1$
    final IFile rasterFile10 = importDataFolder.getFile( new Path( "Waterdepth/wsp_hq10.asc" ) ); //$NON-NLS-1$
    final IFile rasterFile20 = importDataFolder.getFile( new Path( "Waterdepth/wsp_hq20.asc" ) ); //$NON-NLS-1$
    final IFile rasterFile50 = importDataFolder.getFile( new Path( "Waterdepth/wsp_hq50.asc" ) ); //$NON-NLS-1$
    final IFile rasterFile100 = importDataFolder.getFile( new Path( "Waterdepth/wsp_hq100.asc" ) ); //$NON-NLS-1$

    final String raster2 = rasterFile2.getLocation().toFile().toString();
    final String raster5 = rasterFile5.getLocation().toFile().toString();
    final String raster10 = rasterFile10.getLocation().toFile().toString();
    final String raster20 = rasterFile20.getLocation().toFile().toString();
    final String raster50 = rasterFile50.getLocation().toFile().toString();
    final String raster100 = rasterFile100.getLocation().toFile().toString();

    final String[] fileNames = new String[] { raster2, raster5, raster10, raster20, raster50, raster100 };

    // create raster infos, in which the return period is set
    final List<AsciiRasterInfo> rasterInfos = new ArrayList<>();
    for( final String rasterFile : fileNames )
    {
      // TODO: handle coordinate Systems EPSG:31467)
      final AsciiRasterInfo rasterInfo = new AsciiRasterInfo( rasterFile );
      // guess the return period from file name or set it by hand...
      rasterInfo.setReturnPeriod( RiskModelHelper.guessReturnPeriodFromName( rasterFile ) );
      rasterInfos.add( rasterInfo );
    }

    // import the data into the IRasterDataModel
    final ICoreRunnableWithProgress importDepthRunnable = new RiskImportWaterdepthRunnable( rasterDataModel, rasterInfos, folder );
    RunnableContextHelper.execute( new ProgressMonitorDialog( shell ), true, false, importDepthRunnable );

    saveGml( riskVectorFile, riskVectorWorkspace );
    saveGml( riskControlFile, riskControlWorkspace );
    saveGml( riskRasterFile, riskRasterWorkspace );

    /* RASTER LANDUSE CLASSES */
    KalypsoRiskDebug.OPERATION.printf( "%s", "Erzeuge Landnutzungsraster...\n" ); //$NON-NLS-1$ //$NON-NLS-2$

    // final IFolder outputFolder = folder;
    // final ICoreRunnableWithProgress landuseRasterRunnable = new RiskLanduseRasterizationRunnable( rasterDataModel,
    // vectorDataModel, outputFolder );
    // RunnableContextHelper.execute( new ProgressMonitorDialog( shell ), true, true, landuseRasterRunnable );

    /* CREATE SPECIFIC DAMAGE */
    //    KalypsoRiskDebug.OPERATION.printf( "%s", "Erzeuge spezifischen Schaden je Fliesstiefe...\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    // final ICoreRunnableWithProgress runnableWithProgress = new RiskCalcSpecificDamageRunnable(
    // rasterControlDataModel, rasterDataModel, vectorDataModel, folder );
    // RunnableContextHelper.execute( new ProgressMonitorDialog( shell ), true, true, runnableWithProgress );

    /* CREATE RSIK ZONES */

    // FIXME: broken: this code should run on the original operation (which needs to be refaktored to be easily used),
    // but NOT of a copy/pasted operation; that makes really no sense for a test...

    //    KalypsoRiskDebug.OPERATION.printf( "%s", "Erzeuge Risikozonen...\n" ); //$NON-NLS-1$ //$NON-NLS-2$
    // final ICoreRunnableWithProgress calcRiskZonesRunnable = new RiskCalcRiskZonesRunnable( rasterDataModel,
    // vectorDataModel, rasterControlDataModel, folder );
    // RunnableContextHelper.execute( new ProgressMonitorDialog( shell ), true, true, calcRiskZonesRunnable );

    // TODO: generate ascii grid

    // check results?
    // TODO?
  }

  private void saveGml( final IFile file, final GMLWorkspace workspace ) throws CoreException
  {
    GmlSerializer.serializeWorkspace( file, workspace, new NullProgressMonitor() );
  }
}