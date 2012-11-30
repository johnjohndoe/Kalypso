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
package org.kalypso.risk.model.actions.dataImport.landuse;

import java.net.URL;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.swt.awt.SWT_AWT_Utilities;
import org.kalypso.kalypsosimulationmodel.utils.SLDHelper;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.operation.RiskImportDBLanduseRunnable;
import org.kalypso.risk.model.operation.RiskImportNewLanduseRunnable;
import org.kalypso.risk.model.operation.RiskImportPredefinedLanduseRunnable;
import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypso.risk.model.schema.binding.ILanduseClass;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IVectorDataModel;
import org.kalypso.risk.plugin.KalypsoRiskPlugin;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.graphics.sld.Layer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.gml.binding.shape.AbstractShape;
import org.kalypsodeegree_impl.gml.binding.shape.ShapeCollection;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Dejan Antanaskovic
 */
public class ImportLanduseWizard extends Wizard implements INewWizard
{
  protected static final int DB_UNDEFINED_SELECTION = -1;

  protected static final int DB_CREATE_NEW = 0;

  protected static final int DB_IMPORT = 1;

  protected static final int DB_USE_PREDEFINED = 2;

  private static final String PREDEFINED_DATASET_PATH = "models/PredefinedDataset.gml"; //$NON-NLS-1$

  private static final int WARNING_MAX_LANDUSE_CLASSES_NUMBER = 50;

  private static final QName PROP_LANDUSE_COLORS_COLLECTION = new QName( KalypsoRiskSchemaCatalog.NS_PREDEFINED_DATASET, "landuseClassesDefaultColorsCollection" ); //$NON-NLS-1$

  private static final QName PROP_DAMAGE_FUNCTION_COLLECTION = new QName( KalypsoRiskSchemaCatalog.NS_PREDEFINED_DATASET, "damageFunctionsCollection" ); //$NON-NLS-1$

  private static final QName PROP_ASSET_VALUES_CLASSES_COLLECTION = new QName( KalypsoRiskSchemaCatalog.NS_PREDEFINED_DATASET, "assetValueClassesCollection" ); //$NON-NLS-1$

  private IStructuredSelection m_initialSelection;

  protected ImportLanduseWizardPage m_wizardPage;

  private final IFolder m_scenarioFolder;

  private List<Feature> m_predefinedLanduseColorsCollection;

  private List<Feature> m_predefinedDamageFunctionsCollection;

  private List<Feature> m_predefinedAssetValueClassesCollection;

  public ImportLanduseWizard( )
  {
    m_scenarioFolder = ScenarioHelper.getScenarioFolder();
  }

  @Override
  @SuppressWarnings("unchecked")
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    m_initialSelection = selection;
    setNeedsProgressMonitor( true );
    setWindowTitle( Messages.getString( "org.kalypso.risk.model.actions.dataImport.landuse.ImportLanduseWizard.0" ) ); //$NON-NLS-1$

    try
    {
      final URL url = m_scenarioFolder.getFile( PREDEFINED_DATASET_PATH ).getLocationURI().toURL();
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( url, null );
      m_predefinedLanduseColorsCollection = (FeatureList) workspace.getRootFeature().getProperty( PROP_LANDUSE_COLORS_COLLECTION );
      m_predefinedDamageFunctionsCollection = (FeatureList) workspace.getRootFeature().getProperty( PROP_DAMAGE_FUNCTION_COLLECTION );
      m_predefinedAssetValueClassesCollection = (FeatureList) workspace.getRootFeature().getProperty( PROP_ASSET_VALUES_CLASSES_COLLECTION );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  @Override
  public void addPages( )
  {
    m_wizardPage = new ImportLanduseWizardPage();

    final List<String> damageFunctionNamesList = new ArrayList<>();
    final List<String> assetValueClassesList = new ArrayList<>();

    for( final Feature feature : m_predefinedDamageFunctionsCollection )
    {
      final List<String> names = (List<String>) feature.getProperty( Feature.QN_NAME );
      if( names != null && names.size() > 0 && names.get( 0 ) != null )
        damageFunctionNamesList.add( names.get( 0 ) );
    }
    for( final Feature feature : m_predefinedAssetValueClassesCollection )
    {
      final List<String> names = (List<String>) feature.getProperty( Feature.QN_NAME );
      if( names != null && names.size() > 0 && names.get( 0 ) != null )
        assetValueClassesList.add( names.get( 0 ) );
    }

    m_wizardPage.init( m_initialSelection, damageFunctionNamesList, assetValueClassesList );
    addPage( m_wizardPage );
  }

  @Override
  public boolean canFinish( )
  {
    return m_wizardPage.isPageComplete();
  }

  /**
   * This method is called by the wizard framework when the user presses the Finish button.
   */
  @Override
  public boolean performFinish( )
  {
    final int selectedDatabaseOption = m_wizardPage.getSelectedDatabaseOption();
    final String externalProjectName = m_wizardPage.getExternalProjectName();
    final String damageFunctionsCollectionName = m_wizardPage.getDamageFunctionsCollectionName();
    final String assetValuesCollectionName = m_wizardPage.getAssetValuesCollectionName();
    final String landuseProperty = m_wizardPage.getLanduseProperty();
    final String sourceShapeFilePath = m_wizardPage.getSourceLocation().removeFileExtension().toPortableString();
    final String coordinateSystem = m_wizardPage.getCoordinateSystem();

    final IFolder scenarioFolder = ScenarioHelper.getScenarioFolder();
    final IScenarioDataProvider szenarioDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();

    try
    {
      final IVectorDataModel vectorDataModel = szenarioDataProvider.getModel( IVectorDataModel.class.getName() );
      final IRasterizationControlModel controlModel = szenarioDataProvider.getModel( IRasterizationControlModel.class.getName() );

      final ShapeCollection shapeCollection = ShapeSerializer.deserialize( sourceShapeFilePath, coordinateSystem );

      final TransformVisitor visitor = new TransformVisitor( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );

      final GMLWorkspace landuseShapeWS = shapeCollection.getWorkspace();
      final Feature shapeRootFeature = landuseShapeWS.getRootFeature();
      landuseShapeWS.accept( visitor, shapeRootFeature, FeatureVisitor.DEPTH_INFINITE );

      final IFeatureBindingCollection<AbstractShape> shapeFeatureList = shapeCollection.getShapes();

      /* check for right user selection */
      final Set<String> landuseTypeSet = new HashSet<>();

      int count = 0;
      for( final AbstractShape shpFeature : shapeFeatureList )
      {
        final QName shapeLandusePropertyName = new QName( shpFeature.getFeatureType().getQName().getNamespaceURI(), landuseProperty );
        final String shpPropertyValue = shpFeature.getProperty( shapeLandusePropertyName ).toString();
        if( !landuseTypeSet.contains( shpPropertyValue ) )
        {
          count++;

          landuseTypeSet.add( shpPropertyValue );

          if( count > WARNING_MAX_LANDUSE_CLASSES_NUMBER )
          {
            if( !SWT_AWT_Utilities.showSwtMessageBoxConfirm( org.kalypso.risk.i18n.Messages.getString( "org.kalypso.risk.model.actions.dataImport.landuse.ImportLanduseWizard.2" ), org.kalypso.risk.i18n.Messages.getString( "org.kalypso.risk.model.actions.dataImport.landuse.ImportLanduseWizard.3", WARNING_MAX_LANDUSE_CLASSES_NUMBER ) ) ) //$NON-NLS-1$ //$NON-NLS-2$
              return false;
            else
              break;
          }
        }
      }

      ICoreRunnableWithProgress importLanduseRunnable = null;

      /* implement the importing of the existing database or predefined values */
      switch( selectedDatabaseOption )
      {
        case DB_CREATE_NEW:
          importLanduseRunnable = new RiskImportNewLanduseRunnable( controlModel, vectorDataModel, shapeFeatureList, landuseProperty, m_predefinedLanduseColorsCollection );
          break;

        case DB_IMPORT:
          importLanduseRunnable = new RiskImportDBLanduseRunnable( controlModel, vectorDataModel, shapeFeatureList, scenarioFolder, landuseProperty, externalProjectName, m_predefinedLanduseColorsCollection );
          break;

        case DB_USE_PREDEFINED:
          importLanduseRunnable = new RiskImportPredefinedLanduseRunnable( controlModel, vectorDataModel, shapeCollection, landuseProperty, assetValuesCollectionName, damageFunctionsCollectionName, m_predefinedAssetValueClassesCollection, m_predefinedDamageFunctionsCollection, m_predefinedLanduseColorsCollection );

          break;

        default:
          break;
      }

      if( importLanduseRunnable == null )
        return false;

      final IStatus execute = RunnableContextHelper.execute( getContainer(), true, true, importLanduseRunnable );
      ErrorDialog.openError( getShell(), Messages.getString( "org.kalypso.risk.model.actions.dataImport.landuse.ImportLanduseWizard.4" ), "", execute ); //$NON-NLS-1$ //$NON-NLS-2$

      if( !execute.isOK() )
      {
        KalypsoRiskPlugin.getDefault().getLog().log( execute );
      }

      szenarioDataProvider.postCommand( IRasterizationControlModel.class.getName(), new EmptyCommand( "Get dirty!", false ) ); //$NON-NLS-1$
      szenarioDataProvider.postCommand( IVectorDataModel.class.getName(), new EmptyCommand( "Get dirty!", false ) ); //$NON-NLS-1$

      /* creating styles */
      final IFile polygonSldFile = scenarioFolder.getFile( "styles/LanduseVector.sld" ); //$NON-NLS-1$
      if( polygonSldFile.exists() )
        polygonSldFile.delete( false, new NullProgressMonitor() );

      final List<ILanduseClass> landuseClassesList = controlModel.getLanduseClassesList();

      final List<Layer> layers = new ArrayList<>();
      layers.add( SLDHelper.polygonStyleLayer( null, landuseClassesList, ILandusePolygon.PROPERTY_GEOMETRY, ILandusePolygon.PROPERTY_SLDSTYLE, null, null, new NullProgressMonitor() ) );
      SLDHelper.exportPolygonSymbolyzerSLD( polygonSldFile, layers.toArray( (new Layer[0]) ), new NullProgressMonitor() ); //$NON-NLS-1$ //$NON-NLS-2$

      final IFile rasterSldFile = scenarioFolder.getFile( "styles/LanduseCoverage.sld" ); //$NON-NLS-1$
      if( rasterSldFile.exists() )
        rasterSldFile.delete( false, new NullProgressMonitor() );
      SLDHelper.exportRasterSymbolyzerSLD( rasterSldFile, landuseClassesList, "Kalypso style", "Kalypso style", new NullProgressMonitor() ); //$NON-NLS-1$ //$NON-NLS-2$

    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return true;
  }
}