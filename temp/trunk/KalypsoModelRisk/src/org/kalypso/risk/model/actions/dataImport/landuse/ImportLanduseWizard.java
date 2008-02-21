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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.namespace.QName;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.kalypsosimulationmodel.utils.SLDHelper;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypso.risk.model.schema.binding.IAdministrationUnit;
import org.kalypso.risk.model.schema.binding.IAssetValueClass;
import org.kalypso.risk.model.schema.binding.IDamageFunction;
import org.kalypso.risk.model.schema.binding.ILanduseClass;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IVectorDataModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class ImportLanduseWizard extends Wizard implements INewWizard
{
  private final static int WARNING_MAX_LANDUSE_CLASSES_NUMBER = 50;

  protected static final int DB_UNDEFINED_SELECTION = -1;

  protected static final int DB_CREATE_NEW = 0;

  protected static final int DB_IMPORT = 1;

  protected static final int DB_USE_PREDEFINED = 2;

  private static final String PREDEFINED_DATASET_PATH = "models/PredefinedDataset.gml"; //$NON-NLS-1$

  private static final QName PROP_NAME = new QName( NS.GML3, "name" ); //$NON-NLS-1$

  private static final QName PROP_DESCRIPTION = new QName( NS.GML3, "description" ); //$NON-NLS-1$

  private static final QName PROP_VALUE = new QName( KalypsoRiskSchemaCatalog.NS_PREDEFINED_DATASET, "value" ); //$NON-NLS-1$

  private static final QName PROP_LANDUSE_COLORS_COLLECTION = new QName( KalypsoRiskSchemaCatalog.NS_PREDEFINED_DATASET, "landuseClassesDefaultColorsCollection" ); //$NON-NLS-1$

  private static final QName PROP_DAMAGE_FUNCTION_COLLECTION = new QName( KalypsoRiskSchemaCatalog.NS_PREDEFINED_DATASET, "damageFunctionsCollection" ); //$NON-NLS-1$

  private static final QName PROP_ASSET_VALUES_CLASSES_COLLECTION = new QName( KalypsoRiskSchemaCatalog.NS_PREDEFINED_DATASET, "assetValueClassesCollection" ); //$NON-NLS-1$

  private static final QName PROP_DATA_MEMBER = new QName( KalypsoRiskSchemaCatalog.NS_PREDEFINED_DATASET, "dataMember" ); //$NON-NLS-1$

  private IStructuredSelection m_initialSelection;

  protected ImportLanduseWizardPage m_wizardPage;

  private List<ILanduseClass> m_landuseClassesList;

  private final HashSet<String> m_landuseTypeSet;

  private final IFolder m_scenarioFolder;

  private List<Feature> m_predefinedLanduseColorsCollection;

  private List<Feature> m_predefinedDamageFunctionsCollection;

  private List<Feature> m_predefinedAssetValueClassesCollection;

  private IStatus m_status = null;

  private boolean m_wrongLanduseSelectedStatus = false;

  public ImportLanduseWizard( )
  {
    super();
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    m_scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
    m_landuseTypeSet = new HashSet<String>();
  }

  public void init( IWorkbench workbench, IStructuredSelection selection )
  {
    m_initialSelection = selection;
    setNeedsProgressMonitor( true );
    setWindowTitle( Messages.getString( "ImportLanduseWizard.0" ) ); //$NON-NLS-1$

    try
    {
      final URL url = m_scenarioFolder.getFile( PREDEFINED_DATASET_PATH ).getLocationURI().toURL();
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( url, new UrlResolver(), null );
      m_predefinedLanduseColorsCollection = (FeatureList) workspace.getRootFeature().getProperty( PROP_LANDUSE_COLORS_COLLECTION );
      m_predefinedDamageFunctionsCollection = (FeatureList) workspace.getRootFeature().getProperty( PROP_DAMAGE_FUNCTION_COLLECTION );
      m_predefinedAssetValueClassesCollection = (FeatureList) workspace.getRootFeature().getProperty( PROP_ASSET_VALUES_CLASSES_COLLECTION );
    }
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  @Override
  public void addPages( )
  {
    m_wizardPage = new ImportLanduseWizardPage();

    final List<String> damageFunctionNamesList = new ArrayList<String>();
    final List<String> assetValueClassesList = new ArrayList<String>();
    for( final Feature feature : m_predefinedDamageFunctionsCollection )
    {
      final List<String> names = (List<String>) feature.getProperty( PROP_NAME );
      if( names != null && names.size() > 0 && names.get( 0 ) != null )
        damageFunctionNamesList.add( names.get( 0 ) );
    }
    for( final Feature feature : m_predefinedAssetValueClassesCollection )
    {
      final List<String> names = (List<String>) feature.getProperty( PROP_NAME );
      if( names != null && names.size() > 0 && names.get( 0 ) != null )
        assetValueClassesList.add( names.get( 0 ) );
    }

    m_wizardPage.init( m_initialSelection, damageFunctionNamesList, assetValueClassesList );
    addPage( m_wizardPage );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#canFinish()
   */
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
    try
    {
      getContainer().run( true, true, new IRunnableWithProgress()
      {
        public void run( final IProgressMonitor monitor ) throws InterruptedException
        {
          monitor.beginTask( Messages.getString( "ImportLanduseWizard.1" ), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$
          try
          {
            monitor.subTask( Messages.getString( "ImportLanduseWizard.7" ) ); //$NON-NLS-1$
            final IWorkbench workbench = PlatformUI.getWorkbench();
            final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
            final IEvaluationContext context = handlerService.getCurrentState();
            final IFolder scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
            final SzenarioDataProvider szenarioDataProvider = (SzenarioDataProvider) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
            final IVectorDataModel vectorDataModel = szenarioDataProvider.getModel( IVectorDataModel.class );

            final GMLWorkspace landuseShapeWS = ShapeSerializer.deserialize( sourceShapeFilePath, coordinateSystem );

            final Feature shapeRootFeature = landuseShapeWS.getRootFeature();
            final List shapeFeatureList = (List) shapeRootFeature.getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER ); //$NON-NLS-1$ //$NON-NLS-2$

            // create entries for landuse database
            for( int i = 0; i < shapeFeatureList.size(); i++ )
            {
              final Feature shpFeature = (Feature) shapeFeatureList.get( i );
              final QName shapeLandusePropertyName = new QName( shpFeature.getFeatureType().getQName().getNamespaceURI(), landuseProperty );
              final String shpPropertyValue = shpFeature.getProperty( shapeLandusePropertyName ).toString();
              if( !m_landuseTypeSet.contains( shpPropertyValue ) )
                m_landuseTypeSet.add( shpPropertyValue );
            }

            m_wrongLanduseSelectedStatus = false;
            if( m_landuseTypeSet.size() > WARNING_MAX_LANDUSE_CLASSES_NUMBER )
            {
              m_status = null;
              isRightParameterUsed( landuseProperty );
              synchronized( this )
              {
                while( m_status == null )
                  wait( 100 );
              }
              if( Status.CANCEL_STATUS.equals( m_status ) )
              {
                m_wrongLanduseSelectedStatus = true;
                m_landuseTypeSet.clear();
                return;
              }
            }

            monitor.subTask( Messages.getString( "ImportLanduseWizard.10" ) ); //$NON-NLS-1$
            final IRasterizationControlModel controlModel = szenarioDataProvider.getModel( IRasterizationControlModel.class );

            // if there is no administration units defined, define the default one
            final List<IAdministrationUnit> administrationUnits = controlModel.getAdministrationUnits();
            if( administrationUnits.size() == 0 )
              controlModel.createNewAdministrationUnit( "Default administration unit", "" ); //$NON-NLS-1$ //$NON-NLS-2$

            final IFeatureWrapperCollection<ILandusePolygon> landusePolygonCollection = vectorDataModel.getLandusePolygonCollection();
            landusePolygonCollection.clear();

            // implement the importing of the existing database or predefined values
            switch( selectedDatabaseOption )
            {
              case DB_CREATE_NEW:
                break;

              case DB_IMPORT:
                try
                {
                  final Map<String, String> landuseClassesGmlIDsMap = new HashMap<String, String>();
                  final Map<String, String> damageFunctionsGmlIDsMap = new HashMap<String, String>();
                  final Map<String, String> administrationUnitsGmlIDsMap = new HashMap<String, String>();
                  final URL url = m_scenarioFolder.getProject().getParent().getRawLocation().append( "/" + externalProjectName + "/Basis/models/RasterizationControlModel.gml" ).toFile().toURL(); //$NON-NLS-1$ //$NON-NLS-2$
                  final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( url, new UrlResolver(), null );
                  final List<Feature> landuseClassesFeatureList = (FeatureList) workspace.getRootFeature().getProperty( IRasterizationControlModel.PROPERTY_LANDUSE_CLASS_MEMBER );
                  final List<Feature> assetValueClassesFeatureList = (FeatureList) workspace.getRootFeature().getProperty( IRasterizationControlModel.PROPERTY_ASSET_VALUE_CLASS_MEMBER );
                  final List<Feature> damageFunctionsFeatureList = (FeatureList) workspace.getRootFeature().getProperty( IRasterizationControlModel.PROPERTY_DAMAGE_FUNCTION_MEMBER );
                  final List<Feature> administrationUnitsFeatureList = (FeatureList) workspace.getRootFeature().getProperty( IRasterizationControlModel.PROPERTY_ADMINISTRATION_UNIT_MEMBER );
                  for( final Feature importedFeature : damageFunctionsFeatureList )
                  {
                    final IDamageFunction entry = (IDamageFunction) importedFeature.getAdapter( IDamageFunction.class );
                    if( entry != null )
                    {
                      final IDamageFunction newDamageFunction = controlModel.createNewDamageFunction();
                      newDamageFunction.setName( entry.getName() );
                      newDamageFunction.setDescription( "[Import from " + externalProjectName + "] " + entry.getDescription() ); //$NON-NLS-1$ //$NON-NLS-2$
                      newDamageFunction.setFunction( entry.getFunction() );
                      damageFunctionsGmlIDsMap.put( entry.getGmlID(), newDamageFunction.getGmlID() );
                    }
                  }
                  for( final Feature importedFeature : administrationUnitsFeatureList )
                  {
                    final IAdministrationUnit entry = (IAdministrationUnit) importedFeature.getAdapter( IAdministrationUnit.class );
                    if( entry != null )
                    {
                      final String description = "[Import from " + externalProjectName + "] " + entry.getDescription(); //$NON-NLS-1$ //$NON-NLS-2$
                      final IAdministrationUnit newAdministrationUnit = controlModel.createNewAdministrationUnit( entry.getName(), description );
                      administrationUnitsGmlIDsMap.put( entry.getGmlID(), newAdministrationUnit.getGmlID() );
                    }
                  }
                  for( final Feature importedFeature : landuseClassesFeatureList )
                  {
                    final ILanduseClass entry = (ILanduseClass) importedFeature.getAdapter( ILanduseClass.class );
                    if( entry != null )
                    {
                      final ILanduseClass newLanduseClass = controlModel.createNewLanduseClass();
                      newLanduseClass.setName( entry.getName() );
                      newLanduseClass.setDescription( "[Import from " + externalProjectName + "] " + entry.getDescription() ); //$NON-NLS-1$ //$NON-NLS-2$
                      newLanduseClass.setOrdinalNumber( controlModel.getNextAvailableLanduseClassOrdinalNumber() );
                      newLanduseClass.setColorStyle( entry.getColorStyle() );
                      final String damageFunctionGmlID = damageFunctionsGmlIDsMap.get( entry.getDamageFunctionGmlID() );
                      for( final IDamageFunction damageFunction : controlModel.getDamageFunctionsList() )
                        if( damageFunction.getGmlID().equals( damageFunctionGmlID ) )
                          newLanduseClass.setDamageFunction( damageFunction );
                      landuseClassesGmlIDsMap.put( entry.getGmlID(), newLanduseClass.getGmlID() );
                    }
                  }
                  for( final Feature importedFeature : assetValueClassesFeatureList )
                  {
                    final IAssetValueClass entry = (IAssetValueClass) importedFeature.getAdapter( IAssetValueClass.class );
                    if( entry != null )
                    {
                      final String landuseClassGmlID = landuseClassesGmlIDsMap.get( entry.getLanduseClassGmlID() );
                      final String administrationUnitGmlID = administrationUnitsGmlIDsMap.get( entry.getAdministrationUnitGmlID() );
                      final String description = "[Import from " + externalProjectName + "] " + entry.getDescription(); //$NON-NLS-1$ //$NON-NLS-2$
                      controlModel.createNewAssetValueClass( landuseClassGmlID, administrationUnitGmlID, entry.getAssetValue(), description );
                    }
                  }
                }
                catch( final Exception e )
                {
                  e.printStackTrace();
                }
                break;

              case DB_USE_PREDEFINED:
                for( final Feature feature : m_predefinedDamageFunctionsCollection )
                {
                  final List<String> names = (List<String>) feature.getProperty( PROP_NAME );
                  if( names != null && names.size() > 0 )
                    if( names.get( 0 ).equals( damageFunctionsCollectionName ) )
                    {
                      final List<Feature> dataMemberList = (List<Feature>) feature.getProperty( PROP_DATA_MEMBER );
                      for( final Feature featureMember : dataMemberList )
                      {
                        final List<String> nameList = ((List<String>) featureMember.getProperty( PROP_NAME ));
                        if( nameList != null && nameList.size() > 0 )
                        {
                          final String name = nameList.get( 0 );
                          final String value = featureMember.getProperty( PROP_VALUE ).toString();
                          final String description = (String) featureMember.getProperty( PROP_DESCRIPTION );
                          final IDamageFunction damageFunction = controlModel.createNewDamageFunction();
                          damageFunction.setName( name );
                          if( description != null && description.length() > 0 )
                            damageFunction.setDescription( description + " [" + damageFunctionsCollectionName + "]" ); //$NON-NLS-1$ //$NON-NLS-2$
                          else
                            damageFunction.setDescription( "[" + damageFunctionsCollectionName + "]" ); //$NON-NLS-1$ //$NON-NLS-2$
                          damageFunction.setFunction( value );
                        }
                      }
                      break;
                    }
                }
                for( final Feature feature : m_predefinedAssetValueClassesCollection )
                {
                  final List<String> names = (List<String>) feature.getProperty( PROP_NAME );
                  if( names != null && names.size() > 0 )
                    if( names.get( 0 ).equals( assetValuesCollectionName ) )
                    {
                      final List<Feature> dataMemberList = (List<Feature>) feature.getProperty( PROP_DATA_MEMBER );
                      for( final Feature featureMember : dataMemberList )
                      {
                        final List<String> nameList = ((List<String>) featureMember.getProperty( PROP_NAME ));
                        if( nameList != null && nameList.size() > 0 )
                        {
                          final String landuseClassName = nameList.get( 0 );
                          final Double assetValue = new Double( featureMember.getProperty( PROP_VALUE ).toString() );

                          if( !controlModel.containsLanduseClass( landuseClassName ) )
                          {
                            final ILanduseClass newLanduseClass = controlModel.createNewLanduseClass();
                            newLanduseClass.setName( landuseClassName );
                            newLanduseClass.setOrdinalNumber( controlModel.getNextAvailableLanduseClassOrdinalNumber() );
                            newLanduseClass.setColorStyle( getLanduseClassDefaultColor( landuseClassName ) );

                            // final String msg = String.format("Created by %s asset values import",
                            // assetValuesCollectionName );

                            newLanduseClass.setDescription( "Created by " + assetValuesCollectionName + " asset values import" ); //$NON-NLS-1$ //$NON-NLS-2$

                            final String landuseClassGmlID = newLanduseClass.getGmlID();
                            for( final IAdministrationUnit administrationUnit : administrationUnits )
                              controlModel.createNewAssetValueClass( landuseClassGmlID, administrationUnit.getGmlID(), assetValue, "[" + assetValuesCollectionName + "]" ); //$NON-NLS-1$ //$NON-NLS-2$
                          }
                          else
                          {
                            final String landuseClassGmlID = controlModel.getLanduseClassID( landuseClassName ).get( 0 );
                            for( final IAdministrationUnit administrationUnit : administrationUnits )
                            {
                              final IAssetValueClass assetValueClass = controlModel.getAssetValueClass( landuseClassGmlID, administrationUnit.getGmlID(), true );
                              assetValueClass.setAssetValue( assetValue );
                              assetValueClass.setDescription( "[" + assetValuesCollectionName + "]" ); //$NON-NLS-1$ //$NON-NLS-2$
                            }
                          }
                        }
                      }
                      break;
                    }
                }
                break;

              default:
                break;
            }

            m_landuseClassesList = controlModel.getLanduseClassesList();
            // m_landuseClassesList.clear();

            // load new landuse classes
            for( final String landuseType : m_landuseTypeSet )
            {
              if( !controlModel.containsLanduseClass( landuseType ) )
              {
                final ILanduseClass landuseClass = controlModel.createNewLanduseClass();
                landuseClass.setName( landuseType );
                landuseClass.setColorStyle( getLanduseClassDefaultColor( landuseType ) );
                landuseClass.setOrdinalNumber( controlModel.getNextAvailableLanduseClassOrdinalNumber() );
                landuseClass.setDescription( "Created as value of imported landuse shape" ); //$NON-NLS-1$
                for( final IAdministrationUnit administrationUnit : administrationUnits )
                  controlModel.createNewAssetValueClass( landuseClass.getGmlID(), administrationUnit.getGmlID(), 0.0, "" ); //$NON-NLS-1$
              }
            }

            // if there is no damage functions defined, define the default one
            if( controlModel.getDamageFunctionsList().size() == 0 )
            {
              final IDamageFunction newDamageFunction = controlModel.createNewDamageFunction();
              newDamageFunction.setName( "Default function" ); //$NON-NLS-1$
              newDamageFunction.setFunction( "0.0" ); //$NON-NLS-1$
              newDamageFunction.setDescription( "Default damage function" ); //$NON-NLS-1$
            }

            // TODO try to guess damage function if no function is linked to the landuse class

            szenarioDataProvider.postCommand( IRasterizationControlModel.class, new EmptyCommand( "Get dirty!", false ) ); //$NON-NLS-1$

            // creating landuse polygons
            monitor.subTask( Messages.getString( "ImportLanduseWizard.9" ) ); //$NON-NLS-1$
            final List<Feature> createdFeatures = new ArrayList<Feature>();
            for( int i = 0; i < shapeFeatureList.size(); i++ )
            {
              final Feature shpFeature = (Feature) shapeFeatureList.get( i );
              final QName shpPropQName = new QName( shpFeature.getFeatureType().getQName().getNamespaceURI(), landuseProperty );
              final String shpPropertyValue = shpFeature.getProperty( shpPropQName ).toString();
              // final String shpPropertyValue = shpPropQName.toString();
              final ILandusePolygon polygon = landusePolygonCollection.addNew( ILandusePolygon.QNAME );
              final GM_Object shpGeometryProperty = (GM_Object) shpFeature.getProperty( ShapeSerializer.PROPERTY_GEOMETRY );

              // we don't like multi surfaces, so...
              if( shpGeometryProperty instanceof GM_MultiSurface )
              {
                final GM_MultiSurface multiSurface = (GM_MultiSurface) ((GM_MultiSurface) shpGeometryProperty).clone();
                final GM_Surface< ? >[] surfaces = multiSurface.getAllSurfaces();
                for( int k = 0; k < surfaces.length; k++ )
                {
                  polygon.setGeometry( surfaces[k] );
                  polygon.setLanduseClass( getLanduseClassByName( polygon.getFeature(), scenarioFolder, shpPropertyValue ) );
                  polygon.getFeature().invalidEnvelope();
                  createdFeatures.add( polygon.getFeature() );
                  // stule and landuse class ordinal number will be set automatically (property functions)
                }
              }
              else if( shpGeometryProperty instanceof GM_Surface )
              {
                polygon.setGeometry( (GM_Surface< ? >) shpGeometryProperty );
                polygon.setLanduseClass( getLanduseClassByName( polygon.getFeature(), scenarioFolder, shpPropertyValue ) );
                // polygon.setStyleType( shpPropertyValue );
                polygon.getFeature().invalidEnvelope();
                createdFeatures.add( polygon.getFeature() );
              }
              else
                throw new RuntimeException( Messages.getString( "ImportLanduseWizard.4" ) + shpGeometryProperty.getClass().getName() ); //$NON-NLS-1$
            }
            // landuseModel.getWrappedFeature().invalidEnvelope();

            // fireModellEvent to redraw a map...
            final GMLWorkspace workspace = szenarioDataProvider.getCommandableWorkSpace( IVectorDataModel.class );
            workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, landusePolygonCollection.getFeature(), createdFeatures.toArray( new Feature[0] ), FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
            szenarioDataProvider.postCommand( IVectorDataModel.class, new EmptyCommand( "Get dirty!", false ) ); //$NON-NLS-1$

            // creating styles
            final IFile polygonSldFile = scenarioFolder.getFile( "styles/LanduseVector.sld" ); //$NON-NLS-1$
            if( polygonSldFile.exists() )
              polygonSldFile.delete( false, new NullProgressMonitor() );
            SLDHelper.exportPolygonSymbolyzerSLD( polygonSldFile, m_landuseClassesList, ILandusePolygon.PROPERTY_GEOMETRY, ILandusePolygon.PROPERTY_SLDSTYLE, "Kalypso style", "Kalypso style", null ); //$NON-NLS-1$ //$NON-NLS-2$

            final IFile rasterSldFile = scenarioFolder.getFile( "styles/LanduseCoverage.sld" ); //$NON-NLS-1$
            if( rasterSldFile.exists() )
              rasterSldFile.delete( false, new NullProgressMonitor() );
            SLDHelper.exportRasterSymbolyzerSLD( rasterSldFile, m_landuseClassesList, "Kalypso style", "Kalypso style", null ); //$NON-NLS-1$ //$NON-NLS-2$
          }
          catch( final Exception e )
          {
            if( e instanceof IllegalArgumentException )
            {
              throw new IllegalArgumentException( e.getLocalizedMessage() );
            }
            e.printStackTrace();
            throw new InterruptedException( e.getLocalizedMessage() );
          }
        }

      } );
    }
    catch( final Exception e )
    {
      if( e instanceof IllegalArgumentException )
      {
        return false;
      }
      ErrorDialog.openError( getShell(), Messages.getString( "ImportLanduseWizard.6" ), "", StatusUtilities.statusFromThrowable( e ) ); //$NON-NLS-1$ //$NON-NLS-2$
      e.printStackTrace();
      return false;
    }
    return !m_wrongLanduseSelectedStatus;
  }

  private Feature getLanduseClassByName( final Feature feature, final IFolder projectFolder, final String className )
  {
    final String linkedFeaturePath = "project:/" + projectFolder.getProjectRelativePath().toPortableString() + "/models/RasterizationControlModel.gml#"; //$NON-NLS-1$ //$NON-NLS-2$
    for( final Object object : m_landuseClassesList )
      if( object instanceof ILanduseClass )
      {
        final ILanduseClass landuseClass = (ILanduseClass) object;
        if( landuseClass.getName().equals( className ) )
        {
          final String xlinkedFeaturePath = linkedFeaturePath + landuseClass.getGmlID();
          final XLinkedFeature_Impl linkedFeature_Impl = new XLinkedFeature_Impl( feature, landuseClass.getFeature().getParentRelation(), landuseClass.getFeature().getFeatureType(), xlinkedFeaturePath, "", "", "", "", "" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
          return linkedFeature_Impl;
        }
      }
    return null;
  }

  private void isRightParameterUsed( final String shapeLandusePropertyName )
  {
    final Display display = PlatformUI.getWorkbench().getDisplay();
    final UIJob runnable = new UIJob( display, "proba" ) //$NON-NLS-1$
    {
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {
        if( MessageDialog.openQuestion( display.getActiveShell(), "Question", Messages.getString( "ImportLanduseWizard.60" ) + shapeLandusePropertyName //$NON-NLS-1$ //$NON-NLS-2$
            + Messages.getString( "ImportLanduseWizard.61" ) ) ) //$NON-NLS-1$
          m_status = Status.OK_STATUS;
        else
          m_status = Status.CANCEL_STATUS;
        return Status.OK_STATUS;
      }
    };
    runnable.setUser( true );
    runnable.schedule();
  }

  private RGB getLanduseClassDefaultColor( final String landuseClassName )
  {
    for( final Feature feature : m_predefinedLanduseColorsCollection )
    {
      final List<Feature> dataMemberList = (List<Feature>) feature.getProperty( PROP_DATA_MEMBER );
      for( final Feature featureMember : dataMemberList )
      {
        final List<String> nameList = ((List<String>) featureMember.getProperty( PROP_NAME ));
        if( nameList != null && nameList.size() > 0 )
        {
          if( !nameList.get( 0 ).equals( landuseClassName ) )
            continue;
          final String value = featureMember.getProperty( PROP_VALUE ).toString();
          final Pattern pattern = Pattern.compile( "#([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})" ); //$NON-NLS-1$
          final Matcher matcher = pattern.matcher( value );
          if( matcher.matches() )
          {
            final int r = Integer.parseInt( matcher.group( 1 ), 16 );
            final int g = Integer.parseInt( matcher.group( 2 ), 16 );
            final int b = Integer.parseInt( matcher.group( 3 ), 16 );
            return new RGB( r, g, b );
          }
          // If color format is not correct, return random color, or throw an exception?
          return getRandomColor();
        }
      }
      break;
    }
    return getRandomColor();
  }

  private RGB getRandomColor( )
  {
    return new RGB( new Double( 255.0 * Math.random() ).intValue(), new Double( 255.0 * Math.random() ).intValue(), new Double( 255.0 * Math.random() ).intValue() );
  }
}
