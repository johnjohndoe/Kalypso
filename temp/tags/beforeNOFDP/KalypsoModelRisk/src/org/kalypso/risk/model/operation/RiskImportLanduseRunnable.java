package org.kalypso.risk.model.operation;

import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.risk.model.actions.dataImport.landuse.Messages;
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

/**
 * @author Thomas Jung
 * 
 */
public final class RiskImportLanduseRunnable implements ICoreRunnableWithProgress
{

  private static final int DB_CREATE_NEW = 0;

  private static final int DB_IMPORT = 1;

  private static final int DB_USE_PREDEFINED = 2;

  private final static int WARNING_MAX_LANDUSE_CLASSES_NUMBER = 50;

  private static final QName PROP_NAME = new QName( NS.GML3, "name" ); //$NON-NLS-1$

  private static final QName PROP_DATA_MEMBER = new QName( KalypsoRiskSchemaCatalog.NS_PREDEFINED_DATASET, "dataMember" ); //$NON-NLS-1$

  private static final QName PROP_DESCRIPTION = new QName( NS.GML3, "description" ); //$NON-NLS-1$

  private static final QName PROP_VALUE = new QName( KalypsoRiskSchemaCatalog.NS_PREDEFINED_DATASET, "value" ); //$NON-NLS-1$

  private final String m_coordinateSystem;

  private final int m_selectedDatabaseOption;

  private final String m_assetValuesCollectionName;

  private final String m_landuseProperty;

  private final String m_damageFunctionsCollectionName;

  private final String m_sourceShapeFilePath;

  private final String m_externalProjectName;

  private boolean m_wrongLanduseSelectedStatus;

  private final IRasterizationControlModel m_controlModel;

  private final IVectorDataModel m_vectorModel;

  private final List<Feature> m_predefinedAssetValueClassesCollection;

  private final List<Feature> m_predefinedDamageFunctionsCollection;

  private final List<Feature> m_predefinedLanduseColorsCollection;

  private final IFolder m_scenarioFolder;

  public RiskImportLanduseRunnable( IRasterizationControlModel controlModel, IVectorDataModel vectorDataModel, String coordinateSystem, IFolder scenarioFolder, int selectedDatabaseOption, String assetValuesCollectionName, String landuseProperty, String damageFunctionsCollectionName, String sourceShapeFilePath, String externalProjectName, List<Feature> predefinedAssetValueClassesCollection, List<Feature> predefinedDamageFunctionsCollection, List<Feature> predefinedLanduseColorsCollection, boolean wrongLanduseSelectedStatus )
  {
    m_controlModel = controlModel;
    m_vectorModel = vectorDataModel;
    m_coordinateSystem = coordinateSystem;
    m_scenarioFolder = scenarioFolder;
    m_selectedDatabaseOption = selectedDatabaseOption;
    m_assetValuesCollectionName = assetValuesCollectionName;
    m_landuseProperty = landuseProperty;
    m_damageFunctionsCollectionName = damageFunctionsCollectionName;
    m_sourceShapeFilePath = sourceShapeFilePath;
    m_externalProjectName = externalProjectName;
    m_predefinedAssetValueClassesCollection = predefinedAssetValueClassesCollection;
    m_predefinedDamageFunctionsCollection = predefinedDamageFunctionsCollection;
    m_predefinedLanduseColorsCollection = predefinedLanduseColorsCollection;
    m_wrongLanduseSelectedStatus = wrongLanduseSelectedStatus;
  }

  @SuppressWarnings("unchecked")
  public IStatus execute( IProgressMonitor monitor )
  {
    monitor.beginTask( Messages.getString( "ImportLanduseWizard.1" ), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$
    try
    {
      monitor.subTask( Messages.getString( "ImportLanduseWizard.7" ) ); //$NON-NLS-1$

      final IFeatureWrapperCollection<ILandusePolygon> landusePolygonCollection = m_vectorModel.getLandusePolygonCollection();
      final GMLWorkspace landuseShapeWS = ShapeSerializer.deserialize( m_sourceShapeFilePath, m_coordinateSystem );

      final Feature shapeRootFeature = landuseShapeWS.getRootFeature();
      final List shapeFeatureList = (List) shapeRootFeature.getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER ); //$NON-NLS-1$ //$NON-NLS-2$

      /* create entries for landuse database */
      final HashSet<String> landuseTypeSet = new HashSet<String>();

      for( int i = 0; i < shapeFeatureList.size(); i++ )
      {
        final Feature shpFeature = (Feature) shapeFeatureList.get( i );
        final QName shapeLandusePropertyName = new QName( shpFeature.getFeatureType().getQName().getNamespaceURI(), m_landuseProperty );
        final String shpPropertyValue = shpFeature.getProperty( shapeLandusePropertyName ).toString();
        if( !landuseTypeSet.contains( shpPropertyValue ) )
          landuseTypeSet.add( shpPropertyValue );
      }

      m_wrongLanduseSelectedStatus = false;
      if( landuseTypeSet.size() > WARNING_MAX_LANDUSE_CLASSES_NUMBER )
      {
        IStatus status = null;
        status = isRightParameterUsed( m_landuseProperty );
        synchronized( this )
        {
          // while( status == null )
          // m_importLanduseWizard.wait( 100 );
        }
        if( Status.CANCEL_STATUS.equals( status ) )
        {
          m_wrongLanduseSelectedStatus = true;
          landuseTypeSet.clear();
          return status;
        }
      }

      monitor.subTask( Messages.getString( "ImportLanduseWizard.10" ) ); //$NON-NLS-1$

      landusePolygonCollection.clear();

      /* if there is no administration units defined, define the default one */
      final List<IAdministrationUnit> administrationUnits = m_controlModel.getAdministrationUnits();
      if( administrationUnits.size() == 0 )
        m_controlModel.createNewAdministrationUnit( "Default administration unit", "" ); //$NON-NLS-1$ //$NON-NLS-2$

      /* implement the importing of the existing database or predefined values */
      switch( m_selectedDatabaseOption )
      {
        case DB_CREATE_NEW:
          break;

        case DB_IMPORT:
          handleDBImport( m_externalProjectName, m_controlModel, m_scenarioFolder );
          break;

        case DB_USE_PREDEFINED:
          handleUsePreDefinedData( m_damageFunctionsCollectionName, m_assetValuesCollectionName, m_controlModel, administrationUnits, m_predefinedDamageFunctionsCollection, m_predefinedAssetValueClassesCollection );

          break;

        default:
          break;
      }

      /* create new landuse classes */
      createNewLanduseClasses( landuseTypeSet, m_controlModel, administrationUnits, m_predefinedLanduseColorsCollection );

      /* if there is no damage functions defined, define the default one */
      if( m_controlModel.getDamageFunctionsList().size() == 0 )
      {
        final IDamageFunction newDamageFunction = m_controlModel.createNewDamageFunction();
        newDamageFunction.setName( "Default function" ); //$NON-NLS-1$
        newDamageFunction.setFunction( "0.0" ); //$NON-NLS-1$
        newDamageFunction.setDescription( "Default damage function" ); //$NON-NLS-1$
      }

      final List<ILanduseClass> landuseClassesList = m_controlModel.getLanduseClassesList();

      // TODO try to guess damage function if no function is linked to the landuse class

      /* creating landuse polygons */
      final List<Feature> createdFeatures = createLandusePolygons( m_landuseProperty, monitor, m_scenarioFolder, shapeFeatureList, landusePolygonCollection, landuseClassesList );
      final GMLWorkspace workspace = m_vectorModel.getFeature().getWorkspace();
      workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, landusePolygonCollection.getFeature(), createdFeatures.toArray( new Feature[0] ), FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return StatusUtilities.statusFromThrowable( e, "Fehler beim Rastern der Landnutzung" );
    }
  }

  @SuppressWarnings("unchecked")
  private void handleUsePreDefinedData( final String damageFunctionsCollectionName, final String assetValuesCollectionName, final IRasterizationControlModel controlModel, final List<IAdministrationUnit> administrationUnits, List<Feature> predefinedDamageFunctionsCollection, List<Feature> predefinedAssetValueClassesCollection )
  {
    for( final Feature feature : predefinedDamageFunctionsCollection )
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
    for( final Feature feature : predefinedAssetValueClassesCollection )
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
                newLanduseClass.setColorStyle( getLanduseClassDefaultColor( landuseClassName, m_predefinedLanduseColorsCollection ) );

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
  }

  @SuppressWarnings("unchecked")
  private static void handleDBImport( final String externalProjectName, final IRasterizationControlModel controlModel, IFolder scenarioFolder )
  {
    try
    {
      final Map<String, String> landuseClassesGmlIDsMap = new HashMap<String, String>();
      final Map<String, String> damageFunctionsGmlIDsMap = new HashMap<String, String>();
      final Map<String, String> administrationUnitsGmlIDsMap = new HashMap<String, String>();
      final URL url = scenarioFolder.getProject().getParent().getRawLocation().append( "/" + externalProjectName + "/Basis/models/RasterizationControlModel.gml" ).toFile().toURL(); //$NON-NLS-1$ //$NON-NLS-2$
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
  }

  private static void createNewLanduseClasses( final HashSet<String> landuseTypeSet, final IRasterizationControlModel controlModel, final List<IAdministrationUnit> administrationUnits, List<Feature> predefinedLanduseColorsCollection )
  {
    for( final String landuseType : landuseTypeSet )
    {
      if( !controlModel.containsLanduseClass( landuseType ) )
      {
        final ILanduseClass landuseClass = controlModel.createNewLanduseClass();
        landuseClass.setName( landuseType );
        landuseClass.setColorStyle( getLanduseClassDefaultColor( landuseType, predefinedLanduseColorsCollection ) );
        landuseClass.setOrdinalNumber( controlModel.getNextAvailableLanduseClassOrdinalNumber() );
        landuseClass.setDescription( "Created as value of imported landuse shape" ); //$NON-NLS-1$
        for( final IAdministrationUnit administrationUnit : administrationUnits )
          controlModel.createNewAssetValueClass( landuseClass.getGmlID(), administrationUnit.getGmlID(), 0.0, "" ); //$NON-NLS-1$
      }
    }
  }

  @SuppressWarnings("unchecked")
  private List<Feature> createLandusePolygons( final String landuseProperty, final IProgressMonitor monitor, final IFolder scenarioFolder, final List shapeFeatureList, final IFeatureWrapperCollection<ILandusePolygon> landusePolygonCollection, final List<ILanduseClass> landuseClassesList ) throws CloneNotSupportedException
  {
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
          polygon.setLanduseClass( getLanduseClassByName( polygon.getFeature(), scenarioFolder, shpPropertyValue, landuseClassesList ) );
          polygon.getFeature().invalidEnvelope();
          createdFeatures.add( polygon.getFeature() );
          // stule and landuse class ordinal number will be set automatically (property functions)
        }
      }
      else if( shpGeometryProperty instanceof GM_Surface )
      {
        polygon.setGeometry( (GM_Surface< ? >) shpGeometryProperty );
        polygon.setLanduseClass( getLanduseClassByName( polygon.getFeature(), scenarioFolder, shpPropertyValue, landuseClassesList ) );
        // polygon.setStyleType( shpPropertyValue );
        polygon.getFeature().invalidEnvelope();
        createdFeatures.add( polygon.getFeature() );
      }
      else
        throw new RuntimeException( Messages.getString( "ImportLanduseWizard.4" ) + shpGeometryProperty.getClass().getName() ); //$NON-NLS-1$
    }
    return createdFeatures;
  }

  @SuppressWarnings("unchecked")
  private static RGB getLanduseClassDefaultColor( final String landuseClassName, final List<Feature> predefinedLanduseColorsCollection )
  {
    for( final Feature feature : predefinedLanduseColorsCollection )
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

  private static RGB getRandomColor( )
  {
    return new RGB( new Double( 255.0 * Math.random() ).intValue(), new Double( 255.0 * Math.random() ).intValue(), new Double( 255.0 * Math.random() ).intValue() );
  }

  static Feature getLanduseClassByName( final Feature feature, final IFolder projectFolder, final String className, List<ILanduseClass> landuseClassesList )
  {
    final String linkedFeaturePath = "project:/" + projectFolder.getProjectRelativePath().toPortableString() + "/models/RasterizationControlModel.gml#"; //$NON-NLS-1$ //$NON-NLS-2$
    for( final Object object : landuseClassesList )
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

  static IStatus isRightParameterUsed( final String shapeLandusePropertyName )
  {
    final Display display = PlatformUI.getWorkbench().getDisplay();
    final UIJob runnable = new UIJob( display, "proba" ) //$NON-NLS-1$
    {
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {
        if( MessageDialog.openQuestion( display.getActiveShell(), "Question", Messages.getString( "ImportLanduseWizard.60" ) + shapeLandusePropertyName //$NON-NLS-1$ //$NON-NLS-2$
            + Messages.getString( "ImportLanduseWizard.61" ) ) ) //$NON-NLS-1$
          return Status.OK_STATUS;
        else
          return Status.CANCEL_STATUS;
      }
    };
    runnable.setUser( true );
    runnable.schedule();
    return null;
  }

}