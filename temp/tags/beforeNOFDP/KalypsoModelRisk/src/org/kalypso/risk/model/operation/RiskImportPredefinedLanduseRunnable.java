package org.kalypso.risk.model.operation;

import java.util.HashSet;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.risk.model.actions.dataImport.landuse.Messages;
import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypso.risk.model.schema.binding.IAdministrationUnit;
import org.kalypso.risk.model.schema.binding.IDamageFunction;
import org.kalypso.risk.model.schema.binding.ILanduseClass;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IVectorDataModel;
import org.kalypso.risk.model.utils.RiskImportLanduseHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * @author Thomas Jung
 * 
 */
public final class RiskImportPredefinedLanduseRunnable implements ICoreRunnableWithProgress
{

  private final static int WARNING_MAX_LANDUSE_CLASSES_NUMBER = 50;

  private static final QName PROP_NAME = new QName( NS.GML3, "name" ); //$NON-NLS-1$

  private static final QName PROP_DATA_MEMBER = new QName( KalypsoRiskSchemaCatalog.NS_PREDEFINED_DATASET, "dataMember" ); //$NON-NLS-1$

  private static final QName PROP_DESCRIPTION = new QName( NS.GML3, "description" ); //$NON-NLS-1$

  private static final QName PROP_VALUE = new QName( KalypsoRiskSchemaCatalog.NS_PREDEFINED_DATASET, "value" ); //$NON-NLS-1$

  private final String m_assetValuesCollectionName;

  private final String m_landuseProperty;

  private final String m_damageFunctionsCollectionName;

  @SuppressWarnings("unused")
  private boolean m_wrongLanduseSelectedStatus;

  private final IRasterizationControlModel m_controlModel;

  private final IVectorDataModel m_vectorModel;

  private final List<Feature> m_predefinedAssetValueClassesCollection;

  private final List<Feature> m_predefinedDamageFunctionsCollection;

  private final List<Feature> m_predefinedLanduseColorsCollection;

  private final IFolder m_scenarioFolder;

  @SuppressWarnings("unchecked")
  private final List m_shapeFeatureList;

  @SuppressWarnings("unchecked")
  public RiskImportPredefinedLanduseRunnable( final IRasterizationControlModel controlModel, final IVectorDataModel vectorDataModel, final List shapeFeatureList, final IFolder scenarioFolder, final String landuseProperty, final String assetValuesCollectionName, final String damageFunctionsCollectionName, final List<Feature> predefinedAssetValueClassesCollection, final List<Feature> predefinedDamageFunctionsCollection, final List<Feature> predefinedLanduseColorsCollection, boolean wrongLanduseSelectedStatus )
  {
    m_controlModel = controlModel;
    m_vectorModel = vectorDataModel;
    m_shapeFeatureList = shapeFeatureList;
    m_scenarioFolder = scenarioFolder;
    m_assetValuesCollectionName = assetValuesCollectionName;
    m_landuseProperty = landuseProperty;
    m_damageFunctionsCollectionName = damageFunctionsCollectionName;
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

      /* create entries for landuse database */
      final HashSet<String> landuseTypeSet = new HashSet<String>();

      for( int i = 0; i < m_shapeFeatureList.size(); i++ )
      {
        final Feature shpFeature = (Feature) m_shapeFeatureList.get( i );
        final QName shapeLandusePropertyName = new QName( shpFeature.getFeatureType().getQName().getNamespaceURI(), m_landuseProperty );
        final String shpPropertyValue = shpFeature.getProperty( shapeLandusePropertyName ).toString();
        if( !landuseTypeSet.contains( shpPropertyValue ) )
          landuseTypeSet.add( shpPropertyValue );
      }

      m_wrongLanduseSelectedStatus = false;
      if( landuseTypeSet.size() > WARNING_MAX_LANDUSE_CLASSES_NUMBER )
      {
        IStatus status = null;
        status = RiskImportLanduseHelper.isRightParameterUsed( m_landuseProperty );
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

      RiskImportLanduseHelper.handleUserDefinedData( m_damageFunctionsCollectionName, m_assetValuesCollectionName, m_controlModel, administrationUnits, m_predefinedDamageFunctionsCollection, m_predefinedAssetValueClassesCollection, PROP_NAME, PROP_DATA_MEMBER, PROP_DESCRIPTION, PROP_VALUE, m_predefinedLanduseColorsCollection );

      /* create new landuse classes */
      RiskImportLanduseHelper.createNewLanduseClasses( landuseTypeSet, m_controlModel, administrationUnits, m_predefinedLanduseColorsCollection, PROP_NAME, PROP_DATA_MEMBER, PROP_VALUE );

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
      final List<Feature> createdFeatures = RiskImportLanduseHelper.createLandusePolygons( m_landuseProperty, monitor, m_scenarioFolder, m_shapeFeatureList, landusePolygonCollection, landuseClassesList );
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

}