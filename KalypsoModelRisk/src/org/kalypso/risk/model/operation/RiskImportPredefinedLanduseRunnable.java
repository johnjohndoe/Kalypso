package org.kalypso.risk.model.operation;

import java.util.HashSet;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypso.risk.model.schema.binding.IDamageFunction;
import org.kalypso.risk.model.schema.binding.ILanduseClass;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IVectorDataModel;
import org.kalypso.risk.model.utils.RiskLanduseHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * @author Thomas Jung
 */
public final class RiskImportPredefinedLanduseRunnable implements ICoreRunnableWithProgress
{
  private static final QName PROP_DATA_MEMBER = new QName( KalypsoRiskSchemaCatalog.NS_PREDEFINED_DATASET, "dataMember" ); //$NON-NLS-1$

  private static final QName PROP_VALUE = new QName( KalypsoRiskSchemaCatalog.NS_PREDEFINED_DATASET, "value" ); //$NON-NLS-1$

  private final String m_assetValuesCollectionName;

  private final String m_landuseProperty;

  private final String m_damageFunctionsCollectionName;

  private final IRasterizationControlModel m_controlModel;

  private final IVectorDataModel m_vectorModel;

  private final List<Feature> m_predefinedAssetValueClassesCollection;

  private final List<Feature> m_predefinedDamageFunctionsCollection;

  private final List<Feature> m_predefinedLanduseColorsCollection;

  private final List< ? > m_shapeFeatureList;

  public RiskImportPredefinedLanduseRunnable( final IRasterizationControlModel controlModel, final IVectorDataModel vectorDataModel, final List< ? > shapeFeatureList, final String landuseProperty, final String assetValuesCollectionName, final String damageFunctionsCollectionName, final List<Feature> predefinedAssetValueClassesCollection, final List<Feature> predefinedDamageFunctionsCollection, final List<Feature> predefinedLanduseColorsCollection )
  {
    m_controlModel = controlModel;
    m_vectorModel = vectorDataModel;
    m_shapeFeatureList = shapeFeatureList;
    m_assetValuesCollectionName = assetValuesCollectionName;
    m_landuseProperty = landuseProperty;
    m_damageFunctionsCollectionName = damageFunctionsCollectionName;
    m_predefinedAssetValueClassesCollection = predefinedAssetValueClassesCollection;
    m_predefinedDamageFunctionsCollection = predefinedDamageFunctionsCollection;
    m_predefinedLanduseColorsCollection = predefinedLanduseColorsCollection;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    monitor.beginTask( Messages.getString( "org.kalypso.risk.model.actions.dataImport.landuse.ImportLanduseWizard.1" ), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$
    try
    {
      monitor.subTask( Messages.getString( "org.kalypso.risk.model.actions.dataImport.landuse.ImportLanduseWizard.7" ) ); //$NON-NLS-1$

      final IFeatureWrapperCollection<ILandusePolygon> landusePolygonCollection = m_vectorModel.getLandusePolygonCollection();

      /* create entries for landuse database */
      final HashSet<String> landuseTypeSet = RiskLanduseHelper.getLanduseTypeSet( m_shapeFeatureList, m_landuseProperty );

      landusePolygonCollection.clear();

      RiskLanduseHelper.handleUsePreDefinedData( m_damageFunctionsCollectionName, m_assetValuesCollectionName, m_controlModel, m_predefinedDamageFunctionsCollection, m_predefinedAssetValueClassesCollection, Feature.QN_NAME, PROP_DATA_MEMBER, Feature.QN_DESCRIPTION, PROP_VALUE );

      /* create new landuse classes */
      RiskLanduseHelper.createNewLanduseClasses( landuseTypeSet, m_controlModel, m_predefinedLanduseColorsCollection, Feature.QN_NAME, PROP_DATA_MEMBER, PROP_VALUE );

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
      final List<Feature> createdFeatures = RiskLanduseHelper.createLandusePolygons( m_landuseProperty, monitor, m_shapeFeatureList, landusePolygonCollection, landuseClassesList );
      final GMLWorkspace workspace = m_vectorModel.getFeature().getWorkspace();
      workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, landusePolygonCollection.getFeature(), createdFeatures.toArray( new Feature[0] ), FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.risk.model.operation.RiskImportPredefinedLanduseRunnable.4" ) ); //$NON-NLS-1$
    }
  }

}