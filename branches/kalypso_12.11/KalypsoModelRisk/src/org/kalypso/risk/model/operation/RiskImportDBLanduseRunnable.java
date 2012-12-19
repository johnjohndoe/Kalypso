package org.kalypso.risk.model.operation;

import java.util.List;
import java.util.Set;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFolder;
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
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree_impl.gml.binding.shape.AbstractShape;

/**
 * @author Thomas Jung
 */
public final class RiskImportDBLanduseRunnable implements ICoreRunnableWithProgress
{
  private static final QName PROP_DATA_MEMBER = new QName( KalypsoRiskSchemaCatalog.NS_PREDEFINED_DATASET, "dataMember" ); //$NON-NLS-1$

  private static final QName PROP_VALUE = new QName( KalypsoRiskSchemaCatalog.NS_PREDEFINED_DATASET, "value" ); //$NON-NLS-1$

  private final String m_landuseProperty;

  private final List<AbstractShape> m_shapeFeatureList;

  private final String m_externalProjectName;

  private final IRasterizationControlModel m_controlModel;

  private final IVectorDataModel m_vectorModel;

  private final List<Feature> m_predefinedLanduseColorsCollection;

  private final IFolder m_scenarioFolder;

  public RiskImportDBLanduseRunnable( final IRasterizationControlModel controlModel, final IVectorDataModel vectorDataModel, final List<AbstractShape> shapeFeatureList, final IFolder scenarioFolder, final String landuseProperty, final String externalProjectName, final List<Feature> predefinedLanduseColorsCollection )
  {
    m_controlModel = controlModel;
    m_vectorModel = vectorDataModel;
    m_scenarioFolder = scenarioFolder;
    m_landuseProperty = landuseProperty;
    m_shapeFeatureList = shapeFeatureList;
    m_externalProjectName = externalProjectName;
    m_predefinedLanduseColorsCollection = predefinedLanduseColorsCollection;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    monitor.beginTask( Messages.getString( "org.kalypso.risk.model.actions.dataImport.landuse.ImportLanduseWizard.1" ), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$
    try
    {
      monitor.subTask( Messages.getString( "org.kalypso.risk.model.actions.dataImport.landuse.ImportLanduseWizard.7" ) ); //$NON-NLS-1$

      final IFeatureBindingCollection<ILandusePolygon> landusePolygonCollection = m_vectorModel.getLandusePolygonCollection().getLandusePolygonCollection();

      /* create entries for landuse database */
      final Set<String> landuseTypeSet = RiskLanduseHelper.getLanduseTypeSet( m_shapeFeatureList, m_landuseProperty );

      monitor.subTask( Messages.getString( "org.kalypso.risk.model.actions.dataImport.landuse.ImportLanduseWizard.10" ) ); //$NON-NLS-1$

      landusePolygonCollection.clear();

      RiskLanduseHelper.handleDBImport( m_externalProjectName, m_controlModel, m_scenarioFolder );

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
      final GMLWorkspace workspace = m_vectorModel.getWorkspace();
      workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, landusePolygonCollection.getParentFeature(), createdFeatures.toArray( new Feature[0] ), FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.risk.model.operation.RiskImportDBLanduseRunnable.4" ) ); //$NON-NLS-1$
    }
  }

}