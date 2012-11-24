package org.kalypso.kalypsomodel1d2d.schema.binding;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.util.pool.IModelAdaptor;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.ControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsosimulationmodel.core.VersionedModel;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Adapter from original 1d2d control model without ordinal number column and wrong sorting to version 1.0.
 */
public class ControlModelAdapter implements IModelAdaptor
{
  private static final String VERSION_1_0 = "1.0"; //$NON-NLS-1$

  private IStatus m_result = Status.OK_STATUS;

  @Override
  public GMLWorkspace adapt( final GMLWorkspace workspace, final IProgressMonitor monitor )
  {
    final Object property = workspace.getRootFeature().getProperty( VersionedModel.SIM_BASE_PROP_VERSION );
    if( property != null && property.equals( VERSION_1_0 ) )
    {
      // no need to adapt any other models than those without the version property
      return workspace;
    }

    m_result = execute( workspace, monitor );

    return workspace;
  }

  @Override
  public IStatus getResult( )
  {
    return m_result;
  }

  public IStatus execute( final GMLWorkspace workspace, final IProgressMonitor monitor )
  {
    final List<IStatus> statusList = new ArrayList<>();

    final Feature controlModelGroup = workspace.getRootFeature();

    final QName qnameProp1 = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "controlModelCollection" ); //$NON-NLS-1$
    final QName qnameProp2 = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "controlModelMember" ); //$NON-NLS-1$
    final IRelationType controlModelCollectionPropertyType = (IRelationType)controlModelGroup.getFeatureType().getProperty( qnameProp1 );
    final Feature controlModelCollection = (Feature)controlModelGroup.getProperty( controlModelCollectionPropertyType );
    if( controlModelCollection == null )
      return Status.OK_STATUS;

    // set version
    final IPropertyType versionProperty = controlModelGroup.getFeatureType().getProperty( VersionedModel.SIM_BASE_PROP_VERSION );
    controlModelGroup.setProperty( versionProperty, VERSION_1_0 );

    final IRelationType controlModelPropertyType = (IRelationType)controlModelCollection.getFeatureType().getProperty( qnameProp2 );
    final FeatureList controlModels = (FeatureList)controlModelCollection.getProperty( controlModelPropertyType );

    try
    {
      final int numberOfChanges = controlModels.size();
      final int amountOfWork = numberOfChanges * 10;
      monitor.beginTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.ControlModelAdapter.3" ), amountOfWork ); //$NON-NLS-1$

      for( final Object modelObject : controlModels )
      {
        final Feature feature = FeatureHelper.getFeature( workspace, modelObject );
        final Feature obsFeature = (Feature)feature.getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_TIMESTEPS_MEMBER );
        final IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation( obsFeature );
        final TupleResult result = obs.getResult();

        // FIXME: nonsene, we do not need the ordinal number withing the observation, this is just for opticts!
        if( result.getOrdinalNumberComponent() == null )
        {
          final IComponent componentTime = ObservationFeatureFactory.createDictionaryComponent( obsFeature, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
          final IComponent componentOrdnialNumber = ObservationFeatureFactory.createDictionaryComponent( obsFeature, Kalypso1D2DDictConstants.DICT_COMPONENT_ORDINAL_NUMBER );
          result.setSortComponents( new IComponent[] { componentTime } );
          result.setOrdinalNumberComponent( componentOrdnialNumber );
          ObservationFeatureFactory.toFeature( obs, obsFeature );
        }
        monitor.worked( 10 );
      }
    }
    catch( final Exception e )
    {
      statusList.add( StatusUtilities.statusFromThrowable( e ) );
    }
    finally
    {
      monitor.done();
    }

    if( statusList.size() > 0 )
      return StatusUtilities.createStatus( statusList, Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.ControlModelAdapter.4" ) ); //$NON-NLS-1$

    return new Status( IStatus.INFO, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.ControlModelAdapter.5" ) ); //$NON-NLS-1$
  }
}
