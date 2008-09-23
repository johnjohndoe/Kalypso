package org.kalypso.kalypsomodel1d2d.schema.binding;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.ControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.CompositeCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.util.pool.IModelAdaptor;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Adapter from original 1d2d control model without ordinal number column and wrong sorting to version 1.0.
 * 
 */
public class ControlModelAdapter implements IModelAdaptor
{
  private static final String VERSION_1_0 = "1.0";

  private IStatus m_result = Status.OK_STATUS;

  public CommandableWorkspace adapt( final CommandableWorkspace workspace )
  {
    final Object property = workspace.getRootFeature().getProperty( KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_VERSION );
    if( property != null && property.equals( VERSION_1_0 ) )
    {
      // no need to adapt any other models than those without the version property
      return workspace;
    }

    final String name = "Control model collection adapting";
    final Job job = new Job( name )
    {

      @Override
      protected IStatus run( IProgressMonitor monitor )
      {
        if( monitor == null )
        {
          monitor = new NullProgressMonitor();
        }
        final List<IStatus> statusList = new ArrayList<IStatus>();

        final Feature controlModelGroup = workspace.getRootFeature();

        final QName qnameProp1 = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "controlModelCollection" );
        final QName qnameProp2 = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "controlModelMember" );
        final IRelationType controlModelCollectionPropertyType = (IRelationType) controlModelGroup.getFeatureType().getProperty( qnameProp1 );
        final Feature controlModelCollection = (Feature) controlModelGroup.getProperty( controlModelCollectionPropertyType );
        if( controlModelCollection == null )
        {
          // happens if no control model is defined yet
          return StatusUtilities.createOkStatus( "Empty control model collection, nothnig to adapt" );
        }

        final IRelationType controlModelPropertyType = (IRelationType) controlModelCollection.getFeatureType().getProperty( qnameProp2 );
        final FeatureList controlModels = (FeatureList) controlModelCollection.getProperty( controlModelPropertyType );

        try
        {
          final CompositeCommand cc = new CompositeCommand( name );

          final int numberOfChanges = controlModels.size();
          final List<FeatureChange> featureChanges = new ArrayList<FeatureChange>( numberOfChanges );
          final int amountOfWork = numberOfChanges * 10;
          monitor.beginTask( "Control model collection adapting", amountOfWork );

          for( final Object modelObject : controlModels )
          {
            final Feature feature = FeatureHelper.getFeature( workspace, modelObject );
            final Feature obsFeature = (Feature) feature.getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_TIMESTEPS_MEMBER );
            final IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation( obsFeature );
            final TupleResult result = obs.getResult();
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

          // set version
          final IPropertyType versionProperty = controlModelGroup.getFeatureType().getProperty( KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_VERSION );
          featureChanges.add( new FeatureChange( controlModelGroup, versionProperty, VERSION_1_0 ) );

          // create command
          final ChangeFeaturesCommand changeFeaturesCommand = new ChangeFeaturesCommand( workspace, featureChanges.toArray( new FeatureChange[featureChanges.size()] ) );
          cc.addCommand( changeFeaturesCommand );

          workspace.postCommand( cc );
        }
        catch( final Exception e )
        {
          statusList.add( StatusUtilities.statusFromThrowable( e ) );
        }
        finally
        {
          monitor.done();
        }
        final IStatus resultStatus;
        if( statusList.size() > 0 )
          resultStatus = StatusUtilities.createStatus( statusList, "Problems during the control model adaptation." );
        else
          resultStatus = StatusUtilities.createInfoStatus( "Control model is adapted to version 1.0" );
        return resultStatus;
      }
    };

    job.schedule();
    try
    {
      job.join();
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
    }

    m_result = job.getResult();
    return workspace;
  }

  public IStatus getResult( )
  {
    return m_result;
  }
}
