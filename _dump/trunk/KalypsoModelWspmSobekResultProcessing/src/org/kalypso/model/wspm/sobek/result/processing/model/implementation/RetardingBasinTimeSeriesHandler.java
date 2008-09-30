package org.kalypso.model.wspm.sobek.result.processing.model.implementation;

import java.util.ArrayList;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.commons.metadata.MetadataObject;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;
import org.kalypso.model.wspm.sobek.result.processing.interfaces.IRetardingBasinNodeResultWrapper.RETARDING_BASIN_NODE_RESULT;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypsodeegree.model.feature.Feature;

public class RetardingBasinTimeSeriesHandler extends ResultTimeSeriesHandler
{
  private final RETARDING_BASIN_NODE_RESULT m_type;

  public RetardingBasinTimeSeriesHandler( RETARDING_BASIN_NODE_RESULT type, Feature feature, INode node )
  {
    super( feature, node );
    m_type = type;
  }

  public IObservation<TupleResult> getObservation( ) throws CoreException
  {
    final Object obj = getProperty( QN_RESULT_MEMBER );
    if( obj == null )
    {
      try
      {
        /* tuple result table does not exists, create a new tuple result member */
        final IPropertyType property = getFeatureType().getProperty( QN_RESULT_MEMBER );
        final IRelationType relation = (IRelationType) property;

        final Feature fObservation = getWorkspace().createFeature( this, relation, relation.getTargetFeatureType() );
        getWorkspace().setFeatureAsComposition( this, relation, fObservation, true );

        /* new tuple result set */
        final TupleResult result = new TupleResult();

        /* add components to result set */
        result.addComponent( ObservationFeatureFactory.createDictionaryComponent( fObservation, DICT_OBS_DATE ) );

        if( RETARDING_BASIN_NODE_RESULT.eWaterLevel.equals( m_type ) )
        {
          result.addComponent( ObservationFeatureFactory.createDictionaryComponent( fObservation, DICT_OBS_WATERLEVEL ) );
        }
        else if( RETARDING_BASIN_NODE_RESULT.eGrundAblass.equals( m_type ) )
        {
          result.addComponent( ObservationFeatureFactory.createDictionaryComponent( fObservation, DICT_OBS_GRUNDABLASS ) );
        }
        else if( RETARDING_BASIN_NODE_RESULT.eHochwasserEntlastung.equals( m_type ) )
        {
          result.addComponent( ObservationFeatureFactory.createDictionaryComponent( fObservation, DICT_OBS_HOCHWASSERENTLASTUNG ) );
        }

        /* add observation to workspace */
        final IObservation<TupleResult> obs = new Observation<TupleResult>( "name", "description", result, new ArrayList<MetadataObject>() ); //$NON-NLS-1$ //$NON-NLS-2$

        // maybe set phenomenon?
        ObservationFeatureFactory.toFeature( obs, fObservation );

        return obs;
      }
      catch( final Exception e )
      {
        throw new CoreException( StatusUtilities.createErrorStatus( e.getMessage() ) );
      }

    }
    final IObservation<TupleResult> observation = ObservationFeatureFactory.toObservation( (Feature) obj );

    return observation;
  }
}
