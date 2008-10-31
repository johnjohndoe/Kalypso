package org.kalypso.model.wspm.sobek.result.processing.model.implementation;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypsodeegree.model.feature.Feature;

public class NodeTimeSeriesHandler extends ResultTimeSeriesHandler
{
  public NodeTimeSeriesHandler( final Feature feature, final INode node )
  {
    super( feature, node );
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
        result.addComponent( ObservationFeatureFactory.createDictionaryComponent( fObservation, DICT_OBS_WATERLEVEL ) );

        /* add observation to workspace */
        final IObservation<TupleResult> obs = new Observation<TupleResult>( "name", "description", result ); //$NON-NLS-1$ //$NON-NLS-2$

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
