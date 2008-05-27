package org.kalypso.model.wspm.sobek.result.processing.model.implementation;

import java.util.ArrayList;

import org.apache.commons.lang.NotImplementedException;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.commons.metadata.MetadataObject;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.sobek.result.processing.model.IResultTimeSeries;
import org.kalypso.model.wspm.sobek.result.processing.model.IValuePairMembers;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypsodeegree.model.feature.Feature;

public class ResultTimeSeriesHandler extends AbstractFeatureWrapper implements IResultTimeSeries
{
  public ResultTimeSeriesHandler( final Feature feature )
  {
    super( feature );
  }

  public String getBranchId( )
  {
    // FIXME
    throw new NotImplementedException();
  }

  public Double getLastValue( )
  {
    // FIXME
    throw new NotImplementedException();
  }

  public Double getMaxValue( )
  {
    // FIXME
    throw new NotImplementedException();
  }

  public String getName( )
  {
    // FIXME
    throw new NotImplementedException();
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

    // FIXME
    throw new NotImplementedException();
  }

  public String getParameterId( )
  {
    // FIXME
    throw new NotImplementedException();
  }

  public Double getStationBranchPosition( )
  {
    // FIXME
    throw new NotImplementedException();
  }

  public String getStationName( )
  {
    // FIXME
    throw new NotImplementedException();
  }

  public String getUniqueId( )
  {
    // FIXME
    throw new NotImplementedException();
  }

  public String getUnit( )
  {
    // FIXME
    throw new NotImplementedException();
  }

  public IValuePairMembers getValuePairs( )
  {
    // FIXME
    throw new NotImplementedException();
  }

}
