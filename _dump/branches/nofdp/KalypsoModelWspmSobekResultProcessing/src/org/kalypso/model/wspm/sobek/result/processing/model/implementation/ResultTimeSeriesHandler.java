package org.kalypso.model.wspm.sobek.result.processing.model.implementation;

import java.util.ArrayList;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.commons.metadata.MetadataObject;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;
import org.kalypso.model.wspm.sobek.result.processing.model.IResultTimeSeries;
import org.kalypso.model.wspm.sobek.result.processing.model.IValuePairMembers;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;

public class ResultTimeSeriesHandler extends AbstractFeatureWrapper implements IResultTimeSeries
{
  private final INode m_node;

  public ResultTimeSeriesHandler( final Feature feature, INode node )
  {
    super( feature );
    m_node = node;
  }

  public Double getLastValue( )
  {
    return (Double) getProperty( QN_LAST_VALUE );
  }

  public Double getMaxValue( )
  {
    return (Double) getProperty( QN_MAX_VALUE );
  }

  public String getName( )
  {
    return (String) getProperty( QN_NAME );
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

    final IObservation<TupleResult> observation = ObservationFeatureFactory.toObservation( (Feature) obj );

    return observation;
  }

  public String getParameterId( )
  {
    return (String) getProperty( QN_PARAM_ID );
  }

  public String getStationName( )
  {
    return (String) getProperty( QN_STATION_NAME );
  }

  public String getUniqueId( )
  {
    return (String) getProperty( QN_UNIQUE_ID );
  }

  public String getUnit( )
  {
    return (String) getProperty( QN_UNIT );
  }

  public IValuePairMembers getValuePairs( )
  {
    final Object property = getProperty( QN_VALUE_PAIRS );
    if( property instanceof FeatureList )
      return new ValuePairMembersHandler( (FeatureList) property );

    return null;
  }

  public Double getMinValue( )
  {
    return (Double) getProperty( QN_MIN_VALUE );
  }

  public Double getPositionOnBranch( IBranch branch ) throws GM_Exception, CoreException
  {
    final GM_Curve curve = branch.getCurve();
    final GM_Point location = m_node.getLocation();

    final LineString line = (LineString) JTSAdapter.export( curve );
    final Point point = (Point) JTSAdapter.export( location );

    Geometry buffer = line.buffer( 0.1 );
    if( !point.intersects( buffer ) )
      throw new CoreException( StatusUtilities.createErrorStatus( String.format( "INode %s is not connected with Branch %s", m_node.getName(), branch.getName() ) ) );

    final Double distance = JTSUtilities.pointDistanceOnLine( line, point );

    return distance;
  }
}
