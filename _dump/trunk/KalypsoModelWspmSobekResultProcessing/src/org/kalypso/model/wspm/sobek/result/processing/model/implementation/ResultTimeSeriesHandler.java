package org.kalypso.model.wspm.sobek.result.processing.model.implementation;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.model.wspm.sobek.core.interfaces.IBranch;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;
import org.kalypso.model.wspm.sobek.result.processing.i18n.Messages;
import org.kalypso.model.wspm.sobek.result.processing.model.IResultTimeSeries;
import org.kalypso.model.wspm.sobek.result.processing.model.IValuePairMembers;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;

public abstract class ResultTimeSeriesHandler extends AbstractFeatureWrapper implements IResultTimeSeries
{
  private final INode m_node;

  public ResultTimeSeriesHandler( final Feature feature, final INode node )
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

  @Override
  public String getName( )
  {
    return (String) getProperty( Feature.QN_NAME );
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

  public Double getPositionOnBranch( final IBranch branch ) throws GM_Exception, CoreException
  {
    final GM_Curve curve = branch.getCurve();
    final GM_Point location = m_node.getLocation();

    final LineString line = (LineString) JTSAdapter.export( curve );
    final Point point = (Point) JTSAdapter.export( location );

    final Geometry buffer = line.buffer( 0.1 );
    if( !point.intersects( buffer ) )
      throw new CoreException( StatusUtilities.createErrorStatus( String.format( Messages.ResultTimeSeriesHandler_0, m_node.getName(), branch.getName() ) ) );

    final Double distance = JTSUtilities.pointDistanceOnLine( line, point );

    return distance;
  }
}
