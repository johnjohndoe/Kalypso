package org.kalypso.ogc.sensor.filter.filters;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.util.runtime.IVariableArguments;
import org.kalypso.zml.filters.NOperationFilterType;

/**
 * @author doemming
 */
public class NOperationFilter extends AbstractObservationFilter
{
  public final static int OPERATION_UNKNOWN = 0;

  public final static int OPERATION_PLUS = 1;

  public final static int OPERATION_MINUS = 2;

  public final static int OPERATION_MAL = 3;

  public final static int OPERATION_DURCH = 4;

  private final int m_operation;

  private IObservation[] m_innerObservations = null;

  public NOperationFilter( NOperationFilterType filter )
  {
    final String operator = filter.getOperator();
    if( operator.equals( "+" ) )
      m_operation = OPERATION_PLUS;
    else if( operator.equals( "-" ) )
      m_operation = OPERATION_MINUS;
    else if( operator.equals( "*" ) )
      m_operation = OPERATION_MAL;
    else if( operator.equals( "/" ) )
      m_operation = OPERATION_DURCH;
    else
      throw new IllegalArgumentException( "unknown operator '" + operator + "' in filter" );
  }

  public void initFilter( Object conf, IObservation baseObs ) throws SensorException
  {
    super.initFilter( null, baseObs );
    m_innerObservations = (IObservation[])conf;
  }

  public ITuppleModel getValues( IVariableArguments args ) throws SensorException
  {
    ITuppleModel models[] = new ITuppleModel[m_innerObservations.length];
    for( int i = 0; i < models.length; i++ )
      models[i] = m_innerObservations[i].getValues( args );
    return new NOperationTupplemodel( models, m_operation );
  }

  public void setValues( ITuppleModel values )
  {
    throw new UnsupportedOperationException( getClass().getName()
        + " setValues() wird zur Zeit nicht unterstuetzt ." );
  }
}