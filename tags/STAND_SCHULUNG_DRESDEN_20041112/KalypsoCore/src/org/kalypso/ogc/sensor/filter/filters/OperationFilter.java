package org.kalypso.ogc.sensor.filter.filters;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.util.runtime.IVariableArguments;
import org.kalypso.zml.filters.OperationFilterType;

/**
 * @author doemming
 */
public class OperationFilter extends AbstractObservationFilter
{
  public final static int OPERATION_UNKNOWN = 0;

  public final static int OPERATION_PLUS = 1;

  public final static int OPERATION_MINUS = 2;

  public final static int OPERATION_MAL = 3;

  public final static int OPERATION_DURCH = 4;

  private IObservation m_baseobservation = null;

  private final int m_operation;

  private final double m_operand;

  public OperationFilter( OperationFilterType filter )
  {
    m_operand = Double.parseDouble( filter.getOperand() );
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

  public void initFilter( Object dummy, IObservation baseObs ) throws SensorException
  {
    m_baseobservation = baseObs;
    super.initFilter( dummy, baseObs );
  }

  public ITuppleModel getValues( IVariableArguments args ) throws SensorException
  {
    return new OperationTupplemodel( m_operand, m_operation, m_baseobservation.getValues( args ) );

  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.ogc.sensor.IObservation#setValues(org.kalypso.ogc.sensor.ITuppleModel)
   */
  public void setValues( ITuppleModel values )
  {
    throw new UnsupportedOperationException( getClass().getName()
        + " setValues() wird zur Zeit nicht unterstuetzt ." );
  }

}