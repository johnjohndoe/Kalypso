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

  private NOperationFilterType m_baseFilter = null;

  private final int m_operation;

  private IObservation[] m_innerObservations = null;

  public NOperationFilter( final NOperationFilterType filter )
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
      throw new IllegalArgumentException( "unknown operator '" + operator
          + "' in filter" );
  }

  /**
   * @see org.kalypso.ogc.sensor.filter.IObservationFilter#initFilter(java.lang.Object, org.kalypso.ogc.sensor.IObservation)
   */
  public void initFilter( final Object conf, final IObservation baseObs )
      throws SensorException
  {
    super.initFilter( null, baseObs );
    
    m_innerObservations = (IObservation[]) conf;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#getValues(org.kalypso.util.runtime.IVariableArguments)
   */
  public ITuppleModel getValues( final IVariableArguments args )
      throws SensorException
  {
    final ITuppleModel models[] = new ITuppleModel[m_innerObservations.length];
    
    for( int i = 0; i < models.length; i++ )
      models[i] = m_innerObservations[i].getValues( args );
    
    return new NOperationTupplemodel( models, m_operation );
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservation#setValues(org.kalypso.ogc.sensor.ITuppleModel)
   */
  public void setValues( final ITuppleModel values ) throws SensorException
  {
    throw new UnsupportedOperationException( getClass().getName()
        + ".setValues() wird zur Zeit nicht unterstuetzt." );
  }
}