package org.kalypso.ogc.sensor.timeseries.interpolation;

import java.util.Calendar;
import java.util.Date;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.filters.AbstractObservationFilter;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.util.math.LinearEquation;
import org.kalypso.util.math.LinearEquation.SameXValuesException;
import org.kalypso.util.runtime.IVariableArguments;
import org.kalypso.util.runtime.args.DateRangeArgument;

/**
 * InterpolationFilter
 * 
 * @author schlienger
 */
public class InterpolationFilter extends AbstractObservationFilter
{
  private final int m_calField;

  private final int m_amount;

  private final boolean m_fill;

  private final Double m_defValue;

  /**
   * Constructor.
   * 
   * @param calendarField
   *          which field of the date will be used for steping through the
   *          timeserie
   * @param amount
   *          amount of time for the step
   * @param forceFill
   *          when true, fills the model with defaultValue when no base value
   * @param defaultValue
   *          default value to use when filling absent values
   */
  public InterpolationFilter( final int calendarField, final int amount,
      final boolean forceFill, final double defaultValue )
  {
    m_calField = calendarField;
    m_amount = amount;
    m_fill = forceFill;
    m_defValue = new Double( defaultValue );
  }

  /**
   * @see org.kalypso.ogc.sensor.filter.filters.AbstractObservationFilter#getValues(org.kalypso.util.runtime.IVariableArguments)
   */
  public ITuppleModel getValues( IVariableArguments args )
      throws SensorException
  {
    final ITuppleModel values = super.getValues( args );

    DateRangeArgument dr = null;
    if( args instanceof DateRangeArgument )
      dr = (DateRangeArgument) args;

    final IAxis dateAxis = ObservationUtilities.findAxisByClass( values
        .getAxisList(), Date.class )[0];
    final IAxis[] valueAxes = ObservationUtilities.findAxisByClass( values
        .getAxisList(), Number.class );

    SimpleTuppleModel intModel = new SimpleTuppleModel( values.getAxisList() );

    if( values.getCount() == 0 )
      return values;
    
    final Date begin = (Date) values.getElement( 0, dateAxis );
    
    Date d1 = null;
    Date d2 = null;
    final double[] v1 = new double[ valueAxes.length + 1 ];
    final double[] v2 = new double[ valueAxes.length + 1 ];
    
    final Calendar cal = Calendar.getInstance();
    
    // do we need to fill before the begining of the base model?
    if( dr != null && m_fill )
    {
      cal.setTime( dr.getFrom() );
      
      while( cal.getTime().compareTo( begin ) < 0 )
        fillWithDefault( dateAxis, valueAxes, intModel, cal );
    }
    else
    {
      cal.setTime( begin );
    
      final Object[] tupple = new Object[ valueAxes.length + 1 ];
      tupple[ dateAxis.getPosition() ] = cal.getTime();
      
      for( int i = 0; i < valueAxes.length; i++ )
      {
        final Number nb = (Number) values.getElement( 0, valueAxes[i] );
        
        tupple[ valueAxes[i].getPosition() ] = nb;
        v1[ valueAxes[i].getPosition() ] = nb.doubleValue();
      }
      
      intModel.addTupple( tupple );
      
      cal.add( m_calField, m_amount );
    }

    d1 = cal.getTime();
    
    final LinearEquation eq = new LinearEquation();
        
    for( int ix = 1; ix < values.getCount(); ix++ )
    {
      d2 = (Date) values.getElement( ix, dateAxis );

      for( int ia = 0; ia < valueAxes.length; ia++ )
      {
        final Number nb = (Number) values.getElement( ix, valueAxes[ia] );
        v2[ valueAxes[ia].getPosition() ] = nb.doubleValue();
      }
      
      while( cal.getTime().compareTo( d2 ) < 0 )
      {
        long ms = cal.getTimeInMillis();

        Object[] tupple = new Object[valueAxes.length + 1];
        tupple[ dateAxis.getPosition() ] = cal.getTime();
        
        for( int ia = 0; ia < valueAxes.length; ia++ )
        {
          try
          {
            eq.setPoints( d1.getTime(), v1[valueAxes[ia].getPosition()], d2.getTime(), v2[valueAxes[ia].getPosition()] );
            tupple[valueAxes[ia].getPosition()] = new Double( eq.computeY( ms ) );
          }
          catch( SameXValuesException e )
          {
            tupple[valueAxes[ia].getPosition()] = m_defValue;
          }
        }

        intModel.addTupple( tupple );
        
        cal.add( m_calField, m_amount );
      }
      
      d1 = d2;
      System.arraycopy( v2, 0, v1, 0, v2.length );
    }
    
    // do we need to fill after the end of the base model?
    if( dr != null && m_fill )
    {
      while( cal.getTime().compareTo( dr.getTo() ) < 0 )
        fillWithDefault( dateAxis, valueAxes, intModel, cal );
    }
    
    return intModel;
  }

  /**
   * Fills the model with default values
   * 
   * @param dateAxis
   * @param valueAxes
   * @param intModel
   * @param cal
   */
  private void fillWithDefault( final IAxis dateAxis, final IAxis[] valueAxes, final SimpleTuppleModel intModel, final Calendar cal )
  {
    final Object[] tupple = new Object[ valueAxes.length + 1 ];
    tupple[ dateAxis.getPosition() ] = cal.getTime();
    
    for( int i = 0; i < valueAxes.length; i++ )
      tupple[ valueAxes[i].getPosition() ] = m_defValue;
    
    intModel.addTupple( tupple );
    
    cal.add( m_calField, m_amount );
  }
}