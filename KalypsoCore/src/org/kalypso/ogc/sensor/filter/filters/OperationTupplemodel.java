package org.kalypso.ogc.sensor.filter.filters;

import java.util.Date;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.AbstractTuppleModel;

/**
 * @author doemming
 */
public class OperationTupplemodel extends AbstractTuppleModel
{
  private final double m_operand;

  private final int m_operation;

  private final ITuppleModel m_baseModel;

  public OperationTupplemodel( double operand, int operation, ITuppleModel baseModel )
  {
    m_operand = operand;
    m_operation = operation;
    m_baseModel = baseModel;
  }

  public IAxis[] getAxisList()
  {
    return m_baseModel.getAxisList();
  }

  public int getCount() throws SensorException
  {
    return m_baseModel.getCount();
  }

  public int hashCode()
  {
    return m_baseModel.hashCode();
  }

  public String toString()
  {
    return m_baseModel.toString();
  }

  public Object getElement( int index, IAxis axis ) throws SensorException
  {
    // Andreas: ObservationUtilities already has this function so I removed FilterHelper
    IAxis a = ObservationUtilities.findAxisByName( m_baseModel.getAxisList(), axis.getName() );
    if(index>=m_baseModel.getCount())
      return null;
    Object object = m_baseModel.getElement( index, a );
    if( object == null || object instanceof Date )
      return object;
    if( object instanceof Number ) // let it be a Number here so we can handle integers and such
    {
      double value = ( (Number)object ).doubleValue();
      switch( m_operation )
      {
      case OperationFilter.OPERATION_PLUS:
        return new Double( value + m_operand );
      case OperationFilter.OPERATION_MINUS:
        return new Double( value - m_operand );
      case OperationFilter.OPERATION_MAL:
        return new Double( value * m_operand );
      case OperationFilter.OPERATION_DURCH:
        return new Double( value / m_operand );
      }
    }
    throw new UnsupportedOperationException( getClass().getName() + " unterstuetzt den datentyp "
        + object.getClass().getName() + " nicht." );
  }

  /*
   * 
   * @see org.kalypso.ogc.sensor.ITuppleModel#setElement(int, java.lang.Object,
   *      org.kalypso.ogc.sensor.IAxis)
   */
  public void setElement( int index, Object element, IAxis axis )
  {
    throw new UnsupportedOperationException( getClass().getName()
        + " unterstuetzt setElement() nicht." );
    // TODO support it
  }

  /*
   * 
   * @see org.kalypso.ogc.sensor.ITuppleModel#indexOf(java.lang.Object,
   *      org.kalypso.ogc.sensor.IAxis)
   */
  public int indexOf( Object element, IAxis axis ) throws SensorException
  {
    if( element instanceof Date )
      return m_baseModel.indexOf( element, axis );
    throw new UnsupportedOperationException( getClass().getName()
        + " unterstuetzt indexOf fuer die Axe " + axis.getName() + " nicht." );
    // TODO support it
  }

}