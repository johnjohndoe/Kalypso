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
public class NOperationTupplemodel extends AbstractTuppleModel
{
  private final int m_operation;

  private final ITuppleModel[] m_baseModels;

  public NOperationTupplemodel( ITuppleModel[] models, int operation )
  {
    m_baseModels = models;
    m_operation = operation;
  }

  public IAxis[] getAxisList()
  {
    return m_baseModels[0].getAxisList();
  }

  public int getCount() throws SensorException
  {
    return m_baseModels[0].getCount();
  }

  public int hashCode()
  {
    return m_baseModels[0].hashCode();
  }

  public Object getElement( int index, IAxis axis ) throws SensorException
  {
    //    final String axisName = axis.getName();
    final String axisType = axis.getType();

    final Class dataClass = axis.getDataClass();
    if( dataClass.equals( Date.class ) )
    {
      final IAxis a = ObservationUtilities.findAxisByType( m_baseModels[0].getAxisList(), axisType );
      //      final IAxis a = ObservationUtilities.findAxisByName(
      // m_baseModels[0].getAxisList(), axisName );
      return m_baseModels[0].getElement( index, a );
    }

    if( dataClass.equals( Double.class ) )
    {
      //      final IAxis a = ObservationUtilities.findAxisByName(
      // m_baseModels[0].getAxisList(), axisName );
      final IAxis a = ObservationUtilities.findAxisByType( m_baseModels[0].getAxisList(), axisType );
      if( index >= m_baseModels[0].getCount() )
        return null;
      double value = ( (Number)m_baseModels[0].getElement( index, a ) ).doubleValue();
      for( int i = 1; i < m_baseModels.length; i++ )
      {
        ITuppleModel model = m_baseModels[i];
        if( index >= model.getCount() )
          continue;
        
//        final IAxis a2 = ObservationUtilities.findAxisByName( m_baseModels[i].getAxisList(), axisName );
        final IAxis a2 = ObservationUtilities.findAxisByType( m_baseModels[i].getAxisList(), axisType );
        
        double nextValue = ( (Number)model.getElement( index, a2 ) ).doubleValue();
        switch( m_operation )
        {
        case OperationFilter.OPERATION_PLUS:
          value += nextValue;
          break;
        case OperationFilter.OPERATION_MINUS:
          value -= nextValue;
          break;
        case OperationFilter.OPERATION_MAL:
          value *= nextValue;
          break;
        case OperationFilter.OPERATION_DURCH: // macht das sinn, bei mehr als
          // zwei ?
          value /= nextValue;
          break;
        }
      }
      return new Double( value );
    }

    // Integer: vermutlich eine Status-Achse!
    if( dataClass.equals( Integer.class ) )
    {
      final IAxis a = ObservationUtilities.findAxisByType( m_baseModels[0].getAxisList(), axisType );
      if( index >= m_baseModels[0].getCount() )
        return null;

      int value = ( (Number)m_baseModels[0].getElement( index, a ) ).intValue();
      for( int i = 1; i < m_baseModels.length; i++ )
      {
        ITuppleModel model = m_baseModels[i];
        if( index >= model.getCount() )
          continue;
        
        final IAxis a2 = ObservationUtilities.findAxisByType( m_baseModels[i].getAxisList(), axisType );
        
        int nextValue = ( (Number)model.getElement( index, a2 ) ).intValue();
        value |= nextValue;
      }
      
      return new Integer( value );
    }
    
    throw new UnsupportedOperationException( getClass().getName() + " unterstuetzt den Datentyp "
        + axis.getDataClass().getName() + " nicht." );
  }

  public void setElement( int index, Object element, IAxis axis )
  {
    throw new UnsupportedOperationException( getClass().getName()
        + " unterstuetzt setElement() nicht." );
  }

  public int indexOf( Object element, IAxis axis ) throws SensorException
  {
    if( element instanceof Date )
      return m_baseModels[0].indexOf( element, axis );
    throw new UnsupportedOperationException( getClass().getName()
        + " unterstuetzt indexOf fuer die Axe " + axis.getName() + " nicht." );
    // TODO support it
  }
}