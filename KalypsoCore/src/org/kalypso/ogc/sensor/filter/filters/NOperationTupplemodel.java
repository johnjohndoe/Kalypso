package org.kalypso.ogc.sensor.filter.filters;

import java.util.Date;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;

/**
 * @author doemming
 */
public class NOperationTupplemodel implements ITuppleModel
{
    private final int m_operation;

    private final ITuppleModel[] m_baseModels;

    public NOperationTupplemodel(ITuppleModel[] models, int operation)
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
   
    public Object getElement(int index, IAxis axis) throws SensorException
    {
      String axisName = axis.getName();  
      Class dataClass = axis.getDataClass();
        if (dataClass.equals(Date.class))
        {
            IAxis a=FilterHelper.getAxisByName(m_baseModels[0], axisName);
            return m_baseModels[0].getElement(index,a);
        }
            if (dataClass.equals(Double.class))
        {
              IAxis a=FilterHelper.getAxisByName(m_baseModels[0], axisName);
              double value = ((Double) m_baseModels[0].getElement(index, a))
                    .doubleValue();
            for (int i = 1; i < m_baseModels.length; i++)
            {
                ITuppleModel model = m_baseModels[i];
                IAxis a2=FilterHelper.getAxisByName(m_baseModels[i], axisName);
                double nextValue = ((Double) model.getElement(index, a2))
                        .doubleValue();
                switch (m_operation)
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
                case OperationFilter.OPERATION_DURCH: // macht das sinn, bei mehr als zwei ?
                    value /= nextValue;
                break;
                }
            }
            return new Double(value);
        }
        throw new UnsupportedOperationException(getClass().getName()
                + " unterstuetzt den datentyp " + axis.getDataClass().getName()
                + " nicht.");
    }

    public void setElement(int index, Object element, IAxis axis)            
    {
        throw new UnsupportedOperationException(getClass().getName()
                + " unterstuetzt setElement() nicht.");
    }

    public int indexOf(Object element, IAxis axis) throws SensorException
    {
        if (element instanceof Date)
            return m_baseModels[0].indexOf(element, axis);
        throw new UnsupportedOperationException(getClass().getName()
                + " unterstuetzt indexOf fuer die Axe " + axis.getName()
                + " nicht.");
        // TODO support it
    }
}