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
        Class dataClass = axis.getDataClass();
        if (dataClass.equals(Date.class))
            return m_baseModels[0].getElement(index,axis);
        if (dataClass.equals(Double.class))
        {
            double value = ((Double) m_baseModels[0].getElement(index, axis))
                    .doubleValue();
            for (int i = 1; i < m_baseModels.length; i++)
            {
                ITuppleModel model = m_baseModels[i];
                double nextValue = ((Double) model.getElement(index, axis))
                        .doubleValue();
                switch (m_operation)
                {
                case OperationFilter.OPERATION_PLUS:
                    value += nextValue;
                case OperationFilter.OPERATION_MINUS:
                    value -= nextValue;
                case OperationFilter.OPERATION_MAL:
                    value *= nextValue;
                case OperationFilter.OPERATION_DURCH: // macht das sinn, bei mehr als zwei ?
                    value /= nextValue;
                }
            }
            return new Double(value);
        }
        throw new UnsupportedOperationException(getClass().getName()
                + " unterstuetzt den datentyp " + axis.getDataClass().getName()
                + " nicht.");
    }

    public void setElement(int index, Object element, IAxis axis)
            throws SensorException
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