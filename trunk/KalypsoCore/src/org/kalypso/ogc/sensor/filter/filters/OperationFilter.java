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

    private IObservation m_baseobservation=null;
    private OperationFilterType m_baseFilter=null;
    private int m_operation=OPERATION_UNKNOWN;
    private double m_operand;
    
    public void initFilter(Object conf, IObservation baseObs)
            throws SensorException
    {
        super.initFilter(conf,baseObs);
        OperationFilterType filter=(OperationFilterType) conf;
        m_operand = Double.parseDouble(filter.getOperand());
        final String operator = filter.getOperator();
        if (operator.equals("+"))
            m_operation = OPERATION_PLUS;
        else if (operator.equals("-"))
            m_operation = OPERATION_MINUS;
        else if (operator.equals("*"))
            m_operation = OPERATION_MAL;
        else if (operator.equals("/"))
            m_operation = OPERATION_DURCH;
        else
            throw new IllegalArgumentException("unknown operator '" + operator
                    + "' in filter");
        m_baseobservation=baseObs;
        m_baseFilter=filter;        
    }
    
    public ITuppleModel getValues(IVariableArguments args)
            throws SensorException
    {
        return new OperationTupplemodel(m_operand,m_operation,m_baseobservation.getValues(args));
        
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.kalypso.ogc.sensor.IObservation#setValues(org.kalypso.ogc.sensor.ITuppleModel)
     */
    public void setValues(ITuppleModel values) throws SensorException
    {
        throw new UnsupportedOperationException(getClass().getName()
                + " setValues() wird zur Zeit nicht unterstuetzt .");
    }

}