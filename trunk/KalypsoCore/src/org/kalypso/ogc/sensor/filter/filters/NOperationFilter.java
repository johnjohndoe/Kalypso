package org.kalypso.ogc.sensor.filter.filters;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.IObservationFilter;
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

    private IObservation m_baseobservation = null;

    private NOperationFilterType m_baseFilter = null;

    private int m_operation = OPERATION_UNKNOWN;

    private List m_baseFilters = null;

    public void initFilter(Object conf, IObservation baseObs)
            throws SensorException
    {
        super.initFilter(conf, baseObs);
        NOperationFilterType filter = (NOperationFilterType) conf;
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
        m_baseobservation = baseObs;
        filter.getFilter();
        m_baseFilters = filter.getFilter();
    }

    public ITuppleModel getValues(IVariableArguments args)
            throws SensorException
    {
        ITuppleModel models[] = new ITuppleModel[m_baseFilters.size()];
        for (int i = 0; i < models.length; i++)
            models[i] = ((IObservationFilter) m_baseFilters.get(i)).getValues(args);
        return new NOperationTupplemodel(models,m_operation);

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