package org.kalypso.services.calculation.job.impl;

import org.kalypso.services.calculation.service.CalcJobDataBean;

/**
 * @author doemming
 */
 
public class CalcJobHelper
{
    public static CalcJobDataBean getBeanForId(String id,CalcJobDataBean[] beans)
    {
        for (int i = 0; i < beans.length; i++)
        {
         if(beans[i].getId().equals(id))
             return beans[i];
        }
        return null;
    }
}
