package org.kalypso.services.calculation.job.impl;

import java.io.File;
import java.util.Hashtable;

import org.kalypso.java.io.FileUtilities;
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

    public static CalcJobDataBean[] getMergedBeans( CalcJobDataBean[] initialBeans, CalcJobDataBean[] beansToAdd )
    {            
      final Hashtable result=new Hashtable();
      for( int i = 0; i < initialBeans.length; i++ )
      {
        CalcJobDataBean bean = initialBeans[i];
        result.put(bean.getId(), bean);
      }
      for( int i = 0; i < beansToAdd.length; i++ )
      {
        CalcJobDataBean bean = beansToAdd[i];
        result.put(bean.getId(), bean);        
      }
      return (CalcJobDataBean[])result.values().toArray(new CalcJobDataBean[result.size()]);
    }

    public static CalcJobDataBean[] createBeansForNewBaseDir( CalcJobDataBean[] beans, File baseDirOrg, File baseDirNew )
    {
      CalcJobDataBean[] result=new CalcJobDataBean[beans.length];
      for( int i = 0; i < beans.length; i++ )
      {
        CalcJobDataBean bean = beans[i];
        File file=new File(baseDirOrg,bean.getPath());
        String path = FileUtilities.getRelativePathTo(baseDirNew, file);
        result[i]=new CalcJobDataBean(bean.getId(),bean.getName(),path);
      }
      return result;
    }
}
