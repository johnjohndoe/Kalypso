package org.kalypso.dwd.raster;

import java.util.Date;
import java.util.Vector;

/**
 * 
 * @author doemming
 * 
 * Storage of DWD Raster data
 * 
 */
public class DWDRaster
{
    public boolean equals(Object obj)
    {
        if (!(obj instanceof DWDRaster))
            return false;
        return ((DWDRaster) obj).getKey() == getKey();
    }

    public int hashCode()
    {
        return getKey();
    }

    final int m_key;

    final Date m_date;

    final Vector m_data;

    public DWDRaster(final Date date, final int key)
    {
        m_key = key;
        m_date = date;
        m_data = new Vector();
    }

    public int getKey()
    {
        return m_key;
    }

    public void addValues(Object[] values)
    {
        for (int i = 0; i < values.length; i++)
            m_data.add(values[i]);
    }

    public void addValue(Object value)
    {
        m_data.add(value);
    }

    public int size()
    {
        return m_data.size();
    }

    public Object getElementAt(int index)
    {
        return m_data.elementAt(index);
    }

    public Date getDate()
    {
        return m_date;
    }
}
