package org.kalypso.dwd;

import java.util.Date;
import java.util.Vector;

/**
 * @author doemming
 * 
 * Raster DWD Reader
 * 
 * 
 * Aufbau Kennsatz : -----------------
 * 
 * Spalte 1- 11: Datum der Daten YYMMDDHHhh (1X,I10)
 * 
 * 12-15 : Elementkennung der Daten (i4) hier : 114 : geogr. Breite in
 * Grad*100000 des Gittermittelpunktes 115 : geogr. Laenge in Grad*100000 des
 * Gittermittelpunktes 8 : geogr. Hoehe in m des Gittermittelpunktes
 * 
 * Aufbau Daten : --------------
 * 
 * Die Werte der im Kennsatz definierten Elemente an den Gitterpunkten in der
 * angegebenen Dimension. Die Gitterwerte sind hintereinander zeilenweise von
 * West nach Ost und von Sued nach Nord angeordnet .
 * 
 * Jeweils 13 Werte pro Zeile (13i8):
 * 
 * Spalte 1- 8 : Gitterwert 1 z.B. 4908634 bedeutet 49.08634 Grad geogr. Breite
 * 9-16 : Gitterwert 2 4908878 49.08878 Aufbau_LMFiles.txt
 */

public class DWDRaster
{
  
  public static final int KEY_RAIN = 424;

  public static final int KEY_SNOW = 425;

  public static final int KEY_HEIGHT = 8;

  public static final int KEY_100000_LAT = 114;

  public static final int KEY_100000_LON = 115;

  
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
