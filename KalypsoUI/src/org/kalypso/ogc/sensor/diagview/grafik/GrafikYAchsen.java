package org.kalypso.ogc.sensor.diagview.grafik;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.template.obsdiagview.TypeAxis;

/**
 * @author schlienger
 */
public class GrafikYAchsen
{
  private final Map name2ta = new HashMap();

  private final Map name2ga = new HashMap();

  public GrafikYAchsen( List taxList )
  {
    for( final Iterator ita = taxList.iterator(); ita.hasNext(); )
    {
      final TypeAxis ta = (TypeAxis) ita.next();

      name2ta.put( ta.getId(), ta );
    }
  }

  public String getLabelAt( final int pos )
  {
    Object[] objs = name2ga.keySet().toArray();
    if( objs.length == 0 || pos >= objs.length )
      return "";
    
    return objs[pos].toString();
  }
  
  /**
   * @param axisID
   * @return corresponding X-Achse for the Grafik tool or null if not possible
   */
  public GrafikAchse getFor( final String axisID )
  {
    GrafikAchse ga = (GrafikAchse) name2ga.get( axisID );

    // already here?
    if( ga != null )
      return ga;

    // grafik can only have max 2 vertical axes
    if( name2ga.size() == 2 )
      return null;

    // no type axis for this id
    final TypeAxis ta = (TypeAxis) name2ta.get( axisID );
    if( ta == null )
      return null;

    ga = new GrafikAchse( name2ga.size() + 1, ta.getLabel() );
    name2ga.put( axisID, ga );

    return ga;
  }
  
  public static String axis2grafikType( final String axisType )
  {
    if( axisType.equals( TimeserieConstants.TYPE_RAINFALL ) )
      return "N";
    
    return "L";
  }

  /**
   * Holds simple axis information for the grafik tool
   * 
   * @author schlienger
   */
  public final static class GrafikAchse
  {
    private final String m_name;

    private final int m_id;

    public GrafikAchse( final int id, final String name )
    {
      m_id = id;
      m_name = name;
    }

    /**
     * @return Returns the id.
     */
    public int getId( )
    {
      return m_id;
    }

    /**
     * @return Returns the name.
     */
    public String getName( )
    {
      return m_name;
    }
    
    /**
     * @see java.lang.Object#toString()
     */
    public String toString( )
    {
      return getName();
    }
  }
}