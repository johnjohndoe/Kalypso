/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
  
---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.diagview.grafik;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.kalypso.ogc.sensor.diagview.DiagramAxis;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.template.obsdiagview.TypeAxis;

/**
 * @author schlienger
 */
public class GrafikYAchsen
{
  private final Map name2ta = new HashMap();

  private final Map name2ga = new HashMap();

  private String m_leftLabel = "";

  private String m_rightLabel = "";

  public GrafikYAchsen( List taxList )
  {
    for( final Iterator ita = taxList.iterator(); ita.hasNext(); )
    {
      final TypeAxis ta = (TypeAxis) ita.next();

      if( ta.getDirection().equals( DiagramAxis.DIRECTION_VERTICAL ) )
      {
        name2ta.put( ta.getId(), ta );
        
        if( !ta.isInverted() )
        {
          if( ta.getPosition().equals( DiagramAxis.POSITION_LEFT ) )
            m_leftLabel = ta.getLabel() + " [" + ta.getUnit() + "]";
          else if( ta.getPosition().equals( DiagramAxis.POSITION_LEFT ) )
            m_rightLabel = ta.getLabel() + " [" + ta.getUnit() + "]";
        }
      }
    }
  }

  public String getRightLabel()
  {
    return m_rightLabel;
  }
  
  public String getLeftLabel()
  {
    return m_leftLabel;
  }
  
  /**
   * @param axisID
   * @return corresponding Achse for the Grafik tool or null if not possible
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

    ga = new GrafikAchse( name2ga.size() + 1, ta.getLabel() + " [" + ta.getUnit() + "]" );
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