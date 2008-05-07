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
import java.util.logging.Logger;

import org.kalypso.i18n.Messages;
import org.kalypso.ogc.sensor.diagview.DiagramAxis;
import org.kalypso.template.obsdiagview.TypeAxis;

/**
 * @author schlienger
 */
public class GrafikAchsen
{
  /** maps diag-axis-id to grafik-axis (only for vertical axes) */
  private final Map<String, GrafikAchse> m_name2grafikAxis = new HashMap<String, GrafikAchse>();

  private String m_leftLabel = ""; //$NON-NLS-1$

  private String m_rightLabel = ""; //$NON-NLS-1$

  private String m_bottomLabel = ""; //$NON-NLS-1$

  private int m_achseNr = 0;

  private boolean m_rightAchseCreated = false;

  private boolean m_leftAchseCreated = false;

  private boolean m_invertedAchseCreated = false;

  public GrafikAchsen( final List taxList )
  {
    for( final Iterator ita = taxList.iterator(); ita.hasNext(); )
    {
      final TypeAxis ta = (TypeAxis) ita.next();

      if( ta.getDirection().toString().equalsIgnoreCase( DiagramAxis.DIRECTION_VERTICAL ) )
      {
        GrafikAchse gAchse = null;

        final String name = ta.getLabel() + " [" + ta.getUnit() + "]"; //$NON-NLS-1$ //$NON-NLS-2$

        if( !ta.isInverted() ) // Niederschlagsachse ist immer invertiert, und wie nehmen hier nicht
        {
          if( ta.getPosition().toString().equalsIgnoreCase( DiagramAxis.POSITION_LEFT ) )
            gAchse = createLeftAchse( name );
          else if( ta.getPosition().toString().equalsIgnoreCase( DiagramAxis.POSITION_RIGHT ) )
            gAchse = createRightAchse( name );
        }
        else
          gAchse = createInvertedAchse( name );

        if( gAchse != null )
          m_name2grafikAxis.put( ta.getId(), gAchse );
        else
          Logger.getLogger( getClass().getName() ).warning( Messages.getString("org.kalypso.ogc.sensor.diagview.grafik.GrafikAchsen.5") + name + Messages.getString("org.kalypso.ogc.sensor.diagview.grafik.GrafikAchsen.6") + m_achseNr + Messages.getString("org.kalypso.ogc.sensor.diagview.grafik.GrafikAchsen.7") ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      }
      else if( ta.getDirection().toString().equalsIgnoreCase( DiagramAxis.DIRECTION_HORIZONTAL ) )
        m_bottomLabel = ta.getLabel();
    }
  }

  private GrafikAchse createInvertedAchse( final String name )
  {
    if( !m_invertedAchseCreated )
    {
      m_invertedAchseCreated = true;
      return new GrafikAchse( ++m_achseNr, name );
    }
    else
      return null;
  }

  private GrafikAchse createRightAchse( final String name )
  {
    if( !m_rightAchseCreated )
    {
      m_rightAchseCreated = true;
      m_rightLabel = name;
      return new GrafikAchse( ++m_achseNr, name );
    }
    else if( !m_leftAchseCreated )
      return createLeftAchse( name );
    else
      return null;
  }

  private GrafikAchse createLeftAchse( final String name )
  {
    if( !m_leftAchseCreated )
    {
      m_leftAchseCreated = true;
      m_leftLabel = name;
      return new GrafikAchse( ++m_achseNr, name );
    }
    else if( !m_rightAchseCreated )
      return createRightAchse( name );
    else
      return null;
  }

  public String getBottomLabel( )
  {
    return m_bottomLabel;
  }

  public String getRightLabel( )
  {
    return m_rightLabel;
  }

  public String getLeftLabel( )
  {
    return m_leftLabel;
  }

  /**
   * @param diagAxisID
   * @return corresponding Achse for the Grafik tool or null if not possible
   */
  public GrafikAchse getFor( final String diagAxisID )
  {
    return m_name2grafikAxis.get( diagAxisID );
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

    public GrafikAchse( int id, String name )
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
    @Override
    public String toString( )
    {
      return getName();
    }
  }
}