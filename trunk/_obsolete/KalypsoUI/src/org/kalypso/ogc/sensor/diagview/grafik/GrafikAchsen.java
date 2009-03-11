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

  private final Map<String, Integer> m_id2axisnr = new HashMap<String, Integer>();

  private int m_axisIdCounter = 0;

  private String m_leftLabel = ""; //$NON-NLS-1$

  private String m_rightLabel = ""; //$NON-NLS-1$

  private String m_bottomLabel = ""; //$NON-NLS-1$

  private boolean m_rightAchseCreated = false;

  private boolean m_leftAchseCreated = false;

  private boolean m_invertedAchseCreated = false;

  public GrafikAchsen( final List<TypeAxis> taxList )
  {
    for( final TypeAxis ta : taxList )
    {
      if( ta.getDirection().toString().equalsIgnoreCase( DiagramAxis.DIRECTION_VERTICAL ) )
      {
        GrafikAchse gAchse = null;

        final String name = ta.getLabel() + " [" + ta.getUnit() + "]"; //$NON-NLS-1$ //$NON-NLS-2$

        final int axisnr = axisnrByType( ta );
        m_id2axisnr.put( ta.getDatatype(), new Integer( axisnr ) );

        if( !ta.isInverted() ) // Niederschlagsachse ist immer invertiert, und wie nehmen hier nicht
        {
          if( ta.getPosition().toString().equalsIgnoreCase( DiagramAxis.POSITION_LEFT ) )
            gAchse = createLeftAchse( axisnr, name );
          else if( ta.getPosition().toString().equalsIgnoreCase( DiagramAxis.POSITION_RIGHT ) )
            gAchse = createRightAchse( axisnr, name );
        }
        else
          gAchse = createInvertedAchse( axisnr, name );

        if( gAchse != null )
          m_name2grafikAxis.put( ta.getId(), gAchse );
        else
          Logger.getLogger( getClass().getName() ).warning( Messages.getString( "org.kalypso.ogc.sensor.diagview.grafik.GrafikAchsen.5" ) + name + Messages.getString( "org.kalypso.ogc.sensor.diagview.grafik.GrafikAchsen.6" ) + axisnr + Messages.getString( "org.kalypso.ogc.sensor.diagview.grafik.GrafikAchsen.7" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      }
      else if( ta.getDirection().toString().equalsIgnoreCase( DiagramAxis.DIRECTION_HORIZONTAL ) )
        m_bottomLabel = ta.getLabel();
    }
  }

  private GrafikAchse createInvertedAchse( final int axisnr, final String name )
  {
    if( !m_invertedAchseCreated )
    {
      m_invertedAchseCreated = true;
      return new GrafikAchse( axisnr, name );
    }
    else
      return null;
  }

  private GrafikAchse createRightAchse( final int axisnr, final String name )
  {
    if( !m_rightAchseCreated )
    {
      m_rightAchseCreated = true;
      m_rightLabel = name;
      return new GrafikAchse( axisnr, name );
    }
    else if( !m_leftAchseCreated )
      return createLeftAchse( axisnr, name );
    else
      return null;
  }

  private GrafikAchse createLeftAchse( final int axisnr, final String name )
  {
    if( !m_leftAchseCreated )
    {
      m_leftAchseCreated = true;
      m_leftLabel = name;
      return new GrafikAchse( axisnr, name );
    }
    else if( !m_rightAchseCreated )
      return createRightAchse( axisnr, name );
    else
      return null;
  }

  private int axisnrByType( final TypeAxis ta )
  {
    final String axisId = ta.getId();

    if( m_id2axisnr.containsKey( axisId ) )
      return m_id2axisnr.get( axisId ).intValue();

    if( !ta.isInverted() ) // Niederschlagsachse ist immer invertiert, und wie nehmen hier nicht
    {
      final int axisnr = m_axisIdCounter % 2 + 1;

      m_axisIdCounter++;

      return axisnr;
    }

    return 3;
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