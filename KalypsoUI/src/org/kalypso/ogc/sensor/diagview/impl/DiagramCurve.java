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
package org.kalypso.ogc.sensor.diagview.impl;

import java.awt.Color;

import org.kalypso.ogc.sensor.diagview.IAxisMapping;
import org.kalypso.ogc.sensor.diagview.IDiagramCurve;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplate;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplateTheme;
import org.kalypso.ogc.sensor.template.TemplateEvent;

/**
 * Default implementation of the <code>IDiagramCurve</code> interface.
 * 
 * @author schlienger
 */
public class DiagramCurve implements IDiagramCurve
{
  private String m_name;

  private final IDiagramTemplate m_template;

  private final IAxisMapping[] m_mappings;

  private IDiagramTemplateTheme m_theme;

  private final Color m_color;

  private boolean m_shown = true;

  /**
   * Constructor
   * 
   * @param name
   * @param color
   * @param theme
   * @param mappings
   * @param template
   */
  public DiagramCurve( final String name, final Color color,
      final IDiagramTemplateTheme theme, final IAxisMapping[] mappings,
      final IDiagramTemplate template )
  {
    m_name = name;
    m_color = color;
    m_mappings = mappings;
    m_theme = theme;
    m_template = template;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramCurve#getName()
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
    return m_name;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramCurve#getMappings()
   */
  public IAxisMapping[] getMappings( )
  {
    //    final List ms = new ArrayList();
    //
    //    final IAxis[] obsAxes = m_theme.getObservation().getAxisList();
    //    
    //    for( final Iterator it = m_mappings.keySet().iterator(); it.hasNext(); )
    //    {
    //      final String strObsAxis = (String)it.next();
    //      final String strDiagAxis = m_mappings.getProperty( strObsAxis );
    //
    //      final IAxis obsAxis = ObservationUtilities.findAxisByName( obsAxes ,
    // strObsAxis );
    //      final IDiagramAxis diagAxis = m_template.getDiagramAxis( strDiagAxis );
    //
    //      ms.add( new AxisMapping( obsAxis, diagAxis ) );
    //    }
    //
    //    return (IAxisMapping[])ms.toArray( new IAxisMapping[0] );
    return m_mappings;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramCurve#getTheme()
   */
  public IDiagramTemplateTheme getTheme( )
  {
    return m_theme;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramCurve#setName(java.lang.String)
   */
  public void setName( String name )
  {
    m_name = name;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramCurve#getColor()
   */
  public Color getColor( )
  {
    return m_color;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramCurve#isShown()
   */
  public boolean isShown( )
  {
    return m_shown;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramCurve#setShown(boolean)
   */
  public void setShown( boolean shown )
  {
    // only set if necessary
    if( shown != m_shown )
    {
      m_shown = shown;

      m_template.fireTemplateChanged( new TemplateEvent( this,
          TemplateEvent.TYPE_SHOW_STATE ) );
    }
  }
}