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
package org.kalypso.ogc.sensor.tableview.impl;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.ITableViewTemplate;
import org.kalypso.ogc.sensor.tableview.ITableViewTheme;
import org.kalypso.ogc.sensor.template.TemplateEvent;

/**
 * Default implementation of the <code>ITableViewColumn</code> interface
 * 
 * @author schlienger
 */
public class DefaultTableViewColumn implements ITableViewColumn
{
  private String m_name = "";

  private boolean m_isEditable = true;

  private int m_width = 50;

  private final IAxis m_keyAxis;

  private final IAxis m_valueAxis;

  private ITableViewTheme m_theme;

  private boolean m_dirty = false;

  private final ITableViewTemplate m_template;

  private boolean m_shown = true;

  /**
   * Constructor
   * 
   * @param name
   * @param isEditable
   * @param width
   * @param keyAxis
   * @param valueAxis
   * @param theme
   * @param template
   */
  public DefaultTableViewColumn( final String name, final boolean isEditable,
      final int width, final IAxis keyAxis, final IAxis valueAxis,
      final ITableViewTheme theme, final ITableViewTemplate template )
  {
    m_name = name;
    m_isEditable = isEditable;
    m_width = width;
    m_keyAxis = keyAxis;
    m_valueAxis = valueAxis;
    m_theme = theme;
    m_template = template;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#getName()
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

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#isEditable()
   */
  public boolean isEditable( )
  {
    return m_isEditable;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#getWidth()
   */
  public int getWidth( )
  {
    return m_width;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#setWidth(int)
   */
  public void setWidth( int width )
  {
    m_width = width;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#isDirty()
   */
  public boolean isDirty( )
  {
    return m_dirty;
  }

  /**
   * @param dirty
   *          The dirty to set.
   */
  public void setDirty( boolean dirty )
  {
    m_dirty = dirty;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#getColumnClass()
   */
  public Class getColumnClass( )
  {
    return m_valueAxis.getDataClass();
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#getAxis()
   */
  public IAxis getAxis( )
  {
    return m_valueAxis;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#getKeyAxis()
   */
  public IAxis getKeyAxis( )
  {
    return m_keyAxis;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#getTheme()
   */
  public ITableViewTheme getTheme( )
  {
    return m_theme;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#setName(java.lang.String)
   */
  public void setName( String name )
  {
    m_name = name;
  }

  /**
   * @see org.kalypso.eclipse.ui.IViewable#isShown()
   */
  public boolean isShown( )
  {
    return m_shown;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#setShown(boolean)
   */
  public void setShown( boolean shown )
  {
    if( shown != m_shown )
    {
      m_shown = shown;

      m_template.fireTemplateChanged( new TemplateEvent( this, TemplateEvent.TYPE_SHOW_STATE ) );
    }
  }
}