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
package org.kalypso.ogc.sensor.tableview;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.template.IObsProvider;
import org.kalypso.ogc.sensor.template.ObsViewItem;

/**
 * A column for an observation table view. It is based on a key axis and a value
 * axis.
 * 
 * @author schlienger
 */
public class TableViewColumn extends ObsViewItem
{
  private boolean m_isEditable = true;

  private int m_width = 50;

  private final IAxis m_keyAxis;

  private final IAxis m_valueAxis;

  /**
   * flag specifying when the column needs to be saved. It comes along with the
   * dirty flag. Once the dirty flag is set to true, the dirtySave flag becomes
   * true as well. It will only be reset to false once setDirtySave( false ) is
   * called. A call to setDirty( false ) only changes the dirty flag, not the
   * dirtySave one.
   * <p>
   * This mechanism is used to know when the column has been persisted
   * (dirtySave=false)in opposition to the model is in sync (dirty=false).
   */
  private boolean m_dirtySave = false;

  /** column has been modified, model is not in sync */
  private boolean m_dirty = false;

  private boolean m_shown = true;

  public TableViewColumn( final TableView view, final IObsProvider provider, final String name, final boolean isEditable,
      final int width, final IAxis keyAxis, final IAxis valueAxis )
  {
    super( view, provider, name );

    m_isEditable = isEditable;
    m_width = width;
    m_keyAxis = keyAxis;
    m_valueAxis = valueAxis;
  }

  public boolean isEditable( )
  {
    return m_isEditable;
  }

  public int getWidth( )
  {
    return m_width;
  }

  public boolean isDirty( )
  {
    return m_dirty;
  }

  /**
   * As soon as dirty is true, dirtySave also gets true.
   * 
   * @param dirty
   */
  public void setDirty( boolean dirty )
  {
    m_dirty = dirty;
    
    if( dirty )
      m_dirtySave = true;
  }
  
  public boolean isDirtySave( )
  {
    return m_dirtySave;
  }
  
  /**
   * This is the only means by which dirtySave can be set to false.
   */
  public void resetDirtySave( )
  {
    m_dirtySave = false;
  }
  
  public Class getColumnClass( )
  {
    return m_valueAxis.getDataClass();
  }

  public IAxis getAxis( )
  {
    return m_valueAxis;
  }

  public IAxis getKeyAxis( )
  {
    return m_keyAxis;
  }

  public boolean isShown( )
  {
    return m_shown;
  }

  public void setShown( boolean shown )
  {
    if( shown != m_shown )
    {
      m_shown = shown;

      getView().refresh( this );
    }
  }
}