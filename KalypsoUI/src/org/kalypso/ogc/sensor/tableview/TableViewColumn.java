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

import java.util.Set;

import org.kalypso.i18n.Messages;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.template.IObsProvider;
import org.kalypso.ogc.sensor.template.ObsViewItem;

/**
 * A column for an observation table view. It is based on a key axis and a value axis.
 * 
 * @author schlienger
 */
public class TableViewColumn extends ObsViewItem
{
  /** denotes the default column-position in the table: means table can do what it wants */
  private final static int DEFAULT_POSITION = -1;

  private boolean m_isEditable = true;

  private int m_width = 50;

  private final IAxis m_keyAxis;

  private final IAxis m_valueAxis;

  /** column has been modified, model is not in sync */
  private boolean m_dirty = false;

  private boolean m_shown = true;

  private final String m_format;

  private final int m_position;

  /**
   * Constructor with default position
   */
  public TableViewColumn( final TableView view, final IObsProvider provider, final String name, final boolean isEditable, final int width, final IAxis keyAxis, final IAxis valueAxis, final String format )
  {
    this( view, provider, name, isEditable, width, keyAxis, valueAxis, format, DEFAULT_POSITION );
  }

  /**
   * Full Constructor
   */
  public TableViewColumn( final TableView view, final IObsProvider provider, final String name, final boolean isEditable, final int width, final IAxis keyAxis, final IAxis valueAxis, final String format, final int position )
  {
    super( view, provider, name );

    if( format == null )
      throw new IllegalArgumentException( Messages.getString("org.kalypso.ogc.sensor.tableview.TableViewColumn.0") + name + Messages.getString("org.kalypso.ogc.sensor.tableview.TableViewColumn.1") ); //$NON-NLS-1$ //$NON-NLS-2$

    m_isEditable = isEditable;
    m_width = width;
    m_keyAxis = keyAxis;
    m_valueAxis = valueAxis;
    m_format = format;
    m_position = position;
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
   * Set the dirty flag. Optionally an eventSource object can be passed, it designates the origin of the event.
   * 
   * @param eventSource
   *            [optional, can be null] designates the origin of the event
   */
  public void setDirty( final boolean dirty, final Object eventSource )
  {
    m_dirty = dirty;

    // TODO: A ITuppleModel has currently no way to tell its IObservation
    // that the model has changed. The only way is using IObservation.setValues()
    // but since the refactoring in ObservationTableModel, the ITuppleModel is
    // diretly updated and setValues() isn't called any more. So for the meantime,
    // until the whole IObservation stuff is refactored, we use this TableViewColumn
    // to call the fireChangedEvent on the underlying observation.
    // Another solution could have been to give a reference of its IObservation to a
    // ITuppleModel... but I decided not to use that one.
    if( dirty )
      getObservation().fireChangedEvent( eventSource );
  }

  public Class<?> getColumnClass( )
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

  @Override
  public boolean isShown( )
  {
    return m_shown;
  }

  @Override
  public void setShown( final boolean shown )
  {
    if( shown != m_shown )
    {
      m_shown = shown;

      getView().refreshItemState( this, null );
    }
  }

  /**
   * @return the format-specification (non-null).
   */
  public String getFormat( )
  {
    return m_format;
  }

  public int getPosition( )
  {
    return m_position;
  }

  public boolean isDefaultPosition( )
  {
    return m_position == DEFAULT_POSITION;
  }

  /**
   * @see org.kalypso.ogc.sensor.template.ObsViewItem#shouldBeHidden(java.util.List)
   */
  @Override
  public boolean shouldBeHidden( final Set<String> hiddenTypes )
  {
    return hiddenTypes.contains( m_valueAxis.getType() );
  }
}