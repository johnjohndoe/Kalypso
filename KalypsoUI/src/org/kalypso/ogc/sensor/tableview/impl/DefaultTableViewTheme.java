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

import java.util.ArrayList;
import java.util.List;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.ITableViewTheme;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * DefaultTableViewTheme
 * 
 * @author schlienger
 */
public class DefaultTableViewTheme implements ITableViewTheme
{
  private IObservation m_obs = null;

  private IVariableArguments m_args = null;

  private final List m_columns = new ArrayList();

  private final String m_themeName;

  public DefaultTableViewTheme( )
  {
    this( null );
  }

  public DefaultTableViewTheme( final String name )
  {
    m_themeName = name;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTheme#getName()
   */
  public String getName( )
  {
    if( m_themeName != null )
      return m_themeName;
    
    if( m_obs != null )
      return m_obs.getName();
    
    return super.toString();
  }
  
  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTheme#getObservation()
   */
  public IObservation getObservation( )
  {
    return m_obs;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTheme#getArguments()
   */
  public IVariableArguments getArguments( )
  {
    return m_args;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTheme#getColumns()
   */
  public List getColumns( )
  {
    return m_columns;
  }

  /**
   * Adds a column. Renames the column using this theme's name if not null.
   * 
   * @param column
   */
  public void addColumn( final ITableViewColumn column )
  {
    if( m_themeName != null )
      column.setName( m_themeName + " (" + column.getName() + ")" );

      m_columns.add( column );
  }

  /**
   * Removes a column
   * 
   * @param column
   */
  public void removeColumn( final ITableViewColumn column )
  {
    m_columns.remove( column );
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTheme#dispose()
   */
  public void dispose( )
  {
    m_columns.clear();
  }

  /**
   * @param obs
   */
  public void setObservation( IObservation obs )
  {
    m_obs = obs;
  }

  /**
   * @param args
   */
  public void setArguments( IVariableArguments args )
  {
    m_args = args;
  }
  
  /**
   * @see java.lang.Object#toString()
   */
  public String toString( )
  {
    final StringBuffer bf = new StringBuffer();

    if( m_themeName != null )
      bf.append( m_themeName );

    if( m_obs != null )
      bf.append( "Thema: " ).append( m_obs.getName() ).append( " (" )
          .append( m_obs.getHref() ).append( ')' );

    if( bf.length() == 0 )
      return super.toString();

    return bf.toString();
  }
}