package org.kalypso.util.swt;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;

/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/

/**
 * this class extends a combo box to work with objects
 */
public class ComboForObjects
{
  private final Combo m_combo;

  private final List m_objectItems = new ArrayList();

  /*
   * @author doemming
   */
  public ComboForObjects( Composite parent, int style )
  {
    m_combo = new Combo( parent, style );

  }

  /**
   * @see org.eclipse.swt.widgets.Combo#setItem(int, java.lang.String)
   */
  public void setItem( int index, String string )
  {
    m_objectItems.set( index, string );
    m_combo.setItem( index, string );
  }

  /**
   * @see org.eclipse.swt.widgets.Combo#setItems(java.lang.String[])
   */
  public void setItems( String[] items )
  {
    removeAll();
    m_objectItems.addAll( new ArrayList( m_objectItems ) );
    m_combo.setItems( items );
  }

  public void setItemObjects( Object[] items )
  {

    for( int i = 0; i < items.length; i++ )
    {
      addObject( items[i] );
    }
  }

  /**
   * 
   * @see org.eclipse.swt.widgets.Combo#add(java.lang.String, int)
   */
  public void add( String string, int index )
  {
    m_objectItems.add( index, string );
    m_combo.add( string, index );
  }

  /**
   * 
   * @see org.eclipse.swt.widgets.Combo#add(java.lang.String)
   */
  public void add( String string )
  {
    m_objectItems.add( string );
    m_combo.add( string );
  }

  /**
   * 
   * @see org.eclipse.swt.widgets.Combo#remove(int, int)
   */
  public void remove( int start, int end )
  {
    List toRemove = new ArrayList();
    for( int i = start; i < end; i++ )
    {
      toRemove.add( m_objectItems.get( i ) );
    }
    for( Iterator iter = toRemove.iterator(); iter.hasNext(); )
    {
      m_objectItems.remove( iter.next() );
    }
    m_combo.remove( start, end );
  }

  /**
   * 
   * @see org.eclipse.swt.widgets.Combo#remove(int)
   */
  public void remove( int index )
  {
    m_objectItems.remove( index );
    m_combo.remove( index );
  }

  /**
   * 
   * @see org.eclipse.swt.widgets.Combo#remove(java.lang.String)
   */
  public void remove( String string )
  {
    m_objectItems.remove( string );
    m_combo.remove( string );
  }

  /**
   * 
   * @see org.eclipse.swt.widgets.Combo#removeAll()
   */
  public void removeAll()
  {
    m_objectItems.clear();
    m_combo.removeAll();
  }

  public void addObject( Object item )
  {
    m_objectItems.add( item );
    m_combo.add( item.toString() );
  }

  public Object getObject( int index )
  {
    return m_objectItems.get( index );
  }

  public void setLayoutData( GridData layoutData )
  {
    m_combo.setLayoutData( layoutData );
  }

  public int getSelectionIndex()
  {
    return m_combo.getSelectionIndex();
  }

  public void setText( String text )
  {
    m_combo.setText( text );
  }
}