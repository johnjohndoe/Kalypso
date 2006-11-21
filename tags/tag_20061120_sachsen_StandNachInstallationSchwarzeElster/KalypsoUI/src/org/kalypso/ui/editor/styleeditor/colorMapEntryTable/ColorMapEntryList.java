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
package org.kalypso.ui.editor.styleeditor.colorMapEntryTable;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.Vector;

import org.kalypsodeegree.graphics.sld.ColorMapEntry;

public class ColorMapEntryList
{
  private Set changeListeners = new HashSet();

  private Vector m_colorMapEntryList = null;

  public ColorMapEntryList()
  {
    super();
    m_colorMapEntryList = new Vector();
  }

  public Vector getColorMapEntries()
  {
    return m_colorMapEntryList;
  }

  public void addColorMapEntry( ColorMapEntry colorMapEntry )
  {
    m_colorMapEntryList.add( m_colorMapEntryList.size(), colorMapEntry );
    Iterator iterator = changeListeners.iterator();
    while( iterator.hasNext() )
      ( (IColorMapEntryViewer)iterator.next() ).addColorMapEntry( colorMapEntry );
  }

  public void removeColorMapEntry( ColorMapEntry colorMapEntry )
  {
    m_colorMapEntryList.remove( colorMapEntry );
    Iterator iterator = changeListeners.iterator();
    while( iterator.hasNext() )
      ( (IColorMapEntryViewer)iterator.next() ).removeColorMapEntry( colorMapEntry );
  }

  public void colorMapEntryChanged( ColorMapEntry colorMapEntry )
  {
    Iterator iterator = changeListeners.iterator();
    while( iterator.hasNext() )
      ( (IColorMapEntryViewer)iterator.next() ).updateColorMapEntry( colorMapEntry );
  }

  public void removeChangeListener( IColorMapEntryViewer viewer )
  {
    changeListeners.remove( viewer );
  }

  public void addChangeListener( IColorMapEntryViewer viewer )
  {
    changeListeners.add( viewer );
  }

}