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
/*
 * Created on 15.07.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.panels;

import org.eclipse.swt.widgets.Composite;

/**
 * @author F.Lindemann
 *  
 */
public class WellKnownNameComboPanel extends ComboPanel
{

  public WellKnownNameComboPanel( Composite parent, String label, String value )
  {
    super( parent, label );
    items = new String[6];
    items[0] = "square";
    items[1] = "circle";
    items[2] = "triangle";
    items[3] = "star";
    items[4] = "cross";
    items[5] = "x";
    init();
    int i = 0;
    for( ; i < items.length; i++ )
      if( items[i].equals( value ) )
        break;
    setSelection( i );
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.ui.editor.styleeditor.panels.StrokeComboPanel#getSelection()
   */
  public int getSelection()
  {
    return selection_index;
  }

  public static String getWellKnownNameByIndex( int index )
  {
    switch( index )
    {
    case 0:
      return "square";
    case 1:
      return "circle";
    case 2:
      return "triangle";
    case 3:
      return "star";
    case 4:
      return "cross";
    case 5:
      return "x";
    default:
      return "square";
    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.ui.editor.styleeditor.panels.StrokeComboPanel#setSelection(int)
   */
  public void setSelection( int index )
  {
    comboBox.select( index );
  }
}