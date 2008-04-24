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
package org.kalypso.ui.editor.styleeditor.colorMapEntryTable;

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.kalypsodeegree.graphics.sld.ColorMapEntry;

/**
 * Label provider for the ColorMapEntryTable
 * 
 * @see org.eclipse.jface.viewers.LabelProvider
 */
public class ColorMapEntryLabelProvider extends LabelProvider implements ITableLabelProvider
{
  /**
   * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnText(java.lang.Object, int)
   */
  public String getColumnText( Object element, int columnIndex )
  {
    String result = "";
    ColorMapEntry colorMapEntry = (ColorMapEntry) element;
    switch( columnIndex )
    {
      case 0:
        result = colorMapEntry.getLabel();
        break;
      case 1:
        result = colorMapEntry.getQuantity() + "";
        break;
      case 2:
        // no label
        break;
      case 3:
        result = colorMapEntry.getOpacity() + "";
        break;
      default:
        break;
    }
    return result;
  }

  /**
   * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang.Object, int)
   */
  public Image getColumnImage( Object element, int columnIndex )
  {
    Image image = null;
    if( columnIndex == 2 )
    {
      image = new Image( ColorMapEntryTable.table.getDisplay(), 25, 15 );
      GC gc = new GC( image );
      java.awt.Color color = ((ColorMapEntry) element).getColor();
      gc.setBackground( new Color( ColorMapEntryTable.table.getDisplay(), color.getRed(), color.getGreen(), color.getBlue() ) );
      gc.fillRectangle( image.getBounds() );
      gc.dispose();
    }
    return image;
  }

}