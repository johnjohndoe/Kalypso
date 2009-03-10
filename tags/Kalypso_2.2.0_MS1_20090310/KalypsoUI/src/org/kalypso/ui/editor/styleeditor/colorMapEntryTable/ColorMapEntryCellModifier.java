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

import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.TableItem;
import org.kalypsodeegree.graphics.sld.ColorMapEntry;

/**
 * This class implements an ICellModifier. An ICellModifier is called when the user modifes a cell in the tableViewer
 */

public class ColorMapEntryCellModifier implements ICellModifier
{
  private ColorMapEntryTable m_colorMapEntryTable;

  public ColorMapEntryCellModifier( ColorMapEntryTable colorMapEntryTable )
  {
    super();
    m_colorMapEntryTable = colorMapEntryTable;
  }

  /**
   * @see org.eclipse.jface.viewers.ICellModifier#canModify(java.lang.Object, java.lang.String)
   */
  public boolean canModify( Object element, String property )
  {
    return true;
  }

  /**
   * @see org.eclipse.jface.viewers.ICellModifier#getValue(java.lang.Object, java.lang.String)
   */
  public Object getValue( Object element, String property )
  {

    // Find the index of the column
    int columnIndex = m_colorMapEntryTable.getColumnNames().indexOf( property );

    Object result = null;
    ColorMapEntry colorMapEntry = (ColorMapEntry)element;

    switch( columnIndex )
    {
    case 0: // LABEL
      result = colorMapEntry.getLabel();
      break;
    case 1: // Quantity
      result = Double.toString( colorMapEntry.getQuantity() );
      //result = "test_"+colorMapEntry.getQuantity();
      //result = new Double(colorMapEntry.getQuantity());
      break;
    case 2: // COLOR
      java.awt.Color color = colorMapEntry.getColor();
      result = ( new Color( ColorMapEntryTable.table.getDisplay(), color.getRed(), color.getGreen(), color.getBlue() ) )
          .getRGB();
      break;
    case 3: // OPACITY
      result = colorMapEntry.getOpacity() + ""; //$NON-NLS-1$
      break;
    default:
      result = ""; //$NON-NLS-1$
    }
    return result;
  }

  /**
   * @see org.eclipse.jface.viewers.ICellModifier#modify(java.lang.Object, java.lang.String, java.lang.Object)
   */
  public void modify( Object element, String property, Object value )
  {

    // Find the index of the column
    int columnIndex = m_colorMapEntryTable.getColumnNames().indexOf( property );

    TableItem item = (TableItem)element;
    ColorMapEntry colorMapEntry = (ColorMapEntry)item.getData();
    String valueString;

    switch( columnIndex )
    {
    case 0: // LABEL
      valueString = ( (String)value ).trim();
      colorMapEntry.setLabel( valueString );
      break;
    case 1: // VALUE
      valueString = ( (String)value ).trim();
      if( valueString.length() == 0 )
        valueString = "0"; //$NON-NLS-1$
      colorMapEntry.setQuantity( Double.parseDouble( valueString ) );
      break;
    case 2: // COLOR
      colorMapEntry.setColor( ( new java.awt.Color( ( (RGB)value ).red, ( (RGB)value ).green, ( (RGB)value ).blue ) ) );
      break;
    case 3: // OPACITY
      valueString = ( (String)value ).trim();
      if( valueString.length() == 0 )
        valueString = "0"; //$NON-NLS-1$
      double opacity = Double.parseDouble( valueString );
      if( opacity <= 1 && opacity >= 0 )
      {
        item.setBackground( 3, ColorMapEntryTable.table.getDisplay().getSystemColor( SWT.COLOR_WHITE ) );
        colorMapEntry.setOpacity( Double.parseDouble( valueString ) );
      }
      else
      {
        item.setBackground( 3, ColorMapEntryTable.table.getDisplay().getSystemColor( SWT.COLOR_RED ) );
        colorMapEntry.setOpacity( Double.parseDouble( valueString ) );
      }
      break;
    default:
    }
    m_colorMapEntryTable.getColorMapEntryList().colorMapEntryChanged( colorMapEntry );
  }
}