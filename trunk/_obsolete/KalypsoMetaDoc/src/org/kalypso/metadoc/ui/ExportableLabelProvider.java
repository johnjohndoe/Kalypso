/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.metadoc.ui;

import org.eclipse.jface.viewers.IColorProvider;
import org.eclipse.jface.viewers.IFontProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;

/**
 * @author Gernot Belger
 */
public class ExportableLabelProvider extends LabelProvider implements IFontProvider, IColorProvider
{
  private final Color m_grayedForeground;
  private final Color m_grayedBackground;
  private final Font m_grayedFont;

  public ExportableLabelProvider( final Font grayedFont, final Color grayedForeground, final Color grayedBackground )
  {
    m_grayedFont = grayedFont;
    m_grayedForeground = grayedForeground;
    m_grayedBackground = grayedBackground;
  }
  
  /**
   * @see org.eclipse.jface.viewers.IFontProvider#getFont(java.lang.Object)
   */
  public Font getFont( Object element )
  {
    final ExportableTreeItem item = (ExportableTreeItem)element;
    if( item.isGrayed() )
      return m_grayedFont;
    
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.IColorProvider#getForeground(java.lang.Object)
   */
  public Color getForeground( Object element )
  {
    final ExportableTreeItem item = (ExportableTreeItem)element;
    if( item.isGrayed() )
      return m_grayedForeground;
    
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.IColorProvider#getBackground(java.lang.Object)
   */
  public Color getBackground( Object element )
  {
    final ExportableTreeItem item = (ExportableTreeItem)element;
    if( item.isGrayed() )
      return m_grayedBackground;
    
    return null;
  }

}
