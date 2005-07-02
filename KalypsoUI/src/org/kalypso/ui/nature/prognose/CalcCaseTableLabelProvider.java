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
package org.kalypso.ui.nature.prognose;

import java.text.DateFormat;
import java.util.Date;

import org.eclipse.core.resources.IFolder;
import org.eclipse.jface.viewers.IColorProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.kalypso.ui.nature.ModelNature;

/**
 * Label provider used by {@link org.kalypso.ui.nature.prognose.CalcCaseTableTreeViewer}.
 *
 * @author belger
 */
public class CalcCaseTableLabelProvider extends LabelProvider implements ITableLabelProvider, IColorProvider
{
  private WorkbenchLabelProvider m_provider;
  
  private DateFormat m_df = DateFormat.getDateTimeInstance( DateFormat.SHORT, DateFormat.SHORT );

  private final IFolder m_markedCalcCase;

  private final Color m_markedColor;

  public CalcCaseTableLabelProvider( final IFolder markedCalcCase, final Color markedColor )
  {
    m_markedCalcCase = markedCalcCase;
    m_markedColor = markedColor;
    
    m_provider = new WorkbenchLabelProvider();
  }

  /**
   * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang.Object, int)
   */
  public Image getColumnImage( final Object element, final int columnIndex )
  {
    if( columnIndex == 0 )
      return m_provider.getImage( element );

    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnText(java.lang.Object, int)
   */
  public String getColumnText( final Object element, final int columnIndex )
  {
    if( columnIndex == 0 )
    {
      if( element instanceof IFolder && ModelNature.isCalcCalseFolder( (IFolder)element ) )
        return "";
      return m_provider.getText( element );
    }
    
    if( columnIndex == 1 && element instanceof IFolder && ModelNature.isCalcCalseFolder( (IFolder)element ) )
      return m_provider.getText( element );

    if( columnIndex == 2 && element instanceof IFolder )
    {
      final IFolder folder = (IFolder)element;
      return m_df.format( lastModifiedFromFolder(folder) );
    }
    
    return "";
  }

  public final static Date lastModifiedFromFolder( final IFolder folder )
  {
    return new Date( folder.getLocalTimeStamp() );
  }

  /**
   * @see org.eclipse.jface.viewers.IColorProvider#getForeground(java.lang.Object)
   */
  public Color getForeground( final Object element )
  {
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.IColorProvider#getBackground(java.lang.Object)
   */
  public Color getBackground( final Object element )
  {
    if( element != null && element.equals( m_markedCalcCase ) )
      return m_markedColor;
    
    return null;
  }
}
