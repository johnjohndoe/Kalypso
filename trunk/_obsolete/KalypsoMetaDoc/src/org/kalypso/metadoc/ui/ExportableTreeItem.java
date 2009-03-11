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

import java.util.List;

import org.eclipse.jface.resource.ImageDescriptor;
import org.kalypso.metadoc.IExportableObject;

/**
 * 
 * Tree items for the tree.
 * 
 * @author Gernot Belger
 */
public class ExportableTreeItem
{
  private String m_label;
  private ExportableTreeItem[] m_children;
  private ExportableTreeItem m_parent;
  private final IExportableObject m_exportableObject;
  private final boolean m_checked;
  private final boolean m_grayed;
  private final ImageDescriptor m_imageDescriptor;

  public ExportableTreeItem( final String label, final ImageDescriptor imageDescriptor, final ExportableTreeItem parent,
      final IExportableObject exportableObject, final boolean checked, final boolean grayed)
  {
    m_imageDescriptor = imageDescriptor;
    m_exportableObject = exportableObject;

    m_label = label;
    m_parent = parent;
    m_checked = checked;
    m_grayed = grayed;
    
    m_children = new ExportableTreeItem[] {};
  }

  public void setChildren( ExportableTreeItem[] children )
  {
    m_children = children;
  }

  public IExportableObject getExportableObject()
  {
    return m_exportableObject;
  }

  public ExportableTreeItem getParent()
  {
    return m_parent;
  }

  public ExportableTreeItem[] getChildren()
  {
    return m_children;
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString()
  {
    return m_label;
  }

  public ImageDescriptor getImage()
  {
    return m_imageDescriptor;
  }
  
  public boolean isChecked()
  {
    return m_checked;
  }

  public boolean isGrayed()
  {
    return m_grayed;
  }

  public static void filterChecked( final ExportableTreeItem[] items, final List<ExportableTreeItem> checkedItems, final List<ExportableTreeItem> grayedItems )
  {
    for( final ExportableTreeItem item : items )
    {
      if( item.isChecked() )
        checkedItems.add( item );

      if( item.isGrayed() )
        grayedItems.add( item );

      filterChecked( item.getChildren(), checkedItems, grayedItems );
    }
  }

  
}
