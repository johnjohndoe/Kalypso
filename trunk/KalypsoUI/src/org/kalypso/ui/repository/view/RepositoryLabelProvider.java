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
package org.kalypso.ui.repository.view;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;
import org.kalypso.ui.ImageProvider;

/**
 * RepositoryLabelProvider
 * 
 * @author schlienger
 */
public class RepositoryLabelProvider extends LabelProvider
{
  private static final Image IMG_FOLDER = PlatformUI.getWorkbench().getSharedImages().getImage(
      ISharedImages.IMG_OBJ_FOLDER );

  private final Image IMG_ITEM = ImageProvider.IMAGE_ZML_REPOSITORY_ITEM.createImage();
  
  private final Image IMG_REPOSITORY = ImageProvider.IMAGE_ZML_REPOSITORY.createImage();

  /**
   * @see org.eclipse.jface.viewers.LabelProvider#getImage(java.lang.Object)
   */
  public Image getImage( final Object element )
  {
    if( element instanceof IRepositoryItem )
    {
      final IRepositoryItem item = (IRepositoryItem) element;
      
      try
      {
        if( item.getParent() == null )
          return IMG_REPOSITORY;
          
        if( item.hasChildren() )
          return IMG_FOLDER;
      }
      catch( RepositoryException e )
      {
        e.printStackTrace();
      }
      
      return IMG_ITEM;
    }
      
    return null;
  }
}
