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
