package org.kalypso.eclipse.jface.viewers;

import java.io.File;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

/**
 * Label provider for java.io.File objects.
 */
public class FileLabelProvider extends LabelProvider
{
  private static final Image IMG_FOLDER = PlatformUI.getWorkbench().getSharedImages().getImage(
      ISharedImages.IMG_OBJ_FOLDER );

  private static final Image IMG_FILE = PlatformUI.getWorkbench().getSharedImages().getImage(
      ISharedImages.IMG_OBJ_FILE );

  public Image getImage( Object element )
  {
    if( element instanceof File )
    {
      File curr = (File)element;
      if( curr.isDirectory() )
        return IMG_FOLDER;

      return IMG_FILE;
    }
    return null;
  }

  public String getText( Object element )
  {
    if( element instanceof File )
    {
      return ( (File)element ).getName();
    }
    return super.getText( element );
  }
}