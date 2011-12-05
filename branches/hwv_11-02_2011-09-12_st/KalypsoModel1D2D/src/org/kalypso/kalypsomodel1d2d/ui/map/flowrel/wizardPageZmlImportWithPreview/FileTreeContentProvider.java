package org.kalypso.kalypsomodel1d2d.ui.map.flowrel.wizardPageZmlImportWithPreview;

import java.io.File;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

public class FileTreeContentProvider implements ITreeContentProvider
{
  @Override
  public Object[] getChildren(Object element)
  {
    Object[] kids = ((File) element).listFiles();
    return kids == null ? new Object[0] : kids;
  }

  @Override
  public Object[] getElements(Object element)
  {
    return getChildren(element);
  }

  @Override
  public boolean hasChildren(Object element)
  {
    return getChildren(element).length > 0;
  }

  @Override
  public Object getParent(Object element)
  {
    return ((File)element).getParent();
  }
  
  @Override
  public void dispose()
  {
  }

  @Override
  public void inputChanged(Viewer viewer, Object old_input, Object new_input)
  {
  }
}