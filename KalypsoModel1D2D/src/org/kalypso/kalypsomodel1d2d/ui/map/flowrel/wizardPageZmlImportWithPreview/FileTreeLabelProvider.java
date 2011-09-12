package org.kalypso.kalypsomodel1d2d.ui.map.flowrel.wizardPageZmlImportWithPreview;

import java.io.File;

import org.eclipse.jface.viewers.LabelProvider;

public class FileTreeLabelProvider extends LabelProvider
{
  @Override
  public String getText(Object element)
  {
    return ((File) element).getName();
  }
}