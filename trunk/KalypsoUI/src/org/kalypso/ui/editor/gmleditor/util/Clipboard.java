package org.kalypso.ui.editor.gmleditor.util;

import org.kalypsodeegree.model.feature.Feature;

/**
 * @author lindemfn
 */
public class Clipboard
{
  private Feature clipboardFeature = null;

  public void setClipboardFeature( Feature ft )
  {
    clipboardFeature = ft;
  }

  public Feature getClipboardFeature()
  {
    return clipboardFeature;
  }
}
