/*
 * Insert INFORM.DSS licence here.
 */
package org.kalypso.model.wspm.tuhh.ui;

import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider.ImageKey;

/**
 * Utility class for handling images in this plugin.
 * 
 * @author Gernot Belger
 */
public enum KalypsoModelWspmTuhhUIImages implements ImageKey
{
  NEWPROJECT_PROJECT_PAGE_WIZBAN("icons/wizban/kalypso32.gif");

  private final String m_imagePath;

  private KalypsoModelWspmTuhhUIImages( final String imagePath )
  {
    m_imagePath = imagePath;
  }

  /**
   * @see org.kalypso.informdss.KalypsoInformDSSImages.ImageKey#getImagePath()
   */
  public String getImagePath( )
  {
    return m_imagePath;
  }
}
