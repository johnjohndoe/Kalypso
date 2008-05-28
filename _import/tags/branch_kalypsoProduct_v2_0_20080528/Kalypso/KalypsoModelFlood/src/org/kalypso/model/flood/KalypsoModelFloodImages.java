/*
 * Insert INFORM.DSS licence here.
 */
package org.kalypso.model.flood;

import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider.ImageKey;

/**
 * Utility class for handling images in this plugin.
 * 
 * @author belger
 */
public class KalypsoModelFloodImages
{
  public static enum DESCRIPTORS implements ImageKey
  {
    EVENT_ADD("icons/etool16/event_add.gif"),
    TIN_ADD("icons/etool16/tin_add.gif"),
    TIN_UPDATE("icons/etool16/tin_update.gif"),
    TIN_JUMPTO("icons/etool16/tin_jumpto.gif"),
    DELETE("icons/etool16/delete_edit.gif");

    private final String m_imagePath;

    private DESCRIPTORS( final String imagePath )
    {
      m_imagePath = imagePath;
    }

    /**
     * @see org.kalypso.commons.eclipse.core.runtime.PluginImageProvider.ImageKey#getImagePath()
     */
    public String getImagePath( )
    {
      return m_imagePath;
    }
  }
}