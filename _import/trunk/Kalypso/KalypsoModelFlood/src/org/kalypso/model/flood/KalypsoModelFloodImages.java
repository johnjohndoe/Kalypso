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
    EVENT_IMPORT_TIN("icons/etool16/import_event_tin.gif"),
    EVENT_UPDATE_TIN("icons/etool16/update_event_tin.gif"),
    EDIT_STYLES("icons/etool16/style_edit.gif");

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