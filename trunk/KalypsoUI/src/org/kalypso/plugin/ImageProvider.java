package org.kalypso.plugin;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;

/**
 * Convenience class for storing references to image descriptors used by the
 * readme tool.
 */
public class ImageProvider
{
  public static final ImageDescriptor id( final String location )
  {
    return AbstractUIPlugin.imageDescriptorFromPlugin( "org.kalypso.ui", location );
  }
  
  public static final ImageDescriptor IMAGE_MAPVIEW_OUTLINE_UP = id( "icons/full/elcl16/prev_nav.gif" );
  public static final ImageDescriptor IMAGE_MAPVIEW_OUTLINE_DOWN = id( "icons/full/elcl16/next_nav.gif" );

  public static final ImageDescriptor IMAGE_MAPVIEW_OUTLINE_REMOVE = id( "icons/full/elcl16/remove.gif" );
  public static final ImageDescriptor IMAGE_MAPVIEW_OUTLINE_ADD = id( "icons/full/elcl16/newpack_wiz.gif" );
  
  public static final ImageDescriptor IMAGE_MAPVIEW_ZOOMIN = id( "icons/full/elcl16/kde_viewmag+.gif" );
  public static final ImageDescriptor IMAGE_MAPVIEW_ZOOMOUT = id( "icons/full/elcl16/kde_viewmag-.gif" );
  public static final ImageDescriptor IMAGE_MAPVIEW_PAN = id( "icons/full/elcl16/kde_move.gif" );
  public static final ImageDescriptor IMAGE_MAPVIEW_FULLEXTENT = id( "icons/full/elcl16/kde_window_fullscreen.gif" );
  public static final ImageDescriptor IMAGE_MAPVIEW_TOGGLESELECT = id( "icons/full/elcl16/kde_14_polyline.gif" );
  public static final ImageDescriptor IMAGE_MAPVIEW_SELECT = id( "icons/full/elcl16/kde_14_rectangle.gif" );
  public static final ImageDescriptor IMAGE_MAPVIEW_UNSELECT = id( "icons/full/elcl16/kde_abort.gif" );
}

