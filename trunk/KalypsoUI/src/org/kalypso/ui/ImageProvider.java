package org.kalypso.ui;

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
  
  public static final ImageDescriptor IMAGE_ZML_REPOSITORY_ADD = id("icons/observation_repository_add.gif");
  public static final ImageDescriptor IMAGE_ZML_REPOSITORY_REMOVE = id("icons/observation_repository_rem.gif");
  public static final ImageDescriptor IMAGE_ZML_REPOSITORY_CONF = id("icons/observation_repository_conf.gif");
  public static final ImageDescriptor IMAGE_ZML_REPOSITORY_RELOAD = id("icons/observation_repository_reload.gif");
  public static final ImageDescriptor IMAGE_ZML_REPOSITORY_COLLAPSE = id("icons/observation_repository_collapse.gif");
  public static final ImageDescriptor IMAGE_ZML_REPOSITORY_EXPAND = id("icons/observation_repository_expand.gif");
  
  public static final ImageDescriptor IMAGE_MAPVIEW_ZOOMOUT = id( "icons/full/elcl16/kde_viewmag-.gif" );
  public static final ImageDescriptor IMAGE_MAPVIEW_PAN = id( "icons/full/elcl16/kde_move.gif" );
  public static final ImageDescriptor IMAGE_MAPVIEW_FULLEXTENT = id( "icons/full/elcl16/kde_window_fullscreen.gif" );
  public static final ImageDescriptor IMAGE_MAPVIEW_TOGGLESELECT = id( "icons/full/elcl16/kde_14_polyline.gif" );
  public static final ImageDescriptor IMAGE_MAPVIEW_SELECT = id( "icons/full/elcl16/kde_14_rectangle.gif" );
  public static final ImageDescriptor IMAGE_MAPVIEW_UNSELECT = id( "icons/full/elcl16/kde_abort.gif" );

  public static final ImageDescriptor IMAGE_ICON_GTT = id( "icons/kalypso_gtt.gif" );

  public static final ImageDescriptor IMAGE_INTRO_BACKGROUND = id( "etc/kalypso32.gif" );
}

