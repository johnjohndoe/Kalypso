/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
  
---------------------------------------------------------------------------------------------------*/
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
  
  public static final ImageDescriptor IMAGE_ZML_FILE = id("icons/observation/zml-icon.gif");
  
  public static final ImageDescriptor IMAGE_ZML_REPOSITORY = id("icons/repository/repository_rep.gif");
  public static final ImageDescriptor IMAGE_ZML_REPOSITORY_ITEM = id("icons/repository/repository_item.gif");
  public static final ImageDescriptor IMAGE_ZML_REPOSITORY_ADD = id("icons/repository/repository_add.gif");
  public static final ImageDescriptor IMAGE_ZML_REPOSITORY_REMOVE = id("icons/repository/repository_remove.gif");
  public static final ImageDescriptor IMAGE_ZML_REPOSITORY_CONF = id("icons/util/settings.gif");
  public static final ImageDescriptor IMAGE_ZML_REPOSITORY_RELOAD = id("icons/util/update.gif");
  public static final ImageDescriptor IMAGE_ZML_REPOSITORY_COLLAPSE = id("icons/util/tree_collapse_all.gif");
  public static final ImageDescriptor IMAGE_ZML_REPOSITORY_EXPAND = id("icons/util/tree_expand_all.gif");
  public static final ImageDescriptor IMAGE_ZML_UPLOAD = id("icons/repository/zml_upload.gif");
  public static final ImageDescriptor IMAGE_ZML_DOWNLOAD = id("icons/repository/zml_download.gif");
  
  public static final ImageDescriptor IMAGE_OBSERVATION_LINK = id("icons/observation/observation_link.gif");
  
  public static final ImageDescriptor IMAGE_MAPVIEW_ZOOMOUT = id( "icons/full/elcl16/kde_viewmag-.gif" );
  public static final ImageDescriptor IMAGE_MAPVIEW_PAN = id( "icons/full/elcl16/kde_move.gif" );
  public static final ImageDescriptor IMAGE_MAPVIEW_FULLEXTENT = id( "icons/full/elcl16/kde_window_fullscreen.gif" );
  public static final ImageDescriptor IMAGE_MAPVIEW_TOGGLESELECT = id( "icons/full/elcl16/kde_14_polyline.gif" );

  public static final ImageDescriptor IMAGE_MAPVIEW_SELECT = id( "icons/full/elcl16/kde_14_rectangle.gif" );
  public static final ImageDescriptor IMAGE_MAPVIEW_UNSELECT = id( "icons/full/elcl16/kde_abort.gif" );

  public static final ImageDescriptor IMAGE_ICON_GTT = id( "icons/kalypso_gtt.gif" );

  public static final ImageDescriptor IMAGE_KALYPSO_ICON = id( "etc/kalypso16.gif" );
  public static final ImageDescriptor IMAGE_KALYPSO_ICON_BIG = id( "etc/kalypso32.gif" );
  
  public static final ImageDescriptor IMAGE_UTIL_CHECKED = id( "icons/util/checked.gif" );
  public static final ImageDescriptor IMAGE_UTIL_UNCHECKED = id( "icons/util/unchecked.gif" );
  public static final ImageDescriptor IMAGE_UTIL_UPLOAD_WIZ = id("icons/util/upload_wiz.gif");
  public static final ImageDescriptor IMAGE_UTIL_FILTER = id("icons/util/filter.gif");
  public static final ImageDescriptor IMAGE_UTIL_POINT_GREEN = id("icons/util/point_green.gif");

  public static final ImageDescriptor IMAGE_UTIL_BERICHT_WIZ = id("icons/util/bericht_wiz.gif");
  public static final ImageDescriptor IMAGE_UTIL_BERICHT_ICON = id("icons/util/bericht.gif");
  
  public static final ImageDescriptor IMAGE_STYLEEDITOR_SAVE = id( "icons/gistable/save_edit.gif" );
  public static final ImageDescriptor IMAGE_STYLEEDITOR_ADD_RULE = id( "icons/obstable/add_row.gif" );
  public static final ImageDescriptor IMAGE_STYLEEDITOR_ADD_RULE_PATTERN = IMAGE_ZML_REPOSITORY_EXPAND;
  public static final ImageDescriptor IMAGE_STYLEEDITOR_REMOVE = IMAGE_MAPVIEW_OUTLINE_REMOVE;
  public static final ImageDescriptor IMAGE_STYLEEDITOR_BACKWARD = id( "icons/util/backward_nav.gif" );
  public static final ImageDescriptor IMAGE_STYLEEDITOR_FORWARD = id( "icons/util/forward_nav.gif" );
  public static final ImageDescriptor IMAGE_STYLEEDITOR_OK = id( "icons/util/ok.gif" );
  public static final ImageDescriptor IMAGE_STYLEEDITOR_GET_SCALE = id( "icons/map/redo_edit.gif" );
}
