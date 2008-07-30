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
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider.ImageKey;

/**
 * Convenience class for storing references to image descriptors used by the readme tool.
 */
public class ImageProvider
{
  public static enum DESCRIPTORS implements ImageKey
  {
    FEATURE("icons/feature/gis_feature.gif"), //$NON-NLS-1$

    WAIT_LOADING_OBJ("icons/full/obj16/wait_loading.gif"), //$NON-NLS-1$
    FAILED_LOADING_OBJ("icons/full/obj16/failed_loading.gif"), //$NON-NLS-1$

    FORBIDDEN_OVR("icons/full/ovr16/forbidden.gif"), //$NON-NLS-1$

    IMAGE_MAPVIEW_ZOOMIN("icons/map/zoomin.gif"), //$NON-NLS-1$
    IMAGE_MAPVIEW_ZOOMOUT("icons/map/zoomout.gif"), //$NON-NLS-1$
    IMAGE_MAPVIEW_PAN("icons/map/pan.gif"), //$NON-NLS-1$
    IMAGE_MAPVIEW_FULLEXTEND("icons/map/maximize2.gif"), //$NON-NLS-1$

    IMAGE_THEME_STANDARD("icons/full/obj16/standardTheme.gif"), //$NON-NLS-1$
    IMAGE_THEME_FEATURE("icons/full/obj16/featureTheme.gif"), //$NON-NLS-1$
    IMAGE_THEME_WMS("icons/full/obj16/wmsTheme.gif"), //$NON-NLS-1$
    IMAGE_THEME_CASCADING("icons/full/obj16/cascadingTheme.gif"), //$NON-NLS-1$
    IMAGE_THEME_SCALE("icons/full/obj16/scaleTheme.gif"), //$NON-NLS-1$
    IMAGE_THEME_LEGEND("icons/full/obj16/legendTheme.gif"), //$NON-NLS-1$
    IMAGE_THEME_SCRAP("icons/full/obj16/scrapTheme.gif"), //$NON-NLS-1$

    STATUS_LINE_SHOW_MAP_COORDS("icons/map/statusbarPosition.gif"), //$NON-NLS-1$
    STATUS_LINE_SHOW_CRS_INFO("icons/map/statusbarShowCRSInfo.gif"), //$NON-NLS-1$

    STATUS_IMAGE_OK("icons/full/elcl16/ok.gif"); //$NON-NLS-1$

    private final String m_imagePath;

    private DESCRIPTORS( final String imagePath )
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

  public static final ImageDescriptor id( final String pluginID, final String location )
  {
    return AbstractUIPlugin.imageDescriptorFromPlugin( pluginID, location );
  }

  public static final ImageDescriptor id( final String location )
  {
    return ImageProvider.id( "org.kalypso.ui", location ); //$NON-NLS-1$
  }

  // DEPRECATED: The image constants below are deprecated
  // Use the DESCRIPTOR enum above instead

  // public static final ImageDescriptor IMAGE_MAPVIEW_OUTLINE_UP = id( "icons/full/elcl16/prev_nav.gif" );
  // public static final ImageDescriptor IMAGE_MAPVIEW_OUTLINE_DOWN = id( "icons/full/elcl16/next_nav.gif" );
  public static final ImageDescriptor IMAGE_MAPVIEW_OUTLINE_REMOVE = ImageProvider.id( "icons/full/elcl16/remove.gif" ); //$NON-NLS-1$

  // public static final ImageDescriptor IMAGE_MAPVIEW_OUTLINE_ADD = id( "icons/full/elcl16/newpack_wiz.gif" );
  // public static final ImageDescriptor IMAGE_MAPVIEW_ZOOMIN = id( "icons/full/elcl16/kde_viewmag+.gif" );
  // public static final ImageDescriptor IMAGE_MAPVIEW_ZOOMOUT = id( "icons/full/elcl16/kde_viewmag-.gif" );
  // public static final ImageDescriptor IMAGE_MAPVIEW_FULLEXTENT = id( "icons/full/elcl16/kde_window_fullscreen.gif" );
  // public static final ImageDescriptor IMAGE_MAPVIEW_TOGGLESELECT = id( "icons/full/elcl16/kde_14_polyline.gif" );
  // public static final ImageDescriptor IMAGE_MAPVIEW_SELECT = id( "icons/full/elcl16/kde_14_rectangle.gif" );
  // public static final ImageDescriptor IMAGE_MAPVIEW_UNSELECT = id( "icons/full/elcl16/kde_abort.gif" );

  public static final ImageDescriptor IMAGE_NEW_FILE = ImageProvider.id( "icons/util/newfile_wiz.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_ZML_FILE = ImageProvider.id( "icons/observation/zml-icon.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_ZML_REPOSITORY = ImageProvider.id( "icons/repository/repository_rep.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_ZML_REPOSITORY_ITEM = ImageProvider.id( "icons/repository/repository_item.gif" ); //$NON-NLS-1$

// public static final ImageDescriptor IMAGE_ZML_REPOSITORY_ADD = ImageProvider.id(
// "icons/repository/repository_add.gif" );

// public static final ImageDescriptor IMAGE_ZML_REPOSITORY_REMOVE = ImageProvider.id(
// "icons/repository/repository_remove.gif" );

// public static final ImageDescriptor IMAGE_ZML_REPOSITORY_CONF = ImageProvider.id( "icons/util/settings.gif" );

// public static final ImageDescriptor IMAGE_ZML_REPOSITORY_RELOAD = ImageProvider.id( "icons/util/update.gif" );

// public static final ImageDescriptor IMAGE_ZML_REPOSITORY_COLLAPSE = ImageProvider.id(
// "icons/util/tree_collapse_all.gif" );

  public static final ImageDescriptor IMAGE_ZML_REPOSITORY_EXPAND = ImageProvider.id( "icons/util/tree_expand_all.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_ZML_UPLOAD = ImageProvider.id( "icons/repository/zml_upload.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_ZML_DOWNLOAD = ImageProvider.id( "icons/repository/zml_download.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_OBSERVATION_LINK = ImageProvider.id( "icons/observation/observation_link.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_ICON_GTT = ImageProvider.id( "icons/kalypso_gtt.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_ICON_GMT = ImageProvider.id( "icons/kalypso_gmt.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_ICON_ODT = ImageProvider.id( "icons/kalypso_odt.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_ICON_OTT = ImageProvider.id( "icons/kalypso_ott.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_KALYPSO_ICON = ImageProvider.id( "icons/kalypso16.gif" ); //$NON-NLS-1$ //$NON-NLS-2$

  public static final ImageDescriptor IMAGE_KALYPSO_ICON_BIG = ImageProvider.id( "icons/kalypso32.gif" ); //$NON-NLS-1$ //$NON-NLS-2$

  public static final ImageDescriptor IMAGE_UTIL_CHECKED = ImageProvider.id( "icons/util/checked.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_UTIL_UNCHECKED = ImageProvider.id( "icons/util/unchecked.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_UTIL_UPLOAD_WIZ = ImageProvider.id( "icons/util/upload_wiz.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_UTIL_FILTER = ImageProvider.id( "icons/util/filter.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_UTIL_POINT_GREEN = ImageProvider.id( "icons/util/point_green.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_UTIL_BERICHT_WIZ = ImageProvider.id( "icons/util/bericht_wiz.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_UTIL_IMPORT_WIZARD = ImageProvider.id( "icons/util/import_wiz.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_STYLEEDITOR_SAVE = ImageProvider.id( "icons/gistable/save_edit.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_STYLEEDITOR_ADD_RULE = ImageProvider.id( "icons/obstable/add_row.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_STYLEEDITOR_ADD_RULE_PATTERN = ImageProvider.IMAGE_ZML_REPOSITORY_EXPAND;

  public static final ImageDescriptor IMAGE_STYLEEDITOR_REMOVE = ImageProvider.IMAGE_MAPVIEW_OUTLINE_REMOVE;

  public static final ImageDescriptor IMAGE_STYLEEDITOR_BACKWARD = ImageProvider.id( "icons/util/backward_nav.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_STYLEEDITOR_FORWARD = ImageProvider.id( "icons/util/forward_nav.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_STYLEEDITOR_OK = ImageProvider.id( "icons/util/ok.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_STYLEEDITOR_GET_SCALE = ImageProvider.id( "icons/map/redo_edit.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_FILTERDIALOG_ADD_FILTER = ImageProvider.id( "icons/filterdialog/filter_ps.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_FILTERDIALOG_ERROR = ImageProvider.id( "icons/filterdialog/error_obj.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_FILTERDIALOG_WARNING = ImageProvider.id( "icons/filterdialog/warning_obj.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_FEATURE = ImageProvider.id( "icons/feature/gis_feature.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_FEATURE_NEW = ImageProvider.id( "icons/feature/gis_feature_neu.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_FEATURE_DELETE = ImageProvider.id( "icons/feature/gis_feature_delete.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_FEATURE_LINKED = ImageProvider.id( "icons/feature/gis_feature_light.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_FEATURE_RELATION_COMPOSITION = ImageProvider.id( "icons/feature/feature_link_composition.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_GEOM_PROP_POINT = ImageProvider.id( "icons/feature/geom_point.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_GEOM_PROP_MULTIPOINT = ImageProvider.id( "icons/feature/geom_multipoint.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_GEOM_PROP_LINE = ImageProvider.id( "icons/feature/geom_line.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_GEOM_PROP_MULTILINE = ImageProvider.id( "icons/feature/geom_multiline.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_GEOM_PROP_POLYGON = ImageProvider.id( "icons/feature/geom_polygon.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_GEOM_PROP_MULTIPOLYGON = ImageProvider.id( "icons/feature/geom_multipolygon.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_FEATURE_VALIDATION_EMPTY = ImageProvider.id( "icons/validation/validation_empty.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_FEATURE_VALIDATION_OK = ImageProvider.id( "icons/validation/validation_inputok.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_FEATURE_VALIDATION_WARNING = ImageProvider.id( "icons/validation/validation_warning.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_FEATURE_VALIDATION_NOTOK = ImageProvider.id( "icons/validation/validation_error.gif" ); //$NON-NLS-1$

  /* tuple result table toolbar */
  public static final ImageDescriptor IMAGE_TABLE_COPY = ImageProvider.id( "icons/table/copy.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_TABLE_PASTE = ImageProvider.id( "icons/table/paste_edit.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_TABLE_ADD_ROW = ImageProvider.id( "icons/table/add_row.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_TABLE_INSERT_ROW = ImageProvider.id( "icons/table/insert_row.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_TABLE_DELETE_ROW = ImageProvider.id( "icons/table/delete_row.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_TABLE_DELETE_COLUMN = ImageProvider.id( "icons/table/delete_column.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_TABLE_EXPORT = ImageProvider.id( "icons/table/export_wiz.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_TABLE_IMPORT = ImageProvider.id( "icons/table/import_wiz.gif" ); //$NON-NLS-1$
}