package org.kalypso.model.wspm.ui;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;

public class KalypsoModelWspmUIImages
{
  public static final ImageDescriptor ID_SORT_DOWN = id( "icons/elcl16/sort_down.gif" );

  public static final ImageDescriptor ID_SORT_UP = id( "icons/elcl16/sort_up.gif" );

  public static final ImageDescriptor ID_EMPTY = id( "icons/elcl16/empty.gif" );

  public static final ImageDescriptor ID_CHART_EXPORT = id( "icons/elcl16/chart_export_img.gif" );

  public static final ImageDescriptor ID_CHART_MAXIMIZE = id( "icons/elcl16/chart_maximize.gif" );

  public static final ImageDescriptor ID_CHART_ZOOM_OUT = id( "icons/elcl16/chart_zoom_out.gif" );

  public static final ImageDescriptor ID_CHART_ZOOM_IN = id( "icons/elcl16/chart_zoom_in.gif" );

  public static final ImageDescriptor ID_CHART_PAN = id( "icons/elcl16/chart_pan.gif" );

  public static final ImageDescriptor ID_CHART_EDIT = id( "icons/elcl16/chart_edit.gif" );

  public static final ImageDescriptor ID_MARKER_ERROR = id( "icons/obj16/error_tsk.gif" );

  public static final ImageDescriptor ID_MARKER_WARNING = id( "icons/obj16/warn_tsk.gif" );

  public static final ImageDescriptor ID_BUTTON_WEHR_ADD = id( "icons/obj16/wehr_add.gif" );

  public static final ImageDescriptor ID_BUTTON_WEHR_DELETE = id( "icons/obj16/wehr_delete.gif" );

  public static final ImageDescriptor ID_CHART_FIX_RATIO = id( "icons/elcl16/chart_fix_ratio.gif" );
  
  public static final ImageDescriptor ID_ENABLED_OPEN_TABLE = id( "icons/eview16/profil_table.gif" );

  public static final ImageDescriptor id( final String location )
  {
    return AbstractUIPlugin.imageDescriptorFromPlugin( KalypsoModelWspmUIPlugin.ID, location );
  }

  private KalypsoModelWspmUIImages( )
  {
    // wir nicht instantiiert
  }
}
