/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.ui;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;

public class KalypsoModelWspmUIImages
{
  public static final ImageDescriptor ID_SORT_DOWN = id( "icons/elcl16/sort_down.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor ID_SORT_UP = id( "icons/elcl16/sort_up.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor ID_EMPTY = id( "icons/elcl16/empty.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor ID_CHART_EXPORT = id( "icons/elcl16/chart_export_img.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor ID_CHART_SCREENSHOT = id( "icons/elcl16/chart_screenshot_img.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor ID_CHART_MAXIMIZE = id( "icons/elcl16/chart_maximize.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor ID_CHART_ZOOM_OUT = id( "icons/elcl16/chart_zoom_out.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor ID_CHART_ZOOM_IN = id( "icons/elcl16/chart_zoom_in.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor ID_CHART_PAN = id( "icons/elcl16/chart_pan.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor ID_CHART_EDIT = id( "icons/elcl16/chart_edit.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor ID_MARKER_ERROR = id( "icons/obj16/error_tsk.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor ID_MARKER_WARNING = id( "icons/obj16/warn_tsk.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor ID_BUTTON_WEHR_ADD = id( "icons/obj16/wehr_add.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor ID_BUTTON_WEHR_DELETE = id( "icons/obj16/wehr_del.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor ID_CHART_FIX_RATIO = id( "icons/elcl16/chart_fix_ratio.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor ID_ENABLED_OPEN_TABLE = id( "icons/eview16/profil_table.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor id( final String location )
  {
    return AbstractUIPlugin.imageDescriptorFromPlugin( KalypsoModelWspmUIPlugin.ID, location );
  }

  private KalypsoModelWspmUIImages( )
  {
    // wird nicht instantiiert
  }
}
