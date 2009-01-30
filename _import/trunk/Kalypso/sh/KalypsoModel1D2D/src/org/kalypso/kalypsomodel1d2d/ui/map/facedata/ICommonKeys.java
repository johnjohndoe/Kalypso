/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.kalypsomodel1d2d.ui.map.facedata;

/**
 * Holds often used keys
 * 
 * @author Patrice Congo
 */
public interface ICommonKeys
{
  /**
   * Key for for the workspace which root feature constains the boundary conditions
   */
  public static final String KEY_BOUNDARY_CONDITION_CMD_WORKSPACE = "_bc_condition_commandable_workspace_"; //$NON-NLS-1$

  /**
   * Key for data entry holding the map panel
   */
  public static final String KEY_MAP_PANEL = "_map_panel_"; //$NON-NLS-1$

  /**
   * Key used in the context of feature wrapper list editor denoting the data the list of feature wrapper which is to be
   * shown in the list
   */
  public static final String KEY_FEATURE_WRAPPER_LIST = "_feature_wrapper_list_"; //$NON-NLS-1$

  /**
   * Key used in the context of feature wrapper list editor denoting the currently selected feature wrapper
   */
  public static final String KEY_SELECTED_FEATURE_WRAPPER = "_sel_feature_wrapper_"; //$NON-NLS-1$

  /**
   * Key for data entry holding a {@link org.eclipse.swt.widgets.Display}
   */
  public static final String KEY_SELECTED_DISPLAY = "_ui_display_"; //$NON-NLS-1$

  /**
   * Key for data entry holding the discretisation model
   */
  public static final String KEY_DISCRETISATION_MODEL = "_discretisation_model_"; //$NON-NLS-1$

  public static final String KEY_DATA_PROVIDER = "_data_provider_"; //$NON-NLS-1$

  /**
   * Data entry holding the {@link org.kalypso.commons.command.ICommandTarget} used to post command to the
   * discretisation model.
   */
  public static final String KEY_COMMAND_TARGET_DISC_MODEL = "_command target_discmodel_"; //$NON-NLS-1$

  /**
   * Key for data entry holding the {@link org.kalypso.commons.command.ICommandManager} to be use to post command to the
   * discretisation model.
   */
  public static final String KEY_COMMAND_MANAGER_DISC_MODEL = "_command_manager_discmodel_"; //$NON-NLS-1$

  public static final String WIDGET_WITH_STRATEGY = "_widget_with_strategy_"; //$NON-NLS-1$

  public static final String KEY_GRAB_DISTANCE_PROVIDER = "_grab_distance_provider_"; //$NON-NLS-1$

}
