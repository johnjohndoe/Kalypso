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
package org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit;

import java.util.List;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;

/**
 * @author Madanagopal
 * @author Patrice Congo
 */
public class CalculationUnitDataModel extends KeyBasedDataModel
{
  public CalculationUnitDataModel( )
  {
    super( new String[] { ICommonKeys.KEY_FEATURE_WRAPPER_LIST, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER, ICommonKeys.KEY_DATA_PROVIDER, ICommonKeys.KEY_DISCRETISATION_MODEL,
        ICommonKeys.KEY_MAP_PANEL, ICommonKeys.KEY_COMMAND_TARGET_DISC_MODEL, ICommonKeys.WIDGET_WITH_STRATEGY, ICommonKeys.KEY_COMMAND_MANAGER_DISC_MODEL, ICommonKeys.KEY_SELECTED_DISPLAY,
        ICommonKeys.KEY_BOUNDARY_CONDITION_CMD_WORKSPACE, ICommonKeys.KEY_GRAB_DISTANCE_PROVIDER }, null );
  }

  public ICalculationUnit getSelectedCalculationUnit( )
  {
    return (ICalculationUnit)getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
  }

  public ICalculationUnit[] getCalculationUnits( )
  {
    final List<ICalculationUnit> data = (List<ICalculationUnit>)getData( ICommonKeys.KEY_FEATURE_WRAPPER_LIST );
    return data.toArray( new ICalculationUnit[data.size()] );
  }

}
