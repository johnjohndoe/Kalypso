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
package org.kalypso.kalypsomodel1d2d.ui.map.facedata;

import javax.xml.namespace.QName;

import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandManager;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;

/**
 * Utils method arround {@link KeyBasedDataModel}
 *
 * @author Patrice Congo
 *
 */
public class KeyBasedDataModelUtil
{
  /**
   * Posts the given command to the command target in the data model
   *
   * @param dataModel
   *            the data model holding the command target
   * @param command
   *            the command to post
   */
  public static final void postCommand( final KeyBasedDataModel dataModel, final ICommand command, final String key )
  {
    Assert.throwIAEOnNullParam( dataModel, "dataModel" ); //$NON-NLS-1$
    Assert.throwIAEOnNullParam( command, "command" ); //$NON-NLS-1$

    if( key != ICommonKeys.KEY_COMMAND_MANAGER_DISC_MODEL && key != ICommonKeys.KEY_BOUNDARY_CONDITION_CMD_WORKSPACE )
      throw new RuntimeException( "Only commands to discretisation modell or operational modell are supported: " + key ); //$NON-NLS-1$

    final ICommandManager commandManager = dataModel.getData( ICommandManager.class, key );
    if( commandManager == null )
    {
      throw new RuntimeException( "Could not found command target in the model:key=" + key ); //$NON-NLS-1$
    }
    try
    {
      commandManager.postCommand( command );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new RuntimeException( e );
    }
  }

  /**
   * Get and reset the current entry associated with the provided key. This just cause a change event to be raise so
   * views may update.
   *
   * @param dataModel
   *            model to update
   * @param key
   *            the key for the entry to reset
   * @throws IllegalArgumentException
   *             if dataModel or key is null
   */
  public static final void resetCurrentEntry( final KeyBasedDataModel dataModel, final String key )
  {
    Assert.throwIAEOnNullParam( dataModel, "dataModel" ); //$NON-NLS-1$
    Assert.throwIAEOnNullParam( key, "key" ); //$NON-NLS-1$
    dataModel.setData( key, dataModel.getData( key ) );
  }

  public static final CommandableWorkspace getBCWorkSpace( final KeyBasedDataModel dataModel )
  {
    return getCommandableWorkspace( dataModel, IBoundaryCondition.QNAME );
  }

  public static final CommandableWorkspace getCommandableWorkspace( final KeyBasedDataModel dataModel, final QName themeQName )
  {
    final IMapPanel mapPanel = dataModel.getData( IMapPanel.class, ICommonKeys.KEY_MAP_PANEL );
    if( mapPanel == null )
    {
      throw new RuntimeException( "Could not found map panel" ); //$NON-NLS-1$
    }
    final IMapModell mapModell = mapPanel.getMapModell();
    if( mapModell == null )
      throw new RuntimeException( "Could not get map model" ); //$NON-NLS-1$

    final IKalypsoFeatureTheme bcTheme = UtilMap.findEditableTheme( mapPanel, themeQName );
    if( bcTheme == null )
    {
      throw new RuntimeException( "Could not find boundary condition theme" ); //$NON-NLS-1$
    }
    return bcTheme.getWorkspace();
  }

}
