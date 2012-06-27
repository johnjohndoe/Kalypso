/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.model.wspm.pdb.connect.IPdbOperation;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.IWaterBodyStructure;
import org.kalypso.model.wspm.pdb.ui.internal.content.ElementSelector;
import org.kalypso.model.wspm.pdb.ui.internal.content.IConnectionViewer;
import org.kalypso.model.wspm.pdb.ui.internal.content.IRemoveWorker;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class RemoveWaterBodyWorker implements IRemoveWorker
{
  private final WaterBody m_selectedItem;

  public RemoveWaterBodyWorker( final WaterBody selectedItem )
  {
    m_selectedItem = selectedItem;
  }

  @Override
  public String getWindowTitle( )
  {
    return Messages.getString( "RemoveWaterBodyWorker.0" ); //$NON-NLS-1$
  }

  @Override
  public boolean checkPrerequisites( final Shell shell )
  {
    final String name = m_selectedItem.getName();

    final boolean hasData = hasData();
    if( hasData )
    {
      /* show dialog with states/cross-sections -> water cannot be removed */
      final CannotRemoveWaterBodyDialog dialog = new CannotRemoveWaterBodyDialog( shell, getWindowTitle(), m_selectedItem );
      dialog.open();
      return false;
    }

    final String message = String.format( Messages.getString( "RemoveWaterBodyWorker.1" ), m_selectedItem.getLabel(), name ); //$NON-NLS-1$
    return MessageDialog.openConfirm( shell, getWindowTitle(), message );
  }

  private boolean hasData( )
  {
    final boolean hasCrossSections = !m_selectedItem.getCrossSections().isEmpty();
    final boolean hasWaterlevels = !m_selectedItem.getEvents().isEmpty();

    return hasCrossSections || hasWaterlevels;
  }

  @Override
  public IPdbOperation createOperation( )
  {
    return new DeleteWaterBodyOperation( m_selectedItem.getName() );
  }

  @Override
  public void addElementsToSelect( final IConnectionViewer viewer, final ElementSelector selector )
  {
    final IWaterBodyStructure structure = viewer.getStructure();
    final Object parent = structure.getParent( m_selectedItem );
    if( parent instanceof WaterBody )
      selector.addWaterBodyName( ((WaterBody) parent).getName() );
  }
}