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
package org.kalypso.kalypso1d2d.internal.importNet;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;

import com.bce.gis.io.zweidm.IPolygonWithName;

/**
 * @author Gernot Belger
 */
public class ClearDatasetAction extends Action implements IUpdateable
{
  private final Import2dElementsData m_data;

  public ClearDatasetAction( final Import2dElementsData data )
  {
    m_data = data;

    setText( Messages.getString("ClearDatasetAction_0") ); //$NON-NLS-1$
    setToolTipText( Messages.getString("ClearDatasetAction_1") ); //$NON-NLS-1$

    final ImageDescriptor image = PlatformUI.getWorkbench().getSharedImages().getImageDescriptor( ISharedImages.IMG_ELCL_REMOVE );
    setImageDescriptor( image );
  }

  @Override
  public void update( )
  {
    final IPolygonWithName[] elements = m_data.getElements();
    setEnabled( !ArrayUtils.isEmpty( elements ) );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();

    if( !MessageDialog.openConfirm( shell, getText(), Messages.getString("ClearDatasetAction_2") ) ) //$NON-NLS-1$
      return;

    m_data.clearElements();
  }
}
