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
package org.kalypso.model.wspm.pdb.ui.internal.content;

import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.ui.forms.ToolkitUtils;
import org.kalypso.model.wspm.pdb.ui.internal.content.filter.StateFilterControl;
import org.kalypso.model.wspm.pdb.ui.internal.content.filter.WaterBodyFilterControl;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class ContentSearchViewer extends Composite
{
  public ContentSearchViewer( final FormToolkit toolkit, final Composite parent, final StructuredViewer viewer, final IConnectionViewer connectionViewer )
  {
    super( parent, SWT.NONE );

    GridLayoutFactory.fillDefaults().extendedMargins( 0, 0, 0, 5 ).applyTo( this );

    final Group waterGroup = new Group( this, SWT.NONE );
    ToolkitUtils.adapt( toolkit, waterGroup );
    waterGroup.setLayout( new FillLayout() );
    waterGroup.setText( Messages.getString( "ContentSearchViewer.0" ) ); //$NON-NLS-1$
    waterGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    final WaterBodyFilterControl waterFilterControl = new WaterBodyFilterControl( toolkit, waterGroup, connectionViewer );
    waterFilterControl.setViewer( viewer );

    final Group stateGroup = new Group( this, SWT.NONE );
    ToolkitUtils.adapt( toolkit, stateGroup );
    stateGroup.setLayout( new FillLayout() );
    stateGroup.setText( Messages.getString( "ContentSearchViewer.1" ) ); //$NON-NLS-1$
    stateGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    final StateFilterControl stateFilterControl = new StateFilterControl( toolkit, stateGroup );
    stateFilterControl.setViewer( viewer );
  }
}