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
package org.kalypso.ui.rrm.internal.cm.view;

import org.eclipse.jface.action.Action;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.model.rcm.binding.ILinearSumGenerator;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.UIRrmImages.DESCRIPTORS;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeModel;

/**
 * @author Gernot Belger
 */
public class EditGeneratorAction extends Action
{
  private final ITreeNodeModel m_model;

  private final IRainfallGenerator m_generator;

  public EditGeneratorAction( final ITreeNodeModel model, final IRainfallGenerator generator )
  {
    m_model = model;
    m_generator = generator;

    setText( "Edit" );
    setToolTipText( "Edits the properties of the generator" );

    setImageDescriptor( UIRrmImages.id( DESCRIPTORS.GENERATOR_EDIT ) );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();

    // FIXME: check integrity of generator with modell.gml

    // TODO: fixme
    if( m_generator instanceof ILinearSumGenerator )
      editLinearSum( shell );

// final String oldGroup = m_station.getGroup();
//
// final Wizard wizard = new EditStationWizard( m_context, m_station );
// wizard.setWindowTitle( "Edit Properties" );
//
// final WizardDialog dialog = new WizardDialog( shell, wizard );
// if( dialog.open() != Window.OK )
// return;
//
// m_stationControl.refresh();
//
// final String newGroup = m_station.getGroup();
// if( !ObjectUtils.equals( oldGroup, newGroup ) )
// m_context.refreshTree( m_station );
  }

  private void editLinearSum( final Shell shell )
  {
    final LinearSumBean bean = new LinearSumBean( (ILinearSumGenerator) m_generator );

    final EditCatchmentsDialog dialog = new EditCatchmentsDialog( shell, m_model, bean );
    dialog.open();

    // FIXME: apply changes + refresh tree
  }
}
