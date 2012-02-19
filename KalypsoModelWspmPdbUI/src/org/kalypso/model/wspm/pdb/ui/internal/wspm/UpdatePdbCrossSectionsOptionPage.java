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
package org.kalypso.model.wspm.pdb.ui.internal.wspm;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;

/**
 * @author Gernot Belger
 */
public class UpdatePdbCrossSectionsOptionPage extends WizardPage
{
  private IDataBinding m_binding;

  private final UpdatePdbCrossSectionsData m_data;

  protected UpdatePdbCrossSectionsOptionPage( final String pageName, final UpdatePdbCrossSectionsData data )
  {
    super( pageName );

    setTitle( "Options" );
    setDescription( "This page shows the available options for updating the cross sections in the database." );

    m_data = data;
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    setControl( panel );

    m_binding = new DatabindingWizardPage( this, null );

    GridLayoutFactory.swtDefaults().applyTo( panel );

    createCommentOption( panel );
  }

  private void createCommentOption( final Composite parent )
  {
    final Button checkbox = new Button( parent, SWT.CHECK );
    checkbox.setText( "Update comments" );
    checkbox.setToolTipText( "If enabled, the comments of all selected cross sections will be updated." );
    checkbox.setEnabled( false );

    final ISWTObservableValue target = SWTObservables.observeSelection( checkbox );
    final IObservableValue model = BeansObservables.observeValue( m_data, UpdatePdbCrossSectionsData.PROPERTY_UPDATE_COMMENTS );

    m_binding.bindValue( target, model );
  }
}