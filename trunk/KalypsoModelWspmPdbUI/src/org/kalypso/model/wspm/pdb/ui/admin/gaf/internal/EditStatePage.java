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
package org.kalypso.model.wspm.pdb.ui.admin.gaf.internal;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.databinding.validation.StringBlankValidator;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.db.mapping.States;
import org.kalypso.ui.editor.styleeditor.binding.DatabindingWizardPage;

/**
 * @author Gernot Belger
 */
public class EditStatePage extends WizardPage
{
  private final IPdbConnection m_connection;

  private final States m_state;

  private DatabindingWizardPage m_binding;

  public EditStatePage( final String pageName, final IPdbConnection connection, final States state )
  {
    super( pageName );

    m_connection = connection;
    m_state = state;

    setTitle( "Enter State Properties" );
    setDescription( "Enter the properties of the freshly created state" );
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    setControl( panel );
    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( panel );

    m_binding = new DatabindingWizardPage( this, null );

    createNameControls( panel );
    createCommentControls( panel );

    // TODO: alle anderen parameter editieren

    createSourceControls( panel );
  }

  private void createNameControls( final Composite parent )
  {
    new Label( parent, SWT.NONE ).setText( "Name" );

    final Text field = new Text( parent, SWT.BORDER );
    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    field.setMessage( "<Eindeutiger Name des Zustands>" );
    field.setTextLimit( States.NAME_LIMIT );

    final IObservableValue target = SWTObservables.observeText( field );
    final IObservableValue model = BeansObservables.observeValue( m_state, States.PROPERTY_STATE );

    // TODO: validator that checks if a state with same name already exists

    m_binding.bindValue( target, model, new StringBlankValidator( IStatus.ERROR, "'Name' is empty" ) );
  }

  private void createCommentControls( final Composite parent )
  {
    final Label label = new Label( parent, SWT.NONE );
    label.setText( "Description" );
    label.setLayoutData( new GridData( SWT.FILL, SWT.TOP, true, false ) );

    final Text field = new Text( parent, SWT.BORDER | SWT.MULTI | SWT.WRAP );
    field.setTextLimit( States.COMMENT_LIMIT );
    field.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    field.setMessage( "<Beschreibung des Zustands>" );

    final IObservableValue target = SWTObservables.observeText( field );
    final IObservableValue model = BeansObservables.observeValue( m_state, States.PROPERTY_COMMENT );

    m_binding.bindValue( target, model );
  }

  private void createSourceControls( final Composite parent )
  {
    new Label( parent, SWT.NONE ).setText( "Source" );

    final Text field = new Text( parent, SWT.BORDER | SWT.READ_ONLY );
    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final IObservableValue target = SWTObservables.observeText( field );
    final IObservableValue model = BeansObservables.observeValue( m_state, States.PROPERTY_SOURCE );

    m_binding.bindValue( target, model );
  }
}