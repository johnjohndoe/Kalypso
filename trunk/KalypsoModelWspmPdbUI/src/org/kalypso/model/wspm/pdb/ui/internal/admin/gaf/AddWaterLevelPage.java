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
package org.kalypso.model.wspm.pdb.ui.internal.admin.gaf;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.gaf.ImportGafData;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.wspm.DefaultEditEventPageData;
import org.kalypso.model.wspm.pdb.wspm.IEditEventPageData;

/**
 * @author Gernot Belger
 */
public class AddWaterLevelPage extends WizardPage
{
  private final ImportGafData m_data;

  private DatabindingWizardPage m_binding;

  private WaterlevelComposite m_waterlevelComposite;

  protected AddWaterLevelPage( final String pageName, final ImportGafData data )
  {
    super( pageName );
    m_data = data;

    setTitle( Messages.getString( "AddWaterLevelPage.0" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "AddWaterLevelPage.1" ) ); //$NON-NLS-1$
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    setControl( panel );
    GridLayoutFactory.swtDefaults().applyTo( panel );

    m_binding = new DatabindingWizardPage( this, null );

    createDoImportCheck( panel );
    createEventControls( panel );

    updateControl();
  }

  private void createDoImportCheck( final Composite parent )
  {
    final Button checkbox = new Button( parent, SWT.CHECK );
    checkbox.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    checkbox.setText( Messages.getString( "AddWaterLevelPage.2" ) ); //$NON-NLS-1$

    final ISWTObservableValue targetSelection = SWTObservables.observeSelection( checkbox );
    final IObservableValue modelSelection = BeansObservables.observeValue( m_data, ImportGafData.PROPERTY_IMPORT_WATERLEVELS );
    m_binding.bindValue( targetSelection, modelSelection );

    final ISWTObservableValue targetEnablement = SWTObservables.observeEnabled( checkbox );
    final IObservableValue modelEnablement = BeansObservables.observeValue( m_data, ImportGafData.PROPERTY_HAS_WATERLEVELS );
    m_binding.bindValue( targetEnablement, modelEnablement );
  }

  private void createEventControls( final Composite panel )
  {
    final Group group = new Group( panel, SWT.NONE );
    group.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    group.setText( Messages.getString( "AddWaterLevelPage.3" ) ); //$NON-NLS-1$
    group.setLayout( new FillLayout() );

    final Event event = m_data.getWaterlevelEvent();

    final IEditEventPageData data = new DefaultEditEventPageData( null, event, null, false );

    m_waterlevelComposite = new WaterlevelComposite( group, SWT.NONE, data, m_binding, null );

    final ISWTObservableValue targetVisible = SWTObservables.observeVisible( group );
    final IObservableValue modelVisible = BeansObservables.observeValue( m_data, ImportGafData.PROPERTY_HAS_WATERLEVELS );
    m_binding.bindValue( targetVisible, modelVisible );

    final ISWTObservableValue targetEnablement = SWTObservables.observeEnabled( m_waterlevelComposite );
    final IObservableValue modelEnablement = BeansObservables.observeValue( m_data, ImportGafData.PROPERTY_IMPORT_WATERLEVELS );
    m_binding.bindValue( targetEnablement, modelEnablement );
  }

  public void updateControl( )
  {
    if( m_data.getHasWaterlevels() )
      setMessage( null );
    else
      setMessage( Messages.getString( "AddWaterLevelPage.4" ), INFORMATION ); //$NON-NLS-1$
  }
}