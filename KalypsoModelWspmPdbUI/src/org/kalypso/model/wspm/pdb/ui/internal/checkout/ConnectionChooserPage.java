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
package org.kalypso.model.wspm.pdb.ui.internal.checkout;

import java.util.List;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.databinding.viewers.IViewerObservableValue;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.ImageHyperlink;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.databinding.validation.NotNullValidator;
import org.kalypso.contribs.eclipse.jface.action.ActionHyperlink;
import org.kalypso.model.wspm.pdb.connect.IPdbSettings;
import org.kalypso.model.wspm.pdb.ui.internal.content.ConfigureConnectionsAction;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.ui.internal.preferences.PdbSettingsViewer;
import org.kalypso.model.wspm.pdb.ui.internal.wspm.ConnectionChooserData;

/**
 * @author Gernot Belger
 */
public class ConnectionChooserPage extends WizardPage
{
  private final ConnectionChooserData m_data;

  private DatabindingWizardPage m_binding;

  private PdbSettingsViewer m_settingsViewer;

  public ConnectionChooserPage( final String pageName, final ConnectionChooserData data )
  {
    super( pageName );

    m_data = data;

    setTitle( Messages.getString( "ConnectionChooserPage.0" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "ConnectionChooserPage.1" ) ); //$NON-NLS-1$
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    setControl( panel );
    GridLayoutFactory.swtDefaults().applyTo( panel );

    m_binding = new DatabindingWizardPage( this, null );

    createSettingsChooser( panel ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    createActions( panel ).setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
  }

  private Control createSettingsChooser( final Composite panel )
  {
    m_settingsViewer = new PdbSettingsViewer();
    final TableViewer viewer = m_settingsViewer.createViewer( panel );

    final List<IPdbSettings> input = m_settingsViewer.getInput();
    if( input.size() > 0 )
      m_data.setSettings( input.get( 0 ) );

    final IViewerObservableValue target = ViewersObservables.observeSinglePostSelection( viewer );
    final IObservableValue model = BeansObservables.observeValue( m_data, ConnectionChooserData.PROPERTY_SETTINGS );
    final DataBinder dataBinder = new DataBinder( target, model );
    dataBinder.addTargetAfterGetValidator( new NotNullValidator<>( IPdbSettings.class, IStatus.ERROR, Messages.getString( "ConnectionChooserPage.2" ) ) ); //$NON-NLS-1$
    m_binding.bindValue( dataBinder );

    return viewer.getControl();
  }

  private ImageHyperlink createActions( final Composite parent )
  {
    final ConfigureConnectionsAction action = new ConfigureConnectionsAction()
    {
      @Override
      protected void updateViewer( )
      {
        updateControl();
      }
    };

    return ActionHyperlink.createHyperlink( null, parent, SWT.PUSH, action );
  }

  protected void updateControl( )
  {
    m_settingsViewer.reset();
  }
}