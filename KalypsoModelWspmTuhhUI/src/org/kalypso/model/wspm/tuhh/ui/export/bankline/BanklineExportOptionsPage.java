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
package org.kalypso.model.wspm.tuhh.ui.export.bankline;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.databinding.viewers.IViewerObservableValue;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class BanklineExportOptionsPage extends WizardPage
{
  private DatabindingWizardPage m_binding;

  private final BanklineExportData m_data;

  protected BanklineExportOptionsPage( final String pageName, final BanklineExportData data )
  {
    super( pageName );

    m_data = data;

    setTitle( Messages.getString("BanklineExportOptionsPage_0") ); //$NON-NLS-1$
    setDescription( Messages.getString("BanklineExportOptionsPage_1") ); //$NON-NLS-1$
  }

  @Override
  public void createControl( final Composite parent )
  {
    m_binding = new DatabindingWizardPage( this, null );

    final Composite panel = new Composite( parent, SWT.NONE );
    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( panel );

    createMarkerChooser( panel );
    createDensifyControls( panel );

    // TODO geometry type: lines or polygon

    setControl( panel );
  }

  private void createMarkerChooser( final Composite parent )
  {
    final Label label = new Label( parent, SWT.NONE );
    label.setText( Messages.getString("BanklineExportOptionsPage_2") ); //$NON-NLS-1$

    final ComboViewer markerCombo = new ComboViewer( parent, SWT.READ_ONLY | SWT.DROP_DOWN );
    markerCombo.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    markerCombo.setContentProvider( new ArrayContentProvider() );
    markerCombo.setLabelProvider( new LabelProvider() );
    markerCombo.setInput( m_data.getAvailableMarkerChooser() );

    /* binding */
    final IViewerObservableValue target = ViewersObservables.observeSinglePostSelection( markerCombo );
    final IObservableValue model = BeansObservables.observeValue( m_data, BanklineExportData.PROPERTY_MARKER_CHOOSER );

    m_binding.bindValue( target, model );
  }

  private void createDensifyControls( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( panel );

    final Button checkbox = new Button( panel, SWT.CHECK );
    checkbox.setText( Messages.getString("BanklineExportOptionsPage.0") ); //$NON-NLS-1$
    checkbox.setToolTipText( Messages.getString("BanklineExportOptionsPage.1") ); //$NON-NLS-1$

    final Text densifyField = new Text( panel, SWT.BORDER | SWT.RIGHT );
    densifyField.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    densifyField.setText( Messages.getString("BanklineExportOptionsPage.2") ); //$NON-NLS-1$

    /* binding */
    final ISWTObservableValue targetCheckbox = SWTObservables.observeSelection( checkbox );
    final ISWTObservableValue targetFieldEnablement = SWTObservables.observeEnabled( densifyField );
    final ISWTObservableValue targetFieldValue = SWTObservables.observeText( densifyField, SWT.Modify );

    final IObservableValue modelDensifyEnabled = BeansObservables.observeValue( m_data, BanklineExportData.PROPERTY_DENSIFY_ENABLED );
    final IObservableValue modelDensifyValue = BeansObservables.observeValue( m_data, BanklineExportData.PROPERTY_DENSIFY_DISTANCE );

    m_binding.bindValue( targetCheckbox, modelDensifyEnabled );
    m_binding.bindValue( targetFieldEnablement, modelDensifyEnabled );
    m_binding.bindValue( targetFieldValue, modelDensifyValue );
  }
}