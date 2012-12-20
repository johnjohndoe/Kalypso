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
package org.kalypso.kalypso1d2d.internal.importNet;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.databinding.swt.FileAndHistoryData;
import org.kalypso.commons.databinding.swt.FileBinding;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserDelegateOpen;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.transformation.ui.CRSSelectionPanel;

/**
 * @author Gernot Belger
 */
public class Import2dImportPage extends WizardPage
{
  private final Import2dImportData m_data;

  private DatabindingWizardPage m_binding;

  private final IImport2dImportOperation[] m_operations;

  public Import2dImportPage( final String pageName, final Import2dImportData data, final IImport2dImportOperation[] operations )
  {
    super( pageName );

    m_data = data;
    m_operations = operations;

    setTitle( Messages.getString("Import2dImportPage_0") ); //$NON-NLS-1$
    setDescription( Messages.getString("Import2dImportPage_1") ); //$NON-NLS-1$
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    setControl( panel );

    GridLayoutFactory.swtDefaults().numColumns( 3 ).applyTo( panel );

    m_binding = new DatabindingWizardPage( this, null );

    createFileControls( panel );
    createSrsControls( panel );

    setErrorMessage( null );
  }

  private void createFileControls( final Composite parent )
  {
    new Label( parent, SWT.NONE ).setText( Messages.getString("Import2dImportPage_2") ); //$NON-NLS-1$

    final IObservableValue modelFile = BeansObservables.observeValue( m_data, FileAndHistoryData.PROPERTY_FILE );
    final IObservableValue modelHistory = BeansObservables.observeValue( m_data, FileAndHistoryData.PROPERTY_HISTORY );

    final FileChooserDelegateOpen delegate = new FileChooserDelegateOpen();
    for( final IImport2dImportOperation operation : m_operations )
      delegate.addFilter( operation.getFilterName(), operation.getFilterExtension() );

    final FileBinding fileBinding = new FileBinding( m_binding, modelFile, delegate );
    final Control fileField = fileBinding.createFileFieldWithHistory( parent, modelHistory );
    fileField.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    fileBinding.createFileSearchButton( parent, fileField );
  }

  private void createSrsControls( final Composite parent )
  {
    final CRSSelectionPanel srsPanel = new CRSSelectionPanel( parent, SWT.NONE );
    srsPanel.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false, 3, 1 ) );

    final IObservableValue target = srsPanel.observe();
    final IObservableValue modle = BeansObservables.observeValue( m_data, Import2dImportData.PROPERTY_SRS );

    m_binding.bindValue( target, modle );
  }
}