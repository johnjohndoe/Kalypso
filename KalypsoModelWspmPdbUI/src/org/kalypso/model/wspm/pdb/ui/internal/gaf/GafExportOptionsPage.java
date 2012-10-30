/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.pdb.ui.internal.gaf;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.viewers.IViewerObservableValue;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.databinding.swt.FileAndHistoryData;
import org.kalypso.commons.databinding.swt.FileBinding;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserDelegateSave;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * Page for options for gaf export.
 *
 * @author Gernot Belger
 */
class GafExportOptionsPage extends WizardPage
{
  private static final String FILTER_LABEL = Messages.getString("GafExportOptionsPage_0"); //$NON-NLS-1$

  private static final String EXTENSION = "gaf"; //$NON-NLS-1$

  private final GafOptionsData m_data;

  private DatabindingWizardPage m_binding;

  GafExportOptionsPage( final String pageName, final GafOptionsData data )
  {
    super( pageName );

    m_data = data;

    setTitle( Messages.getString("GafExportOptionsPage_1") ); //$NON-NLS-1$
    setDescription( Messages.getString("GafExportOptionsPage_2") ); //$NON-NLS-1$
  }

  @Override
  public void dispose( )
  {
    m_binding.dispose();

    super.dispose();
  }

  @Override
  public void createControl( final Composite parent )
  {
    m_binding = new DatabindingWizardPage( this, null );

    final Composite panel = new Composite( parent, SWT.NONE );
    setControl( panel );

    GridLayoutFactory.fillDefaults().numColumns( 3 ).applyTo( panel );

    createExportFileControls( panel );
    createHykModeControls( panel );

    setErrorMessage( null );
    setPageComplete( false );
  }

  private void createExportFileControls( final Composite parent )
  {
    final Label label = new Label( parent, SWT.NONE );
    label.setText( Messages.getString("GafExportOptionsPage_3") ); //$NON-NLS-1$

    final FileChooserDelegateSave delegateSave = new FileChooserDelegateSave();
    delegateSave.addFilter( FILTER_LABEL, "*." + EXTENSION ); //$NON-NLS-1$

    final IObservableValue modelFile = BeansObservables.observeValue( m_data.getGafFile(), FileAndHistoryData.PROPERTY_FILE );
    final IObservableValue modelHistory = BeansObservables.observeValue( m_data.getGafFile(), FileAndHistoryData.PROPERTY_HISTORY );

    final FileBinding fileBinding = new FileBinding( m_binding, modelFile, delegateSave );

    final Control fileField = fileBinding.createFileFieldWithHistory( parent, modelHistory );
    fileField.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    fileBinding.createFileSearchButton( parent, fileField );
  }

  private void createHykModeControls( final Composite parent )
  {
    final Label label = new Label( parent, SWT.NONE );
    label.setText( Messages.getString("GafExportOptionsPage_4") ); //$NON-NLS-1$

    final ComboViewer chooser = new ComboViewer( parent, SWT.READ_ONLY | SWT.DROP_DOWN );
    chooser.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    chooser.setContentProvider( new ArrayContentProvider() );
    chooser.setLabelProvider( new LabelProvider() );
    chooser.setInput( HykExportMode.values() );

    /* binding */
    final IViewerObservableValue targetSelection = ViewersObservables.observeSinglePostSelection( chooser );
    final IObservableValue modelSelection = BeansObservables.observeValue( m_data, GafOptionsData.PROPERTY_HYK_EXPORT_MODE );

    m_binding.bindValue( targetSelection, modelSelection );
  }
}