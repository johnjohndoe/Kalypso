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
package org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.imports;

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
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.imports.ImportWaterLevelsData.ImportMethod;

/**
 * @author Gernot Belger
 */
public class ImportWaterlevelsOptionsPage extends WizardPage
{
  private final ImportWaterLevelsData m_data;

  private DatabindingWizardPage m_binding;

  protected ImportWaterlevelsOptionsPage( final String pageName, final ImportWaterLevelsData data )
  {
    super( pageName );

    m_data = data;

    setTitle( "Import Optionen" );
    setDescription( "Bitte wählen Sie auf dieser Seite zusätzliche Optionen zum Wasserspiegelimport." );
  }

  @Override
  public void createControl( final Composite parent )
  {
    m_binding = new DatabindingWizardPage( this, null );

    final Group panel = new Group( parent, SWT.NONE );
    setControl( panel );
    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( panel );

    panel.setText( "Optionen" );

    createImportTypeControl( panel );
  }

  private void createImportTypeControl( final Composite panel )
  {
    final Label label = new Label( panel, SWT.NONE );
    label.setText( "Importmethode" );

    final ComboViewer typeChooser = new ComboViewer( panel, SWT.DROP_DOWN | SWT.READ_ONLY );
    typeChooser.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    typeChooser.setContentProvider( new ArrayContentProvider() );
    typeChooser.setLabelProvider( new LabelProvider() );
    typeChooser.setInput( ImportMethod.values() );

    final IViewerObservableValue targetSelection = ViewersObservables.observeSinglePostSelection( typeChooser );
    final IObservableValue modelSelection = BeansObservables.observeValue( m_data, ImportWaterLevelsData.PROPERTY_IMPORT_METHOD );
    m_binding.bindValue( targetSelection, modelSelection );
  }
}