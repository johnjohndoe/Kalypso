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
package org.kalypso.ui.wizards.differences;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.ui.wizards.results.IResultControl;

/**
 * @author Gernot Belger
 */
class GenerateDifferenceControl implements IResultControl
{
  private final DifferenceResultData m_data;

  public GenerateDifferenceControl( final DifferenceResultData data )
  {
    m_data = data;
  }

  @Override
  public Control createControl( final IDataBinding binding, final Composite parent )
  {
    final Group panel = new Group( parent, SWT.NONE );
    panel.setText( Messages.getString("GenerateDifferenceControl_0") ); //$NON-NLS-1$

    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( panel );

    final Label label = new Label( panel, SWT.NONE );
    label.setText( GenerateDifferenceResultTinOperation.STR_PREFIX_DIFFFERENCES + " - " ); //$NON-NLS-1$

    final Text nameField = new Text( panel, SWT.BORDER | SWT.SINGLE );
    nameField.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    /* binding */
    final ISWTObservableValue targetName = SWTObservables.observeText( nameField, SWT.Modify );
    final IObservableValue modelName = BeansObservables.observeValue( m_data, DifferenceResultData.PROPERTY_DESTINATION_NAME );
    binding.bindValue( targetName, modelName );

    return panel;
  }
}