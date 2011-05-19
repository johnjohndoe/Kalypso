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

import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.core.status.StatusDialog2;
import org.kalypso.model.wspm.pdb.gaf.ImportGafData;

/**
 * @author Gernot Belger
 */
public class ImportGafResultDialog extends StatusDialog2
{
  private final ImportGafData m_data;

  public ImportGafResultDialog( final Shell parentShell, final IStatus status, final ImportGafData data )
  {
    super( parentShell, status, "GAF Import" );

    m_data = data;
  }

  @Override
  protected Control createCustomArea( final Composite parent )
  {
    final Composite customArea = (Composite) super.createCustomArea( parent );

    final DataBindingContext context = new DataBindingContext();

    final Button showLogButton = new Button( customArea, SWT.TOGGLE );
    showLogButton.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    showLogButton.setText( "Open Log File" );

    final IObservableValue target = SWTObservables.observeSelection( showLogButton );
    final IObservableValue model = BeansObservables.observeValue( m_data, ImportGafData.PROPERTY_OPEN_LOG );
    context.bindValue( target, model );

    return customArea;
  }
}