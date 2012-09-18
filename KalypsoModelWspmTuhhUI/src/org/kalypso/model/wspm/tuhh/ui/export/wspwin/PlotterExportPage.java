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
package org.kalypso.model.wspm.tuhh.ui.export.wspwin;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.databinding.validation.NotNullValidator;
import org.kalypso.commons.databinding.validation.NumberNotNegativeValidator;
import org.kalypso.contribs.eclipse.jface.wizard.IFileChooserDelegate;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class PlotterExportPage extends ExportPrfFileChooserPage
{
  private final PlotterExportData m_data;

  private DatabindingWizardPage m_binding;

  public PlotterExportPage( final IFileChooserDelegate fileChooser, final PlotterExportData data )
  {
    super( fileChooser );

    m_data = data;
  }

  @Override
  protected void createPageContent( final Composite parent )
  {
    super.createPageContent( parent );

    m_binding = new DatabindingWizardPage( this, null );

    createOptionsGroup( parent );
  }

  private void createOptionsGroup( final Composite parent )
  {
    final Group group = new Group( parent, SWT.NONE );
    group.setLayout( new GridLayout( 2, false ) );
    group.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    group.setText( Messages.getString( "PlotterExportPage_0" ) ); //$NON-NLS-1$

    createFields( group );
  }

  private void createFields( final Composite parent )
  {
    /* Print Button */
    final Button button = new Button( parent, SWT.CHECK );
    button.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
    button.setText( Messages.getString( "PlotterExportPage_1" ) ); //$NON-NLS-1$
    button.setToolTipText( Messages.getString( "PlotterExportPage_2" ) ); //$NON-NLS-1$

    final ISWTObservableValue targetPrint = SWTObservables.observeSelection( button );
    final IObservableValue modelPrint = BeansObservables.observeValue( m_data, PlotterExportData.PROPERTY_DO_PRINT );

    m_binding.bindValue( targetPrint, modelPrint );

    /* Sleep */
    new Label( parent, SWT.NONE ).setText( Messages.getString("PlotterExportPage.0") ); //$NON-NLS-1$

    final Text field = new Text( parent, SWT.BORDER | SWT.SINGLE );
    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    field.setToolTipText( Messages.getString("PlotterExportPage.1") ); //$NON-NLS-1$

    final ISWTObservableValue targetSleep = SWTObservables.observeText( field, SWT.Modify );
    final IObservableValue modelSleep = BeansObservables.observeValue( m_data, PlotterExportData.PROPERTY_SLEEP_TIME );
    final DataBinder binder = new DataBinder( targetSleep, modelSleep );
    binder.addTargetAfterConvertValidator( new NotNullValidator<>( Long.class, IStatus.ERROR, Messages.getString( "PlotterExportPage.2" ) ) ); //$NON-NLS-1$
    binder.addTargetAfterConvertValidator( new NumberNotNegativeValidator( IStatus.ERROR ) );
    m_binding.bindValue( binder );

    /* Enable/Disable sleep field */
    final ISWTObservableValue targetEnabled = SWTObservables.observeEnabled( field );
    final IObservableValue modelEnabled = modelPrint;
    m_binding.bindValue( targetEnabled, modelEnabled );
  }
}
