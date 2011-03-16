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

import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.contribs.eclipse.jface.wizard.IFileChooserDelegate;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class PlotterExportPage extends ExportPrfFileChooserPage
{
  private static final String SETTINGS_DO_PRINT = "directPrint"; //$NON-NLS-1$

  private boolean m_doPrint;

  public PlotterExportPage( final IFileChooserDelegate fileChooser )
  {
    super( fileChooser );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.export.ExportPrfFileChooserPage#createPageContent(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected void createPageContent( final Composite parent )
  {
    super.createPageContent( parent );

    createOptionsGroup( parent );
  }

  private void createOptionsGroup( final Composite parent )
  {
    final IDialogSettings dialogSettings = getDialogSettings();
    if( dialogSettings != null )
      m_doPrint = dialogSettings.getBoolean( SETTINGS_DO_PRINT );

    final Group group = new Group( parent, SWT.NONE );
    group.setLayout( new GridLayout() );
    group.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    group.setText( Messages.getString("PlotterExportPage_0") ); //$NON-NLS-1$

    final Button button = new Button( group, SWT.CHECK );
    button.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    button.setText( Messages.getString("PlotterExportPage_1") ); //$NON-NLS-1$
    button.setToolTipText( Messages.getString("PlotterExportPage_2") ); //$NON-NLS-1$
    button.setSelection( m_doPrint );
    button.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handlePrintSelected( button.getSelection() );
      }
    } );
  }

  protected void handlePrintSelected( final boolean selection )
  {
    m_doPrint = selection;
  }

  public boolean getDoPrint( )
  {
    return m_doPrint;
  }
}
