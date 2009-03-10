/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.util.swt;

import java.io.PrintWriter;
import java.io.StringWriter;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.jface.viewers.DefaultTableViewer;

/**
 * A dialog showing a status in full details.
 * 
 * @author Gernot Belger
 */
public class StatusDialog extends AbstractStatusDialog
{

  public StatusDialog( final Shell parentShell, final IStatus status, final String dialogTitle )
  {
    super( parentShell, status, dialogTitle );
  }

  /**
   * @see org.eclipse.jface.dialogs.MessageDialog#createCustomArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createCustomArea( final Composite parent )
  {
    final Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    composite.setLayout( new GridLayout() );

    // TODO show other properties:
    // - time

    final Throwable exception = getStatus().getException();
    if( exception != null )
    {
      final StringWriter sw = new StringWriter();
      exception.printStackTrace( new PrintWriter( sw ) );

      final Group exceptionGroup = new Group( parent, SWT.NONE );
      exceptionGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
      exceptionGroup.setLayout( new GridLayout() );
      exceptionGroup.setText( "Exception" ); //$NON-NLS-1$

      final String excMsg = exception.getLocalizedMessage();
      if( excMsg != null )
      {
        final Label msgLabel = new Label( exceptionGroup, SWT.NONE );
        msgLabel.setLayoutData( new GridData( SWT.FILL, SWT.NONE, true, false ) );
        msgLabel.setText( excMsg );
      }

      final Text stackText = new Text( exceptionGroup, SWT.MULTI | SWT.READ_ONLY );
      stackText.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
      stackText.setText( sw.toString() );
      stackText.setEnabled( true );
    }

    final IStatus[] children = getStatus().getChildren();
    if( children != null && children.length > 0 )
    {
      final DefaultTableViewer tableViewer = new DefaultTableViewer( composite, SWT.BORDER | SWT.FULL_SELECTION );
      final Table table = tableViewer.getTable();
      table.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
      table.setHeaderVisible( true );
      table.setLinesVisible( true );
      StatusLabelProvider.configureTableViewer( tableViewer );
      tableViewer.setContentProvider( new ArrayContentProvider() );
      tableViewer.setInput( children );

      tableViewer.addDoubleClickListener( new IDoubleClickListener()
      {
        public void doubleClick( final DoubleClickEvent event )
        {
          final IStructuredSelection sel = (IStructuredSelection) event.getSelection();
          final IStatus status = (IStatus) sel.getFirstElement();
          if( status != null )
          {
            final StatusDialog dialog = new StatusDialog( getShell(), status, "Details" ); //$NON-NLS-1$
            dialog.open();
          }
        }
      } );

    }

    return composite;
  }

}
