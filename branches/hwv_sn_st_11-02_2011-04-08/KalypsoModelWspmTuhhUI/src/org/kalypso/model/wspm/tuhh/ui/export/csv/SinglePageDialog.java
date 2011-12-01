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
package org.kalypso.model.wspm.tuhh.ui.export.csv;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogPage;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

/**
 * A dialog that shows a single {@link IDialogPage}.<br/>
 * Useful to open a dialog on a single {@link org.eclipse.jface.wizard.IWizardPage}, the page must not, however, try to
 * access it's wizard.
 * 
 * @author Gernot Belger
 */
public class SinglePageDialog extends Dialog
{
  private final IDialogPage m_page;

  private Point m_initialSize;

  public SinglePageDialog( final IShellProvider provider, final IDialogPage page )
  {
    super( provider );

    m_page = page;
  }

  public SinglePageDialog( final Shell shell, final IDialogPage page )
  {
    super( shell );

    m_page = page;
  }

  /**
   * Sets an initial size to this dialog. If set to non-<code>null</code>, this size will be used, else, the default
   * behaviour of {@link Dialog} will take place.
   */
  public void setInitialSize( final Point initialSize )
  {
    m_initialSize = initialSize;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#getInitialSize()
   */
  @Override
  protected Point getInitialSize( )
  {
    if( m_initialSize != null )
      return m_initialSize;

    return super.getInitialSize();
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#isResizable()
   */
  @Override
  protected boolean isResizable( )
  {
    return true;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#create()
   */
  @Override
  public void create( )
  {
    super.create();

    final Shell shell = getShell();
    final String title = m_page.getTitle();
    if( title != null )
      shell.setText( title );
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( final Composite parent )
  {
    final Control dialogPanel = super.createDialogArea( parent );

    m_page.createControl( parent );

    final Control control = m_page.getControl();
    control.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    return dialogPanel;
  }
}
