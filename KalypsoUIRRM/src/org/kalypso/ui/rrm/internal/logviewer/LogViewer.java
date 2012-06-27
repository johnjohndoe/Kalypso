/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.rrm.internal.logviewer;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.part.ViewPart;

public class LogViewer extends ViewPart
{
  private final ISelectionListener m_listener = new ISelectionListener()
  {
    @Override
    public void selectionChanged( final IWorkbenchPart sourcepart, final ISelection selection )
    {
      if( sourcepart != LogViewer.this )
      {
        showSelection( selection );
      }
    }
  };

  private LogTableViewer m_logTableViewer;

  private IFile m_fileMain = null;

  protected void showSelection( final ISelection selection )
  {
    final IStructuredSelection structured = (IStructuredSelection) selection;
    final Object object = structured.getFirstElement();
    if( object instanceof IFile )
    {
      final IFile file = (IFile) object;
      final String extension = file.getFileExtension();
      final String name = file.getName();
      if( extension != null && extension.equals( "log" ) || name.equals( "error.txt" ) || name.equals( "error.gml" ) ) //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      {
        m_fileMain = file;
        // redirect into LogTableViewer
        m_logTableViewer.fileSelectionChange( m_fileMain );
      }
    }

  }

  @Override
  public void createPartControl( final Composite parent )
  {
    final SashForm sashForm = new SashForm( parent, SWT.HORIZONTAL );
    m_logTableViewer = new LogTableViewer( this );
    m_logTableViewer.createPartControl( sashForm );

    getSite().getPage().addSelectionListener( m_listener );
  }

  /**
   * Passing the focus request to the viewer's control.
   */
  @Override
  public void setFocus( )
  {
    // logTableViewer.getControl().setFocus();
  }

  @Override
  public void dispose( )
  {
    getSite().getPage().removeSelectionListener( m_listener );

    super.dispose();
  }
}