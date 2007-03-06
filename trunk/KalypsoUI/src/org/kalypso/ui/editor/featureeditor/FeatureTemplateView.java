/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ui.editor.featureeditor;

import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Properties;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.util.command.JobExclusiveCommandTarget;

/**
 * @author Stefan Kurzbach
 */
public class FeatureTemplateView extends ViewPart
{
  public static final String ID = "org.kalypso.ui.views.featuretemplateview";

  FeatureTemplateviewer m_templateviewer = new FeatureTemplateviewer( new JobExclusiveCommandTarget( null, null ), 0, 0 );

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    m_templateviewer.createControls( parent, SWT.BORDER );
  }

  public void loadFromTemplate( final IFile file )
  {
    final Display display = m_templateviewer.getControl().getDisplay();
    final UIJob job = new UIJob( display, "Feature Template laden" )
    {
      /**
       * @see org.eclipse.ui.progress.UIJob#runInUIThread(org.eclipse.core.runtime.IProgressMonitor)
       */
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {
        try
        {
          final IFile templateFile = file;
          if( templateFile != null && templateFile.exists() )
          {
            final Reader reader = new InputStreamReader( templateFile.getContents(), templateFile.getCharset() );
            final URL context = ResourceUtilities.createURL( file );
            m_templateviewer.loadInput( reader, context, monitor, new Properties() );
          }
          return Status.OK_STATUS;
        }
        catch( CoreException e )
        {
          e.printStackTrace();
        }
        catch( UnsupportedEncodingException e )
        {
          e.printStackTrace();
        }
        catch( MalformedURLException e )
        {
          // TODO Auto-generated catch block
          e.printStackTrace();
        }
        return Status.CANCEL_STATUS;
      }
    };
    job.schedule();

    // final IStatus status = job.getResult();
    // if( !status.isOK() )
    // {
    // ErrorDialog.openError( display.getActiveShell(), "Feature Template laden", "Fehler beim Laden der Vorlage" +
    // file.getName(), status );
    // m_templateviewer.dispose();
    // }
  }

  @Override
  public void dispose( )
  {
    if( m_templateviewer != null )
      m_templateviewer.dispose();
    super.dispose();
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {
    final Control control = m_templateviewer.getControl();
    if( control != null )
    {
      control.setFocus();
    }
  }

}
