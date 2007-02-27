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
package org.kalypso.ui.wizards.imports.baseMap;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.wizard.image.ImportImageWizardPage;
import org.kalypso.ui.wizards.imports.INewWizardKalypsoImport;
import org.kalypso.ui.wizards.imports.Messages;

/**
 * @author Madanagopal
 */
public class ImportBaseMapWizard extends Wizard implements INewWizardKalypsoImport
{
  private IStructuredSelection initialSelection;

  private BaseMapMainPage mPage;

  private String m_scenarioFolder;

  ProgressMonitorDialog monitor;

  /**
   * Construct a new instance and initialize the dialog settings for this instance.
   */
  public ImportBaseMapWizard( )
  {
    super();
  }

  /**
   * @param workbench
   *          the current workbench
   * @param selection
   *          the current object selection
   */
  public void init( IWorkbench workbench, IStructuredSelection selection )
  {
    initialSelection = selection;
    setNeedsProgressMonitor( true );
    setWindowTitle( Messages.getString( "BaseMapWizard.0" ) );
  }

  /**
   * @see org.kalypso.ui.wizards.imports.INewWizardKalypsoImport#initModelProperties(java.util.HashMap)
   */
  public void initModelProperties( HashMap<String, Object> map )
  {
    m_scenarioFolder = (String) map.get( "ScenarioFolder" );
  }

  @Override
  public void addPages( )
  {
    mPage = new BaseMapMainPage();
    addPage( mPage );
    mPage.init( initialSelection );
  }

  /**
   * This method is called by the wizard framework when the user presses the Finish button.
   */
  @Override
  public boolean performFinish( )
  {
    final String dstFilePath = ResourcesPlugin.getWorkspace().getRoot().getLocation().toOSString() + File.separator + m_scenarioFolder + File.separator;
    final File srcFileTif = new File( mPage.getSourceLocation().toOSString() );
    final File srcFileTfw = new File( mPage.getSourceLocation().removeFileExtension().addFileExtension( "tfw" ).toOSString() );
    final File dstFileTif = new File( dstFilePath + mPage.getSourceLocation().lastSegment() );
    final File dstFileTfw = new File( dstFilePath + mPage.getSourceLocation().removeFileExtension().addFileExtension( "tfw" ).lastSegment() );

    try
    {
      getContainer().run( true, true, new IRunnableWithProgress()
      {
        public void run( final IProgressMonitor monitor )
        {
          copy( srcFileTif, dstFileTif, monitor );
          copy( srcFileTfw, dstFileTfw, monitor );
        }

        private boolean copy( final File src, final File dst, final IProgressMonitor monitor2 )
        {
          InputStream in = null;
          OutputStream out = null;
          try
          {
            in = new FileInputStream( src );
            out = new FileOutputStream( dst );

            final byte[] buf = new byte[1024];
            final int lens = ((int) src.length() / 1024 + 1);
            monitor2.beginTask( "Copying..", lens );
            int len;
            while( (len = in.read( buf )) > 0 )
            {
              monitor2.worked( 1 );
              out.write( buf, 0, len );
            }
            monitor2.done();
            return true;
          }
          catch( final Exception e )
          {
            e.printStackTrace();
            return false;
          }
          finally
          {
            IOUtils.closeQuietly( in );
            IOUtils.closeQuietly( out );
          }
        }
      } );
    }
    catch( final InvocationTargetException e1 )
    {
      e1.printStackTrace();
    }
    catch( final InterruptedException e1 )
    {
      e1.printStackTrace();
      if( dstFileTfw.exists() )
      {
        dstFileTfw.delete();
      }
      if( dstFileTif.exists() )
      {
        dstFileTif.delete();
      }
    }

    if( !initialSelection.isEmpty() )
    {
      try
      {
        final IResource resource = (IResource) initialSelection.getFirstElement();
        resource.getProject().refreshLocal( IResource.DEPTH_INFINITE, null );
      }
      catch( final CoreException e )
      {
        e.printStackTrace();
      }
    }

    return true;
  }
}
