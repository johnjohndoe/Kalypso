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
package org.kalypso.model.wspm.tuhh.ui.extension;

import java.io.File;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.wizard.IWizard;
import org.kalypso.afgui.wizards.INewProjectWizard;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.wizards.DemoProjectWizard;
import org.kalypso.model.wspm.tuhh.ui.wizards.NewProjectImportWspwinWizard;
import org.kalypso.model.wspm.tuhh.ui.wizards.NewProjectWizard;
import org.kalypso.project.database.client.core.model.interfaces.ILocalProject;
import org.kalypso.project.database.client.extension.IKalypsoModule;
import org.kalypso.project.database.client.extension.database.IProjectDatabaseFilter;
import org.kalypso.project.database.client.extension.database.IProjectHandler;
import org.kalypso.project.database.client.extension.pages.module.AbstractKalypsoModulePage;
import org.kalypso.project.database.client.extension.project.IKalypsoModuleProjectOpenAction;

/**
 * @author kuch
 */
public class KalypsoWspmModulePage extends AbstractKalypsoModulePage
{
  public KalypsoWspmModulePage( final IKalypsoModule module )
  {
    super( module );
  }

  protected static boolean INFO_PAGE_EXTRACTED = false;



  @Override
  public String getHeader( )
  {
    return "KalypsoWSPM"; //$NON-NLS-1$
  }

  @Override
  public IProjectDatabaseFilter getDatabaseFilter( )
  {
    return new IProjectDatabaseFilter()
    {
      @Override
      public boolean select( final IProjectHandler handler )
      {
        if( handler instanceof ILocalProject )
        {
          final ILocalProject local = (ILocalProject) handler;
          final IProject project = local.getProject();
          final IFile file = project.getFile( "WSPM.gmv" ); //$NON-NLS-1$

          return file.exists();
        }

        return false;
      }
    };
  }

  @Override
  public URL getInfoURL( ) throws MalformedURLException
  {

    if( !INFO_PAGE_EXTRACTED )
    {
      try
      {
        /* info page of plugin */
        final InputStream zipStream = getClass().getResourceAsStream( "infoPage.zip" ); //$NON-NLS-1$
        try
        {
          final IPath stateLocation = KalypsoModelWspmTuhhUIPlugin.getDefault().getStateLocation();
          final File targetDir = new File( stateLocation.toFile(), "infoPage" ); //$NON-NLS-1$
          // final boolean mkdir = dir.mkdir();

          ZipUtilities.unzip( zipStream, targetDir );

          INFO_PAGE_EXTRACTED = true;
        }
        finally
        {
          zipStream.close();
        }
      }
      catch( final Exception e )
      {
        KalypsoModelWspmTuhhUIPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
      }

    }

    final IPath stateLocation = KalypsoModelWspmTuhhUIPlugin.getDefault().getStateLocation();
    final URL baseUrl = stateLocation.toFile().toURI().toURL();

    final URL url = new URL( baseUrl, "infoPage/index.html" ); //$NON-NLS-1$
    return url;
  }

  @Override
  public INewProjectWizard getDemoProjectWizard( )
  {
    return new DemoProjectWizard();
  }

  /**
   * @see org.kalypso.afgui.extension.IKalypsoModuleEnteringPageHandler#hasDemoProjectWizard()
   */
  @Override
  public boolean hasDemoProjectWizard( )
  {
    return true;
  }

  @Override
  public INewProjectWizard getProjectWizard( )
  {
    return new NewProjectWizard();
  }

  /**
   * @see org.kalypso.afgui.extension.IKalypsoModuleEnteringPageHandler#hasImportWizard()
   */
  @Override
  public boolean hasImportWizard( )
  {
    return true;
  }

  @Override
  public IWizard getImportWizard( )
  {
    return new NewProjectImportWspwinWizard();
  }

  @Override
  public String getImportWizardLabel( )
  {
    return Messages.getString( "org.kalypso.model.wspm.tuhh.ui.extension.KalypsoWspmTuhhModule.9" ); //$NON-NLS-1$
  }

  @Override
  public Integer getPriority( )
  {
    return 2;
  }

  @Override
  public IKalypsoModuleProjectOpenAction getProjectOpenAction( )
  {
    return new WspmOpenAction();
  }


}
