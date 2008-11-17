package org.kalypso.kalypso1d2d.extension;

import java.io.File;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.swt.graphics.Image;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypso1d2d.pjt.Kalypso1D2DDemoProjectWizard;
import org.kalypso.kalypso1d2d.pjt.Kalypso1D2DNewProjectWizard;
import org.kalypso.kalypso1d2d.pjt.Kalypso1D2DProjectNature;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypsosimulationmodel.extension.IKalypsoModule;
import org.kalypso.kalypsosimulationmodel.extension.IKalypsoModuleEnteringPageHandler;
import org.kalypso.kalypsosimulationmodel.extension.IKalypsoModuleWelcomePageHandler;
import org.kalypso.kalypsosimulationmodel.extension.INewProjectWizard;
import org.kalypso.project.database.common.interfaces.IProjectDatabaseFilter;
import org.kalypso.project.database.common.model.ProjectHandler;

public class Kalypso1d2dModule implements IKalypsoModule
{
  protected static final Image IMG_1D_2D_MODULE = new Image( null, Kalypso1d2dModule.class.getResourceAsStream( "icons/button_1d2d.gif" ) );

  protected static boolean INFO_PAGE_EXTRACTED = false;

  public Kalypso1d2dModule( )
  {
  }

  @Override
  public IKalypsoModuleWelcomePageHandler getWelcomePageHandler( )
  {
    return new IKalypsoModuleWelcomePageHandler()
    {

      @Override
      public Image getIcon( )
      {
        return IMG_1D_2D_MODULE;
      }

      @Override
      public String getLabel( )
      {
        return "Kalypso 1D/2D";
      }

      @Override
      public String getTooltip( )
      {
        return "Kalypso 1D/2D öffnen";
      }
    };
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.extension.IKalypsoModule#getModuleEnteringPage()
   */
  @Override
  public IKalypsoModuleEnteringPageHandler getModuleEnteringPage( )
  {
    return new IKalypsoModuleEnteringPageHandler()
    {
      @Override
      public IProjectDatabaseFilter getDatabaseFilter( )
      {
        return new IProjectDatabaseFilter()
        {
          @Override
          public boolean select( final ProjectHandler handler )
          {
            if( handler.isLocal() )
            {
              try
              {
                final IProject project = handler.getProject();
                return Kalypso1D2DProjectNature.isOfThisNature( project );
              }
              catch( final CoreException e )
              {
                Kalypso1d2dProjectPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
              }
            }
            // TODO handle remote projects

            return false;
          }
        };
      }

      @Override
      public String getHeader( )
      {
        return "Kalypso 1D/2D";
      }

      @Override
      public URL getInfoURL( ) throws MalformedURLException
      {
        if( !INFO_PAGE_EXTRACTED )
        {
          try
          {
            /* info page of plugin */
            final InputStream zipStream = getClass().getResourceAsStream( "infoPage.zip" );
            try
            {
              final IPath stateLocation = Kalypso1d2dProjectPlugin.getDefault().getStateLocation();
              final File targetDir = new File( stateLocation.toFile(), "infoPage" );
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
            Kalypso1d2dProjectPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
          }
        }

        final IPath stateLocation = Kalypso1d2dProjectPlugin.getDefault().getStateLocation();
        final URL baseUrl = stateLocation.toFile().toURI().toURL();

        final URL url = new URL( baseUrl, "infoPage/index.html" );
        return url;
      }

      @Override
      public INewProjectWizard getDemoProjectWizard( )
      {
        return new Kalypso1D2DDemoProjectWizard();
      }

      @Override
      public INewProjectWizard getProjectWizard( )
      {
        return new Kalypso1D2DNewProjectWizard();
      }

      @Override
      public String getRemoteCommitType( )
      {
        return "Kalypso1d2dModel";
      }
    };
  }
}
