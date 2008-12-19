package org.kalypso.kalypso1d2d.extension;

import java.io.File;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.swt.graphics.Image;
import org.kalypso.afgui.extension.IKalypsoModule;
import org.kalypso.afgui.extension.IKalypsoModuleEnteringPageHandler;
import org.kalypso.afgui.extension.IKalypsoModuleWelcomePageHandler;
import org.kalypso.afgui.extension.INewProjectWizard;
import org.kalypso.afgui.extension.IProjectDatabaseFilter;
import org.kalypso.afgui.extension.IProjectHandler;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypso1d2d.i18n.Messages;
import org.kalypso.kalypso1d2d.pjt.Kalypso1D2DDemoProjectWizard;
import org.kalypso.kalypso1d2d.pjt.Kalypso1D2DNewProjectWizard;
import org.kalypso.kalypso1d2d.pjt.Kalypso1D2DProjectNature;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;

public class Kalypso1d2dModule implements IKalypsoModule
{
  protected static final Image IMG = new Image( null, Kalypso1d2dModule.class.getResourceAsStream( "icons/1d_2d_no.gif" ) ); //$NON-NLS-1$

  protected static final Image IMG_HOVER = new Image( null, Kalypso1d2dModule.class.getResourceAsStream( "icons/1d_2d_hover.gif" ) ); //$NON-NLS-1$

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
        return IMG;
      }

      @Override
      public String getLabel( )
      {
        return "Kalypso1D2D"; //$NON-NLS-1$
      }

      @Override
      public String getTooltip( )
      {
        return Messages.getString("org.kalypso.kalypso1d2d.extension.Kalypso1d2dModule.3"); //$NON-NLS-1$
      }

      @Override
      public Image getHoverIcon( )
      {
        return IMG_HOVER;
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
          public boolean select( final IProjectHandler handler )
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
        return "Kalypso1D2D"; //$NON-NLS-1$
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
              final IPath stateLocation = Kalypso1d2dProjectPlugin.getDefault().getStateLocation();
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
            Kalypso1d2dProjectPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
          }
        }

        final IPath stateLocation = Kalypso1d2dProjectPlugin.getDefault().getStateLocation();
        final URL baseUrl = stateLocation.toFile().toURI().toURL();

        final URL url = new URL( baseUrl, "infoPage/index.html" ); //$NON-NLS-1$
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
        return "Kalypso1d2dModel"; //$NON-NLS-1$
      }

      @Override
      public IWizard getImportWizard( )
      {
        return null;
      }

      @Override
      public String getImportWizardLabel( )
      {
        return null;
      }

      @Override
      public Integer getPriority( )
      {
        return 3;
      }
    };
  }
}
