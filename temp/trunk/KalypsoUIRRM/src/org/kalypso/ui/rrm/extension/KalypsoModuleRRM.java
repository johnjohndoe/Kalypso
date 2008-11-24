package org.kalypso.ui.rrm.extension;

import java.io.File;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;

import kalypsoUIRRM.KalypsoUIRRMPlugin;

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
import org.kalypso.ui.rrm.wizards.NewNAAsciiProjectWizard;

public class KalypsoModuleRRM implements IKalypsoModule
{
  protected static final Image IMG_MODULE = new Image( null, KalypsoModuleRRM.class.getResourceAsStream( "icons/button_na.gif" ) );

  protected static boolean INFO_PAGE_EXTRACTED = false;

  public KalypsoModuleRRM( )
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
        return IMG_MODULE;
      }

      @Override
      public String getLabel( )
      {
        return "KalypsoHydrology";
      }

      @Override
      public String getTooltip( )
      {
        return "KalypsoHydrology �ffnen";
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
            return false;
          }
        };
      }

      @Override
      public String getHeader( )
      {
        return "KalypsoHydrology";
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
              final IPath stateLocation = KalypsoUIRRMPlugin.getDefault().getStateLocation();
              final File targetDir = new File( stateLocation.toFile(), "infoPage" );
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
            KalypsoUIRRMPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
          }

        }

        final IPath stateLocation = KalypsoUIRRMPlugin.getDefault().getStateLocation();
        final URL baseUrl = stateLocation.toFile().toURI().toURL();

        final URL url = new URL( baseUrl, "infoPage/index.html" );
        return url;
      }

      @Override
      public INewProjectWizard getDemoProjectWizard( )
      {
        return null;
      }

      @Override
      public INewProjectWizard getProjectWizard( )
      {
        return new NewNAAsciiProjectWizard();
      }

      @Override
      public String getRemoteCommitType( )
      {
        return "KalypsRrmModel";
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
    };
  }

}
