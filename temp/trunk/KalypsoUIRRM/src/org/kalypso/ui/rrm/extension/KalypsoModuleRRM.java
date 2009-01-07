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
  protected static final Image IMG = new Image( null, KalypsoModuleRRM.class.getResourceAsStream( "icons/hydrology_no.gif" ) );

  protected static final Image IMG_HOVER = new Image( null, KalypsoModuleRRM.class.getResourceAsStream( "icons/hydrology_hover.gif" ) );

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
        return IMG;
      }

      @Override
      public String getLabel( )
      {
        return "KalypsoHydrology";
      }

      @Override
      public String getTooltip( )
      {
        return "KalypsoHydrology öffnen";
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

      /**
       * @see org.kalypso.afgui.extension.IKalypsoModuleEnteringPageHandler#hasDemoProjectWizard()
       */
      @Override
      public boolean hasDemoProjectWizard( )
      {
        return false;
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

      /**
       * @see org.kalypso.afgui.extension.IKalypsoModuleEnteringPageHandler#hasImportWizard()
       */
      @Override
      public boolean hasImportWizard( )
      {
        return false;
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
        return 1;
      }
    };
  }

}
