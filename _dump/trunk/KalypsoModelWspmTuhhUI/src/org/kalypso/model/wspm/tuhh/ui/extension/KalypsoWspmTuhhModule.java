package org.kalypso.model.wspm.tuhh.ui.extension;

import java.io.File;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.swt.graphics.Image;
import org.kalypso.afgui.extension.IKalypsoModule;
import org.kalypso.afgui.extension.IKalypsoModuleEnteringPageHandler;
import org.kalypso.afgui.extension.IKalypsoModuleWelcomePageHandler;
import org.kalypso.afgui.extension.IKalypsoProjectOpenAction;
import org.kalypso.afgui.extension.INewProjectWizard;
import org.kalypso.afgui.extension.IProjectDatabaseFilter;
import org.kalypso.afgui.extension.IProjectHandler;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.wizards.DemoProjectWizard;
import org.kalypso.model.wspm.tuhh.ui.wizards.NewProjectImportWspwinWizard;
import org.kalypso.model.wspm.tuhh.ui.wizards.NewProjectWizard;

public class KalypsoWspmTuhhModule implements IKalypsoModule
{

  protected static final Image IMG = new Image( null, KalypsoWspmTuhhModule.class.getResourceAsStream( "icons/wspm_no.gif" ) );

  protected static final Image IMG_HOVER = new Image( null, KalypsoWspmTuhhModule.class.getResourceAsStream( "icons/wspm_hover.gif" ) );

  protected static boolean INFO_PAGE_EXTRACTED = false;

  public KalypsoWspmTuhhModule( )
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
        return "KalypsoWSPM"; //$NON-NLS-1$
      }

      @Override
      public String getTooltip( )
      {
        return Messages.getString( "org.kalypso.model.wspm.tuhh.ui.extension.KalypsoWspmTuhhModule.2" ); //$NON-NLS-1$
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
            if( handler.isLocal() )
            {
              final IProject project = handler.getProject();
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

      @Override
      public String getRemoteCommitType( )
      {
        return "KalypsoWspmModel"; //$NON-NLS-1$
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
      public IKalypsoProjectOpenAction getProjectOpenAction( )
      {
        return new WspmOpenAction();
      }

    };
  }
}
