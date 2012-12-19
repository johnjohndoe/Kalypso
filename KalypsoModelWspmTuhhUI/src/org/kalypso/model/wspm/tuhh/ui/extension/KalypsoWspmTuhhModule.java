package org.kalypso.model.wspm.tuhh.ui.extension;

import java.net.URL;
import java.util.Collection;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.action.IAction;
import org.kalypso.model.wspm.tuhh.ui.IWspmTuhhUIConstants;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.wizards.DemoProjectWizard;
import org.kalypso.model.wspm.tuhh.ui.wizards.NewProjectImportWspwinWizard;
import org.kalypso.model.wspm.tuhh.ui.wizards.NewWspmProjectWizard;
import org.kalypso.module.AbstractKalypsoModule;
import org.kalypso.module.IKalypsoModuleProjectOpenAction;
import org.kalypso.module.IKalypsoModuleWelcomePageFrame;
import org.kalypso.module.ISetAsBaseScenarioHandler;
import org.kalypso.module.welcome.INewProjectWizard;
import org.kalypso.module.welcome.INewProjectWizardProvider;
import org.kalypso.module.welcome.SpecialImportProjectAction;

/**
 * @author Dirk Kuch
 */
public class KalypsoWspmTuhhModule extends AbstractKalypsoModule
{
  public static final String ID = "KalypsoWspmModel"; //$NON-NLS-1$

  // public constructor, needed because of declared extension point and java class loader
  public KalypsoWspmTuhhModule( )
  {
  }

  @Override
  public String getHeader( )
  {
    return "KalypsoWSPM"; //$NON-NLS-1$
  }

  @Override
  public URL getInfoURL( )
  {
    return getInfoURL( getClass(), KalypsoModelWspmTuhhUIPlugin.getDefault() );
  }

  @Override
  public IKalypsoModuleWelcomePageFrame getWelcomePageFrame( )
  {
    return new KalypsoWspmWelcomePageFrame();
  }

  @Override
  public String getId( )
  {
    return ID;
  }

  @Override
  public Integer getPriority( )
  {
    return 2;
  }

  /**
   * @see org.kalypso.project.database.client.extension.AbstractKalypsoModule#getNewProjectWizard()
   */
  @Override
  protected INewProjectWizardProvider getNewProjectWizard( )
  {
    return new INewProjectWizardProvider()
    {
      @Override
      public INewProjectWizard createWizard( )
      {
        return new NewWspmProjectWizard();
      }
    };
  }

  /**
   * @see org.kalypso.project.database.client.extension.AbstractKalypsoModule#getDemoProjectWizard()
   */
  @Override
  protected INewProjectWizardProvider getDemoProjectWizard( )
  {
    return new INewProjectWizardProvider()
    {
      @Override
      public INewProjectWizard createWizard( )
      {
        return new DemoProjectWizard();
      }
    };
  }

  /**
   * @see org.kalypso.project.database.client.extension.AbstractKalypsoModule#addProjectActions(java.util.Collection)
   */
  @Override
  protected void addProjectActions( final Collection<IAction> actions )
  {
    final INewProjectWizardProvider provider = new INewProjectWizardProvider()
    {
      @Override
      public INewProjectWizard createWizard( )
      {
        return new NewProjectImportWspwinWizard();
      }
    };

    final String label = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.extension.KalypsoWspmTuhhModule.9" ); //$NON-NLS-1$
    actions.add( new SpecialImportProjectAction( label, provider ) );
  }

  @Override
  public IKalypsoModuleProjectOpenAction getProjectOpenAction( )
  {
    return new WspmOpenAction();
  }

  /**
   * @see org.kalypso.module.IKalypsoModule#acceptProject(org.eclipse.core.resources.IProject)
   */
  @Override
  public boolean acceptProject( final IProject project )
  {
    final IFile file = project.getFile( "WSPM.gmv" ); //$NON-NLS-1$
    return file.exists();
  }

  /**
   * @see org.kalypso.module.IKalypsoModule#getNewProjectCategoryId()
   */
  @Override
  public String getNewProjectCategoryId( )
  {
    return IWspmTuhhUIConstants.WSPM_TUHH_PROJECT_TEMPLATE_CATEGORY;
  }

  /**
   * @see org.kalypso.module.IKalypsoModule#getSetAsBaseScenarioHandler()
   */
  @Override
  public ISetAsBaseScenarioHandler getSetAsBaseScenarioHandler( )
  {
    return null;
  }
}