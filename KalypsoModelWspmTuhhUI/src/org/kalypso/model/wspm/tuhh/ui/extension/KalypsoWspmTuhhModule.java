package org.kalypso.model.wspm.tuhh.ui.extension;

import java.net.URL;
import java.util.Collection;

import org.eclipse.jface.action.IAction;
import org.kalypso.afgui.wizards.INewProjectWizard;
import org.kalypso.afgui.wizards.INewProjectWizardProvider;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.wizards.DemoProjectWizard;
import org.kalypso.model.wspm.tuhh.ui.wizards.NewProjectImportWspwinWizard;
import org.kalypso.model.wspm.tuhh.ui.wizards.NewWspmProjectWizard;
import org.kalypso.module.IKalypsoModuleProjectOpenAction;
import org.kalypso.module.IKalypsoModuleWelcomePageFrame;
import org.kalypso.project.database.client.extension.AbstractKalypsoModule;
import org.kalypso.project.database.client.extension.database.IKalypsoModuleDatabaseSettings;
import org.kalypso.project.database.client.ui.composites.SpecialImportProjectAction;

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

  /**
   * @see org.kalypso.project.database.client.extension.IKalypsoModule#getRemoteDatabaseSettings()
   */
  @Override
  public IKalypsoModuleDatabaseSettings getDatabaseSettings( )
  {
    return new KalypsoWspmRemoteDatabaseSettings();
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

}
