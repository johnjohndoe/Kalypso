package org.kalypso.ui.rrm.extension;

import java.net.URL;
import java.util.Collection;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.action.IAction;
import org.kalypso.afgui.wizards.ProjectConversionWizard;
import org.kalypso.module.AbstractKalypsoModule;
import org.kalypso.module.IKalypsoModuleProjectOpenAction;
import org.kalypso.module.IKalypsoModuleWelcomePageFrame;
import org.kalypso.module.welcome.INewProjectWizard;
import org.kalypso.module.welcome.INewProjectWizardProvider;
import org.kalypso.module.welcome.SpecialImportProjectAction;
import org.kalypso.ui.rrm.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.i18n.Messages;
import org.kalypso.ui.rrm.wizards.KalypsoNAProjectWizard;

public class KalypsoModuleRRM extends AbstractKalypsoModule
{
  public static final String ID = "KalypsRrmModel"; //$NON-NLS-1$

  // public constructor, needed because of declared extension point and java class loader
  public KalypsoModuleRRM( )
  {
  }

  @Override
  public String getHeader( )
  {
    return "KalypsoHydrology"; //$NON-NLS-1$
  }

  @Override
  public URL getInfoURL( )
  {
    return getInfoURL( getClass(), KalypsoUIRRMPlugin.getDefault() );
  }

  @Override
  public Integer getPriority( )
  {
    return 1;
  }

  @Override
  public IKalypsoModuleWelcomePageFrame getWelcomePageFrame( )
  {
    return new KalypsoRrmWelcomePageFrame();
  }

  @Override
  public String getId( )
  {
    return ID;
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
        return new KalypsoNAProjectWizard();
      }
    };
  }

  /**
   * @see org.kalypso.project.database.client.extension.AbstractKalypsoModule#getDemoProjectWizard()
   */
  @Override
  protected INewProjectWizardProvider getDemoProjectWizard( )
  {
    return null;
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
        return new ProjectConversionWizard( ID, KalypsoNAProjectWizard.CATEGORY_TEMPLATE );
      }
    };

    actions.add( new SpecialImportProjectAction( Messages.getString("KalypsoModuleRRM_0"), provider ) ); //$NON-NLS-1$
  }

  @Override
  public IKalypsoModuleProjectOpenAction getProjectOpenAction( )
  {
    return new KalypsoRRMOpenAction();
  }

  /**
   * @see org.kalypso.module.IKalypsoModule#acceptProject(org.eclipse.core.resources.IProject)
   */
  @Override
  public boolean acceptProject( final IProject project )
  {
    final IFile file = project.getFile( "hydrotop.gml" ); //$NON-NLS-1$
    return file.exists();
  }

}
