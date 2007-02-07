package org.kalypso.kalypso1d2d.pjt;

import org.apache.log4j.Logger;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.ui.wizards.newresource.BasicNewProjectResourceWizard;

/**
 * Creates a new Kalypso 1d 2d Project
 * 
 * @author Patrice Congo
 *
 */
public class Kalypso1D2DNewProjectWizard extends BasicNewProjectResourceWizard
{
	final static public String ID=
			"org.kalypso.kalypso1d2d.pjt.Kalypso1D2DNewProjectWizard"; 
	final static private Logger logger= 
				Logger.getLogger(Kalypso1D2DNewProjectWizard.class);
	public Kalypso1D2DNewProjectWizard()
	{
		//empty
	}
	
    @Override
    /**
     * This method was overriden in order to get rid of the 
     * 'select dependend projects' page from the BasicNewProjectResourceWizard.
     */
    public IWizardPage getNextPage( IWizardPage page )
    {
      // HACK: to do so, we just skip this particular page
      // Unfortunateley we cannot just overide 'addPages' and do not add the secod page,
      //because the BasicNewProjectResourceWizard relies on the second page to exist.
      final IWizardPage[] pages = getPages();

      if( page.equals( pages[0] ) )
        return null;

      return super.getNextPage( page );
    }
	
	@Override
  public boolean performFinish() {
		boolean result = super.performFinish();
		final String MSG="Error while adding natur od metadata folder"; 
		
		if (!result)
		{
			return false;
		}
		else
		{
			IProject project = getNewProject();
			try 
			{
				Kalypso1D2DProjectNature.addNature( project );
                
                /* Also activate new project */
                ActiveWorkContext.getInstance().setActiveProject( project );
                
				logger.info("DADADADADA:\n"+project);
			} 
			catch (CoreException e) 
			{
				logger.info(MSG, e);
				Kalypso1d2dProjectPlugin.getDefault().showException("", e);
				return false;
			}
//			catch (IOException e)
//			{
//				logger.info(MSG, e);
//				Kalypso1d2dProjectPlugin.getDefault().showException(MSG, e);
//				return false;
//			}
			catch(Throwable th)
			{
				logger.error(MSG, th);
				return false;
			}
			return true;
		}
		
	}
}
