package org.kalypso.ui.calcwizard;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;

public class CalcWizard extends Wizard
{
  private final IFolder m_folder;

  public CalcWizard( final IFolder calcCaseFolder )
  {
    m_folder = calcCaseFolder;
  }

  public boolean performFinish()
  {
    final IWizardPage[] pages = getPages();
    for( int i = 0; i < pages.length; i++ )
    {
      final ICalcWizardPage page = (ICalcWizardPage)pages[i];
      final boolean bFinished = page.performFinish();
      if( bFinished == false )
        return false;
    }

    return true;
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performCancel()
   */
  public boolean performCancel()
  {
    // den Rechenfall löschen
    final IFolder calcFolder = m_folder;
    final Job job = new Job( "Rechenfall löschen" )
    {
      protected IStatus run( IProgressMonitor monitor )
      {
        try
        {
          calcFolder.delete( false, false, monitor );
        }
        catch( final CoreException e )
        {
          return e.getStatus();
        }
        
        return Status.OK_STATUS;
      }
    };
    job.schedule();
    try
    {
      job.join();
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
    }

    return super.performCancel();
  }
}