package org.kalypso.ui.calcwizard;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.kalypso.ui.nature.ModelNature;

/**
 * @author belger
 */
public class CreateCalcCaseWizardPage extends AbstractCalcWizardPage
{
  public CreateCalcCaseWizardPage()
  {
    super( "Rechenfallerzeugen" );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Text text = new Text( parent, SWT.NONE );
    text.setText( "Rechenfall erzeugen" );
  }

  /**
   * @see org.kalypso.ui.calcwizard.ICalcWizardPage#performFinish()
   */
  public boolean performFinish()
  {
    IRunnableWithProgress runnable = new IRunnableWithProgress()
    {
      public void run( IProgressMonitor monitor ) throws InvocationTargetException,
          InterruptedException
      {
        try
        {
          ModelNature.createCalculationCaseInFolder( null, monitor );
        }
        catch( final Exception e )
        {
          e.printStackTrace();
          
          throw new InvocationTargetException( e, "Failed to create CalcCase" );
        }

        
        if( monitor.isCanceled() )
          throw new InterruptedException();
      }
    };

    try
    {
      getWizard().getContainer().run( false, true, runnable );
    }
    catch( InvocationTargetException e )
    {
      e.printStackTrace();
      return false;
    }
    catch( InterruptedException e )
    {
      e.printStackTrace();
      
      return false;
    }

    return true;
  }
}