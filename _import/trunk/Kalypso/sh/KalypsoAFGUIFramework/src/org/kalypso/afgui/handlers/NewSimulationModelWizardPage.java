/**
 * 
 */
package org.kalypso.afgui.handlers;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.afgui.scenarios.Scenario;

/**
 * @author pat_dev
 */
public class NewSimulationModelWizardPage extends WizardPage
{
  // private IWorkflowData workflowData;

  NewSimulationModelControlBuilder c;

  IUpdateListener updateListener = new IUpdateListener()
  {
    public void update( )
    {
      boolean status = c.isValid();
      if( status )
      {
        NewSimulationModelWizardPage.this.setErrorMessage( null );
      }
      else
      {
        NewSimulationModelWizardPage.this.setErrorMessage( c.getErrorMessage() );
      }
      NewSimulationModelWizardPage.this.setPageComplete( status );
    }
  };

  private final Scenario m_scenario;

  public NewSimulationModelWizardPage( final String pageName, final Scenario workflowData )
  {
    super( pageName );
    m_scenario = workflowData;
    super.setTitle( pageName );
  }

  public void createControl( Composite parent )
  {
    c = new NewSimulationModelControlBuilder( m_scenario, parent );
    setControl( c.getControl() );
    c.setUpdateListerner( updateListener );
    updateListener.update();
  }

  @Override
  public boolean isPageComplete( )
  {
    return super.isPageComplete();
  }

  public NewSimulationModelControlBuilder getNewSimulaionControlBuilder( )
  {
    return c;
  }
}