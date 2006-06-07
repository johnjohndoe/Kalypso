/**
 * 
 */
package org.kalypso.model.wspm.ui.profil.wizard.pointsInsert;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPoints;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;


/**
 * @author Gernot
 * 
 */
public class InsertPointsWizard extends Wizard
{
  protected final IProfilEventManager m_pem;

  protected PointsSourceChooserPage m_pointsSourceChooserPage;

  protected PointsTargetChooserPage m_pointsTargetChooserPage;

  public InsertPointsWizard( final IProfilEventManager pem )
  {
    m_pem = pem;

    final IDialogSettings wizardSettings = PluginUtilities.getDialogSettings( KalypsoModelWspmUIPlugin.getDefault(), "InsertPointsWizardSettings" ); //$NON-NLS-1$
    setDialogSettings( wizardSettings );

    setWindowTitle( "Profilpunkte einfügen" );
    setNeedsProgressMonitor( true );
  }

  @Override
  public void addPages( )
  {
    super.addPages();

    m_pointsSourceChooserPage = new PointsSourceChooserPage();
    m_pointsTargetChooserPage = new PointsTargetChooserPage();

    addPage( m_pointsSourceChooserPage );
    addPage( m_pointsTargetChooserPage );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    final IRunnableWithProgress runnable = new IRunnableWithProgress()
    {
      public void run( final IProgressMonitor monitor ) throws InvocationTargetException
      {
        final IPointsSource choosenSource = m_pointsSourceChooserPage.getChoosenSource();
        final IPointsTarget selectedTarget = m_pointsTargetChooserPage.getSelectedTarget();

        final IProfilPoints points = choosenSource.getPoints();
        selectedTarget.insertPoints( m_pem, points );
      }
    };

    try
    {
      getContainer().run( false, true, runnable );
      return true;
    }
    // TODO: use helper methods from kalypso to extrakt status an show error dialog
    catch( final InvocationTargetException e )
    {
      e.printStackTrace();
      // TODO: error handling
      return false;
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
      return false;
    }
  }
}
