/**
 * 
 */
package org.kalypso.model.wspm.ui.profil.wizard.pointsInsert.impl;


import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.kalypso.model.wspm.core.profil.IProfilPoints;
import org.kalypso.model.wspm.ui.profil.wizard.pointsInsert.AbstractPointsSource;


/**
 * @author Gernot
 * 
 */
public class SinglePointsSource extends AbstractPointsSource
{
  /**
   * @see org.kalypso.model.wspm.ui.profil.wizard.pointsInsert.AbstractPointsSource#doCreateControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control doCreateControl( final Composite parent )
  {
    return new Label( parent, SWT.NONE );
  }

  /**
   * @see org.kalypso.model.wspm.ui.profil.wizard.pointsInsert.IPointsSource#getPoints()
   */
  public IProfilPoints getPoints( )
  {
    // ein einzelnen punkt interpolieren
    
    // braucht das profil als randbedingung
    return null;
  }

  @Override
  protected void loadState( IDialogSettings settings )
  {
    // TODO Auto-generated method stub
    
  }

  public void saveState( IDialogSettings settings )
  {
    // TODO Auto-generated method stub
    
  }

}
