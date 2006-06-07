package org.kalypso.model.wspm.ui.profil.wizard.pointsInsert;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.model.wspm.core.profil.IProfilPoints;


public interface IPointsSource extends IExecutableExtension
{
  public IProfilPoints getPoints( ) throws InvocationTargetException;

  public void createControl( final Composite parent );

  /** Write current state into settings. */
  public void saveState( final IDialogSettings settings );

  public String getLabel( );

  public String getDescription( );

  public String getID( );

  public Control getControl( final IDialogSettings settings );
}
