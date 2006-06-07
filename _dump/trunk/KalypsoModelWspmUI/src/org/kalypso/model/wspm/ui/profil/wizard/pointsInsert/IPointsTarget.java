package org.kalypso.model.wspm.ui.profil.wizard.pointsInsert;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IExecutableExtension;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPoints;


public interface IPointsTarget extends IExecutableExtension
{
  public void insertPoints( final IProfilEventManager pem, final IProfilPoints points ) throws InvocationTargetException;

  public String getLabel( );

  public String getDescription( );

  public String getID( );

}
