package org.kalypso.model.wspm.ui.profil.view;

import org.eclipse.core.resources.IFile;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;

/**
 * @author Gernot Belger
 */
public interface IProfilProvider2
{
  public IProfilEventManager getEventManager();
  
  public ProfilViewData getViewData();
  
  public IFile getFile( );

  public void addProfilProviderListener( final IProfilProviderListener l );
  
  public void removeProfilProviderListener( final IProfilProviderListener l );

  public void dispose( );
}
