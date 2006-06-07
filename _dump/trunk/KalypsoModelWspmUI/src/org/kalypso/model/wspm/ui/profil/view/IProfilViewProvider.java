package org.kalypso.model.wspm.ui.profil.view;

import org.kalypso.model.wspm.core.profil.IProfilEventManager;

/**
 * @author gernot
 *
 */
public interface IProfilViewProvider extends IProfilProvider
{
  public ProfilViewData getViewData( );
  
  public IProfilEventManager getProfilEventManager();
}
