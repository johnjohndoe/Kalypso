package com.bce.eind.core.profil;

import java.util.EventListener;

import com.bce.eind.core.profil.impl.points.ProfilPointProperty;


/**
 * @author Belger
 * 
 * TODO: Kim mit mir diese Klasse diskutieren 
 * 
 * wird zurzeit noch nicht verwendet
 * 
 * TODO: IProfilListener
 */
public interface ProfilListener extends EventListener
{
  // onPointChanged
  public void onPointChanged( final IProfilPoint point,final ProfilPointProperty pointProperty );

  // spalte verschwindet oder wird neu angelegt
 
  // onMetadataChanged
  public void onMetaDataChanged( final IProfil.METADATA metadata,final String value );
  
  // ??
  public void onProfilDataChanged(final ProfilPointProperty pointProperty,final Object value); 
}