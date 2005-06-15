package com.bce.eind.core.profil;

import com.bce.eind.core.profil.IProfil.METADATA;

/**
 * Default implementation of {@link com.bce.eind.core.profil.IProfilListener}. The default
 * behavious is to do nothing.
 */
public class ProfilAdapter implements IProfilListener
{
  /**
   * Does nothing.
   * 
   * @see com.bce.eind.core.profil.IProfilListener#onPointValuesChanged(com.bce.eind.core.profil.ProfilChange[])
   */
  public void onPointValuesChanged( ProfilChange[] changes )
  {
  }

  /**
   * Does nothing.
   * 
   * @see com.bce.eind.core.profil.IProfilListener#onPointsAdded(com.bce.eind.core.profil.IProfilPoint[])
   */
  public void onPointsAdded( IProfilPoint[] newPoints )
  {
  }

  /**
   * Does nothing.
   * 
   * @see com.bce.eind.core.profil.IProfilListener#onPointsRemoved(com.bce.eind.core.profil.IProfilPoint[])
   */
  public void onPointsRemoved( IProfilPoint[] removedPoints )
  {
  }

  /**
   * Does nothing.
   * 
   * @see com.bce.eind.core.profil.IProfilListener#onPointPropertiesAdded(com.bce.eind.core.profil.ProfilPointProperty[])
   */
  public void onPointPropertiesAdded( ProfilPointProperty[] addedProperties )
  {
  }

  /**
   * Does nothing.
   * 
   * @see com.bce.eind.core.profil.IProfilListener#onPointPropertiesRemoved(com.bce.eind.core.profil.ProfilPointProperty[])
   */
  public void onPointPropertiesRemoved( ProfilPointProperty[] removedProperties )
  {
  }

  /**
   * Does nothing.
   * 
   * @see com.bce.eind.core.profil.IProfilListener#onBauwerkChanged()
   */
  public void onBauwerkChanged( )
  {
  }

  /**
   * Does nothing.
   * 
   * @see com.bce.eind.core.profil.IProfilListener#onMetaDataChanged(com.bce.eind.core.profil.IProfil.METADATA,
   *      java.lang.String)
   */
  public void onMetaDataChanged( METADATA metadata, Object value )
  {
  }

  /**
   * Does nothing.
   * 
   * @see com.bce.eind.core.profil.IProfilListener#onBauwerkDataChanged(com.bce.eind.core.profil.IProfilBuilding,
   *      com.bce.eind.core.profil.ProfilBuildingProperty, double)
   */
  public void onBauwerkDataChanged( final IProfilBuilding building,
      final ProfilBuildingProperty buildingProperty, double value )
  {
  }

  /**
   * Does nothing.
   * 
   * @see com.bce.eind.core.profil.IProfilListener#onCommentChanged()
   */
  public void onCommentChanged( )
  {
  }
}
