package com.bce.eind.core.profil;

import com.bce.eind.core.profil.IPlainProfil.DeviderTyp;
import com.bce.eind.core.profil.IPlainProfil.METADATA;

/**
 * Default implementation of {@link com.bce.eind.core.profil.IProfilListener}. The default
 * behavious is to do nothing.
 */
public class ProfilAdapter implements IProfilListener
{
  
  /**
   * Does nothing.
   * 
   * @see com.bce.eind.core.profil.IProfilListener#onDeviderChanged(com.bce.eind.core.profil.PointChange[])
   */
 
  public void onDeviderChanged( IProfilPoint point, DeviderTyp devider )
  {
     
  }

  /**
   * Does nothing.
   * 
   * @see com.bce.eind.core.profil.IProfilListener#onPointValuesChanged(com.bce.eind.core.profil.PointChange[])
   */
  public void onPointValuesChanged( PointChange[] changes )
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
   * @see com.bce.eind.core.profil.IProfilListener#onBuildingChanged()
   */
  public void onBuildingChanged( )
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
   * @see com.bce.eind.core.profil.IProfilListener#onBuildingDataChanged(com.bce.eind.core.profil.IProfilBuilding,
   *      com.bce.eind.core.profil.ProfilBuildingProperty, double)
   */
  public void onBuildingDataChanged( final IProfilBuilding building,
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
