package com.bce.eind.core.profil;

import com.bce.eind.core.profil.IProfilConstants.BUILDING_PROPERTY;


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
 
  public void onDeviderChanged( IProfilPoint point, IProfilDevider devider )
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
   * @see com.bce.eind.core.profil.IProfilListener#onPointPropertiesAdded(com.bce.eind.core.profil.PointProperty[])
   */
  public void onPointPropertiesAdded( PointProperty[] addedProperties )
  {
  }

  /**
   * Does nothing.
   * 
   * @see com.bce.eind.core.profil.IProfilListener#onPointPropertiesRemoved(com.bce.eind.core.profil.PointProperty[])
   */
  public void onPointPropertiesRemoved( PointProperty[] removedProperties )
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
  public void onMetaDataChanged( Object key, Object value )
  {
  }

  /**
   * Does nothing.
   * 
   * @see com.bce.eind.core.profil.IProfilListener#onBuildingDataChanged(com.bce.eind.core.profil.IProfilBuilding,
   *      com.bce.eind.core.profil.BUILDING_PROPERTY, double)
   */
  public void onBuildingDataChanged( final IProfilBuilding building,
      final BUILDING_PROPERTY buildingProperty, double value )
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
