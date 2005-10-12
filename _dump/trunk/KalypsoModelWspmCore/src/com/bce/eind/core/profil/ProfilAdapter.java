package com.bce.eind.core.profil;

import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;
import com.bce.eind.core.profil.changes.BuildingDataChange;
import com.bce.eind.core.profil.changes.PointChange;


/**
 * Default implementation of {@link com.bce.eind.core.profil.IProfilListener}. The default
 * behavious is to do nothing.
 */
public class ProfilAdapter implements IProfilListener
{
  
  /**
   * Does nothing.
   * 
   * @see com.bce.eind.core.profil.IProfilListener#onBuildingChanged()
   */
  public void onBuildingChanged(final IProfilBuilding oldBuilding )
  {
  }

  /**
   * Does nothing.
   * 
   * @see com.bce.eind.core.profil.IProfilListener#onBuildingDataChanged(BuildingDataChange[])
   */
  public void onBuildingDataChanged( BuildingDataChange[] changes )
  {
  }

  /**
   * Does nothing.
   * 
   * @see com.bce.eind.core.profil.IProfilListener#onDeviderChanged(com.bce.eind.core.profil.changes.PointChange[])
   */
 
  public void onDeviderChanged( IProfilPoint oldPoint, IProfilDevider devider )
  {
     
  }

  /**
   * Does nothing.
   * 
   * @see com.bce.eind.core.profil.IProfilListener#onDeviderRemoved( IProfilDevider[] deviders )
   */
  public void onDeviderRemoved( IProfilDevider[] deviders )
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
   * @see com.bce.eind.core.profil.IProfilListener#onPointPropertiesAdded(com.bce.eind.core.profil.POINT_PROPERTY[])
   */
  public void onPointPropertiesAdded( POINT_PROPERTY[] addedProperties )
  {
  }

  /**
   * Does nothing.
   * 
   * @see com.bce.eind.core.profil.IProfilListener#onPointPropertiesRemoved(com.bce.eind.core.profil.POINT_PROPERTY[])
   */
  public void onPointPropertiesRemoved( POINT_PROPERTY[] removedProperties )
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
   * @see com.bce.eind.core.profil.IProfilListener#onPointValuesChanged(com.bce.eind.core.profil.changes.PointChange[])
   */
  public void onPointValuesChanged( PointChange[] changes )
  {
  }

  


}
