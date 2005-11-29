package com.bce.eind.core.profil;

import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;
import com.bce.eind.core.profil.changes.AbstractChange;
import com.bce.eind.core.profil.changes.BuildingChange;
import com.bce.eind.core.profil.changes.DeviderChange;
import com.bce.eind.core.profil.changes.PointChange;
import com.bce.eind.core.profil.changes.ProfilChange;


/**
 * Default implementation of {@link com.bce.eind.core.profil.IProfilListener}. The default
 * behavious is to do nothing.
 */
public class ProfilAdapter implements IProfilListener
{
  
  /**
   * Does nothing.
   * @see com.bce.eind.core.profil.IProfilListener#onBuildingAdded(com.bce.eind.core.profil.changes.BuildingChange[])
   */
  public void onBuildingAdded( BuildingChange[] changes )
  {
   
    
  }

  /**
   * Does nothing.
   * @see com.bce.eind.core.profil.IProfilListener#onBuildingRemoved(com.bce.eind.core.profil.changes.BuildingChange[])
   */
  public void onBuildingRemoved( BuildingChange[] changes )
  {
   
    
  }

  /**
   * Does nothing.
   * @see com.bce.eind.core.profil.IProfilListener#onDeviderAdded(com.bce.eind.core.profil.changes.DeviderChange[])
   */
  public void onDeviderAdded( DeviderChange[] changes )
  {
   
    
  }

  /**
   * Does nothing.
   * @see com.bce.eind.core.profil.IProfilListener#onDeviderChanged(com.bce.eind.core.profil.changes.DeviderChange[])
   */
  public void onDeviderChanged( DeviderChange[] changes )
  {
   
    
  }

  /**
   * Does nothing.
   * @see com.bce.eind.core.profil.IProfilListener#onDeviderRemoved(com.bce.eind.core.profil.changes.DeviderChange[])
   */
  public void onDeviderRemoved( DeviderChange[] changes )
  {
  
    
  }

  /**
   * Does nothing.
   * @see com.bce.eind.core.profil.IProfilListener#onPointPropertiesAdded(com.bce.eind.core.profil.changes.PointChange[])
   */
  public void onPointPropertiesAdded( PointChange[] changes )
  {
 
    
  }

  /**
   * Does nothing.
   * @see com.bce.eind.core.profil.IProfilListener#onPointPropertiesRemoved(com.bce.eind.core.profil.changes.PointChange[])
   */
  public void onPointPropertiesRemoved( PointChange[] changes )
  {
 
    
  }

  /**
   * Does nothing.
   * @see com.bce.eind.core.profil.IProfilListener#onPointsAdded(com.bce.eind.core.profil.changes.PointChange[])
   */
  public void onPointsAdded( PointChange[] changes )
  {
      
  }

  /**
   * Does nothing.
   * @see com.bce.eind.core.profil.IProfilListener#onPointsChanged(com.bce.eind.core.profil.changes.PointChange[])
   */
  public void onPointsChanged( PointChange[] changes )
  {
        
  }

  /**
   * Does nothing.
   * @see com.bce.eind.core.profil.IProfilListener#onPointsRemoved(com.bce.eind.core.profil.changes.PointChange[])
   */
  public void onPointsRemoved( PointChange[] changes )
  {
     
  }

  /**
   * Does nothing.
   * @see com.bce.eind.core.profil.IProfilListener#onProfilDataChanged(com.bce.eind.core.profil.changes.ProfilChange[])
   */
  public void onProfilDataChanged( ProfilChange[] changes )
  {
       
  }

  

  /**
   * Does nothing.
   * 
   * @see com.bce.eind.core.profil.IProfilListener#onBuildingChanged(BuildingChange[])
   */
  public void onBuildingChanged( BuildingChange[] changes )
  {
  }

  


  


}
