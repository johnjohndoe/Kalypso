package com.bce.eind.core.profil;

import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;
import com.bce.eind.core.profil.changes.BuildingDataChange;
import com.bce.eind.core.profil.changes.PointChange;



/**
 * @author Belger
 */
public interface IProfilListener
{
  /** Wird aufgerufen, wenn sich ein oder mehrere Werte eines Punktes geändert haben */
  public void onPointValuesChanged( final PointChange[] changes );

  public void onPointsAdded( final IProfilPoint[] newPoints );
  
  public void onPointsRemoved( final IProfilPoint[] removedPoints );
  
  public void onPointPropertiesAdded( final POINT_PROPERTY[] addedProperties );
  
  public void onPointPropertiesRemoved( final POINT_PROPERTY[] removedProperties );
  
  public void onBuildingChanged(final IProfilBuilding oldBuilding);

  public void onBuildingDataChanged( BuildingDataChange[] changes );
  
  public void onMetaDataChanged( final Object key, final Object value );
  /**
   * 
   * @param oldPoint wenn != Null wurde der Trenner verschoben
   * @param devider
   * @param property TODO
   */
  public void onDeviderChanged( final IProfilPoint oldPoint, final IProfilDevider devider, Object property );
  
  public void onDeviderRemoved(final IProfilDevider[] deviders);
}