package com.bce.eind.core.profil;

import com.bce.eind.core.profil.IProfilConstants.BUILDING_PROPERTY;



/**
 * @author Belger
 */
public interface IProfilListener
{
  /** Wird aufgerufen, wenn sich ein oder mehrere Werte eines Punktes geändert haben */
  public void onPointValuesChanged( final PointChange[] changes );

  public void onPointsAdded( final IProfilPoint[] newPoints );
  
  public void onPointsRemoved( final IProfilPoint[] removedPoints );
  
  public void onPointPropertiesAdded( final PointProperty[] addedProperties );
  
  public void onPointPropertiesRemoved( final PointProperty[] removedProperties );
  
  public void onBuildingChanged();

  public void onBuildingDataChanged( final IProfilBuilding building, final BUILDING_PROPERTY buildingProperty, final double value );
  
  public void onMetaDataChanged( final Object key, final Object value );
  
  public void onCommentChanged();
  
  public void onDeviderChanged( final IProfilPoint point, final IProfilDevider devider );
}