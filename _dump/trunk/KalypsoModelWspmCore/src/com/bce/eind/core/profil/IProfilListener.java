package com.bce.eind.core.profil;

import com.bce.eind.core.profil.IPlainProfil.DeviderTyp;


/**
 * @author Belger
 */
public interface IProfilListener
{
  /** Wird aufgerufen, wenn sich ein oder mehrere Werte eines Punktes ge�ndert haben */
  public void onPointValuesChanged( final PointChange[] changes );

  public void onPointsAdded( final IProfilPoint[] newPoints );
  
  public void onPointsRemoved( final IProfilPoint[] removedPoints );
  
  public void onPointPropertiesAdded( final ProfilPointProperty[] addedProperties );
  
  public void onPointPropertiesRemoved( final ProfilPointProperty[] removedProperties );
  
  public void onBuildingChanged();

  public void onBuildingDataChanged( final IProfilBuilding building, final ProfilBuildingProperty buildingProperty, final double value );
  
  public void onMetaDataChanged( final IProfil.METADATA metadata, final Object value );
  
  public void onCommentChanged();
  
  public void onDeviderChanged( final IProfilPoint point, final DeviderTyp devider );
}