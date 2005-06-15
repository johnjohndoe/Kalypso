package com.bce.eind.core.profil;


/**
 * @author Belger
 */
public interface IProfilListener
{
  /** Wird aufgerufen, wenn sich ein oder mehrere Werte eines Punktes geändert haben */
  public void onPointValuesChanged( final ProfilChange[] changes );

  public void onPointsAdded( final IProfilPoint[] newPoints );
  
  public void onPointsRemoved( final IProfilPoint[] removedPoints );
  
  public void onPointPropertiesAdded( final ProfilPointProperty[] addedProperties );
  
  public void onPointPropertiesRemoved( final ProfilPointProperty[] removedProperties );
  
  public void onBauwerkChanged();

  public void onBauwerkDataChanged( final IProfilBuilding building, final ProfilBuildingProperty buildingProperty, final double value );
  
  public void onMetaDataChanged( final IProfil.METADATA metadata, final Object value );
  
  public void onCommentChanged();
}